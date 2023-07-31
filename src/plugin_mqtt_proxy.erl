-module(plugin_mqtt_proxy).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").
-include_lib("emqtt/include/emqtt.hrl").

%% for logging
-include_lib("emqx/include/logger.hrl").

-export([
    load/1,
    unload/0
]).

%% Client Lifecycle Hooks
-export([
    on_client_connect/3,
    on_client_connack/4,
    on_client_connected/3,
    on_client_disconnected/4,
    on_client_authenticate/3,
    on_client_authorize/5,
    on_client_subscribe/4,
    on_client_unsubscribe/4
]).

%% Session Lifecycle Hooks
-export([
    on_session_created/3,
    on_session_subscribed/4,
    on_session_unsubscribed/4,
    on_session_resumed/3,
    on_session_discarded/3,
    on_session_takenover/3,
    on_session_terminated/4
]).

%% Message Pubsub Hooks
-export([
    on_message_publish/2,
    on_message_delivered/3,
    on_message_acked/3,
    on_message_dropped/4
]).

-define(PROXY_PID_KEY, {?MODULE, proxy_pid}).
-define(WITH_PROXY_PID(BIND_VAR, EXPR),
    case get(?PROXY_PID_KEY) of
        BIND_VAR when is_pid(BIND_VAR) ->
            (EXPR);
        _ ->
            ignore
    end
).
-define(HOOKS, #{
    'client.connect' => on_client_connect,
    'client.disconnected' => on_client_disconnected,
    'client.subscribe' => on_client_subscribe,
    'client.unsubscribe' => on_client_unsubscribe,
    'session.resumed' => on_session_resumed,
    'message.publish' => on_message_publish
}).

%% Called when the plugin application start
load(Env) ->
    maps:foreach(
        fun(Hookpoint, FnName) ->
            hook(Hookpoint, {?MODULE, FnName, [Env]})
        end,
        ?HOOKS
    ),
    ok.

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

on_client_connect(_ConnInfo = #{peercert := undefined, socktype := ssl}, _Props, _Env) ->
    ?SLOG(warning, #{
        msg => "no_peercert",
        hint => "set verify = verify_peer in EMQX TLS listener"
    }),
    {stop, {error, ?RC_BAD_USER_NAME_OR_PASSWORD}};
on_client_connect(ConnInfo = #{socktype := ssl}, Props, Env) ->
    ?SLOG(warning, #{
        msg => "demo_log_msg_on_client_connect",
        me => self(),
        env => Env,
        conninfo => ConnInfo,
        props => Props
    }),
    #{
        clientid := ClientId,
        peercert := DERCertificate,
        %% , expiry_interval := ExpiryInterval
        keepalive := KeepAlive,
        proto_ver := ProtoVer0
    } = ConnInfo,
    PrivateKeysDir = persistent_term:get({?MODULE, private_keys_dir}),
    Host = persistent_term:get({?MODULE, remote_host}),
    Port = persistent_term:get({?MODULE, remote_port}),
    Keyfile = filename:join([to_bin(PrivateKeysDir), [ClientId, <<".pem">>]]),
    ConnPid = self(),
    ProtoVer =
        case ProtoVer0 of
            ?MQTT_PROTO_V3 -> v3;
            ?MQTT_PROTO_V4 -> v4;
            ?MQTT_PROTO_V5 -> v5
        end,
    ConnOpts = [
        {host, Host},
        {port, Port},
        {clientid, ClientId},
        {owner, self()},
        {proto_ver, ProtoVer},
        {keepalive, KeepAlive},
        {ssl, true},
        {ssl_opts, [
            {cert, [DERCertificate]},
            {keyfile, Keyfile}
        ]},
        {reconnect, 0},
        {msg_handler, #{
            publish => {plugin_mqtt_proxy_worker, on_publish, [ConnPid]},
            disconnected => {plugin_mqtt_proxy_worker, on_disconnected, [ConnPid]}
        }}
    ],
    case plugin_mqtt_proxy_worker_sup:start_child(ConnOpts) of
        {ok, Pid} ->
            put(?PROXY_PID_KEY, Pid),
            %% the exit signal can disrupt the takeover process
            unlink(Pid),
            {ok, Props};
        Error ->
            ?SLOG(warning, #{
                msg => "failed_to_connect_to_remote_broker",
                clientid => ClientId,
                error => Error
            }),
            {stop, {error, ?RC_UNSPECIFIED_ERROR}}
    end;
on_client_connect(_ConnInfo, Props, _Env) ->
    {ok, Props}.

%% unused
on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
    io:format(
        "Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
        [ClientId, ConnInfo, Rc, Props]
    ),
    {ok, Props}.

%% unused
on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    io:format(
        "Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
        [ClientId, ClientInfo, ConnInfo]
    ).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
    io:format(
        "Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
        [ClientId, ReasonCode, ClientInfo, ConnInfo]
    ),
    ?WITH_PROXY_PID(Pid, ok = plugin_mqtt_proxy_worker_sup:ensure_child_deleted(Pid)),
    ok.

%% unused
on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, Env) ->
    io:format(
        "Client(~s) authenticate, ClientInfo:~n~p~n, Result:~p,~nEnv:~p~n",
        [ClientId, ClientInfo, Result, Env]
    ),
    {ok, Result}.

%% unused
on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, Result, Env) ->
    io:format(
        "Client(~s) authorize, ClientInfo:~n~p~n, ~p to topic(~s) Result:~p,~nEnv:~p~n",
        [ClientId, ClientInfo, PubSub, Topic, Result, Env]
    ),
    {ok, Result}.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    ?WITH_PROXY_PID(Pid, begin
        ?SLOG(
            warning,
            #{
                msg => "proxy_will_subscribe",
                clientid => ClientId,
                topic_filters => TopicFilters
            }
        ),
        Props = #{},
        Topics = [{TopicFilter, maps:to_list(SubOpts)} || {TopicFilter, SubOpts} <- TopicFilters],
        {ok, _, _} = emqtt:subscribe(Pid, Props, Topics),
        ok
    end),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    ?WITH_PROXY_PID(Pid, begin
        ?SLOG(
            warning,
            #{
                msg => "proxy_will_unsubscribe",
                clientid => ClientId,
                topic_filters => TopicFilters
            }
        ),
        Props = #{},
        Topics = [TopicFilter || {TopicFilter, _SubOpts} <- TopicFilters],
        {ok, _, _} = emqtt:unsubscribe(Pid, Props, Topics),
        ok
    end),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecycle Hooks
%%--------------------------------------------------------------------

%% unused
on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

%% unused
on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

%% unused
on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    ?WITH_PROXY_PID(Pid, begin
        ?SLOG(
            warning,
            #{
                msg => "proxy_resuming_session",
                clientid => ClientId,
                session_info => SessInfo
            }
        ),
        #{subscriptions := Subs} = SessInfo,
        Props = #{},
        Topics = [
            {TopicFilter, maps:to_list(SubOpts)}
         || {TopicFilter, SubOpts} <- maps:to_list(Subs)
        ],
        {ok, _, _} = emqtt:subscribe(Pid, Props, Topics),
        ok
    end),
    ok.

%% unused
on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

%% unused
on_session_takenover(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takenover. Session Info: ~p~n", [ClientId, SessInfo]).

%% unused
on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format(
        "Session(~s) is terminated due to ~p~nSession Info: ~p~n",
        [ClientId, Reason, SessInfo]
    ).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message, _Env) ->
    ?WITH_PROXY_PID(Pid, begin
        ?SLOG(
            warning,
            #{
                msg => "proxy_forwarding_msg",
                message => Message
            }
        ),
        #{
            flags := #{retain := Retain},
            headers := #{properties := Properties},
            payload := Payload,
            qos := QoS,
            topic := Topic
        } = emqx_message:to_map(Message),
        _ = emqtt:publish(Pid, Topic, Properties, Payload, [{qos, QoS}, {retain, Retain}]),
        ok
    end),
    {ok, Message}.

%% unused
on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
    io:format(
        "Message dropped by node ~p due to ~p:~n~p~n",
        [Node, Reason, emqx_message:to_map(Message)]
    ).

%% unused
on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format(
        "Message delivered to client(~s):~n~p~n",
        [ClientId, emqx_message:to_map(Message)]
    ),
    {ok, Message}.

%% unused
on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format(
        "Message acked by client(~s):~n~p~n",
        [ClientId, emqx_message:to_map(Message)]
    ).

%% Called when the plugin application stop
unload() ->
    maps:foreach(
        fun(Hookpoint, FnName) ->
            unhook(Hookpoint, {?MODULE, FnName})
        end,
        ?HOOKS
    ),
    ok.

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).

to_bin(Str) when is_list(Str) -> unicode:characters_to_binary(Str, utf8);
to_bin(Bin) when is_binary(Bin) -> Bin.
