-module(plugin_mqtt_proxy_worker).

-include_lib("emqtt/include/emqtt.hrl").
-include_lib("emqx/include/logger.hrl").

-export([start_link/1, on_publish/2, on_disconnected/2]).

start_link(ConnOpts) ->
    %% Opts = [ {host, Host}
    %%        , {port, Port}
    %%        , {clientid, ClientId}
    %%        , {ssl, true}
    %%        , {ssl_opts, [ {cacerts, [DERCertificate]}
    %%                     , {keyfile, Keyfile}
    %%                     ]}
    %%        ],
    ?SLOG(warning, #{
        msg => "starting_proxy",
        conn_opts => ConnOpts
    }),
    case emqtt:start_link(ConnOpts) of
        {ok, Pid} ->
            case emqtt:connect(Pid) of
                {ok, _} ->
                    {ok, Pid};
                Error ->
                    catch emqtt:stop(Pid),
                    Error
            end;
        Error ->
            Error
    end.

on_publish(Message, ConnPid) ->
    ?SLOG(warning, #{
        msg => "on_publish",
        pub_message => Message,
        conn_pid => ConnPid
    }),
    #{
        topic := Topic,
        dup := Dup,
        qos := QoS,
        retain := Retain,
        payload := Payload,
        properties := Props
    } = Message,
    %% DeliveryMessage = emqx_message:from_map(Message),
    %% FIXME: headers?
    DeliveryMessage =
        emqx_message:set_flags(
            #{dup => Dup, retain => Retain},
            emqx_message:make(proxy, QoS, Topic, Payload)
        ),
    ?SLOG(warning, #{
        msg => "on_publish",
        delivery_message => DeliveryMessage,
        conn_pid => ConnPid
    }),
    ConnPid ! {deliver, Topic, DeliveryMessage},
    ok.

on_disconnected({ReasonCode, Properties}, ConnPid) when is_map(Properties) ->
    ReasonName = disconnect_reason(ReasonCode),
    ?SLOG(warning, #{
        msg => "on_disconnected",
        reason => {ReasonCode, ReasonName, Properties},
        conn_pid => ConnPid
    }),
    ConnPid ! {disconnect, ReasonCode, ReasonName, Properties},
    ok;
on_disconnected(Reason, ConnPid) ->
    Properties = #{},
    ReasonCode = ?RC_UNSPECIFIED_ERROR,
    ReasonName = disconnect_reason(ReasonCode),
    ?SLOG(warning, #{
        msg => "on_disconnected",
        reason => {ReasonCode, ReasonName, Properties},
        conn_pid => ConnPid
    }),
    ConnPid ! {disconnect, ReasonCode, ReasonName, Properties},
    ok.

%% From emqx_channel; not exported
disconnect_reason(?RC_SUCCESS) -> normal;
disconnect_reason(ReasonCode) -> emqx_reason_codes:name(ReasonCode).
