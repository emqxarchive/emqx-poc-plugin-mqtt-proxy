-module(plugin_mqtt_proxy_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, ensure_child_deleted/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ConnOpts) ->
    supervisor:start_child(?MODULE, [ConnOpts]).

ensure_child_deleted(Pid) ->
    case supervisor:terminate_child(?MODULE, Pid) of
        ok ->
            supervisor:delete_child(?MODULE, Pid),
            ok;
        {error, not_found} ->
            ok
    end.

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1_000,
        period => 100
    },
    ChildSpec = #{
        id => proxy_conn,
        start => {plugin_mqtt_proxy_worker, start_link, []},
        type => worker,
        %% Might get disconnected and then enter a crash loop if we set to `transient'
        restart => temporary
    },
    {ok, {SupFlags, [ChildSpec]}}.
