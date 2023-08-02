-module(plugin_mqtt_proxy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 100
    },
    ChildSpecs = [
        %% #{
        %%     id => plugin_mqtt_proxy_worker_sup,
        %%     start => {plugin_mqtt_proxy_worker_sup, start_link, []},
        %%     type => supervisor,
        %%     restart => permanent,
        %%     shutdown => infinity
        %% }
    ],
    {ok, {SupFlags, ChildSpecs}}.
