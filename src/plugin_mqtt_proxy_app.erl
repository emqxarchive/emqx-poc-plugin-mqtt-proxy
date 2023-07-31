-module(plugin_mqtt_proxy_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = plugin_mqtt_proxy_sup:start_link(),
    Env = maps:from_list(application:get_all_env()),
    maps:foreach(
      fun(K, V) ->
        persistent_term:put({plugin_mqtt_proxy, K}, V)
      end,
      Env),
    plugin_mqtt_proxy:load(Env),

    emqx_ctl:register_command(plugin_mqtt_proxy, {plugin_mqtt_proxy_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(plugin_mqtt_proxy),
    plugin_mqtt_proxy:unload().
