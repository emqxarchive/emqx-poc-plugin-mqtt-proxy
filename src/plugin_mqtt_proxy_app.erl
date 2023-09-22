-module(plugin_mqtt_proxy_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = plugin_mqtt_proxy_sup:start_link(),
    Env = load_config_from_env(),
    maps:foreach(
        fun(K, V) ->
            persistent_term:put({plugin_mqtt_proxy, K}, V)
        end,
        Env
    ),
    plugin_mqtt_proxy:load(Env),

    emqx_ctl:register_command(plugin_mqtt_proxy, {plugin_mqtt_proxy_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(plugin_mqtt_proxy),
    plugin_mqtt_proxy:unload().

load_config_from_env() ->
    RemoteHost = os:getenv("PROXYPLUGIN_REMOTE_HOST", "127.0.0.1"),
    RemotePort = list_to_integer(os:getenv("PROXYPLUGIN_REMOTE_PORT", "48883")),
    PrivateKeysDir = os:getenv("PROXYPLUGIN_PRIVATE_KEYS_DIR", "/tmp/privkeys/"),
    #{
        private_keys_dir => PrivateKeysDir,
        remote_host => RemoteHost,
        remote_port => RemotePort
    }.
