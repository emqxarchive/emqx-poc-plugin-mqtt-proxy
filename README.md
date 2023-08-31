# plugin_mqtt_proxy

## Assumptions

1. The private keys are all located in the file system where EMQX is running.
2. The OS user running EMQX has read permissions for all keys.
3. The private keys filenames have the form `${clientid}.pem`, when `${clientid}` exactly matches the MQTT Client ID of the connecting client.
4. If a matching private key for a connecting client is not found, the client is allowed connection and no proxied connection to the remote broker is made.
5. Peer certificate is only available when `verify = verify_peer` in the listener.  Therefore, if `verify = verify_none` and the peer certificate info is missing, we abort the connection.
6. We don't rely on "local" (EMQX) persistent sessions, only on the remote broker's persistent sessions capabilities.
7. If either client <-> EMQX or EMQX <-> remote broker connections is stopped, the other is also stopped.

## Configuration

In order to configure this plugin, the following configuration options must be set:

- `private_keys_dir`: the directory on EMQX's file system that contains **all** client private keys to be used.  They must all be in PEM format, and the filename must match that client's `clientid`.
- `remote_host`: the hostname or IP of the remote broker to connect to.
- `remote_port`: the port of the remote broker's TLS listener.

Those options must be set on all EMQX nodes.  In order to do so, the following command may be executed on one of EMQX's nodes:

```sh
emqx eval 'erpc:multicall(emqx:running_nodes(), fun() -> ok = persistent_term:put({plugin_mqtt_proxy, private_keys_dir}, "/path/to/private/keys"), ok = persistent_term:put({plugin_mqtt_proxy, remote_host}, "remote.broker.host"), ok = persistent_term:put({plugin_mqtt_proxy, remote_port}, 8883) end).'
```

Note: this command must be run **after** the plugin is installed.

## Release

An EMQX plugin release is a tar file including including a subdirectory of this plugin's name and it's version, that contains:

1. A JSON format metadata file describing the plugin
2. Versioned directories for all applications needed for this plugin (source and binaries).

In a shell from this plugin's working directory execute `make rel` to have the package created like:

```
_build/default/emqx_plugrel/emqx_plugin_template-<vsn>.tar.gz
```

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.
