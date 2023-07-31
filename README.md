# plugin_mqtt_proxy

## Assumptions

1. The private keys are all located in the file system where EMQX is running.
2. The OS user running EMQX has read permissions for all keys.
3. The private keys filenames have the form `${clientid}.pem`, when `${clientid}` exactly matches the MQTT Client ID of the connecting client.
4. If a matching private key for a connecting client is not found, the client is denied connection.
5. Peer certificate is only available when `verify = verify_peer` in the listener.  Therefore, if `verify = verify_none` and the peer certificate info is missing, we abort the connection.

## Release

An EMQX plugin release is a tar file including including a subdirectory of this plugin's name and it's version, that contains:

1. A JSON format metadata file describing the plugin
2. Versioned directories for all applications needed for this plugin (source and binaries).

In a shell from this plugin's working directory execute `make rel` to have the package created like:

```
_build/default/emqx_plugrel/emqx_plugin_template-<vsn>.tar.gz
```

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.
