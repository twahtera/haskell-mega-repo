# planmill-proxy

Planmill proxy implementing Haxl interface

## Development compile-test-loop

```sh
stack build planmill-proxy
tajna exec -s planmill-proxy-server  # Start the server
tajna exec -s planmill-proxy-client  # Make test request
```
