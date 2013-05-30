Herp
====

HPCloud OpenStack bindings in Erlang
------------------------------------

Currently you can:

* Login
* List Containers

By providing a sys.config file you can login via
`herp_identity:login_conf`, otherwise you can provide them directly.

```erlang

{ok, Client} = herp_identity:login(Username, Password, TenantID).
{ok, Client2} = herp_identity:login_conf().
```
