Herp
====

HPCloud OpenStack bindings in Erlang
====================================

Currently you can:

* Login
* List Containers
* Create Containers
* Upload Files

By providing a sys.config file you can login via
`herp_identity:login_conf`, otherwise you can provide them directly.

```erlang

{ok, Client} = herp_identity:login(Username, Password, TenantID).
{ok, Client2} = herp_identity:login_conf().
```

Once you have a client, it will be managed by the `herp_sup`
supervisor, restarting your client if you cause it to crash, logging
back in and getting a new AuthToken. This happens entirely
transparently from your perspective. There's a slight race condition
currently when your reference will be invalid whilst the asynchronous
re-Auth takes place.

Configuration
=============

For some features your sys.config or environment should contain:

```erlang
[
 {herp, [
         {has_proxy, boolean() },
         {proxyaddr, proxy_address::string()},
         {proxyport, proxy_port::integer()},
         {username, username::string()},
         {password, password::string()},
         {tenant_id, tenant_id::string()}
        ]
 }].
```

Building
========

Prerequisites:

* rebar
* relx

```shell
$ git clone https://github.com/AeroNotix/herp.git
$ cd herp
$ make
```

The documentation can be built with:

```shell
$ make docs
```

This will create the `docs/` folder in your current directory.

Listing containers
==================

```erlang

ContainersTop = herp_object:list(Client).
ContainersDetail = herp_object:list(Client, "container").
```

This returns a proplist as such:

```erlang

[[{<<"count">>,13},
  {<<"bytes">>,4904536},
  {<<"name">>,<<"ContainerName">>}],
 [{<<"count">>,0},
  {<<"bytes">>,0},
  {<<"name">>,<<"OtherContainerName">>}],
 [{<<"count">>,11},
  {<<"bytes">>,384012},
  {<<"name">>,<<"Awseum">>}]]
```

Creating Containers
===================

```erlang

ok = herp_object:create_directory(Client, "NewDirectory"),

%% You can also set custom headers on the request.
ok = herp_object:create_directory(Client, "NewDirectory", [{"Header", "Option"}]).
```

Uploading Files
===============

```erlang

ok = herp_object:upload_file(Client, "/path/to/file", "container_name"),

%% Set extra headers.
ok = herp_object:upload_file(Client, "/path/to/file2", "container_name", [{"header", "option"}]),

%% Specify your timeout.
ok = herp_object:upload_file(Client, "/path/to/file2", "container_name", [{"header", "option"}], 5000),
```

Files are md5'd in order to check for end-to-end integrity, they are
also checked for their Content-type when being uploaded.

Provision New Servers
=====================

```erlang

ok = herp_compute:create_server(Client, [{<<"name">>, <<"awesomium">>},
                                          {<<"flavorRef">>, herp_compute:flavour(xsmall)},
                                          {<<"imageRef">>, <<"1359">>}]).
```
