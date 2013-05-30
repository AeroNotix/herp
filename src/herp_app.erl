-module(herp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case application:get_env(herp, has_proxy) of
        {ok, true} ->
            {ok, ProxyAddress} = application:get_env(proxyaddr),
            {ok, ProxyPort} = application:get_env(proxyport),
            httpc:set_options([{proxy, {{ProxyAddress, ProxyPort},
                                        ["localhost"]}}]);
        _Else ->
            ok
    end,
    herp_sup:start_link().

stop(_State) ->
    ok.
