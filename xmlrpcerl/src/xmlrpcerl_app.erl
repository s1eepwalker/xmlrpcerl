%%%-------------------------------------------------------------------
%% @doc xmlrpcerl public API
%% @end
%%%-------------------------------------------------------------------

-module('xmlrpcerl_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    'xmlrpcerl_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
