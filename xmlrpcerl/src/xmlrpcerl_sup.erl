%%%-------------------------------------------------------------------
%% @doc xmlrpcerl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('xmlrpcerl_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	RpcServer = case application:get_env(xmlrpcerl, server) of
		{ok, RPCServer} -> RPCServer;
		_ -> "http://127.0.0.1:8000/RPC2"
	end,

	Xmlrpc = {xmlrpc_client, {xmlrpc_client, start_link, [RpcServer]}, permanent, 2000, worker, [xmlrpc_client]},

	ChildSpecs = [Xmlrpc],
	ok = supervisor:check_childspecs(ChildSpecs),
	{ok, {{one_for_one, 10, 1}, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
