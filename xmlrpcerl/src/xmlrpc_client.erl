-module(xmlrpc_client).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-define(SERVER, ?MODULE).
-define(CALL_TIMEOUT, 10000).


-export([get_state/0]).
-export([request/2,
         request_pub/2,
         request_pub/3,
         safe_log_request_pub/2,
         safe_log_request_pub/3]). %xmlrpc
-export([request/3, response/2]). %rpc call
-export([ping/0]).


-export([start_link/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
			xmlrpc_server = "http://127.0.0.1:8000/RPC2"  :: string(),
			auth_info = []                                :: [{atom(), any()}],
			ping_timer
		}).


% start([Name, Server]) when is_atom(Name) ->
% 	gen_server:start_link({local, Name}, ?MODULE, [Server], []);
start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode(Body) ->
	case xmlrpc_decode:payload(Body) of
		{ok, {response, [Ret]}} ->
			{ok, Ret};
		{ok, {response, Ret}} ->
			{ok, Ret};

		{error, Ret} ->
			{error, lists:flatten(io_lib:format("XML RPC decode error ~p", [Ret]))}

	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call(Method, Params) when is_atom(Method) andalso is_list(Params) ->
	xmlrpc_encode:payload({call, Method, Params}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
response(Code, String) when is_integer(Code) andalso is_list(String) ->
	xmlrpc_encode:payload({response, {fault, Code, String}}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_state() ->
	gen_server:call(?SERVER, get_state).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
request(Module, Method, Params) when is_atom(Module) andalso is_atom(Method) andalso is_list(Params) ->
	case catch gen_server:call(?SERVER, {request_rpc, Module, Method, Params}) of
		{'EXIT', Ret } -> Ret;
		Val -> Val
	end.
request_rpc_handler(Module, Method, Params, #state{auth_info = AuthInfo}) ->
	Node = list_to_atom(proplists:get_value(node, AuthInfo, "node@node")),
	rpc:call(Node, Module, Method, Params).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
request(Method, Params) when is_atom(Method) andalso is_list(Params) ->
	gen_server:call(?SERVER, {request_xmlrpc, Method, Params, true}, ?CALL_TIMEOUT);
request({Method, non_secure}, Params) when is_atom(Method) andalso is_list(Params) ->
	gen_server:call(?SERVER, {request_xmlrpc, Method, Params, false}, ?CALL_TIMEOUT).
request_pub(Method, Params) when is_atom(Method) andalso is_list(Params) ->
	gen_server:call(?SERVER, {request_xmlrpc, Method, Params, false}, ?CALL_TIMEOUT).
request_pub(Srv, Method, Params) when is_atom(Method) andalso is_list(Params) ->
	gen_server:call(?SERVER, {request_xmlrpc, Srv, Method, Params, false}, ?CALL_TIMEOUT).

safe_log_request_pub(Method, Params) when is_atom(Method) andalso is_list(Params) ->
    gen_server:call(?SERVER, {safe_log_request_xmlrpc, Method, Params, false}, ?CALL_TIMEOUT).
safe_log_request_pub(Srv, Method, Params) when is_atom(Method) andalso is_list(Params) ->
	gen_server:call(?SERVER, {safe_log_request_xmlrpc, Srv, Method, Params, false}, ?CALL_TIMEOUT).

request_xmlrpc_handler(Method, Params, Secure, State) ->
    request_xmlrpc_handler(Method, Params, Secure, State, true).

request_xmlrpc_handler(Method, Params, Secure,
                       #state{xmlrpc_server = ServUrl, auth_info = AuthInfo}, ISArgInLog)
when is_atom(Method) andalso is_list(Params) andalso is_boolean(Secure) ->
	ContentType = "text/xml", %"application/x-www-form-urlencoded",
	{ok, Request} = case Secure of
		true ->
			Token = proplists:get_value(auth_token, AuthInfo, "No token"),
			call(Method, [Token] ++ Params);
		_ ->
			call(Method, Params)
	end,
	% xmlrpc_client:request('webfilter.ifconfig_list', []).
    lager:info("=================== XmRpc call begin ==============================="),
    if ISArgInLog ->
           lager:info("Request: ~p ~p", [Method, Params]),
           lager:debug("~p", [lists:flatten(Request)]);
       true ->
           lager:info("Request: ~p", [Method])
    end,
	Ret = httpc:request(post, {ServUrl, [], ContentType, lists:flatten(Request)}, [{timeout, ?CALL_TIMEOUT}], []),
	Return = case Ret of
		{ok, {{_HttpVer, 200, _Reason}, _Headers, Body}} ->
			{ok, Answer} = decode(Body),
			Answer;
		{ok, {{_HttpVer, Status, _Reason}, _Headers, Body}} ->
			Str = lists:flatten(io_lib:format("(~p)~p", [Status, binary_to_list(Body)])),
			lager:error(Str),
			{fault, Status, Str};
		_ ->
			Str = lists:flatten(io_lib:format("~p", [Ret])),
			lager:error(Str),
			{fault, 500, Str}
	end,
    lager:info("Return: ~p", [Return]),
    lager:info("=================== XmRpc call end ==============================="),
	Return.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ping() ->
	gen_server:call(?SERVER, ping).
ping_handler(State) ->
	request_xmlrpc_handler('v1.core.session.ping', [], true, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}, hibernate};
init([Srv | _]) ->
	lager:info("XMLRPC SERVER >> ~p", [Srv]),
	{ok, #state{xmlrpc_server = Srv}, hibernate}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({safe_log_request_xmlrpc, Srv, Method, Params, Sec}, _From, State) ->
    Reply = request_xmlrpc_handler(Method, Params, Sec, State #state{xmlrpc_server = Srv}, false),
    {reply, Reply, State, hibernate};
handle_call({safe_log_request_xmlrpc, Method, Params, Sec}, _From, State) ->
	Reply = request_xmlrpc_handler(Method, Params, Sec, State, false),
	{reply, Reply, State, hibernate};
handle_call({request_xmlrpc, Srv, Method, Params, Sec}, _From, State) ->
	Reply = request_xmlrpc_handler(Method, Params, Sec, State #state{xmlrpc_server = Srv}),
	{reply, Reply, State, hibernate};
handle_call({request_xmlrpc, Method, Params, Sec}, _From, State) ->
	Reply = request_xmlrpc_handler(Method, Params, Sec, State),
	{reply, Reply, State, hibernate};
handle_call({request_rpc, Module, Method, Params}, _From, State) ->
	Reply = request_rpc_handler(Module, Method, Params, State),
	{reply, Reply, State, hibernate};
handle_call(ping, _From, State) ->
	Reply = ping_handler(State),
	{reply, Reply, State, hibernate};
handle_call(get_state, _From, State) ->
	{reply, State, State, hibernate};
handle_call(_Request, From, State) ->
	Reply = {ok, From},
	{reply, Reply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
