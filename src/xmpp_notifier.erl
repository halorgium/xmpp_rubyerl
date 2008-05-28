%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc talk to the ruby program
%% @end
%%%-------------------------------------------------------------------
-module(xmpp_notifier).

-behaviour(gen_server).

-include_lib("logging.hrl").

%% API
-export([start_link/0,
         deliver/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

deliver(Message) when is_list(Message) ->
    deliver(list_to_binary(Message));
deliver(Message) when is_binary(Message) ->
    gen_server:call(?SERVER, {deliver, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?INFO("Starting the xmpp_notifier~n", []),
    process_flag(trap_exit, true),
    {ok, BaseDir} = application:get_env(xmpp_rubyerl, base_dir),
    Cmd = lists:flatten(io_lib:format("ruby ~s/ebin/client.rb", [BaseDir])),
    Port = open_port({spawn, Cmd}, [{packet, 4}, use_stdio, exit_status, binary]),
    {ok, #state{port = Port}}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call({deliver, Message}, _From, State = #state{port = Port}) when is_port(Port) and is_binary(Message) ->
    ok = invoke_command(Port, {deliver, Message}),
    {reply, ok, State};

handle_call(stop, _From, #state{port = Port}) ->
    Port ! {self(), close}, 
    receive
        {Port, closed} -> exit(normal)
    end;

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p~n", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    ?INFO("Got an exit from our port (~p): ~p~n", [Port, Reason]),
    {stop, port_terminated, State#state{port = undefined}};

handle_info(Info, State) ->
    ?WARN("Unexpected info ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
invoke_command(Port, Command) ->
    Data = {command, term_to_binary(Command)},
    Self = self(),
    {Self, Data} = Port ! {Self, Data},
    ok.
