%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc the main app
%% @end
%%%-------------------------------------------------------------------
-module(xmpp_rubyerl).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    application:start(sasl),
    application:start(?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case xmpp_notifier_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
