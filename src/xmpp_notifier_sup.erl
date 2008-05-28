%%%-------------------------------------------------------------------
%% @author Tim Carey-Smith <tim@spork.in>
%% @version {@vsn}, {@date} {@time}
%% @doc keep an eye on the server
%% @end
%%%-------------------------------------------------------------------
-module(xmpp_notifier_sup).

-behaviour(supervisor).

-include_lib("logging.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Children = [{"xmpp_notifier",
                 {xmpp_notifier,start_link,[]},
                 permanent,2000,worker,
                 [xmpp_notifier]
                }],
    {ok,{{one_for_one,1,5}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
