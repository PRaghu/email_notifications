%%%-------------------------------------------------------------------
%% @doc email_notifications top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(email_notifications_sup).

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

init([]) ->

    Child = [email_notifications:child_spec()],
    {ok, { {one_for_all, 0, 1}, Child} }.