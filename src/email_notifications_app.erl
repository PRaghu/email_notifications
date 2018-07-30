%%%-------------------------------------------------------------------
%% @doc email_notifications public API
%% @end
%%%-------------------------------------------------------------------

-module(email_notifications_app).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
  	{ok, _} = application:ensure_all_started(email_notifications),
  	ok.

-spec stop() -> ok.
stop() ->
  	application:stop(email_notifications).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
start(_StartType, _StartArgs) ->
    email_notifications_sup:start_link().

stop(_State) ->
    ok.