-module(email_notifications_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test Cases
-export([
    send/1
]).

-type config() :: proplists:proplist().

-export_type([config/0]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
  [send].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    _ = email_notifications_app:start(),
    _ = application:ensure_all_started(meck),
    ok = mock_smtp_send(),
    Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    email_notifications_app:stop().

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec send(config()) -> ok.
send(_Config) ->
    true = register(?MODULE, self()),
    ok = email_notifications:send(payload()),
    receive
        email_sent -> ok
        after 1000 -> ct:fail("failed to send an email notification")
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
mock_smtp_send() ->
    Fun =
        fun({_Sender, _Recipients, _Email}, _Options) ->
            ?MODULE ! email_sent,
            {ok, ?MODULE}
        end,
    meck:expect(gen_smtp_client, send, Fun).

%% @private
payload() ->
    #{
        body => <<"Hello World">>, 
        subject => <<"Testing">>, 
        recipients => [<<"recipient1@gmail.com">>, <<"recipient2@gmail.com">>]
    }.