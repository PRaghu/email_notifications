-module(email_notifications).

-behaviour(gen_server).

%% API
-export([
    child_spec/0,
    start_link/0,
    send/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-type payload() :: #{ 
    recipients := list(),
    subject    := binary() | string(),
    body       := binary() | string() 
}.

-define(TAG, email_notifications).

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
child_spec() ->
    #{
        id      => ?MODULE,
        start   => {?MODULE, start_link, []},
        restart => permanent
    }.

-spec send(Args) -> Res when
    Args    :: payload(),
    Res     :: ok.
send(Args) ->
    gen_server:cast(?MODULE, {send, Args}).

-spec start_link() -> Res when
    Res :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
    {ok, SmtpConfig} =  application:get_env(email_notifications, smtp),
    Sender = proplists:get_value(sender, SmtpConfig),
    Relay = proplists:get_value(relay, SmtpConfig),
    {ok, #{<<"sender">> => Sender, <<"relay">> => Relay}}.

%% @hidden
handle_cast({send, Payload}, State) ->
    ok = send_internal(Payload, State),
    {noreply, State}.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
send_internal(Payload, #{<<"sender">> := Sender, <<"relay">> := Relay}) ->
    Options = [{relay, Relay}, {username, Sender}],
    Recipients = maps:get(<<"recipients">>, Payload, []),
        Email = generate_email(Payload, Recipients),
        case gen_smtp_client:send({Sender, Recipients, Email}, Options) of
            {error, Reason} ->
                LogMsg = "~p Reason: ~p Email: ~p To ~p",
                ok = lager:error(LogMsg, [?TAG, Reason, Email, Recipients]),
                {error, Reason};
            {ok, _Pid} ->
                ok
        end.
 
%% @private
generate_email(#{subject := Subject, body:= Body}, Recipients) ->
    {ok, Email} = email_notifications_dtl:render(
        [
            {to,      Recipients}, 
            {subject, Subject},
            {body,    Body}
        ]
    ),
    Email.