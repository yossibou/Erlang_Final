  %%%-------------------------------------------------------------------
%%% @author Yossi Bouskila, Tal Tubul
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(ride).
-author("Yossi Bouskila, Tal Tubul").

-behaviour(gen_statem).

%% API
-export([start/3,stop/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, callback_mode/0]).
-export([maintenance/3,work/3,open/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

start(Father,Status,RideName) ->
  gen_statem:start_link({local, RideName},?MODULE, [Father,Status], []).

stop(RideName) ->
  gen_statem:stop({local,RideName}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Father,Status]) ->
  {ok, Status, [Father]}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() -> [state_functions,state_enter].

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

open(enter, _OldState, Father) ->
  gen_server:cast({local,Father},{ride,open}),
  {next_state, open, Father, 2000};

open(timeout, _, Data) ->
    {next_state, work, Data}.

work(enter, _OldState, Father) ->
  gen_server:cast({local,Father},{ride,work}),
  {next_state, work, Father, 10000};
work(timeout, _, Data) ->
  case rand:uniform(10)>9 of
    true -> {next_state, maintenance, Data};
    _    -> {next_state, open, Data}
  end.

maintenance(enter, _OldState, Father) ->
  gen_server:cast({local,Father},{ride,maintenance}),
  {next_state, maintenance, Father, 10000};
maintenance(timeout, _, Data) ->
  {next_state, open, Data}.
%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _ ) -> ok.
