  %%%-------------------------------------------------------------------
%%% @author Yossi Bouskila, Tal Tubul
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(ride).
-author("Tal Tubul").

-behaviour(gen_statem).

%% API
-export([start/3,stop/0]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).
-export([maintenance/3,work/3,open/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

start(Father,Status,RideName) ->
  gen_statem:start_link({local, RideName},?MODULE, [Father,Status], []).

stop() ->
  gen_statem:stop(?MODULE).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Father,Status]) ->
  {ok, Status, Father, 1000}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() -> [state_functions].

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

open(timeout, _, Father) ->
    %io:format("ride: open ~n"),
    gen_server:cast({global,Father},{ride,open}),
    {next_state, work, Father ,2000}.

work(timeout, _, Father) ->
  %io:format("ride: work ~n"),
  gen_server:cast({global,Father},{ride,work}),
  case rand:uniform(10) > 9 of
    true -> {next_state, maintenance, Father, 30000};
    _    -> {next_state, open, Father, 30000}
  end.

maintenance(timeout, _, Father) ->
  %io:format("ride: maintenance ~n"),
  gen_server:cast({global,Father},{ride,maintenance}),
  {next_state, open, Father, 10000}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _ ) -> ok.
