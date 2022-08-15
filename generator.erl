%%%-------------------------------------------------------------------
%%% @author Yossi Bouskila, Tal Tubul
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(generator).
-author("Yossi Bouskila, Tal Tubul").
-include("computers.hrl").
-behaviour(gen_statem).

%% API
-export([start/0,stop/1]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).
-define(REFRESH, 50).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

start() ->
  gen_statem:start_link({local, generator},?MODULE, [], []).

stop(Name) ->
  gen_statem:stop({local,Name}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init() ->
  {ok, main, []]}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() -> [state_functions,state_enter].

main(enter, _OldState, Data) ->
  {next_state, main, Data, ?REFRESH};

main(timeout, _, Data) ->
  gen_server:cast({?PC1,?PC1},trigger),
  gen_server:cast({?PC2,?PC2},trigger),
  gen_server:cast({?PC3,?PC3},trigger),
  gen_server:cast({?PC4,?PC4},trigger),
  {next_state, main, Data, ?REFRESH}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, Data ) ->
  ok.
