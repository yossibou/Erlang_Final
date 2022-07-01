%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(child).
-author("yossi").

-behaviour(gen_statem).

%% API
-export([start/0,stop/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, callback_mode/0]).
-export([walking/3,in_queue/3,on_ride/3]).
-define(WALKING_TIMEOUT, 10000).
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%start_link() ->
 % gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
start() ->
  gen_statem:start_link(?MODULE, [], []).
 % gen_statem:call(?SERVER, start).
stop() ->
  gen_statem:stop(?SERVER).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  ets:new(data,[set,named_table]),
  ets:insert(data,{money,rand:uniform(5)*10}),
  ets:insert(data,{position, {0,0}}),
  ets:insert(data,{destination, {0,0}}),
  {ok, walking, null}.

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

walking(enter, _OldState, Data) ->
  io:format("start walking ~n"),
  %%%%select dest!!!!
  case rand:uniform(4) of
    1 -> io:format("Ferris wheel ~n"),NewDest = {5,5};
    2 -> io:format("Pirate ship ~n"),NewDest = {10,3};
    3 -> io:format("roller coaster ~n"),NewDest = {0,7};
    4 -> io:format("go home ~n"),NewDest = {10,10}
  end,
  ets:update_element(data,destination,{2,NewDest}),
  {next_state, walking, Data, 1000};
walking(timeout, _, Data) ->
  [{_, {CurX,CurY}}] = ets:lookup(data,position),
  [{_,{DstX,DstY}}] = ets:lookup(data,destination),
  io:format("CurX: ~p CurY: ~p  ~n",[CurX,CurY]),
  case CurX =:= DstX andalso CurY =:= DstY of
    false ->
      %%%%%Update X pos%%%%%%
      case CurX =:= DstX of
        false -> case CurX > DstX of
                   true -> NewX = CurX-1;
                   _    -> NewX = CurX+1
                 end;
        _ -> NewX = CurX
      end,
      %%%%%Update Y pos%%%%%%
      case CurY =:= DstY of
         false -> case CurY > DstY of
                    true -> NewY = CurY-1;
                    _    -> NewY = CurY+1
                  end;
         _ -> NewY = CurY
      end,
      ets:update_element(data,position,{2, {NewX,NewY}}),{next_state, walking, Data, ?WALKING_TIMEOUT};
    %true  when {CurX,CurY} =:= {10,10} ->  child:stop();
    _    -> {next_state, in_queue, Data}%%%arive
  end.

in_queue(enter, _OldState, Data) ->
  io:format("in_queue~n"),
  {next_state, in_queue, Data, 3000};
in_queue(timeout, _, Data) ->
  io:format("finish_queue~n"),
  {next_state, on_ride, Data}.


on_ride(enter, _OldState, Data) ->
  io:format("on_ride~n"),
  {next_state, on_ride, Data, 3000};
on_ride(timeout, _, Data) ->
  io:format("finish_ride~n"),
  {next_state, walking, Data}.

%walking({call,From}, forward, State)->
%  io:format("key:here  ~n"),
%  {next_state,walking,forward,[{reply,From,forward}]}.

%walking(_EventType, _EventContent, State)->
%  {forward, walking, null}.


 % [{_,CurrentPos}] = ets:lookup(data,position),
  %[{_,Destination}] = ets:lookup(data,destination),
 % case CurrentPos =:= Destination of
  %  true -> select_destination()
%  end,
 % ok .
%open(state_timeout, lock,  Data) ->
%  do_lock(),
%  {next_state, locked, Data};

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%handle_event(_EventType, _EventContent, _StateName, State) ->
%  NextStateName = the_next_state_name,
%  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State ) ->
  ets:delete(data).



%%%===================================================================
%%% Internal functions
%%%===================================================================
