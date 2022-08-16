%%%-------------------------------------------------------------------
%%% @author Yossi Bouskila, Tal Tubul
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(child).
-author("Tal Tubul").

-behaviour(gen_statem).

%% API
-export([start/5,stop/1]).

%% gen_statem callbacks
-export([init/1,terminate/3, callback_mode/0]).
-export([walking/3,in_queue/3,on_ride/3]).
-define(WALKING_TIMEOUT, 100).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

start(Father,Child_Name,Destination,Position,Money) ->
  gen_statem:start_link({local, Child_Name},?MODULE, [Father,Child_Name,Destination,Position,Money], []).

stop(Name) ->
  gen_statem:stop({local,Name}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Father,Child_Name,Destination,Position,Money]) ->
  ets:new(Child_Name,[set,named_table]),
  ets:insert(Child_Name,{money,Money}),
  ets:insert(Child_Name,{position,Position}),
  ets:insert(Child_Name,{destination, Destination}),
  ets:insert(Child_Name,{father, Father}),
  {ok, walking, Child_Name}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() -> [state_functions,state_enter].

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

walking(enter, _OldState, Data) ->
  [{_,Destination}] = ets:lookup(Data,destination),
  [{_,Position}] = ets:lookup(Data,position),
  case Destination =:= Position of
    true ->
      [{_,Money}] = ets:lookup(Data,money),
      Ride_dst = rand:uniform(4),
      case Money>0 of
        true ->   case Ride_dst of
                    1 -> NewDest = {590,245};%io:format("Ferris wheel ~n");
                    2 -> NewDest = {200,162};%io:format("Pirate ship ~n");
                    3 -> NewDest = {160,377};%io:format("roller coaster ~n")
                    4 -> NewDest = {426,423} %io:format("Haunted house ~n")
                  end;
        false ->  NewDest = {0,0}
      end,
      ets:insert(Data,{destination, NewDest});
    _ -> ok
  end,
  {next_state, walking, Data, ?WALKING_TIMEOUT};

walking(timeout, _, Data) ->
  [{_, {CurX,CurY}}] = ets:lookup(Data,position),
  [{_,Money}] = ets:lookup(Data,money),
  case Money>0 of
    true  -> [{_,{DstX,DstY}}] = ets:lookup(Data,destination);
    false -> {DstX,DstY} = {0,0}
  end,
  [{_,Father}] = ets:lookup(Data,father),
  gen_server:cast({Father,node()},{Data,{{DstX,DstY},{CurX,CurY},Money}}),

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
      ets:insert(Data,{position, {NewX,NewY}}),
      {next_state, walking, Data, ?WALKING_TIMEOUT};
    _    -> {next_state, in_queue, Data}%%%arrive
  end.

in_queue(enter, _OldState, Data) ->
  %io:format("in_queue~n"),
  {next_state, in_queue, Data, ?WALKING_TIMEOUT};

in_queue(timeout, _, Data) ->
  [{_,Father}] = ets:lookup(Data,father),
  [{_,Status}] = ets:lookup(Father,ride),
  case Status of
    open -> {next_state, on_ride, Data};
    _    -> {next_state, in_queue, Data, ?WALKING_TIMEOUT}
  end.

on_ride(enter, _OldState, Data) ->
  %io:format("on_ride~n"),
  [{_,Money}] = ets:lookup(Data,money),
  [{_,Father}] = ets:lookup(Data,father),
  gen_server:cast({Father,node()},money),
  ets:insert(Data,{money, Money-1}),
  enter_ride(Data),
  {next_state, on_ride, Data, 30000};

on_ride(timeout, _, Data) ->
  %io:format("finish_ride~n"),
  [{_,{DstX,DstY}}] = ets:lookup(Data,destination),
  [{_,Father}] = ets:lookup(Data,father),
  [{_,Money}] = ets:lookup(Data,money),
  gen_server:cast({Father,node()},{Data,{{DstX,DstY},{DstX,DstY},Money}}),
  {next_state, walking, Data}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, Data ) ->
  ets:delete(Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================
enter_ride(Data)->
  [{_,{DstX,DstY}}] = ets:lookup(Data,destination),
  [{_,Father}] = ets:lookup(Data,father),
  [{_,Money}] = ets:lookup(Data,money),
  case {DstX,DstY} of
    {590,245} -> case rand:uniform(3) of
            1 -> Cur_pos = {614,126};
            2 -> Cur_pos = {707,128};
            3 -> Cur_pos = {677,199}
         end;
    {200,162} -> case rand:uniform(2) of
            1 -> Cur_pos = {305,91};
            2 -> Cur_pos = {325,68}
         end;
    {160,377} -> case rand:uniform(3) of
            1 -> Cur_pos = {189,270};
            2 -> Cur_pos = {207,271};
            3 -> Cur_pos = {296,309}
         end;
    {426,423} -> case rand:uniform(2) of
            1 -> Cur_pos = {474,387};
            2 -> Cur_pos = {534,390}      
         end;
        _     -> Cur_pos ={DstX,DstY}
  end,
  gen_server:cast({Father,Father},{Data,{{DstX,DstY},Cur_pos,Money}}).

