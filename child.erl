%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(child).
-author("Yossi Bouskila, Tal Tubul").

-behaviour(gen_statem).

%% API
-export([start/5,stop/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, callback_mode/0]).
-export([walking/3,in_queue/3,on_ride/3]).
-define(WALKING_TIMEOUT, 200).
%%-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.

%start_link() ->
 % gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Father,Child_Name,Destination,Position,Money) ->
  gen_statem:start_link({global, Child_Name},?MODULE, [Father,Child_Name,Destination,Position,Money], []).
 % gen_statem:call(?SERVER, start).
stop(Name) ->
  gen_statem:stop(Name).
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
  %io:format("init ~p~p~n",[Position,Destination]),
  {ok, walking, Child_Name}.

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
  %io:format("start walking ~n"),
  %%%%select dest!!!!
  [{_,Destination}] = ets:lookup(Data,destination),
  [{_,Position}] = ets:lookup(Data,position),
  %io:format("start walking ~p~p~n",[Position,Destination]),
  case Destination =:= Position of
    true ->
      [{_,Money}] = ets:lookup(Data,money),
      Ride_dst = rand:uniform(4),
      %ets:insert(Data,{ride_dst,Ride_dst}),
      case Money>0 of
        true ->   case Ride_dst of
                    1 -> NewDest = {590,252};%io:format("Ferris wheel ~n");
                    2 -> NewDest = {200,162};%io:format("Pirate ship ~n");
                    3 -> NewDest = {160,377};%io:format("roller coaster ~n")
                    4 -> NewDest = {426,423}%io:format("Haunted house ~n")
                  end;
        false ->
            %io:format("go home ~n"),
            NewDest = {0,0}
      end,
      ets:update_element(Data,destination,{2,NewDest});
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
  %io:format("CurX: ~p CurY: ~p  ~n",[CurX,CurY]),
  [{_,Father}] = ets:lookup(Data,father),

  gen_server:cast({global,Father},{Data,{{DstX,DstY},{CurX,CurY},Money}}),

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
      ets:update_element(Data,position,{2, {NewX,NewY}}),{next_state, walking, Data, ?WALKING_TIMEOUT};
    %true  when {CurX,CurY} =:= {10,10} ->  child:stop();
    _    -> {next_state, in_queue, Data}%%%arive
  end.

in_queue(enter, _OldState, Data) ->
  %io:format("in_queue~n"),
  {next_state, in_queue, Data, 5000};
in_queue(timeout, _, Data) ->
  %io:format("finish_queue~n"),
  {next_state, on_ride, Data}.


on_ride(enter, _OldState, Data) ->
  %io:format("on_ride~n"),
  [{_,Money}] = ets:lookup(Data,money),

  ets:update_element(Data,money,{2,Money-1}),
  enter_ride(Data),
  {next_state, on_ride, Data, 3000};
on_ride(timeout, _, Data) ->
  [{_,{DstX,DstY}}] = ets:lookup(Data,destination),
  [{_,Father}] = ets:lookup(Data,father),
  [{_,Money}] = ets:lookup(Data,money),
  gen_server:cast({global,Father},{Data,{{DstX,DstY},{DstX,DstY},Money}}),
  %io:format("finish_ride~n"),
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
terminate(_Reason, _StateName, Data ) ->ets:delete(Data).



%%%===================================================================
%%% Internal functions
%%%===================================================================
enter_ride(Data)->
  [{_,{DstX,DstY}}] = ets:lookup(Data,destination),
  [{_,Father}] = ets:lookup(Data,father),
  [{_,Money}] = ets:lookup(Data,money),
  %[{_,Ride_dst}] = ets:lookup(Data,ride_dst),
  case {DstX,DstY} of
    {590,252} -> case rand:uniform(2) of
            1 -> Cur_pos = {614,126};
            2 -> Cur_pos = {707,128}
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
    {426,423} -> case rand:uniform(3) of
            1 -> Cur_pos = {474,387};
            2 -> Cur_pos = {534,390};
            3 -> Cur_pos = {677,199}
         end
  end,
  gen_server:cast({global,Father},{Data,{{DstX,DstY},Cur_pos,Money}}).

