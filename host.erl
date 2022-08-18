%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------


-module(host).
-author("Yossi Bouskila").
-behaviour(gen_server).
-include("computers.hrl").
-export([start/4,stop/0]).
-export([init/1, handle_cast/2,handle_call/3,
         terminate/2]).
-define(MaxTotalChildren, 120).
-define(RATE, 20).

start(HostName,Entrance,Borders,Count) ->
    Return = gen_server:start_link({global, HostName}, ?MODULE, [HostName,Entrance,Borders,Count], []),
    io:format("start_link: ~p~n", [Return]),
    Return.
stop() ->
  gen_server:stop(?MODULE).
init([HostName,Entrance,Borders,Count]) ->
    %initRide(HostName),
    Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
    ets:new(Ets_children,[set,named_table]),
    ets:new(HostName,[set,named_table]),
    {West_border,East_border,North_border,South_border} = Borders,
    ets:insert(HostName,{entrance,Entrance}),
    ets:insert(HostName,{hostName,HostName}),
    ets:insert(HostName,{west_border,West_border}),
    ets:insert(HostName,{east_border,East_border}),
    ets:insert(HostName,{south_border,South_border}),
    ets:insert(HostName,{north_border,North_border}),
    ets:insert(HostName,{children_count,Count}),
    ets:insert(HostName,{ride,open}),
    ets:insert(HostName,{money,0}),
    ets:insert(HostName,{msg,0}),
    ets:insert(HostName,{total_child,0}),
    io:format("ready"),
    Return = {ok, [HostName,Ets_children]},
    Return.

handle_call(_Request, _From, []) ->
    Reply = ok,
    {reply, Reply, []}.

handle_cast(money, [HostName,Ets_children]) ->
    [{_,Money}] = ets:lookup(HostName,money),
    ets:insert(HostName,{money,Money+1}),
    {noreply, [HostName,Ets_children]};

handle_cast({transfer,_,Data}, [HostName,Ets_children]) ->
    [{_,Msg}] = ets:lookup(HostName,msg),
    ets:insert(HostName,{msg,Msg+1}),
    import_child([HostName,Ets_children],Data),
    %io:format("get transfer msg: ~p~n", [Data]),
    {noreply, [HostName,Ets_children]};

handle_cast({children_count,Children_count},[HostName,Ets_children]) ->
    ets:insert(HostName,{total_child,Children_count}),
    {noreply, [HostName,Ets_children]};

handle_cast({ride,Status}, [HostName,Ets_children]) ->
  ets:insert(HostName,{ride,Status}),
  {noreply, [HostName,Ets_children]};

handle_cast(trigger, [HostName,Ets_children]) ->
    new_child([HostName,Ets_children]),
    Children = ets:tab2list(Ets_children),
    gen_server:cast({global,master},Children),
    [{_,Money}] = ets:lookup(HostName,money),
    gen_server:cast({global,master},{money,Money}),
    ets:insert(HostName,{money,0}),
    [{_,Msg}] = ets:lookup(HostName,msg),
    gen_server:cast({global,master},{msg,Msg}),
    ets:insert(HostName,{msg,0}),
    {noreply, [HostName,Ets_children]};

handle_cast(Child_data, [HostName,Ets_children]) ->
    {Child_name,Data} = Child_data,
    case [] =:= ets:lookup(Ets_children,Child_name) of
        false ->
            [{_,Exit_point}] = ets:lookup(HostName,entrance),
            {_,{CurX,CurY},Money} = Data,
            [{_,West_border}] = ets:lookup(HostName,west_border),
            [{_,East_border}] = ets:lookup(HostName,east_border),
            [{_,South_border}] = ets:lookup(HostName,south_border),
            [{_,North_border}] = ets:lookup(HostName,north_border),

            case Money =:= 0 andalso {CurX,CurY} =:= Exit_point of
                true -> exit(global:whereis_name(Child_name),kill),ets:delete(Ets_children,Child_name);
                false ->
                    ets:insert(Ets_children,{Child_name,Data}),
                    case HostName of
                        ?PC1 -> pc1(Child_name,CurX,CurY,East_border,South_border,Ets_children);
                        ?PC2 -> pc2(Child_name,CurX,CurY,East_border,North_border,Ets_children);
                        ?PC3 -> pc3(Child_name,CurX,CurY,West_border,North_border,Ets_children);
                        ?PC4 -> pc4(Child_name,CurX,CurY,West_border,South_border,Ets_children)
                    end
            end;

        _ -> nothing
    end,
    {noreply, [HostName,Ets_children]}.

terminate(Reason, [HostName,Ets_children]) ->
    Children = ets:tab2list(Ets_children),
    Function = fun({Child_name,_}) -> exit(global:whereis_name(Child_name),kill) end,
    lists:foreach(Function, Children),
    ets:delete(HostName),
    ets:delete(Ets_children),
    io:format("terminate: ~p~n", [Reason]),
    ok.

new_child([HostName,Ets_children]) ->
    [{_,Total_child}] = ets:lookup(HostName,total_child),
    case Total_child<?MaxTotalChildren of
        true ->
            case rand:uniform(?RATE) of
                1 ->
                     [{_,Children_count}] = ets:lookup(HostName,children_count),
                     Child_name = list_to_atom(lists:flatten(io_lib:format("child_~p_N~B", [HostName,Children_count]))),
                     [{_,Entrance}] = ets:lookup(HostName,entrance),
                     ets:insert(HostName,{children_count,Children_count+1}),
                     Money = rand:uniform(10),
                     ets:insert(Ets_children,{Child_name,[{Entrance,Entrance,Money}]}),
                     spawn(child,start,[HostName,Child_name,Entrance,Entrance,Money]);
                     %io:format("New child: ~p~n", [Children_count+1]);
                _ -> ok
            end;
        _ -> ok
    end.

import_child([HostName,Ets_children],[{Child_name,{Destination,Position,Money}}]) ->
     ets:insert(Ets_children,{Child_name,[{Destination,Position,Money}]}),
     spawn(child,start,[HostName,Child_name,Destination,Position,Money]).


handle_child_transfer(Child_name,Dst_pc,Ets_children)->
    exit(global:whereis_name(Child_name),kill),
    Data = ets:lookup(Ets_children,Child_name),
    %io:format("transfer: ~p to: ~p from: ~p data: ~p~n", [Child_name,Dst_pc,Ets_children,Data]),
    gen_server:cast({global,Dst_pc},{transfer,Child_name,Data}),
    ets:delete(Ets_children,Child_name).

pc1(Child_name,CurX,CurY,East_border,South_border,Ets_children) ->
    case CurX > East_border andalso CurY > South_border of
        true ->  handle_child_transfer(Child_name,?PC3,Ets_children);
        _    -> case CurX > East_border of
                    true -> handle_child_transfer(Child_name,?PC4,Ets_children);
                    _    -> ok
                end,
                case CurY > South_border  of
                    true ->  handle_child_transfer(Child_name,?PC2,Ets_children);
                    _    -> ok
                end
    end.

pc2(Child_name,CurX,CurY,East_border,North_border,Ets_children) ->
    case CurX > East_border andalso CurY < North_border of
        true ->  handle_child_transfer(Child_name,?PC4,Ets_children);
        _    -> case CurX > East_border of
                    true -> handle_child_transfer(Child_name,?PC3,Ets_children);
                    _    -> ok
                end,
                case CurY < North_border  of
                    true ->  handle_child_transfer(Child_name,?PC1,Ets_children);
                    _    -> ok
                end
    end.
pc3(Child_name,CurX,CurY,West_border,North_border,Ets_children) ->
    case CurX < West_border andalso CurY < North_border of
        true ->  handle_child_transfer(Child_name,?PC1,Ets_children);
        _    -> case CurX < West_border of
                    true -> handle_child_transfer(Child_name,?PC2,Ets_children);
                    _    -> ok
                end,
                case CurY < North_border  of
                    true ->  handle_child_transfer(Child_name,?PC4,Ets_children);
                    _    -> ok
                end
    end.

pc4(Child_name,CurX,CurY,West_border,South_border,Ets_children) ->
    case CurX < West_border andalso CurY > South_border of
        true ->  handle_child_transfer(Child_name,?PC2,Ets_children);
        _    -> case CurX < West_border of
                    true -> handle_child_transfer(Child_name,?PC1,Ets_children);
                    _    -> ok
                end,
                case CurY > South_border  of
                    true ->  handle_child_transfer(Child_name,?PC3,Ets_children);
                    _    -> ok
                end
    end.

initRide(HostName)->
  case HostName of
    ?PC1 -> spawn(ride,start,[HostName,open,ridePc1]);
    ?PC2 -> spawn(ride,start,[HostName,open,ridePc2]);
    ?PC3 -> spawn(ride,start,[HostName,open,ridePc3]);
    ?PC4 -> spawn(ride,start,[HostName,open,ridePc4])
  end.
