%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------

%[ets:tab2list(pc1_children),ets:tab2list(pc2_children),ets:tab2list(pc3_children),ets:tab2list(pc4_children)].

-module(host).
-author("Yossi Bouskila, Tal Tubul").
-behaviour(gen_server).
-include("computers.hrl").
-export([start/4,stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(STATUS_TIMEOUT, 50).
-define(MaxTotalChildren, 6).
-define(RATE, 500). %STATUS_TIMEOUT*RATE

start(HostName,Entrance,Borders,Count) ->
    Return = gen_server:start_link({local, HostName}, ?MODULE, [HostName,Entrance,Borders,Count], []),
    io:format("start_link: ~p~n", [Return]),
    Return.
stop(HostName) ->
  gen_server:stop({local, HostName}).
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
    ets:insert(HostName,{total_child,0}),
    io:format("ready"),
    Return = {ok, HostName,?STATUS_TIMEOUT},
    Return.

handle_call({transfer,_,Data}, _From, HostName) ->
    import_child(HostName,Data),
    Reply = ok,
    io:format("handle_call: ~p~n", [Data]),
    {reply, Reply, HostName,?STATUS_TIMEOUT}.

handle_cast({transfer,_,Data}, HostName) ->
    import_child(HostName,Data),
    io:format("handle_call: ~p~n", [Data]),
    {noreply, HostName,?STATUS_TIMEOUT};

handle_cast({children_count,Children_count}, HostName) ->
    %io:format("msg: ~p~n", [Children_count]),
    ets:insert(HostName,{total_child,Children_count}),
    {noreply, HostName,?STATUS_TIMEOUT};

handle_cast({ride,Status}, HostName) ->
  ets:insert(HostName,{ride,Status}),
  {noreply, HostName,?STATUS_TIMEOUT};

handle_cast(Msg, HostName) ->
    %io:format("msg: ~p~n", [Msg]),
    {Child_name,Data} = Msg,
    Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
    case [] =:= ets:lookup(Ets_children,Child_name) of
        false ->
            [{_,Exit_point}] = ets:lookup(HostName,entrance),
            {_,{CurX,CurY},Money} = Data,
            [{_,West_border}] = ets:lookup(HostName,west_border),
            [{_,East_border}] = ets:lookup(HostName,east_border),
            [{_,South_border}] = ets:lookup(HostName,south_border),
            [{_,North_border}] = ets:lookup(HostName,north_border),

            case Money =:= 0 andalso {CurX,CurY} =:= Exit_point of
                true -> child:stop(Child_name),ets:delete(Ets_children,Child_name);
                false ->
                    ets:update_element(Ets_children,Child_name,{2, Data}),
                    case HostName of
                        ?PC1 -> pc1(Child_name,CurX,CurY,East_border,South_border,Ets_children);
                        ?PC2 -> pc2(Child_name,CurX,CurY,East_border,North_border,Ets_children);
                        ?PC3 -> pc3(Child_name,CurX,CurY,West_border,North_border,Ets_children);
                        ?PC4 -> pc4(Child_name,CurX,CurY,West_border,South_border,Ets_children)
                    end
            end;

        _ -> nothing
    end,
    {noreply, HostName,?STATUS_TIMEOUT}.


handle_info(_Info, HostName) ->
    new_child(HostName),
    Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
    Children = ets:tab2list(Ets_children),
    gen_server:cast({local,?MASTER},Children),
    %io:format("handle_call-host: ~p~n", [Children]),
    {noreply, HostName,?STATUS_TIMEOUT}.

terminate(_Reason, HostName) ->
    Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
    Children = ets:tab2list(Ets_children),
    Function = fun({Child_name,_}) -> rpc:call(node(),child,stop,[Child_name]) end,
    lists:foreach(Function, Children),
    ets:delete(HostName),
    ets:delete(Ets_children),
    %io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    %io:format("code_change: ~p~n", [Return]),
    Return.

new_child(HostName) ->
    [{_,Total_child}] = ets:lookup(HostName,total_child),
    io:format("new_child-host: ~p~n", [Total_child]),
    case  Total_child<?MaxTotalChildren of
        true ->
            case rand:uniform(?RATE) of
                1 ->
                     [{_,Children_count}] = ets:lookup(HostName,children_count),
                     Child_name = list_to_atom(lists:flatten(io_lib:format("child_~p_N~B", [HostName,Children_count]))),
                     Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
                     [{_,Entrance}] = ets:lookup(HostName,entrance),
                     ets:update_element(HostName,children_count,{2,Children_count+1}),
                     %Father,Child_Name,Destination,Position,Money
                     Money = rand:uniform(10),
                     ets:insert(Ets_children,{Child_name,[{Entrance,Entrance,Money}]}),
                     %io:format("New child: ~p~n", [Children_count+1]),
                     %%child:start(HostName,Child_name,Entrance,Entrance,Money),
                     %spawn(child,start,[HostName,Child_name,Entrance,Entrance,Money]),
                     rpc:call(node(),child,start,[HostName,Child_name,Entrance,Entrance,Money]),
                     io:format("New child: ~p~n", [Children_count+1]);
                _ -> ok
            end;
        _ -> ok
    end.

import_child(HostName,[{Child_name,{Destination,Position,Money}}]) ->
     %[{_,Entrance}] = ets:lookup(HostName,entrance),
     %io:format("import ~p~p~n",[Position,Destination]),
     Ets_children=list_to_atom(lists:flatten(io_lib:format("~p_~p", [HostName,children]))),
     ets:insert(Ets_children,{Child_name,[{Destination,Position,Money}]}),
     io:format(" status ~p~n", [rpc:call(node(),child,start,[HostName,Child_name,Destination,Position,Money])]).


handle_child_transfer(Child_name,Dst_pc,Ets_children)->
    io:format(" status2 ~p~n", [rpc:call(node(),child,stop,[Child_name])]),

    Data = ets:lookup(Ets_children,Child_name),
    io:format("transfer: ~p to: ~p from: ~p data: ~p~n", [Child_name,Dst_pc,Ets_children,Data]),
    case gen_server:call({local,Dst_pc},{transfer,Child_name,Data}) of
        ok -> ets:delete(Ets_children,Child_name);
        _  -> io:format("problem in transfer")
   end.

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
    ?PC1 -> rpc:call(node(),ride,start,[HostName,open,ridePc1]);
    ?PC2 -> rpc:call(node(),ride,start,[HostName,open,ridePc2]);
    ?PC3 -> rpc:call(node(),ride,start,[HostName,open,ridePc3]);
    ?PC4 -> rpc:call(node(),ride,start,[HostName,open,ridePc4])
  end.
