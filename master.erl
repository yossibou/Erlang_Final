%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------


-module(master).
-author("Yossi Bouskila").
-behaviour(gen_server).
-include("computers.hrl").
-export([start/0,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
start() ->
    Return = gen_server:start_link({local, master}, ?MODULE, [], []),
    Return.
stop() ->

  gen_server:stop({local, master}).
init([]) ->

    net_kernel:monitor_nodes(true), % monitor nodes

    ets:new(children,[set,named_table]),
    ets:new(data,[set,named_table]),
    ets:insert(data,{children_count,0}),
    ets:insert(data,{money,0}),

    % start all servers
    erpc:cast(?PC1,host,start,[?PC1,{0,0},{0,400,0,250},0]),
    timer:sleep(200),
    erpc:cast(?PC2,host,start,[?PC2,{0,500},{0,400,250,500},0]),
    timer:sleep(200),
    erpc:cast(?PC3,host,start,[?PC3,{800,500},{400,800,250,500},0]),
    timer:sleep(200),
    erpc:cast(?PC4,host,start,[?PC4,{800,0},{400,800,0,250},0]),
    timer:sleep(200),
    %spawn(gui,start,[]),
    spawn(generator,start,[]),
    Return = {ok, []},
    Return.

handle_call(_Request, _From, []) ->
    Reply = ok,
    {reply, Reply, []}.

handle_cast({money,Val}, []) ->
    [{_,Money}] = ets:lookup(data,money),
    ets:insert(data,{money,Money + Val}),
    {noreply, []};

handle_cast(statistics, []) ->
    io:format("statistics ~n"),
    [{_,Money}] = ets:lookup(data,money),
    io:format("money: ~p ~n",[Money]),
    io:format("children list: ~n ~p ~n",[ets:tab2list(children)]),
    {noreply, []};

handle_cast(Msg, []) ->
    Function = fun({Child_name,Data}) -> ets:insert(children,{Child_name,Data}) end,
    lists:foreach(Function, Msg),
    Children_count = length(ets:tab2list(children)),
    gen_server:cast({gui,?MASTER},refresh),
    ets:insert(data,{children_count,Children_count}),
    %io:format("children_count: ~p~n", [Children_count]),
    gen_server:cast({?PC1,?PC1},{children_count,Children_count}),
    gen_server:cast({?PC2,?PC2},{children_count,Children_count}),
    gen_server:cast({?PC3,?PC3},{children_count,Children_count}),
    gen_server:cast({?PC4,?PC4},{children_count,Children_count}),
    %io:format("handle_call: ~p~n", [Msg]),
    {noreply, []}.

handle_info({nodeup,PC},State)->
  io:format("~p nodeup ~n",[PC]),
  {noreply, State};

handle_info({nodedown,PC},State)-> % if a node is down, check which PC, move responsibilities to different PC
  io:format("~p nodedown ~n",[PC]),
  case PC of
    ?PC1 -> erpc:call(?PC2,host,start,[?PC1,{0,0},{0,400,0,250},100]),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX<400 andalso CurY<250 of
                    true -> gen_server:call({?PC1,?PC2},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC2 -> erpc:call(?PC3,host,start,[?PC2,{0,500},{0,400,250,500},100]),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX<400 andalso CurY>250 of
                    true -> gen_server:call({?PC2,?PC3},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC3 -> erpc:call(?PC4,host,start,[?PC3,{800,500},{400,800,250,500},100]),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX>400 andalso CurY>250 of
                    true -> gen_server:call({?PC3,?PC4},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC4 -> erpc:call(?PC1,host,start,[?PC4,{800,0},{400,800,0,250},100]),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX>400 andalso CurY<250 of
                    true -> gen_server:call({?PC4,?PC1},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children))
  end,
  {noreply, State};

handle_info(_Info, []) ->
    io:format("handle_call: ~p~n", [ets:tab2list(children)]),
    {noreply, [],1000}.

terminate(_Reason, []) ->
    erpc:cast(?PC1,host,stop,[]),
    erpc:cast(?PC2,host,stop,[]),
    erpc:cast(?PC3,host,stop,[]),
    erpc:cast(?PC4,host,stop,[]),
    ets:delete(data),
    ets:delete(children),

    io:format("terminate: all data erase~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    %io:format("code_change: ~p~n", [Return]),
    Return.
