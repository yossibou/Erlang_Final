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
    Return = gen_server:start_link({global, master}, ?MODULE, [], []),
    Return.
stop() ->

  gen_server:stop({local, master}).
init([]) ->

    net_kernel:monitor_nodes(true), % monitor nodes

    ets:new(children,[set,named_table]),
    ets:new(data,[set,named_table]),
    ets:insert(data,{children_count,0}),
    ets:insert(data,{money,0}),
    ets:insert(data,{msg,0}),
    ets:insert(data,{msg2,0}),

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
        [{_,Msg}] = ets:lookup(data,msg),     
    ets:insert(data,{msg,Msg+1}),
    [{_,Money}] = ets:lookup(data,money),
    ets:insert(data,{money,Money + Val}),
    {noreply, []};

handle_cast({msg,Val}, []) ->
    [{_,Msg}] = ets:lookup(data,msg),     
    ets:insert(data,{msg,Msg+1}),
    [{_,Msg2}] = ets:lookup(data,msg2),
    ets:insert(data,{msg2,Msg2 + Val}),
    {noreply, []};

handle_cast(statistics, []) ->
    io:format("statistics ~n"),
    [{_,Money}] = ets:lookup(data,money),
    [{_,Children_count}] = ets:lookup(data,children_count),
    [{_,Msg}] = ets:lookup(data,msg),   
    [{_,Msg2}] = ets:lookup(data,msg2), 
    io:format("Children_count: ~p ~n",[Children_count]),
    io:format("money: ~p ~n",[Money]),
    io:format("msg host-master count: ~p ~n",[Msg]),
    io:format("msg host-host count: ~p ~n",[Msg2]),
    %io:format("children list: ~n ~p ~n",[ets:tab2list(children)]),
    {noreply, []};

handle_cast(Msg, []) ->
        [{_,Msg1}] = ets:lookup(data,msg),     
    ets:insert(data,{msg,Msg1+1}),
    Function = fun({Child_name,Data}) -> ets:insert(children,{Child_name,Data}) end,
    lists:foreach(Function, Msg),
    Children_count = length(ets:tab2list(children)),
    gen_server:cast({global,gui},refresh),
    ets:insert(data,{children_count,Children_count}),
    %io:format("children_count: ~p~n", [Children_count]),
    gen_server:cast({global,?PC1},{children_count,Children_count}),
    gen_server:cast({global,?PC2},{children_count,Children_count}),
    gen_server:cast({global,?PC3},{children_count,Children_count}),
    gen_server:cast({global,?PC4},{children_count,Children_count}),
    %io:format("handle_call: ~p~n", [Msg]),
    {noreply, []}.

handle_info({nodeup,PC},State)->
  io:format("~p nodeup ~n",[PC]),
  {noreply, State};

handle_info({nodedown,PC},State)-> % if a node is down, check which PC, move responsibilities to different PC
  io:format("~p nodedown ~n",[PC]),
  case PC of
    ?PC1 -> global:unregister_name(?PC1),erpc:cast(?PC2,host,start,[?PC1,{0,0},{0,400,0,250},100]),
            timer:sleep(200),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX<400 andalso CurY<250 of
                    true -> global:unregister_name(Child_name),gen_server:cast({global,?PC1},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC2 -> global:unregister_name(?PC2),erpc:cast(?PC3,host,start,[?PC2,{0,500},{0,400,250,500},100]),
            timer:sleep(200),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX<400 andalso CurY>250 of
                    true -> global:unregister_name(Child_name),gen_server:cast({global,?PC2},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC3 -> global:unregister_name(?PC3),erpc:cast(?PC4,host,start,[?PC3,{800,500},{400,800,250,500},100]),
            timer:sleep(200),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX>400 andalso CurY>250 of
                    true -> global:unregister_name(Child_name),gen_server:cast({global,?PC3},{transfer,Child_name,[Child]});
                    _    -> ok
                end
            end,
            lists:foreach(Function, ets:tab2list(children));
    ?PC4 -> global:unregister_name(?PC4),erpc:cast(?PC1,host,start,[?PC4,{800,0},{400,800,0,250},100]),
            timer:sleep(200),
            Function = fun(Child) ->
                {Child_name,{_,{CurX,CurY},_}} = Child,
                case CurX>400 andalso CurY<250 of
                    true -> global:unregister_name(Child_name),gen_server:cast({global,?PC4},{transfer,Child_name,[Child]});
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
