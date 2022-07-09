%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------

%[ets:tab2list(pc1_children),ets:tab2list(pc2_children),ets:tab2list(pc3_children),ets:tab2list(pc4_children)].

-module(master).
-author("Yossi Bouskila, Tal Tubul").
-behaviour(gen_server).
-include("computers.hrl").
-export([start/0,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
start() ->
    Return = gen_server:start_link({global, master}, ?MODULE, [], []),
    Return.
stop() ->

  gen_server:stop({global, master}).
init([]) ->

    %net_kernel:monitor_nodes(true), % monitor nodes
    %timer:sleep(200),
    %net_kernel:connect_node(?PC1), % connect all nodes
    %timer:sleep(200),
    %net_kernel:connect_node(?PC2),
    %timer:sleep(200),
    %net_kernel:connect_node(?PC3),
    %timer:sleep(200),
    %net_kernel:connect_node(?PC4),
    %timer:sleep(200),

    ets:new(children,[set,named_table]),
    ets:new(data,[set,named_table]),
    ets:insert(data,{children_count,0}),
    %host:start(pc1,{0,0},{0,10,0,10}),
    %host:start(pc2,{0,20},{0,10,10,20}),
    %host:start(pc3,{20,20},{10,20,10,20}),
    %host:start(pc4,{20,0},{10,20,0,10}),
    % start all servers
    rpc:call(?PC1,host,start,[pc1,{0,0},{0,400,0,250}]),
    rpc:call(?PC2,host,start,[pc2,{0,500},{0,400,250,500}]),
    rpc:call(?PC3,host,start,[pc3,{800,500},{400,800,250,500}]),
    rpc:call(?PC4,host,start,[pc4,{800,0},{400,800,0,250}]),
    spawn(gui,start,[]),
    Return = {ok, [],2000},
    Return.

handle_call(Request, _From, []) ->
    Reply = ok,
    %io:format("handle_call: ~p~n", [Request]),
    {reply, Reply, [],2000}.

handle_cast(Msg, []) ->
    Function = fun({Child_name,Data}) -> ets:insert(children,{Child_name,Data}) end,
    lists:foreach(Function, Msg),
    %io:format("handle_call: ~p~n", [Msg]),
    %{Child_name,Data} = Msg,
    {noreply, [],2000}.


handle_info(_Info, []) ->
    io:format("handle_call: ~p~n", [ets:tab2list(children)]),
    {noreply, [],2000}.

terminate(_Reason, []) ->
    rpc:call(?PC1,host,stop,[pc1]),
    rpc:call(?PC2,host,stop,[pc2]),
    rpc:call(?PC3,host,stop,[pc3]),
    rpc:call(?PC4,host,stop,[pc4]),
    ets:delete(data),
    ets:delete(children),

    %host:stop(pc1),
    %host:stop(pc2),
    %host:stop(pc3),
    %host:stop(pc4),

    io:format("terminate: all data erase~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    %io:format("code_change: ~p~n", [Return]),
    Return.
