%%%-------------------------------------------------------------------
%%% @author yossi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:20
%%%-------------------------------------------------------------------
-module(gui).
-author("Yossi Bouskila, Tal Tubul").
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

-export([start/0,init/1,handle_event/2,handle_sync_event/3,handle_info/2,handle_cast/2]).
-define(max_x, 800).
-define(max_y,500).
-define(Timer,1000).

-define(SERVER, ?MODULE).
-record(state, {frame, panel, dc, paint, list,bmpMap,bmpChild}).
%%%-------------------------------------------------------------------

start() ->
  wx_object:start({global,gui},?MODULE,[],[]).

init([]) ->
  % graphics
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,{?max_x, ?max_y}}]),
  Panel  = wxPanel:new(Frame),
  DC=wxPaintDC:new(Panel),
  Paint = wxBufferedPaintDC:new(Panel),
  % create bitmap to all images
  {BmpMap,BmpChild} = createBitMaps(),

  % connect panel
  wxFrame:show(Frame),
  erlang:send_after(?Timer, self(), timer),

  wxPanel:connect(Panel, paint, [callback]),
  %wxPanel:connect (Panel, left_down),
  %wxPanel:connect (Panel, right_down),
  %wxFrame:connect(Frame, close_window),

  {Frame,#state{frame = Frame, panel = Panel, dc=DC, paint = Paint,
    bmpMap = BmpMap,bmpChild =BmpChild}}.

%%%-------------------------------------------------------------------

handle_event(#wx{event = #wxClose{}},State = #state {frame = Frame}) -> % close window event
  io:format("Exiting\n"),
  wxWindow:destroy(Frame),
  wx:destroy(),
  {stop,normal,State}.


handle_sync_event(#wx{event=#wxPaint{}}, _,  _State = #state{
  panel = Panel,
  bmpMap = BmpMap,
  bmpChild =BmpChild}) ->
  DC=wxPaintDC:new(Panel),
  wxDC:clear(DC),
  wxDC:drawBitmap(DC,BmpMap,{0,0}),
  Children = ets:tab2list(children),
  %io:format("gui: ~p~n", [Children]),
  Function = fun({_,{_,{X,Y},_}}) -> wxDC:drawBitmap(DC,BmpChild,{X,Y}) end,
  lists:foreach(Function, Children);
 % printChildren();

handle_sync_event(_Event,_,State) ->
  {noreply, State}.

handle_info(timer, State=#state{frame = Frame}) ->  % refresh screen for graphics

  wxWindow:refresh(Frame), % refresh screen
  erlang:send_after(?Timer,self(),timer),
  {noreply, State}.

handle_cast(refresh, State=#state{frame = Frame}) ->
  wxWindow:refresh(Frame), % refresh screen
  erlang:send_after(?Timer,self(),timer),
  {noreply, State}.


createBitMaps() ->         % create bitmap to all images
  Map = wxImage:new("map.png"),
  MapScale = wxImage:scale(Map,?max_x,?max_y),
  BmpMap = wxBitmap:new(MapScale),
  wxImage:destroy(Map),
  wxImage:destroy(MapScale),

  Child = wxImage:new("child.png"),
  ChildScale = wxImage:scale(Child,15,15),
  BmpChild = wxBitmap:new(ChildScale),
  wxImage:destroy(Child),
  wxImage:destroy(ChildScale),

  {BmpMap,BmpChild}.