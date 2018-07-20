%% cd("../lib/wx-0.98.2/examples/drawer").

-module(mdrawer_board).

-export([new/1, setup_board/2, clear_board/1, left/1,
	 get_board_data/1,set_board_data/2, 
	 draw/3, 
	 %% Callbacks
	 init/1, handle_sync_event/3, 
	 handle_event/2, handle_info/2, handle_call/3, 
	 code_change/3, terminate/2]).

-include("mdrawer.hrl").

-record(state, {win, parent, board=[], bitmap = null, mapping}).
-record(sq, {key,val,correct=true,given=false}).
-record(drawcontext, {dc, pen, brush, font}).

-define(BRD,10).
-define(ARC_R, 10).
    
-behaviour(wx_object).

%% API 
new(ParentObj) ->
    {ok,Content}=file:consult("/home/nevena/Desktop/Project/mygraph.graphic"),
    R = wx_object:start_link(?MODULE, [ParentObj, self()], []),
    set_board_data(R, lists:last(Content)),
    R.

setup_board(Board, Init) ->
    wx_object:call(Board, {set_board_data, []}).

clear_board(Board) ->
    wx_object:call(Board, clear_board).

left(Board) ->
    wx_object:call(Board, left).

get_board_data(Board) ->
    wx_object:call(Board, get_board_data).
set_board_data(Board, List) ->
    wx_object:call(Board, {set_board_data, List}).


draw(Board, DC, Size) ->
    wx_object:call(Board, {draw, DC, Size}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentObj, ParentPid]) ->
    Win = wxWindow:new(ParentObj, ?wxID_ANY, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setFocus(Win), %% Get keyboard focus
    wxWindow:setSizeHints(Win, {250,250}),
    wxWindow:connect(Win, paint,  [callback]),
    wxWindow:connect(Win, size,  []),
    wxWindow:connect(Win, erase_background, []),
    wxWindow:connect(Win, key_up, [{skip, true}]),
    wxWindow:connect(Win, left_down, [{skip, true}]),
    wxWindow:connect(Win, enter_window, [{skip, true}]), 

    {Win, redraw(#state{win=Win, 
                board=[], 
                mapping = {{-30, 30}, {10, 10}},
                parent=ParentPid})}.

handle_sync_event(#wx{event=#wxPaint{}}, _Obj, State = #state{win=Win, bitmap = Bitmap}) ->
    DC = wxPaintDC:new(Win),
    redraw_buf(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{win=Win}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {noreply,State};
handle_event(#wx{event=#wxMouse{type=left_down,x=X,y=Y}},
	     S = #state{parent=Gui, win=F}) ->
    case error of
	error -> ignore;
	_ -> create_popup_menu(Gui,0,X,Y,F)
    end,
    {noreply, S};
handle_event(#wx{event=#wxSize{}}, State) ->
    {noreply,redraw(State)};
handle_event(_Ev, State) ->
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%

handle_call(clear_board,_From, State = #state{board=B0}) ->    
    B = [Butt || Butt = #sq{given=true} <- B0],
    S = redraw(State#state{board=B}),
    Given = [{Key, Val} || #sq{key=Key,val=Val,given=true} <- B],
    {reply, Given, S};
handle_call(get_board_data,_From, S=#state{board=B0, mapping = Mapping}) ->    
    {reply, {'Graphics', B0, [{mapping, Mapping}]}, S};
handle_call({set_board_data, B},_From, S0) ->
    S = case B of
        {'Graphics', Data} ->
            redraw(S0#state{board=Data});
        {'Graphics', Data, Options} ->
            Mapping = proplists:get_value(mapping, Options, S0#state.mapping),
            redraw(S0#state{board=Data, mapping = Mapping});
        _ -> 
            redraw(S0#state{board=[]})
    end,
    {reply, ok, S};
handle_call(left,_From, S = #state{board=B}) ->
    Res = 81 - length([ok || #sq{correct=C} <- B, C /= false]),
    {reply, Res, S};
handle_call({draw, DC, Size},_From, S = #state{bitmap = Bitmap}) ->    
    % redraw_buf(DC, Bitmap),
    redraw(DC, Size, S),
    {reply, ok, S}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redraw_buf(DC, null) -> ok;
redraw_buf(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),

    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),

    wxMemoryDC:destroy(MemoryDC),
    
    ok.

%redraw(S) -> S;
redraw(S = #state{win=Win, bitmap = OldBitmap}) ->

    case OldBitmap of
        null -> 0;
        _ -> wxBitmap:destroy(OldBitmap)
    end,

    {W, H} = wxWindow:getSize(Win),
    Bitmap = wxBitmap:new(W,H),
    MemoryDC = wxMemoryDC:new(Bitmap),
    redraw(MemoryDC, {W, H}, S),
    wxMemoryDC:destroy(MemoryDC),

    % trigger a refresh
    wxWindow:refresh(Win),

    S#state{bitmap = Bitmap}.

redraw(DC, Size, S = #state{win=Win, board = Board, mapping = Mapping}) ->    
    wx:batch(fun() -> 

             %% Init pens and fonts
             Pen = wxPen:new({0,0,0}, [{width, 1}]),
             Font  = wxFont:new(10, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[]),
             Brush = wxBrush:new(),

		     wxDC:setBackground(DC, ?wxWHITE_BRUSH),
		     wxDC:clear(DC),
             {W, H} = Size,
             wxDC:setFont(DC, Font),
             wxDC:setPen(DC, Pen),
             wxDC:setBrush(DC, Brush),

             % wxDC:drawText(DC, "hello world!", {W div 2, H div 2}),
             draw_entities(#drawcontext{dc = DC, pen = Pen, brush = Brush, font = Font}, Mapping, Board),

             wxPen:destroy(Pen),
             wxFont:destroy(Font),
             wxBrush:destroy(Brush)
     end).

map_logic_to_dc({X, Y}, {{OriginX, OriginY}, {MapX, MapY}}) ->
    {round((X - OriginX) * MapX), round((OriginY - Y) * MapY)}.

map_to_byte(R) ->
    case round(R * 255) of
        X when X =< 0 -> 0;
        X when X >= 255 -> 255;
        X -> X
    end.

%% Point ！ a point or list of points in 2D or 3D
%% PointSize  ? AbsolutePointSize  ? PlotMarkers  ? RGBColor
%%     
%% Line ！ a line joining a sequence of points in 2D or 3D
%% BezierCurve, BSplineCurve ！ splined curves in 2D or 3D
%% Thick  ? Thin  ? Thickness  ? AbsoluteThickness
%% Dashed  ? Dotted  ? DotDashed  ? Dashing  ? AbsoluteDashing
%%     
%% Arrow ！ an arrow from one point to another in 2D or 3D
%% Arrowheads  ? Thickness  ? Dashing  ? RGBColor
%%     
%% Polygon ！ a polygon in 2D or 3D
%% RGBColor  ? Opacity  ? EdgeForm  ? FaceForm  ? VertexColors  ? ...
%%     
%% Disk, Circle ！ filled 2D disk, open 2D circle, ellipse or arc
%% Rectangle ！ 2D filled rectangle
%% RGBColor  ? Opacity  ? EdgeForm  ? Thickness  ? Dashing

draw_entities(Context = #drawcontext{dc = DC}, Mapping, Primitives) when is_list(Primitives) ->
    lists:map(fun (Prim) -> draw_entities(Context, Mapping, Prim) end, Primitives);

draw_entities(Context, Mapping, {'RGBColor', {R, G, B}}) ->
    draw_entities(Context, Mapping, {'RGBColor', {R, G, B, 1}});

draw_entities(Context = #drawcontext{dc = DC, pen = Pen, brush = Brush}, _Mapping, {'RGBColor', {R, G, B, A}}) ->
    Color = {map_to_byte(R), map_to_byte(G), map_to_byte(B), map_to_byte(A)}, 
    wxDC:setTextForeground(DC, Color),
    wxPen:setColour(Pen, Color),
    wxDC:setPen(DC, Pen),
    wxBrush:setColour(Brush, Color),
    wxDC:setBrush(DC, Brush);

draw_entities(Context = #drawcontext{dc = DC, pen = Pen}, _Mapping, {'AbsoluteThickness', Thick}) ->
    wxPen:setWidth(Pen, Thick),
    wxDC:setPen(DC, Pen);

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Line', [H | PtList10] = Pts}) ->
    case H of
        [H0 | _] -> lists:map(fun (X) -> draw_lines(DC, Mapping, X) end, Pts);
        _ -> draw_lines(DC, Mapping, Pts)
    end;

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Disk', {X, Y}, R, {Theta1, Theta2}}) ->
    % wxDC::DrawEllipticArc
    0;

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Text', Expr}) ->
    draw_entities(Context, Mapping, {'Text', Expr, {0, 0}, {0, 0}, {0, 0}});

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Text', Expr, {X, Y}}) ->
    draw_entities(Context, Mapping, {'Text', Expr, {X, Y}, {0, 0}, {0, 0}});

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Text', Expr, {X, Y}, Offset}) ->
    draw_entities(Context, Mapping, {'Text', Expr, {X, Y}, Offset, {0, 0}});

draw_entities(Context = #drawcontext{dc = DC}, Mapping, {'Text', Expr, {X, Y}, {OffX, OffY}, _Dir}) ->
    {W, H} = wxDC:getTextExtent(DC, Expr),
    {X0, Y0} = map_logic_to_dc({X, Y}, Mapping),
    {XD, YD} = {X0 + ((-1 - OffX) * W div 2), Y0 + ((OffY - 1) * H div 2)},
    wxDC:drawText(DC, Expr, {XD, YD});

draw_entities(Context = #drawcontext{dc = DC}, _Mapping, P) ->
    io:format("Bad primitive: ~p~n", [P]).

draw_lines(DC, Mapping, Pts) ->
    DevPts = lists:map(fun (P) -> map_logic_to_dc(P, Mapping) end, Pts),
    wxDC:drawLines(DC, DevPts).

sel_font(_BS,[{_H,_Sz,F}]) ->
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,_H, _BS]),
    F;
sel_font(BS,[{H,_Sz,F}|_]) when BS > (H + 6) -> 
    %%   io:format("Font sz ~p height ~p in BS ~p~n",[_Sz,H, BS]),
    F;
sel_font(BS,[_|Fs]) ->
    sel_font(BS,Fs).

draw_number(DC,F,Sz,#sq{key={R,C},val=Num,given=Bold,correct=Correct}) ->
    {X,Y} = get_coords(Sz,R-1,C-1),
    TBox = Sz div 3,
    if Bold -> 
	    wxFont:setWeight(F,?wxBOLD),
	    wxDC:setTextForeground(DC,{0,0,0});
       Correct =:= false ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{255,40,40,255});
       true ->
	    wxFont:setWeight(F,?wxNORMAL),
	    wxDC:setTextForeground(DC,{50,50,100,255})
    end,
    wxDC:setFont(DC,F),
    CH = (TBox - wxDC:getCharHeight(DC)) div 2,
    CW = (TBox - wxDC:getCharWidth(DC)) div 2,
    wxDC:drawText(DC, integer_to_list(Num), {X+CW,Y+CH+1}),
    ok.

get_coords(Sz,R,C) ->
    TBox = Sz div 3,
    R1 = R div 3,
    R2 = R rem 3,
    C1 = C div 3,
    C2 = C rem 3,
    {?BRD + C1*Sz + C2*TBox,
     ?BRD + R1*Sz + R2*TBox}.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 3.


%% popupmenu

create_popup_menu(GFX,Butt,X,Y,Frame) ->
    Port = wx:get_env(),
    spawn_link(fun() -> create_popup_menu1(GFX,Butt,Port,X,Y,Frame) end).

create_popup_menu1(GFX,Butt,Port,X,Y,Frame) ->
    wx:set_env(Port),
    PopupMenu = wxMenu:new(),
    create_popup_menu2(1, PopupMenu),

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    wxWindow:popupMenu(Frame,PopupMenu,X,Y),
    receive 
	#wx{event=#wxCommand{type=command_menu_selected},id=What} ->
	    GFX ! {set_val,Butt,What}
    end.

create_popup_menu2(N,PP) when N > 9 ->
    wxMenu:append(PP, 0, "Clear");
create_popup_menu2(N,PP) ->
    wxMenu:append(PP, N,integer_to_list(N)),
    create_popup_menu2(N+1,PP).

