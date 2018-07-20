%%
%% 

-module(mdrawer_gui).

-export([init/1, handle_info/2, handle_call/3, handle_event/2, 
	 terminate/2, code_change/3]).

-compile(export_all).

-behaviour(wx_object).

-include("mdrawer.hrl").

-import(sudoku_game, [indx/1]).

%%%%%%%%%%  Graphic engine %%%%%%%%%%%%%%

-record(gs,{board,show_err=true,level=hard,game,frame,orig=[], print_d, print_psdd}).

set_graph(App, G) ->
    wx_object:call(App, {set_graph, G}).

new(Game) ->
    wx:new(),
    Game ! {gfx, wx_object:start_link(?MODULE, [Game], [])}.

%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

init([Game]) ->
    {Frame, Board} = wx:batch(fun() -> create_window() end),
    {Frame, init_printer(#gs{board=Board,game=Game,frame=Frame})}.

create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "MGraphic Drawing Tool", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "&New Graphic"),
    wxMenu:append(File, ?OPEN, "&Open Graphic"),
    wxMenu:append(File, ?SAVE, "&Save Graphic"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?PRINT, "Print"),
    wxMenu:append(File, ?PRINT_PAGE_SETUP, "Page Setup"),
    wxMenu:append(File, ?PRINT_PRE, "Print Preview"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?QUIT, "&Quit"),

    wxMenu:append(Help, ?ABOUT, "About"), 

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Top    = wxBoxSizer:new(?wxHORIZONTAL),

    Panel = wxPanel:new(Frame), 
    NewGraphic = wxButton:new(Panel, ?NEW, [{label,"&New Graphic"}]),
    wxButton:connect(NewGraphic, command_button_clicked),
    OpenGraphic = wxButton:new(Panel, ?OPEN, [{label,"&Open Graphic"}]),
    wxButton:connect(OpenGraphic, command_button_clicked),

    wxSizer:addSpacer(Top,2),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:add(Top, NewGraphic, wxSizerFlags:left(SF)), 
    wxSizer:addSpacer(Top,3),
    wxSizer:add(Top, OpenGraphic,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   

    wxSizer:addSpacer(MainSz,3),
    wxSizer:add(MainSz, Top, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:addSpacer(MainSz,3),

    Board = mdrawer_board:new(Panel),

    wxSizer:add(MainSz, Board, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxWindow:setSizer(Panel,MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),

    {Frame, Board}.

status(Win, F, A) ->
    Str = lists:flatten(io_lib:format(F, A)),
    wxFrame:setStatusText(Win, Str).

%%%%%%%%%%%%%%%% Info i.e. messages %%%%%%%%%%%%%%%%%%%%%

handle_info(quit, S=#gs{game=G,frame=F}) ->
    wxWindow:close(F),
    wx_core:quit(), 
    G ! quit,
    {stop, shutdown, S}.

%%%%%%%%%%%%%%%%% GUI-Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event=#wxClose{}},
	     S = #gs{game=G,frame=F}) ->
    catch wxWindow:'Destroy'(F),
    G ! quit,
    {stop, shutdown, S};

handle_event(#wx{id=?QUIT, event=#wxCommand{type=command_menu_selected}},
	     S = #gs{game=G,frame=F}) ->
    wxWindow:close(F,[]),
    G ! quit,
    {stop, shutdown, S};

%% type=command_button_clicked,
handle_event(#wx{id=?NEW, event=#wxCommand{}},
	     S = #gs{game=G, board=Board}) ->
    G ! {op,?NEW,S#gs.level},
    mdrawer_board:setup_board(Board,[]),
    {noreply, S#gs{orig=[]}};
handle_event(#wx{id=ID, event=#wxCommand{}}, S) when ID > 125 ->
    New = dialog(ID, S),
    {noreply, New};
handle_event(Msg,S) ->
    io:format("~p: Unhandled event ~p~n",[?MODULE, Msg]),
    %%sudoku_board:event(Msg, Ids),
    {noreply, S}.

handle_call(frame, _From, S = #gs{frame=F}) ->
    {reply, F, S};

handle_call({set_graph, G}, _From, S=#gs{frame=Frame, board=Board}) ->
    {reply, mdrawer_board:set_board_data(Board, G), S};
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    normal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dialog(?SAVE, S=#gs{frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor 
				   ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    {ok,Fd} = file:open(Path, [write]),
	    List = mdrawer_board:get_board_data(Board),
	    io:format(Fd, "~p.~n", [List]),
	    file:close(Fd);
	_ ->
	    ignore
    end,
    wxDialog:destroy(FD),
    S;
dialog(?OPEN, S=#gs{game=Server, frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    case file:consult(Path) of
		{ok, [Game]} when is_tuple(Game) ->
		    Vals = mdrawer_board:set_board_data(Board, Game),
		    Server ! {loaded, Vals};		    
		_ ->
		    ignore
	    end;
	_ ->
	    ignore
    end,
    wxFileDialog:destroy(FD),
    S;
dialog(?ABOUT,  S=#gs{frame=Frame}) ->
    Str = "A tool for drawing Mathematica Graphic\n\n/lzj",
    MD = wxMessageDialog:new(Frame,Str,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About box"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    S;

dialog(?PRINT_PAGE_SETUP, S = #gs{frame=Frame, print_psdd=PsDD0, print_d=PD0}) ->
    wxPageSetupDialogData:setPrintData(PsDD0, PD0),
    PSD = wxPageSetupDialog:new(Frame, [{data,PsDD0}]),
    wxPageSetupDialog:showModal(PSD),
	
    PSDD1 = wxPageSetupDialog:getPageSetupData(PSD),
    PD1 = wxPageSetupDialogData:getPrintData(PSDD1),
    %% Create new objects using copy constr.
    PD = wxPrintData:new(PD1),
    PsDD = wxPageSetupDialogData:new(PSDD1),
    wxPageSetupDialog:destroy(PSD),
    wxPageSetupDialogData:destroy(PsDD0),
    wxPrintData:destroy(PD0),
    S#gs{print_psdd=PsDD, print_d=PD};
dialog(?PRINT_PRE, S = #gs{frame=Frame, print_d=PD}) ->    
    PDD = wxPrintDialogData:new(PD),
    Printout1 = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Printout2 = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Preview = wxPrintPreview:new(Printout1, [{printoutForPrinting,Printout2},{data,PDD}]), 
    case wxPrintPreview:isOk(Preview) of
	true ->
	    PF = wxPreviewFrame:new(Preview, Frame, [{title, "Print Preview"}]),
	    wxPreviewFrame:centre(PF, [{dir, ?wxBOTH}]),
	    wxPreviewFrame:initialize(PF),
	    wxPreviewFrame:centre(PF),
	    wxPreviewFrame:show(PF);
	false ->
	    io:format("Could not create preview window.\n"
		      "Perhaps your current printer is not set correctly?~n", []),
	    wxPrintPreview:destroy(Preview)
    end,
    S;
dialog(?PRINT, S = #gs{frame=Frame, print_d=PD}) ->    
    PDD = wxPrintDialogData:new(PD),
    Printer = wxPrinter:new([{data,PDD}]),
    Printout = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,S) end,
			      [{getPageInfo, fun getPageInfo/1}]),

    case wxPrinter:print(Printer, Frame, Printout, [{prompt,true}]) of
	false -> 
	    case wxPrinter:getLastError() of
		?wxPRINTER_ERROR ->
		    io:format("There was a problem printing.\n"
			      "Perhaps your current printer is not set correctly?~n", []);
		_ ->
		    io:format("You canceled printing~n", [])
	    end,
	    wxPrinter:destroy(Printer),
	    S;
	true ->
	    PDD2 = wxPrinter:getPrintDialogData(Printer),
	    PD2  = wxPrintDialogData:getPrintData(PDD2),
	    %% Copy data PD2 will be deleted when Printer is destroyed
	    PD3 = wxPrintData:new(PD2),  
	    wxPrintData:destroy(PD),
	    wxPrinter:destroy(Printer),
	    S#gs{print_d = PD3}
    end;

dialog(Other, S) ->
    io:format("other ~p~n",[Other]),
    S.

init_printer(S) ->
    PD   = wxPrintData:new(),

    %% You could set an initial paper size here
    %%    g_printData->SetPaperId(wxPAPER_LETTER); // for Americans
    %%    g_printData->SetPaperId(wxPAPER_A4);    // for everyone else    

    wxPrintData:setPaperId(PD, ?wxPAPER_ENV_ITALY),
    wxPrintData:setOrientation(PD, ?wxLANDSCAPE),

    PSDD = wxPageSetupDialogData:new(PD),
    wxPageSetupDialogData:setMarginTopLeft(PSDD,{15,15}),
    wxPageSetupDialogData:setMarginBottomRight(PSDD,{15,15}),

    S#gs{print_d=PD, print_psdd=PSDD}.

getPageInfo(_This) -> 
    {1,1,1,1}.

printout(This, _Page, #gs{board=Board, print_psdd=PsDD}) ->
    MX = 1200,
    MY = 450,  
    wxPrintout:fitThisSizeToPageMargins(This, {MX,MY}, PsDD),
    
    _DBG = {_X,_Y,W,H} = wxPrintout:getLogicalPageMarginsRect(This, PsDD),
    wxPrintout:offsetLogicalOrigin(This,(W-MX) div 2, (H-MY) div 2),
%%    io:format("~p ->{~p,~p} ~n", [_DBG, (W-MX) div 2, (H-MY) div 2]),

    DC = wxPrintout:getDC(This),
    mdrawer_board:draw(Board, DC, {MX, MY}),
    true.


