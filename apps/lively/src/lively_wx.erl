-module(lively_wx).

-include_lib("wx/include/wx.hrl").

-compile([export_all]).

stop() -> wx_object:stop(?MODULE).

start() ->
    case wx_object:start(?MODULE, [], []) of
  Err = {error, _} -> Err;
  _Obj -> ok
    end.


init(_Args) ->
    register(?MODULE, self()),
    wx:new(),
    catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Lively Erlang",
      [{size, {850, 600}}, {style, ?wxDEFAULT_FRAME_STYLE}]),
    State = setup(#{ frame => Frame }),
    process_flag(trap_exit, true),
    {Frame, State}.

-define(stc, wxStyledTextCtrl).

-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.


setup(#{ frame := Frame }) ->
	Panel = wxPanel:new(Frame, []),

	Sz    = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
	wxPanel:setSizer(Panel, Sz),


	FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
	Ed = wxStyledTextCtrl:new(Panel),

	?stc:styleClearAll(Ed),
	?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
	?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
	?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
	LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
	?stc:setMarginWidth(Ed, 0, LW),
	?stc:setMarginWidth(Ed, 1, 0),

	#{ source := Src } = lively:prepare("./lively.erl", lively),
	?stc:addText(Ed, Src),

	?stc:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
	%%?stc:hideSelection(Ed, true),

	?stc:connect(Ed, stc_updateui,
							 [{callback, fun(_, _) ->
															 NewSrc = ?stc:getText(Ed),
															 io:format("text updated, recompiling:\n\n ~s~n \n\n", [NewSrc]),
io:format("~p\n\n", [lively:reload(NewSrc)])
													 end}]),


	Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
						 {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
						 {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
						 {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
						 {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
						 {?wxSTC_ERLANG_STRING,   {170,45,132}},
						 {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
						 {?wxSTC_ERLANG_ATOM,     {0,0,0}},
						 {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
						 {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
						 {?wxSTC_ERLANG_MACRO,    {40,144,170}},
						 {?wxSTC_ERLANG_RECORD,   {40,100,20}},
						 {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
						 {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
						 %% Optional 2.9 stuff
						 {?wxSTC_ERLANG_COMMENT_FUNCTION, {160,53,35}},
						 {?wxSTC_ERLANG_COMMENT_MODULE, {160,53,35}},
						 {?wxSTC_ERLANG_COMMENT_DOC, {160,53,35}},
						 {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160,53,35}},
						 {?wxSTC_ERLANG_ATOM_QUOTED, {0,0,0}},
						 {?wxSTC_ERLANG_MACRO_QUOTED, {40,144,170}},
						 {?wxSTC_ERLANG_RECORD_QUOTED, {40,100,20}},
						 {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0,0,0}},
						 {?wxSTC_ERLANG_BIFS, {130,40,172}},
						 {?wxSTC_ERLANG_MODULES, {64,102,244}},
						 {?wxSTC_ERLANG_MODULES_ATT, {64,102,244}}
						],
	SetStyle = fun({Style, Color}) ->
								 ?stc:styleSetFont(Ed, Style, FixedFont),
								 ?stc:styleSetForeground(Ed, Style, Color)
						 end,
	[SetStyle(Style) || Style <- Styles],
	?stc:setKeyWords(Ed, 0, keyWords()),

	%% Scrolling
	Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
	?stc:setYCaretPolicy(Ed, Policy, 3),
	?stc:setVisiblePolicy(Ed, Policy, 3),

	%% ?stc:connect(Ed, stc_doubleclick),
	%% ?stc:connect(Ed, std_do_drop, fun(Ev, Obj) -> io:format("Ev ~p ~p~n",[Ev,Obj]) end),
	?stc:setReadOnly(Ed, false),


	wxSizer:add(Sz, Ed, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxWindow:refresh(Panel),
	wxWindow:raise(Frame),
	wxWindow:setFocus(Frame),
	wxFrame:show(Frame),

	#{ frame => Frame
	 , panel => Panel
	 , editor => Ed
	 }.


keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
   "end","fun","if","let","of","receive","when","bnot","not",
   "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).

