%%


-module(mdrawer).

-export([go/0, start/0, set_graph/2]).

-include("mdrawer.hrl").

start() -> 
    %spawn_link(fun() -> init(halt) end).
    mdrawer_gui:new(self()),
    receive {gfx, GFX} -> GFX end.
    
go() -> 
    spawn_link(fun() -> init(keep) end).

init(Halt) ->
    ?TC(mdrawer_gui:new(self())),
    receive {gfx, GFX} -> GFX end.

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
        {_,{'EXIT',Reason}} -> exit(Reason);
        {T,R} ->
            io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
            R
    end.

set_graph(App, G) ->
    mdrawer_gui:set_graph(App, G).
