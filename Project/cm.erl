% Chandy-Misra shortest path algorithm
% Context: CM algo without termination

% Compile with: c(cm).
% Run e.g. with: cm:master("graph_cm.txt","res.txt",0,1).

% Ref: K. M. Chandy, J. Misra : "Distributed Computation on Graphs: Shortest Path
%      Algorithms"; CACM, Vol. 25, n° 11, Nov. 1982, pp. 833-837.

% @spec master(In::file(), Out::file(), SourceVertex::atom(), Timer::integer()) -> list() of results
%	Deploy the app, gather and output the results

% @cm(V::pid(), WSuccs::list(), D::integer(), Hist::list()) -> ok
%	on receiving a new distance NewDist --- loop until stop
%		execute cm algorithm (i.e. possibly update D and send new Ds to WSuccs)

% @sketch of message exchanges between MASTER and CMs processes
%	MASTER  ---!start-->  source CM
%	MASTER  ---!stop--->  all CMs
%	MASTER  <--!res-----  all CMs
%
%	---Pred!dist--->  CM  ---!dist+w_i--->  all succs of CM
%   ---Pred!stop--->  CM  ---!result----->  MASTER

% Authors: Beat Hirsbrunner, Daniel Weibel, University of Fribourg, Swiztzerland; 
% Version 1.0, 30 April 2013

-module(cm).
-export([master/4, cm/0, cm/3]).

master(In, Out, Source, Timeout) -> 
	{ok, [Name,Vs|WEs]} = file:consult(In),          % read weighted directed graph
	Regs = deploy_cm_vertices(Vs),                   % deploy all cm_vertices, where Regs is a list() of {V,Pid_V}

	Pid_Vs = pids(Vs, Regs),                         % conversion of vertex names to pids
	Pid_WEs = pids2(WEs, Regs),                      % conversion of successor vertex names to pids

	init_cm_vertices(Pid_WEs),                       % initialize all cm_vertices
	pid(Source, Regs) ! {dist, 0},                   % start Source vertex

	timer:sleep(Timeout),                            % wait Timeout msec
	terminate_cm_vertices(Pid_Vs),                   % signal all cm_vertices to terminate
	Res = gather_results(Pid_Vs, Regs), 			 % gather the results
	print_results(In, Out, Source, Timeout, Name, Vs, WEs, Res), % print the results nicely
	Res.                                             % return the results
	
cm() ->
    receive {init, WSuccs} -> cm(WSuccs, infinity, []) end.

cm(WSuccs, Dist, Hist) ->
    receive
        {dist, NewDist} when NewDist < Dist ->
            [ Succ ! {dist, NewDist + Weight}  ||  {Succ, Weight} <- WSuccs ],
            cm(WSuccs, NewDist, [NewDist|Hist]);

        {dist, NewDist} when NewDist >= Dist -> cm(WSuccs, Dist, [{t,NewDist}|Hist]); % t for trashed

		{stop, ReqPid} -> ReqPid ! {res, self(), Dist, [stop | Hist]}, ok
    end.

% ------------------------------------------------------------------------------
% Helper master functions
% ------------------------------------------------------------------------------
deploy_cm_vertices(Vs) -> [ {V, spawn(?MODULE, cm, [])}  ||  V <- Vs ].
	
init_cm_vertices(Pid_WEs) -> [ V ! {init, WSuccs}  ||  [V | WSuccs] <- Pid_WEs ], ok.

gather_results(Pid_Vs, Regs) -> [ receive {res, Pid, D, Hist} -> {name(Pid, Regs), D, Hist} end  ||   Pid <- Pid_Vs ].

terminate_cm_vertices(Pid_Vs) -> [ V ! {stop, self()}  ||  V <- Pid_Vs ], ok.

print_results(In, Out, Source, T, Graph_Name, Vs, WEs, Results) ->
	file:write_file(Out, io_lib:fwrite("~p:master(~p, ~p, ~p, ~p)~n", [?MODULE, In, Out, Source, T]), [append]),
	file:write_file(Out, io_lib:fwrite("In:  ~p ~p ~p~n", [Graph_Name, Vs, WEs]), [append]),
	file:write_file(Out, io_lib:fwrite("Out: ~p ~p~n~n", [Graph_Name, Results]), [append]).

% ------------------------------------------------------------------------------
% Helper 'name <-> pid' functions
% ------------------------------------------------------------------------------
% @spec name(P::pid(), Rs::list()) -> N::atom()
%	Return the symbolic name N associated to P via Rs = [{Name,Pid}|T]
name(Pid, [{Name,Pid}|_]) -> Name;
name(Pid, [_|T])          -> name(Pid, T);
name(_, [])               -> undefined.

% @spec pid(N::atom(), Rs::list()) -> P::pid()
%	Return P associated to the symbolic name N via Rs = [{Name,Pid}|T]
pid(Name, [{Name,Pid}|_]) -> Pid;
pid(Name, [_|T])          -> pid(Name, T);
pid(_, [])                -> undefined.
		
% @spec pids(Ns::list(), Rs::list()) -> Ps::list() of pid()
%	Return Ps associated to the symbolic names Ns via Rs = [{Name,Pid}|T]
pids(Names, Regs) -> [ pid(Name, Regs)  ||  Name <- Names ].

% @spec pids2(WEs::list() of [V, WSuccs], Rs::list()) -> list()
%	Return a list where all symbolic vertex names of WEs have been replaced by 
%	there pid via Rs = [{Name,Pid}|T]
pids2(WEs, Regs) -> 
	[ [pid(V, Regs) | pids3(WSuccs, Regs)]  ||  [V | WSuccs] <- WEs ].

pids3(WSuccs, Regs)  ->
	[ {pid(SuccName, Regs), Weight}  ||  {SuccName, Weight} <- WSuccs ].