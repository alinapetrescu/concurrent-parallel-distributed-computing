% Concurrent, Parallel and Distributed Computing: Project 2014
% Challenge: Graph interactivity (change weights, add/remove vertices, add/remove edges...)
% Compile with: c(sp).
% Run with: sp:start().

-module(sp).
-compile(export_all).

% Démarre l'application
start()->
	G=digraph:new(),
	usage(G).

% Affiche les fonctionnalités mises à disposition de l'utilisateur 
usage(N)->
	{ok, [X]} = io:fread("\nUsage:\nn)   new graph\naV)  add vertex\nrV)  remove vertex\naE)  add edge\nrE)  remove edge\nchW) change edge weight\ngW)  get edge weight\nshPath)shortest path\nimpPath)improved path\ndG)  delete graph\ns)   show data\nv)   visualize\nQ)   quit\n \n>","~s"),
	if
	     X=="n"->newGraph();
	     X=="aV"->addVertex(N);
	     X=="rV"->removeVertex(N);
	     X=="aE"->addEdge(N);
	     X=="rE"->removeEdge(N);
	     X=="dG"->deleteGraph(N);
	     X=="s"->display(N);
	     X=="v"->visualize(N);	     
	     X=="chW"->changeWeight(N);
	     X=="gW"->getWeight(N);
	     X=="shPath"->shPath(N);
	     X=="imPath"->improvedPath(N);
	     X=="Q"->quit();
	     true->usage(N)
	end.

% Crée un nouveau graphe (vide)
newGraph()->
	N=digraph:new(),
	io:format("\nGraph is created ~p~n",[N]),
	usage(N).

% Crée un nouveau sommet avec une identité précisée par l'utilisateur
addVertex(G)->   
	  {ok, [X]} = io:fread("\nVertex ID:","~d"),
	   file:write_file("graph_cm1.txt",""),
	   io:format("\nVertex added ~p~n",[digraph:add_vertex(G, X)]),
	   file:write_file("improvedRes.txt",io_lib:fwrite("{~p,infinity,[stop]}.\n",[X]),[append]),
	   usage(G).

% Supprime le sommet dont l'identité est précisée par l'utilisateur
removeVertex(G)->
	  {ok, [X]} = io:fread("\nVertex to remove:","~d"),
	  io:format("\nVertex removed ~p~n",[digraph:del_vertex(G, X)]),
	  usage(G).

% Crée une nouvelle arête en demandant à l'utilisateur 
% les identités des deux sommets à relier, l'identité de l'arête et son poids
addEdge(G)->
	{ok, [X]} = io:fread("\nFrom vertex:","~d"),
	{ok, [Y]} = io:fread("To vertex:","~d"),
	{ok, [E]} = io:fread("Edge ID:","~d"),
	{ok, [Z]} = io:fread("Weight:","~d"),
	{ok, Res} = file:consult("improvedRes.txt"),
	El=element(2,lists:keyfind(X,1,Res)),
	case El of
	infinity->Q=0;
	Otherwise->Q=El
	end,
	P=lists:keyreplace(Y,1,Res,{Y,Z+Q,[stop,Z+Q]}),
	io:format("\nEdge ~p added\n",[digraph:add_edge(G,E, X, Y, Z)]),
	file:write_file("improvedRes.txt",""),
	lists:foreach(fun(M)->
		file:write_file("improvedRes.txt",io_lib:fwrite("~p.\n",[M]),[append])
	end,P),
	usage(G).

% Supprime l'arête dont l'identité est précisée par l'utilisateur
removeEdge(G)->
	{ok, [X]} = io:fread("\nID of edge to remove:","~d"),
	digraph:del_edge(G,X),
	io:format("Edge deleted\n"),
	usage(G).

% Change le poids d'une arête dont l'identité et le nouveau poids 
% sont indiqués par l'utilisateur
changeWeight(G)->
	{ok, [X]} = io:fread("\nID of edge:","~d"),
	{ok, [W]} = io:fread("New weigth:","~d"),
	FromV=element(2,digraph:edge(G,X)),
	ToV=element(3,digraph:edge(G,X)),
	io:format("\nWeigth of edge ~p changed\n",[digraph:add_edge(G,X,FromV,ToV, W)]),
	io:format("~p\n",[digraph:edge(G,X)]),
	usage(G).

% Retourne le poids d'une arête dont l'identité est précisée par l'utilisateur
getWeight(G)->
	{ok, [X]} = io:fread("\nID of edge:","~d"),
	io:format("\nWeight: ~p\n",[element(4,digraph:edge(G,X))]),
	usage(G).

% Supprime le graphe courant et crée un nouveau graphe
deleteGraph(G)->
	io:format("\nGraph deleted: ~p~n\n",[digraph:delete(G)]),
	N=digraph:new(),
	usage(N).

% Affiche des informations concernant le graphe courant sous forme 
% d'une liste de données (sommets et arêtes)
display(G)->
	io:format("\nVertices: ~p~n\n",[digraph:vertices(G)]),
	io:format("Edges: ~p~n\n",[digraph:edges(G)]),
	usage(G).

% Associe à chaque sommet du graphe courant des coordonnées 
% X et Y obtenues aléatoirement et stocke ces résultats dans une table ets
makeList(G)->
	ets:new(vert,[set,named_table]),
	lists:foreach(fun(E) ->
		X=random:uniform(10000),
		Y=random:uniform(15),
		ets:insert(vert,{E,X,Y}) end,
		digraph:vertices(G)),
	io:format("").

% Affiche ce qui se trouve dans le fichier improvedRes.txt 
% généré après chaque ajout d'un sommet ou d'une arête
improvedPath(G)->
	 cm(G),
	 {ok, [Name,Vs|WEs]} = file:consult("graph_cm1.txt"),
	{ok, Res} = file:consult("improvedRes.txt"),
	 New=lists:keyreplace(1,1,Res,{1,0,[stop,0]}),
	 io:format("\nIn:  ~p ~p ~p\nOut:~p", [Name, Vs, WEs, New]),
	usage(G).
	  
% Prépare le fichier graph_cm1.txt qui décrit le graphe courant et qui 
% sera utilisé par la fonction master/4 du module cm afin de calculer 
% le chemin le plus court
cm(G)->
	file:write_file("graph_cm1.txt",""),
	file:write_file("graph_cm1.txt",io_lib:fwrite("graph_cm.\n~p.\n",[digraph:vertices(G)]),[append]),	
	lists:foreach(fun(Q)->
		file:write_file("graph_cm1.txt",io_lib:fwrite("[~p",[Q]),[append]),
		lists:foreach(fun(N)->
			%io:format("vertex:~p nei: ~p",[Q,N])
			lists:foreach(fun(Z)->
				D=digraph:edge(G,Z),
				case element(2,D)==Q andalso element(3,D)==N of
					true-> file:write_file("graph_cm1.txt",io_lib:fwrite(",{~p,~p}",[N,element(4,D)]),[append]);
					false->io:format("")
				end,
				case element(3,D)==Q andalso element(2,D)==N of
				         true->file:write_file("graph_cm1.txt",io_lib:fwrite(",{~p,~p}",[N,element(4,D)]),[append]);
					 false->io:format("")
				end
				
			end,digraph:in_edges(G,N))
		end,digraph:out_neighbours(G,Q)),
		file:write_file("graph_cm1.txt",io_lib:fwrite("].\n",[]),[append])
	end,digraph:vertices(G)).

% Demande à l'utilisateur le sommet "source" du graphe courant 
% et calcule le chemin le plus court à l'aide de l'algorithme de Chandy-Misra 
shPath(G)->
	{ok, [X]} = io:fread("\nSource:","~d"),
	file:write_file("graph_cm1.txt",""),
	file:write_file("res.txt",""),
	cm(G),
	cm:master("graph_cm1.txt","res.txt",X,1),
	{ok,Data}=file:read_file("res.txt"),
	io:format("~s", [Data]),
	usage(G).
		
% {'Graphics',[{'Text',"",{0,0}},{'AbsoluteThickness',3} doit être écrit dans le fichier mygraph.graphic
% Affiche la représentation graphique du graphe courant
visualize(G)->
	file:write_file("mygraph.graphic", ""),
	file:write_file("mygraph.graphic", "{'Graphics',[{'Text',\"\",{0,0}},{'AbsoluteThickness',3}"),
	makeList(G),
	lists:foreach(fun(E)->
		Xcor=element(2,lists:last(ets:lookup(vert,E))),
		Ycor=element(3,lists:last(ets:lookup(vert,E))),	
		file:write_file("mygraph.graphic",io_lib:fwrite(",{'Text',\"O\",{~p,~p}},{'AbsoluteThickness',3}",[Xcor,Ycor]),[append]),	
			lists:foreach(fun(W) -> X2cor=element(2,lists:last(ets:lookup(vert,W))),
						Y2cor=element(3,lists:last(ets:lookup(vert,W))),				
						if
						       X2cor>Xcor ->
							    SXcor=600+Xcor+(X2cor-Xcor)/2;
							true -> 
							    SXcor=600+X2cor+(Xcor-X2cor)/2
						end,
						if
						       Y2cor>Ycor ->
							    SYcor=Ycor+(Y2cor-Ycor)/2;
							true -> 
							    SYcor=Y2cor+(Ycor-Y2cor)/2
						end,
						file:write_file("mygraph.graphic",io_lib:fwrite(" ,{'Line',[{~p,~p},{~p,~p}]}",[Xcor,Ycor	,X2cor,Y2cor]),[append]),
						lists:foreach(fun(Z)->
							D=digraph:edge(G,Z),
							case element(2,D)==E andalso element(3,D)==W of
								true-> file:write_file("mygraph.graphic",io_lib:fwrite(",[{'Text',\"~p\",{~p,~p}},{'AbsoluteThickness',3}]",[element(4,D),SXcor,SYcor]),[append]);
								false->io:format("")	
							end,
							case element(3,D)==E andalso element(2,D)==W of
							         true->file:write_file("mygraph.graphic",io_lib:fwrite(",[{'Text',\"~p\",{~p,~p}},{'AbsoluteThickness',3}]",[element(4,D),SXcor,SYcor]),[append]);
								 false->io:format("")
							end
						end,digraph:edges(G))
			end,digraph:out_neighbours(G,E))
	end, digraph:vertices(G)),
        file:write_file("mygraph.graphic",io_lib:fwrite("], [{mapping,{{-3800,16},{0.02,28}}}]}.",[]),[append]),
	mdrawer:start(),
	usage(G).

% Efface les données des fichiers .txt et permet à l'utilisateur de mettre fin au programme interactif
quit()->
	file:write_file("graph_cm1.txt",""),
	file:write_file("res.txt",""),
	file:write_file("improvedRes.txt",""),
	file:write_file("mygraph.graphic", "{'Graphics',[{'Text',\"\",{0,0}},{'AbsoluteThickness',3}"),
	exit.
		
