-module(gossip).
-export([start/3]).

init_actors(Nodes) ->
  Middle_Node = trunc(Nodes/2),
  io:format("Printing middle node ~w~n",[Middle_Node]).


init_algorithm_gossip(Actors,Topology,Nodes,Algorithm) ->
%%  case Topology of
%%    full ->
%%      io:format("Using full topology"),
%%      Neighbours = get_full_neighbours(Actors),
%%      if Algorithm == "gossip" -> gossip(Actors,Neighbours,Nodes);
%%         Algorithm == "pushsum" -> pushsum(Actors,Neighbours,Nodes)
%%      end;
%%    twod ->
%%      io:format("Using 2D"),
%%      Neighbours = get_2d_neighbours(Actors);
%%    line ->
%%      io:format("Using line"),
%%    Neighbours = get_line_neighbours(Actors);
%%    imp3d ->
%%      io:format("Using imperfect 3d"),
%%    Neighbours = get_imp3d_neighbours(Actors);
%%    _ ->
%%      io:format("Invalid topology specified")
%%  end.
  hello.

%%init_algorithm_pushsum(Actors,Topology,Nodes,Algorithm) ->
%%  case Topology of
%%    full ->
%%      io:format("Using full topology"),
%%      Neighbours = get_full_neighbours(Actors);
%%    twod ->
%%      io:format("Using 2D"),
%%      Neighbours = get_2d_neighbours(Actors);
%%    line ->
%%      io:format("Using line"),
%%      Neighbours = get_line_neighbours(Actors);
%%    imp3d ->
%%      io:format("Using imperfect 3d"),
%%      Neighbours = get_imp3d_neighbours(Actors);
%%    _ ->
%%      io:format("Invalid topology specified")
%%  end.

get_full_neighbours(Actors) ->
  Pairs = lists:filter(fun([A,B]) -> A =/= B end, [[X, Y] || X <- Actors, Y <- Actors]),
  Pairs.


start(Topology, Nodes, Algorithm) ->
  case Algorithm of
    "gossip" ->
      io:format("Using gossip algorithm"),
      Actors = init_actors(Nodes),
      init_algorithm_gossip(Actors, Topology, Nodes, Algorithm),
      lists:filter(fun(X) -> X rem 3 /= 0 end, lists:seq(0,30));
    "pushsum" ->
      io:format("Using pushsum algorithm"),
      Actors=init_actors(Nodes);
      %init_algorithm_pushsum(Actors,Topology, Nodes, Algorithm);
    _ ->
      io:format("Not implemented algorithm")
  end.