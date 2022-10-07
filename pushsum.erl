%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2022 5:03 PM
%%%-------------------------------------------------------------------
-module(pushsum).
-author("bhagyaraj").

%% API
-export([pushsum_worker/4]).

pushsum_worker(Actors, Topology, Nodes, Algorithm) ->
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
