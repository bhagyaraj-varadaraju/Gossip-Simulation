%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2022 5:05 PM
%%%-------------------------------------------------------------------
-module(topology).
-author("bhagyaraj").

%% API
-export([spawn_oneD/4, spawn_twoD/4]).

spawn_oneD(CurrentSpawnIndex, ParticipantCount, Topology, Algorithm) ->
  if
    % Spawn the actor based on the algorithm and update its PID in the map
    CurrentSpawnIndex =< ParticipantCount ->
      case Algorithm of
        gossip -> CurrentSpawnPID = spawn_link(node(), gossip, gossip_worker, [CurrentSpawnIndex, ParticipantCount, Topology]);
        pushsum -> CurrentSpawnPID = done% spawn_link(node(), pushsum, pushsum_worker, [CurrentSpawnIndex, ParticipantCount, Topology, {CurrentPosition, 1}])
      end,
      % Insert into the ETS table in the format {ActorIndex, ActorPID}
      ets:insert(pidTable, {CurrentSpawnIndex, CurrentSpawnPID}),
      spawn_oneD(CurrentSpawnIndex + 1, ParticipantCount, Topology, Algorithm);

    % Return the pidTable after all the spawns are done
    true ->
      io:format("All ~p ~p workers have been spawned~n", [ParticipantCount, Algorithm])
  end.


spawn_twoD(_CurrentSpawnIndex, _ParticipantCount, _Topology, _Algorithm) ->
  done.
