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
        pushsum -> CurrentSpawnPID = spawn_link(node(), pushsum, pushsum_worker, [CurrentSpawnIndex, ParticipantCount, {CurrentSpawnIndex, 1}, Topology])
      end,
      % Insert into the ETS table in the format {ActorIndex, ActorPID}
      ets:insert(pidTable, {CurrentSpawnIndex, CurrentSpawnPID}),
      spawn_oneD(CurrentSpawnIndex + 1, ParticipantCount, Topology, Algorithm);

    % Return the pidTable after all the spawns are done
    true ->
      io:format("All ~p ~p workers have been spawned~n", [ParticipantCount, Algorithm])
  end.


spawn_twoD({CurrentRow, CurrentColumn}, {MaxRows, MaxColumns}, Topology, Algorithm) ->
  %Checks if all actors are spawned if true ends or if in each row if Max Column is reached increases the row and resets the column else increases the column in the same row
  case (CurrentRow > MaxRows ) of
    true ->
      io:format("Succesffully spawned a 2d grid of ~w ~w ~n", [MaxRows, MaxColumns]);

    false ->
      case (CurrentColumn == MaxColumns) of
        true ->
          case Algorithm of
            gossip -> CurrentSpawnPID = spawn_link(node(), gossip, gossip_worker, [{CurrentRow, CurrentColumn}, {MaxRows, MaxColumns}, Topology]);
            pushsum ->  CurrentSpawnPID = spawn_link(node(), pushsum, pushsum_worker(), [{CurrentRow, CurrentColumn}, {MaxRows, MaxColumns}, {((CurrentRow - 1) * MaxColumns) + CurrentColumn, 1}, Topology])
          end,
          ets:insert(pidTable, {{CurrentRow, CurrentColumn}, CurrentSpawnPID}),
          spawn_twoD({CurrentRow + 1, 1}, {MaxRows, MaxColumns}, Topology, Algorithm);

        false ->
          case Algorithm of
            gossip -> CurrentSpawnPID = spawn_link(node(), gossip, gossip_worker, [{CurrentRow, CurrentColumn}, {MaxRows, MaxColumns}, Topology]);
            pushsum -> CurrentSpawnPID = spawn_link(node(), pushsum, pushsum_worker(), [{CurrentRow, CurrentColumn}, {MaxRows, MaxColumns}, {((CurrentRow - 1) * MaxColumns) + CurrentColumn, 1}, Topology])
          end,
          ets:insert(pidTable, {{CurrentRow, CurrentColumn}, CurrentSpawnPID}),
          spawn_twoD({CurrentRow, CurrentColumn + 1}, {MaxRows, MaxColumns}, Topology, Algorithm)
      end
  end.
