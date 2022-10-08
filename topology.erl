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
        pushsum -> CurrentSpawnPID = done% spawn_link(node(), pushsum, pushsum_worker, [CurrentSpawnIndex, ParticipantCount, Topology])
      end,
      % Insert into the ETS table in the format {ActorIndex, ActorPID}
      ets:insert(pidTable, {CurrentSpawnIndex, CurrentSpawnPID}),
      spawn_oneD(CurrentSpawnIndex + 1, ParticipantCount, Topology, Algorithm);

    % Return the pidTable after all the spawns are done
    true ->
      io:format("All ~p ~p workers have been spawned~n", [ParticipantCount, Algorithm])
  end.


spawn_twoD({CurrentRow, CurrentColumn}, {MaxRows,MaxColumns}, Topology, Algorithm) ->
%%  if
%%    (CurrentRow > MaxRows )->
%%      io:format("Succesffully spawned a 2d grid of ~w ~w ~n",[MaxRows,MaxColumns]),
%%      ets:match_object(pidTable,{'$0','$1'});
%%    (CurrentColumn == MaxColumns) ->
%%      case Algorithm of
%%        gossip -> CurrentSpawnPID = spawn_link(node(),gossip,gossip_worker,[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
%%        pushsum ->  CurrentSpawnPID = done %spawn_link(node(),pushsum,pushsum_worker(),[{CurrentRow+1,CurrentColumn},{MaxRows,MaxColumns},Topology])
%%      end,
%%      ets:insert(pidTable,{{CurrentRow,CurrentColumn},CurrentSpawnPID}),
%%      spawn_twoD({CurrentRow + 1, CurrentColumn }, {MaxRows, MaxColumns}, Topology, Algorithm);
%%    true ->
%%      io:format("~w ~w ~n",[CurrentRow,CurrentColumn]),
%%      case Algorithm of
%%        gossip -> CurrentSpawnPID = spawn_link(node(),gossip,gossip_worker,[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
%%        pushsum -> CurrentSpawnPID = done % spawn_link(node(),pushsum,pushsum_worker(),[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
%%      end,
%%      ets:insert(pidTable,{{CurrentRow,CurrentColumn},CurrentSpawnPID}),
%%      spawn_twoD({CurrentRow,CurrentColumn+1},{MaxRows,MaxColumns},Topology,Algorithm)
%%  end.


case (CurrentRow > MaxRows ) of
  true -> io:format("Succesffully spawned a 2d grid of ~w ~w ~n",[MaxRows,MaxColumns]);
  false -> case  (CurrentColumn == MaxColumns) of
             true ->
               case Algorithm of
                        gossip -> CurrentSpawnPID = spawn_link(node(),gossip,gossip_worker,[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
                        pushsum ->  CurrentSpawnPID = done %spawn_link(node(),pushsum,pushsum_worker(),[{CurrentRow+1,CurrentColumn},{MaxRows,MaxColumns},Topology])
                     end,
                     ets:insert(pidTable,{{CurrentRow,CurrentColumn},CurrentSpawnPID}),
                     spawn_twoD({CurrentRow + 1, 1 }, {MaxRows, MaxColumns}, Topology, Algorithm);
             false ->
                  case Algorithm of
                    gossip -> CurrentSpawnPID = spawn_link(node(),gossip,gossip_worker,[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
                    pushsum -> CurrentSpawnPID = done % spawn_link(node(),pushsum,pushsum_worker(),[{CurrentRow,CurrentColumn},{MaxRows,MaxColumns},Topology]);
                  end,
                  ets:insert(pidTable,{{CurrentRow,CurrentColumn},CurrentSpawnPID}),
                  spawn_twoD({CurrentRow,CurrentColumn+1},{MaxRows,MaxColumns},Topology,Algorithm)
           end
end.