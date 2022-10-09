%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2022 5:27 PM
%%%-------------------------------------------------------------------
-module(supervising_boss).
-author("bhagyaraj").
-define(MAX_WAIT_TIME_FOR_ALL_WORKERS_CONVERGENCE, 3000).

%% API
-export([main/3]).


wait_for_convergence(NumOfMsgsRcvd, MaxActors, Algorithm) ->
  if
    NumOfMsgsRcvd == MaxActors ->
      io:format("All ~p workers finished gossiping and converged. Convergence efficiency = ~p~n", [MaxActors, (NumOfMsgsRcvd / MaxActors) * 100]);
    true ->
      % Keep listening to the success messages from the workers
      receive
        {success, SentActorIndex, ReceivedMsg} ->
          case Algorithm of
            gossip -> io:format("Received success message from Actor [~p]~n", [SentActorIndex]);
            pushsum -> io:format("Received success message from Actor [~p], Sum estimate was ~p ~n", [SentActorIndex, ReceivedMsg])
          end,
          wait_for_convergence(NumOfMsgsRcvd + 1, MaxActors, Algorithm)

      % If there is no message for 3 seconds, stop waiting for convergence
      after ?MAX_WAIT_TIME_FOR_ALL_WORKERS_CONVERGENCE ->
        if
          NumOfMsgsRcvd == 0 ->
            %  Still no worker has converged, so keep waiting
            wait_for_convergence(NumOfMsgsRcvd, MaxActors, Algorithm);

          true ->
            %  Few workers have converged. So, it's unlikely that other workers will converge after a wait time of 3 sec.
            io:format("No messages received from workers for ~p milliseconds. Totally ~p workers converged out of ~p workers. Convergence efficiency = ~p~n",
              [?MAX_WAIT_TIME_FOR_ALL_WORKERS_CONVERGENCE, NumOfMsgsRcvd, MaxActors,  (NumOfMsgsRcvd / MaxActors) * 100])
        end
      end
  end.


start_transmission(SelectedActorPID, Algorithm) ->
  case Algorithm of
    gossip ->
      % Send the rumor to the selected process for gossiping
      io:format("Using gossip algorithm~n"),
      SelectedActorPID ! {gossip_from_boss, bhagyaraj};
    pushsum ->
      % Send the {sum, weight} to the selected node for calculating the sum of all nodes
      io:format("Using pushsum algorithm~n"),
      SelectedActorPID ! {pushsum_from_boss, {1, 1}}
  end.


main_2D_topology(ActorCount, Topology, Algorithm) ->
  % Round up actor count to a perfect square
  AdjustedActorCount = utils:round_up_to_perfect_square(ActorCount),

  % Get rows and columns to create the topology
  {Rows, Cols} = {trunc(math:sqrt(AdjustedActorCount)), trunc(math:sqrt(AdjustedActorCount))},

  % Create an ETS table 'pidTable' for storing {Index, PID} for each actor
  ets:new(pidTable, [set, named_table, protected]),

  % Spawn the given number of actors in 2D topology
  topology:spawn_twoD({1, 1}, {Rows, Cols}, Topology, Algorithm),

  % Select the first participant for starting the gossip
  SelectedActorPID = ets:lookup_element(pidTable, {1, 1}, 2),

  % Let the supervisor handover the message to the TransmissionPID process
  start_transmission(SelectedActorPID, Algorithm),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  wait_for_convergence(0, AdjustedActorCount, Algorithm),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, TimeElapsed} = statistics(wall_clock),

  %% Output the time taken to converge
  io:format("Actual clock time taken to converge is ~p milliseconds~n", [TimeElapsed]).


main_1D_topology(ActorCount, Topology, Algorithm) ->
  % Create an ETS table 'pidTable' for storing {Index, PID} for each actor
  ets:new(pidTable, [set, named_table, protected]),

  % Spawn the given number of actors in 1D topology
  topology:spawn_oneD(1, ActorCount, Topology, Algorithm),

  % Select the first participant and extract PID of it, for starting the gossip
  SelectedActorPID = ets:lookup_element(pidTable, 1, 2),

  % Let the supervisor handover the message to the TransmissionPID process
  start_transmission(SelectedActorPID, Algorithm),

  % Start the clock for finding wall clock time for convergence
  statistics(wall_clock),

  % Wait for all the workers to return success.
  wait_for_convergence(0, ActorCount, Algorithm),

  % Stop the wall clock for calculating the time elapsed for convergence.
  {_, TimeElapsed} = statistics(wall_clock),

  %% Output the time taken to converge
  io:format("Actual clock time taken to converge is ~p milliseconds~n", [TimeElapsed]).


main(ParticipantCount, Topology, Algorithm) ->
  %% Register the current process ID as module name - supervisor
  erlang:register(?MODULE, self()),

  %% Call the appropriate main function for each topology
  case Topology of
    full ->
      io:format("Using full network topology~n"),
      main_1D_topology(ParticipantCount, Topology, Algorithm);
    line ->
      io:format("Using line network topology~n"),
      main_1D_topology(ParticipantCount, Topology, Algorithm);
    twoD ->
      io:format("Using twoD network topology~n"),
      main_2D_topology(ParticipantCount, Topology, Algorithm);
    imp2D ->
      io:format("Using imp2D network topology~n"),
      main_2D_topology(ParticipantCount, Topology, Algorithm)
  end,

  %% Delete the ETS table from the storage
  ets:delete(pidTable),

  %% Unregister the current process
  erlang:unregister(?MODULE).
