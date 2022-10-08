%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2022 5:29 PM
%%%-------------------------------------------------------------------
-module(gossip).
-author("bhagyaraj").
-define(MAX_RUMOR_COUNT_TO_CONVERGE, 10).

%% API
-export([gossip_worker/3]).


spread_gossip_to_neighbor(MYWorkerIndex, MaxWorkerIndex, Topology, RumorList) ->
  % Select a random actor from the neighbors
  RandNeighborIndex = utils:get_next_actor(MYWorkerIndex, MaxWorkerIndex, Topology),

  % Lookup the PID of the selected neighbor
  RandNeighborPID = ets:lookup_element(pidTable, RandNeighborIndex, 2),

  % Send the most recent gossip message from the RumorList to the selected worker
  RandNeighborPID ! {gossip_from_worker, lists:last(RumorList)},

  % Continue listening to gossips after spreading the gossip to a neighbor
  listen_to_gossip(MYWorkerIndex, MaxWorkerIndex, Topology, RumorList).


listen_to_gossip(MYWorkerIndex, MaxWorkerIndex, Topology, RumorList) ->
  % Get the number of rumors seen by this worker.
  RumorListLength = lists:flatlength(RumorList),

  % Check if we received the maximum amount of rumors for convergence,
  Convergence_in_progress = RumorListLength < ?MAX_RUMOR_COUNT_TO_CONVERGE,

  if
    Convergence_in_progress ->
      if
        % Wait indefinitely until we receive at least one rumor.
        RumorListLength == 0 ->
          receive
          % Gossip sent by the supervisor, only one selected process will receive 'gossip_from_supervisor' message.
            {gossip_from_boss, Rumor} ->
              io:format("Rumor '~p' received from the supervisor~n", [Rumor]),
              spread_gossip_to_neighbor(MYWorkerIndex, MaxWorkerIndex, Topology, lists:append(RumorList, [Rumor]));

          % Receive the gossip sent from other workers
            {gossip_from_worker, Rumor} ->
              spread_gossip_to_neighbor(MYWorkerIndex, MaxWorkerIndex, Topology, lists:append(RumorList, [Rumor]))
          end;

        % If we have already received a gossip previously, continue spreading rumors after processing message queue
        RumorListLength > 0 ->
          % Process any received gossips from other workers
          receive
            {gossip_from_worker, Rumor} ->
              spread_gossip_to_neighbor(MYWorkerIndex, MaxWorkerIndex, Topology, lists:append(RumorList, [Rumor]))

          % keep spreading the rumor if there are no messages in the mailbox
          after 0 ->
            spread_gossip_to_neighbor(MYWorkerIndex, MaxWorkerIndex, Topology, RumorList)
          end
      end;

    % Send 'success' message to the supervisor after convergence is done
    true ->
      supervising_boss ! {success, MYWorkerIndex}
  end.


gossip_worker(MYWorkerIndex, MaxWorkerIndex, Topology) ->
  listen_to_gossip(MYWorkerIndex, MaxWorkerIndex, Topology, []).
