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
-export([gossip_worker/4]).

spread_gossip_to_neighbor(MYWorkerIndex, TotalWorkerCount, Topology, RumorList) ->
  done.

listen_to_gossip(MYWorkerIndex, TotalWorkerCount, Topology, RumorList) ->
  % Get the number of rumors seen by this worker.
  RumorListLength = lists:flatlength(RumorList),

  Converge_in_progress = RumorListLength < ?MAX_RUMOR_COUNT_TO_CONVERGE,

  if
    RumorListLength == 0 ->
    receive ->
    % Gossip sent by the supervisor, only one selected process will receive initiate_gossip message.
    {initiate_gossip, Rumor} ->
    io:format("Rumor '[~p]' received from the supervisor~n", [Rumor]),
    % Receive the gossip sent from other workers
    {spread_gossip, Rumor} -> listen_to_gossips(MYWorkerIndex, TotalWorkerCount, Topology, RumorList)
    true ->
  listen_to_gossip(MYWorkerIndex, TotalWorkerCount, Topology, RumorList)
  end

  if
    Converge_in_progress ->
      spread_gossip_to_neighbor(MYWorkerIndex, TotalWorkerCount, Topology, RumorList);
      true ->
      % Check whether the max rumor count to converge has been reached
      RumorListLength < ?MAX_RUMOR_COUNT_TO_CONVERGE ->
        % Keep listening to the rumors from other workers


    % Send 'success' message to the supervisor after gossiping is done, so the supervisor can terminate this actor.
    true ->
      supervisor ! {success, MYWorkerIndex}
  end,
    % Listen to gossips and every 1 ms
    %if not spread the gossip to neighbors

  done.

gossip_worker(MYWorkerIndex, TotalWorkerCount, Topology, RumorList) ->
  listen_to_gossip(MYWorkerIndex, TotalWorkerCount, Topology, RumorList).
