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


send_pushsum_to_neighbor(MYWorkerIndex, MaxWorkerIndex, {Sum, Weight}, Topology, SumEstimateList, WasMessageReceived) ->
  % Select a random actor from the neighbors
  RandNeighborIndex = utils:get_next_actor(MYWorkerIndex, MaxWorkerIndex, Topology),

  % Lookup the PID of the selected neighbor
  RandNeighborPID = ets:lookup_element(pidTable, RandNeighborIndex, 2),

  % Send half the weights to the selected neighbor
  {NewSum, NewWeight} = {Sum / 2, Weight / 2},

  % Send the half of {Sum, Weight} to the selected worker
  RandNeighborPID ! {pushsum_from_worker, {NewSum, NewWeight}},

  % Continue listening to messages with the remaining half of {Sum, Weight}
  if
    WasMessageReceived ->
      listen_to_pushsum(MYWorkerIndex, MaxWorkerIndex, {NewSum, NewWeight}, Topology, utils:update_sum_estimate_list(SumEstimateList, NewSum / NewWeight));
    true ->
      listen_to_pushsum(MYWorkerIndex, MaxWorkerIndex, {NewSum, NewWeight}, Topology, SumEstimateList)
  end.


listen_to_pushsum(MYWorkerIndex, MaxWorkerIndex, {MYSum, MYWeight}, Topology, SumEstimateList) ->
  % Check if we received even one message using SumEstimateList
  SumEstimateListLength = lists:flatlength(SumEstimateList),

  % Check if the pushsum converged, based on the sum estimate in the last 3 rounds
  Is_pushsum_done = utils:check_if_pushsum_converged(SumEstimateList),

  if
    % Send 'success' message to the supervisor if the sum estimate has not changed in last 3 consecutive rounds
    Is_pushsum_done ->
      supervising_boss ! {success, MYWorkerIndex, lists:last(SumEstimateList)};

    % Listen to the messages if not yet converged
    true ->
      if
      % Wait indefinitely until we receive at least one message
        SumEstimateListLength == 0 ->
          receive
          % {Sum, weight} sent by the supervisor, only one selected process will receive 'pushsum_from_boss' message
          % DO NOT update {sum, weight} values as this is just initiation from boss. But update that message has been received by appending to SumEstimateList
            {pushsum_from_boss, {SumRcvd, WeightRcvd}} ->
              io:format("Message received from the boss to initiate PushSum, My Index - [~p]~n", [MYWorkerIndex]),
              {UpdatedSum, UpdatedWeight} = {MYSum + SumRcvd, MYWeight + WeightRcvd},
              send_pushsum_to_neighbor(MYWorkerIndex, MaxWorkerIndex, {UpdatedSum, UpdatedWeight}, Topology, SumEstimateList, true);

          % Receive the message sent from other workers and update the {sum, weight} accordingly
            {pushsum_from_worker, {SumRcvd, WeightRcvd}} ->
              {UpdatedSum, UpdatedWeight} = {MYSum + SumRcvd, MYWeight + WeightRcvd},
              send_pushsum_to_neighbor(MYWorkerIndex, MaxWorkerIndex, {UpdatedSum, UpdatedWeight}, Topology, SumEstimateList, true)
          end;

      % If we have already received a message previously, continue sending {sum, weight} after processing message queue
        SumEstimateListLength > 0 ->
          % Process any received messages from other workers
          receive
            {pushsum_from_worker, {SumRcvd, WeightRcvd}} ->
              {UpdatedSum, UpdatedWeight} = {MYSum + SumRcvd, MYWeight + WeightRcvd},
              send_pushsum_to_neighbor(MYWorkerIndex, MaxWorkerIndex, {UpdatedSum, UpdatedWeight}, Topology, SumEstimateList, true)

          % keep spreading the {sum, weight} if there are no messages in the mailbox
          after 100 ->
            send_pushsum_to_neighbor(MYWorkerIndex, MaxWorkerIndex, {MYSum, MYWeight}, Topology, SumEstimateList, false)
          end
      end
  end.


pushsum_worker(MYWorkerIndex, MaxWorkerIndex, {MYSum, MYWeight}, Topology) ->
  listen_to_pushsum(MYWorkerIndex, MaxWorkerIndex, {MYSum, MYWeight}, Topology, []).
