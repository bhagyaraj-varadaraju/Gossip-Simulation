%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Oct 2022 5:05 PM
%%%-------------------------------------------------------------------
-module(utils).
-author("bhagyaraj").
-define(MAX_ROUNDS_WITH_SAME_STATE_TO_CONVERGE, 3).

%% API
-export([round_up_to_perfect_square/1, get_next_actor/3, check_if_pushsum_converged/1, update_sum_estimate_list/2]).

check_if_pushsum_converged(SumEstimateList) ->
  % True if converged else false
  ListLength = lists:flatlength(SumEstimateList),
  if ListLength == ?MAX_ROUNDS_WITH_SAME_STATE_TO_CONVERGE ->
      SE_1 = lists:nth(1, SumEstimateList),
      lists:all(
        fun(SE_i) ->
          erlang:abs(SE_1 - SE_i) < 0.0000000001
        end,
        SumEstimateList);

    true -> false
  end.

update_sum_estimate_list(SumEstimateList, SE) ->
  SumEstimateListLength = lists:flatlength(SumEstimateList),
  case SumEstimateListLength of
    3 -> [_H | T] = SumEstimateList,
      UpdatedSumEstimateList = lists:append(T, [SE]);
    _ -> UpdatedSumEstimateList = lists:append(SumEstimateList, [SE])
  end,
  UpdatedSumEstimateList.


round_up_to_perfect_square(ActorCount) ->
  % Round up the ActorCount to a perfect square
  Sqrt = math:sqrt(ActorCount),
  RoundedSqrt = round(Sqrt),
  RoundedActorCount = RoundedSqrt*RoundedSqrt,
  % Given Actor Count is already a perfect square, don't print anything
  case ActorCount == RoundedActorCount of
       true -> ActorCount;

    % Square root of actor count has been rounded down, so add 1 and calculate adjusted actor count again
       false ->
         case ActorCount > RoundedActorCount of
           true -> RoundedUpSqrt = round(math:sqrt(ActorCount)) + 1,
                   RoundedUpActorCount = RoundedUpSqrt*RoundedUpSqrt,
                   io:format("Given number of participants ~p is not a square - so adjusted to ~p~n", [ActorCount, RoundedUpActorCount]),
                   RoundedUpActorCount;

           false  ->
                   io:format("Given number of participants ~p is not a square - so adjusted to ~p~n", [ActorCount, RoundedActorCount]),
                   RoundedActorCount
          end
  end.


choose_random_neighbour(NeighbourList)->
  NeighbourNumber = lists:flatlength(NeighbourList),
  RandomNeighbour = rand:uniform(NeighbourNumber),
  lists:nth(RandomNeighbour,NeighbourList).


get_neighbour_oneD(CurrentActorIndex, MaxActorIndex) ->
  if CurrentActorIndex == 1 -> [2];
     CurrentActorIndex == MaxActorIndex -> [MaxActorIndex-1];
     true -> [CurrentActorIndex-1,CurrentActorIndex+1]
  end.


get_neighbours_twoD(Row,Column,MaxRows,MaxColumns)->
  %checks for boundares like corner elements and edge elements and sends neighbours accordingly
  case Row of
    1 ->
      case Column of
        1 ->
          [{1,2},{2,1},{2,2}];
        MaxColumns ->
          [{1,MaxColumns-1},{2,MaxColumns},{2,MaxColumns-1}];
        _ ->
          [{2,Column-1},{2,Column},{2,Column+1},{1,Column-1},{1,Column+1}]
      end;

    MaxRows ->
      case Column of
        1 ->
          [{MaxRows,2},{MaxRows-1,1},{MaxRows-1,2}];
        MaxColumns ->
          [{MaxRows,MaxColumns-1},{MaxRows-1,MaxColumns-1},{MaxRows-1,MaxColumns}];
        _ ->
          [{MaxRows,Column-1},{MaxRows,Column+1},{MaxRows-1,Column-1},{MaxRows-1,Column},{MaxRows-1,Column+1}]
      end;

    _ ->
      case Column of
        1 ->
          [{Row+1,1},{Row-1,1},{Row,2},{Row-1,2},{Row+1,2}];
        MaxColumns ->
          [{Row-1,MaxColumns},{Row+1,MaxColumns},{Row-1,MaxColumns-1},{Row,MaxColumns-1},{Row+1,MaxColumns-1}];
        _ ->
          [{Row, Column-1}, {Row,Column+1}, {Row-1,Column-1}, {Row-1,Column}, {Row-1,Column+1}, {Row+1,Column-1}, {Row+1,Column}, {Row+1,Column+1}]
      end
  end.


get_next_actor(CurrentActorIndex, MaxActorIndex, Topology) ->
  %returns the next actor by receiving the neighbours list and sending the random neighbour of it
  case Topology of
    full ->
      NextActorIndex = rand:uniform(MaxActorIndex),
      if
        CurrentActorIndex == NextActorIndex -> get_next_actor(CurrentActorIndex, MaxActorIndex, Topology);
        true -> NextActorIndex
      end;

    line ->
      NeighboursList = get_neighbour_oneD(CurrentActorIndex,MaxActorIndex),
      NextActorIndex = choose_random_neighbour(NeighboursList),
      NextActorIndex;

    twoD ->
      {CurrentRow, CurrentColumn} = CurrentActorIndex,
      {MaxRows,MaxColumns} = MaxActorIndex,
      NeighboursList = get_neighbours_twoD(CurrentRow,CurrentColumn,MaxRows,MaxColumns),
      NextActorIndex = choose_random_neighbour(NeighboursList),
      {NextActorRow, NextActorColumn} = NextActorIndex,
      {NextActorRow, NextActorColumn};

    imp2D ->
      {CurrentRow, CurrentColumn} = CurrentActorIndex,
      {MaxRows, MaxColumns} = MaxActorIndex,
      NeighboursList = get_neighbours_twoD(CurrentRow,CurrentColumn,MaxRows,MaxColumns),
      RandomActorIndex = {choose_random_neighbour(lists:seq(1,MaxRows)),choose_random_neighbour(lists:seq(1,MaxColumns))},
      NextImperfectActor = lists:append(NeighboursList,[RandomActorIndex]),
      {NextRow, NextColumn} = choose_random_neighbour(NextImperfectActor),
      {NextRow,NextColumn}
  end.
