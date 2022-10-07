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

%% API
-export([round_up_to_perfect_square/1, get_next_actor/3]).

round_up_to_perfect_square(ActorCount) ->
  % Round up the ActorCount to a perfect square
  Sqrt = math:sqrt(ActorCount),
  RoundedSqrt = round(Sqrt),
  RoundedActorCount = RoundedSqrt*RoundedSqrt,

  if
    % Given Actor Count is already a perfect square, don't print anything
    ActorCount == RoundedActorCount -> io:format("");

    % Square root of actor count has been rounded down, so add 1 and calculate adjusted actor count again
    ActorCount > RoundedActorCount ->
      RoundedUpSqrt = round(math:sqrt(ActorCount)) + 1,
      RoundedUpActorCount = RoundedUpSqrt*RoundedUpSqrt,
      io:format("Given number of participants ~p is not a square - so adjusted to ~p~n", [ActorCount, RoundedUpActorCount]);

    true ->
      io:format("Given number of participants ~p is not a square - so adjusted to ~p~n", [ActorCount, RoundedActorCount])
  end.


get_next_actor(CurrentActorIndex, MaxActorIndex, Topology) ->
  case Topology of
    full ->
      NextActorIndex = rand:uniform(MaxActorIndex),
      if
        CurrentActorIndex == NextActorIndex -> get_next_actor(CurrentActorIndex, MaxActorIndex, Topology);
        true -> NextActorIndex
      end;
    line ->
      done;
    twoD ->
      done;
    imp2D ->
      done

  end.

