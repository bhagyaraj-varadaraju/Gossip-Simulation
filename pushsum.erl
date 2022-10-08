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


send_pushsum_to_neighbor() ->
  done.

receive_pushsum(MyWorkerIndex, MaxWorkerIndex, CurrentState, Topology) ->
  % Get the sum and weights from the current state
  {CurrentSum, CurrentWeight} = CurrentState,

  case  of
      ->;
  end


pushsum_worker(MyWorkerIndex, MaxWorkerIndex, CurrentState, Topology) ->
  receive_pushsum(MyWorkerIndex, MaxWorkerIndex, CurrentState, Topology).
