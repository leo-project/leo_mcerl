%%======================================================================
%%
%% Leo Memory Cache Library for Erlang(leo_mcerl)
%%
%% Copyright (c) 2012-2013 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------
%% Leo Memory Cache
%% @doc
%% @end
%%======================================================================
-module(basho_bench_driver_leo_mcerl).
-author("Yosuke Hara").

-export([new/1,
         run/4]).

-record(state, {handler :: binary(), check_integrity :: boolean()}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    case code:which(leo_mcerl) of
        non_existing ->
            io:format("Cherly-benchmark requires leo_mcerl to be available on code path.\n");
        _ ->
            void
    end,

    CacheCapacity = basho_bench_config:get(
                      cache_capacity, 1073741824), %% default:1GB
    io:format("Cache capacity: ~w\n", [CacheCapacity]),
    CI = basho_bench_config:get(
                      check_integrity, false), %% should be false when doing benchmark
    io:format("Check Integrity: ~p\n", [CI]),

    {ok, C} = leo_mcerl:start(CacheCapacity),
    {ok, #state{handler = C, check_integrity = CI}}.


run(get, KeyGen, _ValueGen, #state{handler = C, check_integrity = CI} = State) ->
    Key = KeyGen(),
    case leo_mcerl:get(C, Key) of
        {ok, Value} ->
            case CI of
                true ->
                    LocalMD5 = erlang:get(Key),
                    RemoteMD5 = erlang:md5(Value),
                    case RemoteMD5 =:= LocalMD5 of
                        true -> {ok, State};
                        false -> {error, checksum_error}
                    end;
                false -> {ok, State}
            end;
        not_found ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(put, KeyGen, ValueGen, #state{handler = C, check_integrity = CI} = State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    case leo_mcerl:put(C, Key, Val) of
        ok ->
            case CI of
                true ->
                    LocalMD5 = erlang:md5(Val),
                    erlang:put(Key, LocalMD5);
                false -> void
            end,
            {ok, State};
        {error, Reason} ->
            {error, Reason, State};
        Other ->
            io:format("put unexpected result:~p \n", [Other]),
            {error, Other, State}
    end.

