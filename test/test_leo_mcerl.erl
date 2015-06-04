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
-module(test_leo_mcerl).
-author('Yoshiyuki Kanno').
-author('Yosuke Hara').

-include("leo_mcerl.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

simple_1_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V = <<"value">>,
    Len   = byte_size(K) + byte_size(V),
    leo_mcerl:put(C, K, V),

    ?assertEqual({ok, V},   leo_mcerl:get(C, <<"key">>)),
    ?assertEqual({ok, Len}, leo_mcerl:size(C)),
    leo_mcerl:stop(C).

simple_2_test() ->
    application:start(leo_mcerl),
    {ok,_Pid} = leo_mcerl_sup:start_child(worker_1, 1073741824),

    ok = leo_mcerl_server:put(worker_1, <<"KEY_1">>, <<"VALUE_1">>),
    {ok, <<"VALUE_1">>} = leo_mcerl_server:get(worker_1, <<"KEY_1">>),
    ok.


put_plural_objects_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    Keys = ["A","B","C","D","E","F",
            "G","H","I","J","K","L",
            "M","N","O","P","Q","R",
            "S","T","U","V","W","X",
            "Y","Z","1","2","3","4",
            "5","6","7","8","9","0"],
    lists:foreach(fun(K) ->
                          leo_mcerl:put(C, list_to_binary(K), <<"LEOFS">>)
                  end, Keys),
    lists:foreach(fun(K) ->
                          {ok, <<"LEOFS">>} = leo_mcerl:get(C, list_to_binary(K))
                  end, Keys),

    Items = length(Keys),
    Size  = Items + (Items * 5),

    ?assertEqual({ok, Items}, leo_mcerl:items(C)),
    ?assertEqual({ok, Size},  leo_mcerl:size(C)),
    leo_mcerl:stop(C).

put_term_key_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = term_to_binary({1234567890, "server/erlang"}),
    V = <<"LEOFS">>,
    Len = byte_size(K) + byte_size(V),

    ok = leo_mcerl:put(C, K, V),
    {ok, V} = leo_mcerl:get(C, K),

    ?assertEqual({ok, 1},   leo_mcerl:items(C)),
    ?assertEqual({ok, Len}, leo_mcerl:size(C)),
    leo_mcerl:stop(C).

put_including_null_key_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    H = <<"abcdefghijklmnopqrstuvwxyz">>,
    T = <<0:64>>,
    K = <<H/binary,T/binary>>,
    V = <<"LEOFS">>,
    Len = byte_size(K) + byte_size(V),

    ok = leo_mcerl:put(C, K, V),
    {ok, V} = leo_mcerl:get(C, K),

    ?assertEqual({ok, 1},   leo_mcerl:items(C)),
    ?assertEqual({ok, Len}, leo_mcerl:size(C)),
    leo_mcerl:stop(C).

put_get_and_remove_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V = <<"value">>,

    ?assertEqual(not_found, leo_mcerl:get(C, K)),
    leo_mcerl:put(C, K, V),
    ?assertEqual({ok, V}, leo_mcerl:get(C, K)),
    leo_mcerl:delete(C, K),
    ?assertEqual(not_found, leo_mcerl:get(C, K)),
    ?assertEqual({ok, 0}, leo_mcerl:size(C)),
    leo_mcerl:stop(C).

put_with_lru_eject_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    V = <<"value">>,
    lists:foldl(fun(_, Str) ->
                        Mod = list_to_binary(succ(Str)),
                        ?debugVal(Mod),
                        leo_mcerl:put(C, Mod, V),
                        binary_to_list(Mod)
                end, "abc", lists:seq(1, 10)),
    ?debugVal(leo_mcerl:size(C)),
    %% ?assertEqual({ok, 8}, leo_mcerl:items(C)),
    leo_mcerl:stop(C).

what_goes_in_must_come_out_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,

    leo_mcerl:put(C, K, list_to_binary([<<"val1">>, <<"val2">>])),
    ?assertEqual({ok, list_to_binary([<<"val1">>, <<"val2">>])}, leo_mcerl:get(C, K)),
    leo_mcerl:stop(C).

big_stuff_that_goes_in_must_come_out_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V1 = <<0:262144>>,
    V2 = <<1:262144>>,

    leo_mcerl:put(C, K, list_to_binary([V1, V2])),
    {ok, Ret} = leo_mcerl:get(C, K),
    ?assertEqual(list_to_binary([V1,V2]), Ret),
    leo_mcerl:stop(C).

put_one_thing_in_no_list_big_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V = <<0:524288>>,

    leo_mcerl:put(C, K, V),
    ?assertEqual({ok, V}, leo_mcerl:get(C, K)),
    leo_mcerl:stop(C).

put_one_thing_in_no_list_small_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V = <<1:8>>,
    leo_mcerl:put(C, K, V),
    ?assertEqual({ok, V}, leo_mcerl:get(C, K)),
    leo_mcerl:stop(C).

remove_nonexistant_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,

    leo_mcerl:delete(C, K),
    ?assertEqual(not_found, leo_mcerl:get(C, K)),
    leo_mcerl:stop(C).

put_bigger_thing_than_1MB_test() ->
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"key">>,
    V = crypto:rand_bytes(1024 * 1024 * 10),
    Ret = leo_mcerl:put(C, K, V),
    ?assertEqual({error, 'out_of_memory'}, Ret),
    ?assertEqual(not_found, leo_mcerl:get(C, K)),
    {ok, 0}  = leo_mcerl:items(C),
    {ok, 0} = leo_mcerl:size(C),
    leo_mcerl:stop(C).

double_get_test() ->
    %% outputv modifies the iovec with a skipsize.  That's fucking rad
    {ok, C} = leo_mcerl:start(16*1024*1024),
    K = <<"aczup">>,
    V = list_to_binary([<<131,108,0,0,0,1,104,2,107,0,9,60,48,46,52,55,50,46,48,
                          62,99,49,46,50,51,54,53,51,49,53,54,49,57,53,57,56,55,
                          50,57,54,49,48,52,101,43,48,57,0,0,0,0,0,106>>,
                        <<235,105,34,223,191,105,56,25,199,24,148,52,180,112,
                          198,246,56,150,15,175,56,34,38,120,99,41,59,53,204,
                          233,41,246,189,135,39,171,124,233,143,40,108,119,63,
                          130,237,8,121,35,97,121,172,20,149,241,129,191,2,211,
                          151,167,0,102,103,63,242,240,41,83,150,211,189,32,56,
                          65,217,241,234,237,58,216,34,245,253,153,140,190,186,
                          24,147,240,181,63,222,161,13,217,55,232,254,148>>]),
    leo_mcerl:put(C, K, V),
    ?assertEqual({ok, V}, leo_mcerl:get(C, K)),
    ?assertEqual({ok, V}, leo_mcerl:get(C, K)),
    leo_mcerl:stop(C).

server_test() ->
    K = <<"KEY-1">>,
    V = <<"VALUE-1">>,

    ProcId = 'test_leo_mcerl',
    leo_mcerl_server:start_link(ProcId, (16 * 1024 * 1024)),
    ok = leo_mcerl_server:put(ProcId, K, V),
    {ok, V}  = leo_mcerl_server:get(ProcId, K),
    {ok, 1}  = leo_mcerl_server:items(ProcId),
    {ok, 12} = leo_mcerl_server:size(ProcId),

    {ok, Stats1} = leo_mcerl_server:stats(ProcId),
    ?assertEqual(#cache_stats{gets = 1,
                              puts = 1,
                              dels = 0,
                              hits = 1,
                              records = 1,
                              cached_size = 12}, Stats1),

    ok = leo_mcerl_server:delete(ProcId, K),
    {ok, Stats2} = leo_mcerl_server:stats(ProcId),
    ?assertEqual(#cache_stats{gets = 1,
                              puts = 1,
                              dels = 1,
                              hits = 1,
                              records = 0,
                              cached_size = 0}, Stats2),

    {ok, 0} = leo_mcerl_server:items(ProcId),
    {ok, 0} = leo_mcerl_server:size(ProcId),

    leo_mcerl_server:stop(ProcId),
    ok.


%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
succ([]) ->
    [];
succ(Str) ->
    succ_int(lists:reverse(Str), []).


succ_int([Char|Str], Acc) ->
    if
        Char >= $z -> succ_int(Str, [$a|Acc]);
        true -> lists:reverse(lists:reverse([Char+1|Acc]) ++ Str)
    end.

-endif.

