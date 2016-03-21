%%======================================================================
%%
%% Leo Memory Cache Library for Erlang(leo_mcerl)
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
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
%%======================================================================
%% records.
-record(cache_stats, {
    gets = 0 :: non_neg_integer(),
    puts = 0 :: non_neg_integer(),
    dels = 0 :: non_neg_integer(),
    hits = 0 :: non_neg_integer(),
    records = 0 :: non_neg_integer(),
    cached_size = 0 :: non_neg_integer()
}).

