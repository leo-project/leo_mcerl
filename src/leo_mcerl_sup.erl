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
%% @doc Supervisor
%% @end
%%======================================================================
-module(leo_mcerl_sup).
-author("Yosuke Hara").

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start_link/0, stop/0]).

%% Callbacks
-export([init/1]).

-define(MAX_RESTART,              5).
-define(MAX_TIME,                60).
-define(SHUTDOWN_WAITING_TIME, 2000).

-ifdef(TEST).
-define(DEF_TOTA_CACHE_SIZE, 1024 * 1024). %% 1MB
-else.
-define(DEF_TOTA_CACHE_SIZE, 1024 * 1024 * 1024). %% 1GB
-endif.


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc start link.
%% @end
start_link() ->
    TotalCacheSize = case application:get_env(leo_mcerl, total_cache_size) of
                         {ok, Value1} when is_integer(Value1) ->
                             Value1;
                         _ ->
                             ?DEF_TOTA_CACHE_SIZE
                     end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TotalCacheSize]).


%% @doc stop process.
%% @end
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) == true ->
            exit(Pid, shutdown),
            ok;
        _ -> not_started
    end.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @doc stop process.
%% @end
%% @private
init([TotalCacheSize]) ->
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
          [{leo_mcerl_server, {leo_mcerl_server, start_link, [TotalCacheSize]},
            permanent, ?SHUTDOWN_WAITING_TIME, worker, [leo_mcerl_server]}]}}.

