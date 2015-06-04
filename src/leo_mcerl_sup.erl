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
-export([start_child/2]).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc stop process.
%% @end
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) == true ->
            exit(Pid, shutdown),
            ok;
        _ -> not_started
    end.


%% @doc start a chile under this
%% @end
start_child(ProcId, CacheCapacity) ->
    case supervisor:start_child(
           ?MODULE, {ProcId,
                     {leo_mcerl_server, start_link, [ProcId, CacheCapacity]},
                     permanent, 2000, worker, [leo_mcerl_server]}) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "start_child/2"},
                                    {line, ?LINE}, {body, Cause}]),
            {error, Cause}
    end.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @doc stop process.
%% @end
%% @private
init([]) ->
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, []}}.
