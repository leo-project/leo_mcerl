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
%% @doc The memory cache server
%% @reference https://github.com/leo-project/leo_mcerl/blob/master/src/leo_mcerl_server.erl
%% @end
%%======================================================================
-module(leo_mcerl_server).
-author("Yosuke Hara").

-behaviour(gen_server).

-include("leo_mcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2, stop/1,
         get/2, put/3, delete/2, stats/1, items/1, size/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {handler,
                total_cache_size = 0 :: integer(),
                stats_gets	     = 0 :: integer(),
                stats_puts	     = 0 :: integer(),
                stats_dels	     = 0 :: integer(),
                stats_hits       = 0 :: integer()
               }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(Id, CacheSize) ->
             {ok, pid()} | ignore | {error, any()} when Id::atom(),
                                                        CacheSize::non_neg_integer()).
start_link(Id, CacheSize) ->
    gen_server:start_link({local, Id}, ?MODULE, [CacheSize], []).


%% @doc Stop the server
-spec(stop(Id) ->
             ok when Id::atom()).
stop(Id) ->
    gen_server:cast(Id, stop).


%% @doc Retrieve a value associated with a specified key
-spec(get(Id, Key) ->
             undefined | binary() | {error, any()} when Id::atom(),
                                                        Key::binary()).
get(Id, Key) ->
    gen_server:call(Id, {get, Key}).


%% @doc Insert a key-value pair into the leo_mcerl
-spec(put(Id, Key, Value) ->
             ok | {error, any()} when Id::atom(),
                                      Key::binary(),
                                      Value::binary()).
put(Id, Key, Value) ->
    gen_server:call(Id, {put, Key, Value}).


%% @doc Remove a key-value pair by a specified key into the leo_mcerl
-spec(delete(Id, Key) ->
             ok | {error, any()} when Id::atom(),
                                      Key::binary()).
delete(Id, Key) ->
    gen_server:call(Id, {delete, Key}).


%% @doc Return server's state
-spec(stats(Id) ->
             any() when Id::atom()).
stats(Id) ->
    gen_server:call(Id, {stats}).


%% @doc Return server's items
-spec(items(Id) ->
             any() when Id::atom()).
items(Id) ->
    gen_server:call(Id, {items}).


%% @doc Return server's summary of cache size
-spec(size(Id) ->
             any() when Id::atom()).
size(Id) ->
    gen_server:call(Id, {size}).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([CacheSize]) ->
    {ok, Handler} = leo_mcerl:start(CacheSize),
    {ok, #state{total_cache_size = CacheSize,
                handler          = Handler}}.

handle_call({get, Key}, _From, #state{handler    = Handler,
                                      stats_gets = Gets,
                                      stats_hits = Hits} = State) ->
    case catch leo_mcerl:get(Handler, Key) of
        {ok, Value} ->
            {reply, {ok, Value}, State#state{stats_gets = Gets + 1,
                                             stats_hits = Hits + 1}};
        not_found ->
            {reply, not_found, State#state{stats_gets = Gets + 1}};
        {error, Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "handle_call/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {reply, {error, Cause}, State};
        {'EXIT', Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "handle_call/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {reply, {error, Cause}, State}
    end;

handle_call({put, Key, Val}, _From, #state{handler    = Handler,
                                           stats_puts = Puts} = State) ->
    case catch leo_mcerl:put(Handler, Key, Val) of
        ok ->
            {reply, ok, State#state{stats_puts = Puts + 1}};
        {'EXIT', Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "handle_call/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {reply, {error, Cause}, State};
        {error, Cause} ->
            case Cause of
                'out_of_memory' ->
                    void;
                _ ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING},
                                            {function, "handle_call/3"},
                                            {line, ?LINE}, {body, Cause}])
            end,
            {reply, {error, Cause}, State}
    end;

handle_call({delete, Key}, _From, State = #state{handler    = Handler,
                                                 stats_dels = Dels}) ->
    case catch leo_mcerl:delete(Handler, Key) of
        ok ->
            {reply, ok, State#state{stats_dels = Dels + 1}};
        {'EXIT', Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "handle_call/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {reply, {error, Cause}, State};
        {error, Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "handle_call/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {reply, {error, Cause}, State}
    end;

handle_call({stats}, _From, State = #state{handler    = Handler,
                                           stats_hits = Hits,
                                           stats_gets = Gets,
                                           stats_puts = Puts,
                                           stats_dels = Dels}) ->
    {ok, Items} = leo_mcerl:items(Handler),
    {ok, Size}  = leo_mcerl:size(Handler),
    Stats = #cache_stats{hits        = Hits,
                         gets        = Gets,
                         puts        = Puts,
                         dels        = Dels,
                         records     = Items,
                         cached_size = Size},
    {reply, {ok, Stats}, State};

handle_call({items}, _From, #state{handler = Handler} = State) ->
    Reply = leo_mcerl:items(Handler),
    {reply, Reply, State};

handle_call({size}, _From, #state{handler = Handler} = State) ->
    Reply  = leo_mcerl:size(Handler),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    terminated.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
