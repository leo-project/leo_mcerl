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
%% @doc The mcerl API
%% @reference https://github.com/leo-project/leo_mcerl/blob/master/src/leo_mcerl.erl
%% @end
%%======================================================================
-module(leo_mcerl).
-author('Yoshiyuki Kanno').

-export([start/1, put/3, get/2, delete/2, size/1, items/1, stop/1]).
-on_load(init/0).


%% @doc Initialize
%%
-spec(init() ->
             ok).
init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", "leo_mcerl"]);
                         _ ->
                             filename:join("../priv", "leo_mcerl")
                     end;
                 Dir ->
                     filename:join(Dir, "leo_mcerl")
             end,
    erlang:load_nif(SoName, 0).


%% @doc Launch leo_mcerl
%%
-spec(start(Size) ->
             {ok, any()} when Size::integer()).
start(_Size) ->
    exit(nif_library_not_loaded).


%% @doc Insert an object into the leo_mcerl
%%
-spec(put(Res, Key, Value) ->
             ok | {error, any()} when Res::any(),
                                      Key::binary(),
                                      Value::binary()).
put(_Res, _Key, _Value) ->
    exit(nif_library_not_loaded).


%% @doc Retrieve an object from the leo_mcerl
%%
-spec(get(Res, Key) ->
             {ok, binary()} | not_found | {error, any()} when Res::any(),
                                                              Key::binary()).
get(_Res, _Key) ->
    exit(nif_library_not_loaded).

%% @doc Delete an object from the leo_mcerl
%%
-spec(delete(Res, Key) ->
             ok | {error, any()} when Res::any(),
                                      Key::binary()).
delete(_Res, _Key) ->
    exit(nif_library_not_loaded).


%% @doc Retrieve size of cached objects
%%
-spec(size(Res) ->
             {ok, integer()} | {error, any()} when Res::any()).
size(_Res) ->
    exit(nif_library_not_loaded).

%% @doc Retrieve total of cached objects
%%
-spec(items(Res) ->
             {ok, integer()} | {error, any()} when Res::any()).
items(_Res) ->
    exit(nif_library_not_loaded).


%% @doc Halt the leo_mcerl
%%
-spec(stop(Res) ->
             ok | {error, any()} when Res::any()).
stop(_Res) ->
    exit(nif_library_not_loaded).
