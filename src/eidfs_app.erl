%% Copyright (c) 2016-2022 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(eidfs_app).


-behaviour(application).
-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
    try
        {ok, Sup} = eidfs_sup:start_link(),
        {ok, Sup, #{listeners => [start_http(http)]}}
    catch
        _:Reason ->
            {error, Reason}
    end.


stop(#{listeners := Listeners}) ->
    lists:foreach(fun cowboy:stop_listener/1, Listeners);

stop(_State) ->
    ok.


start_http(Prefix) ->
    {ok, _} = cowboy:start_clear(Prefix,
                                 [{port, eidfs_config:port(Prefix)}],
                                 #{env => dispatch(Prefix)}),
    Prefix.


dispatch(Prefix) ->
    #{dispatch => cowboy_router:compile(resources(Prefix))}.


resources(http) ->
    [{'_',
      [{<<"/hello">>, eidfs_hello_world_resource, []}]}].
