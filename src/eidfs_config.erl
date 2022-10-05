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

-module(eidfs_config).


-export([acceptors/1]).
-export([port/1]).


port(http) ->
    envy:to_integer(eidfs, http_port, default(80)).


acceptors(http) ->
    envy:to_integer(eidfs, http_acceptors, default(100)).


default(Default) ->
    [os_env, app_env, {default, Default}].
