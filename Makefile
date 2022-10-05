#-*- mode: makefile-gmake -*-
# Copyright (c) 2012-2022 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = eidfs
PROJECT_DESCRIPTION = Erlang in Docker from scratch
PROJECT_VERSION = 0.0.2


RELX_TAR = 0

REL_DEPS = \
	relx

SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync

SHELL_DEPS = \
	sync

DEPS = \
	cowboy \
	shelly

LOCAL_DEPS = \
	sasl

dep_shelly = git https://github.com/shortishly/shelly.git

dep_cowboy_commit = 2.9.0
dep_shelly_commit = 0.4.0

include erlang.mk
