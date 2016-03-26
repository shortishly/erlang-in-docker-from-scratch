# Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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
FROM scratch
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

ENV REL_NAME minerl
ENV ERTS_VSN 7.3

ENV BINDIR /erts-${ERTS_VSN}/bin

ENTRYPOINT exec ${BINDIR}/erlexec -boot_var /lib -boot /releases/1/${REL_NAME} -noinput -config /releases/1/sys.config -args_file /releases/1/vm.args

ADD _rel/${REL_NAME}/* /
