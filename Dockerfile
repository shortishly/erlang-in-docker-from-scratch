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

ENV REL_NAME=minerl
ENV REL_VSN=1
ENV ERTS_VSN 7.3
ENV REL_DIR /releases/${REL_VSN}
ENV ERTS_DIR /erts-${ERTS_VSN}
ENV BINDIR /erts-7.3/bin
ENV EMU beam
ENV PROGNAME erl
ENV LD_LIBARY_PATH ${ERTS_DIR}/lib:${LD_LIBRARY_PATH}
ENV ERTS_LIB_DIR /lib

ENTRYPOINT ${BINDIR}/erlexec -boot_var ${ERTS_LIB_DIR} -boot ${REL_DIR}/${REL_NAME} -noinput

ADD _rel/minerl/ /
