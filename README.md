When packaging an application as a [Docker](https://www.docker.com)
container it is too easy to just be lazy and put `FROM debian` (other
distributions are available, replace `debian` with your distribution
of choice). For sure it is going to work, but you have just included
dozens of libraries and binaries that your application
[just does not need](https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/). An
image that could be tens of megabytes is now at least several
hundred - we are building containers not virtual machines here!

One of the things I like about [Go](https://golang.org) is that
typical application binaries are small with no runtime
dependencies. Fewer dependencies mean less patching and security
headaches. The less friction in the CI build cycle, the better. Go
achieves this by having statically linked applications meaning that
just one binary is necessary in `ADD`, and they are typically built
from
[scratch](https://docs.docker.com/engine/userguide/eng-image/baseimages/)
(etcd as a good [example](https://github.com/coreos/etcd/blob/master/scripts/build-docker#L12-L16)).

[Erlang](http://www.erlang.org) was designed to be embedded in
telecoms equipment, so we must be able to package applications in
Docker with a small footprint too?

This repository contains a regular erlang application that is packaged
in a docker container from scratch. You will need both `erlang`
installed and `docker` service running preferably on a Linux
environment. The release needs to be built on Linux to be able to run
on Linux because we are going include the ERTS.

On MacOS you might want to run `shortishly/docker-erlang`
which will give you erlang and docker packaged together in a shell
(`brew install docker-machine` if you don't already have
it):

```shell
docker run \
       -v /var/run/docker.sock:/var/run/docker.sock \
       -t \
       -i \
       --rm \
       shortishly/docker-erlang \
       /bin/bash
```

Clone and build this
[erlang-in-docker-from-scratch](https://github.com/shortishly/erlang-in-docker-from-scratch)
repository, which contains a minimal erlang application that builds a
release into the `_rel` directory:

```shell
git clone https://github.com/shortishly/erlang-in-docker-from-scratch.git eidfs
cd eidfs
make
```

At the end of `make` a standard erlang release for the eidfs
application is now present in the `_rel` directory. To make it run
inside a scratch container we need to include any runtime dependencies
too. This is where `mkimage` comes in:

```shell
./bin/mkimage
```

The `./bin/mkimage` script copies in any dynamic libraries that ERTS
needs to run the erlang release:

```shell
'/lib/x86_64-linux-gnu/libc.so.6' -> '_rel/eidfs/lib/x86_64-linux-gnu/libc.so.6'
'/lib/x86_64-linux-gnu/libdl.so.2' -> '_rel/eidfs/lib/x86_64-linux-gnu/libdl.so.2'
'/lib/x86_64-linux-gnu/libm.so.6' -> '_rel/eidfs/lib/x86_64-linux-gnu/libm.so.6'
'/lib/x86_64-linux-gnu/libpthread.so.0' -> '_rel/eidfs/lib/x86_64-linux-gnu/libpthread.so.0'
'/lib/x86_64-linux-gnu/librt.so.1' -> '_rel/eidfs/lib/x86_64-linux-gnu/librt.so.1'
'/lib/x86_64-linux-gnu/libtinfo.so.5' -> '_rel/eidfs/lib/x86_64-linux-gnu/libtinfo.so.5'
'/lib/x86_64-linux-gnu/libutil.so.1' -> '_rel/eidfs/lib/x86_64-linux-gnu/libutil.so.1'
'/lib/x86_64-linux-gnu/libz.so.1' -> '_rel/eidfs/lib/x86_64-linux-gnu/libz.so.1'
```

It also copies `/bin/sh` so that we can run the release too. We can
build a docker image for the release using the following command:

```shell
docker build \
       --build-arg REL_NAME=$(bin/release_name) \
       --build-arg ERTS_VSN=$(bin/system_version) \
       --pull=true \
       --no-cache=true \
       --force-rm=true \
       -t $(bin/release_name):$(bin/version) .
```

The `$(bin/release_name)` `$(bin/system_version)` and `$(bin/version)`
are short escripts that respond with the release name, system ERTS
version and the application version respectively.

Quite a lot of effort, what is the reward? Try `docker images` and
look at the size of the resultant container:

```shell
REPOSITORY                 TAG                 IMAGE ID            CREATED             SIZE
eidfs                      0.0.1               6748931f94e4        4 seconds ago       16.74 MB
```

We have a docker packaged erlang release in ~17MB. Lets run it!

```shell
docker run \
       --name $(bin/release_name) \
       -d \
       $(bin/release_name):$(bin/version)
```

Check the logs using `docker logs $(bin/release_name)` and
you will see lots of application startup messages from SASL.

You might notice that the
[ENTRYPOINT](https://docs.docker.com/engine/reference/builder/#entrypoint)
used in the
[Dockerfile](https://github.com/shortishly/erlang-in-docker-from-scratch/blob/master/Dockerfile)
directly invokes `erlexec`. I have done this to reduce dependencies
further so that the release, ERTS dynamic libraries, and `/bin/bash`
only are present in the container.

```dockerfile
FROM scratch
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

ARG REL_NAME
ARG REL_VSN=1
ARG ERTS_VSN

ENV BINDIR /erts-${ERTS_VSN}/bin
ENV BOOT /releases/1/${REL_NAME}
ENV CONFIG /releases/${REL_VSN}/sys.config
ENV ARGS_FILE /releases/${REL_VSN}/vm.args

ENV TZ=GMT

ENTRYPOINT exec ${BINDIR}/erlexec \
           -boot_var /lib \
           -boot ${BOOT} \
           -noinput \
           -config ${CONFIG} \
           -args_file ${ARGS_FILE}

ADD _rel/${REL_NAME}/ /
```
