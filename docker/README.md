# C2Rust Docker environment

Tested with Docker Community Edition 18.03. The version distributed with your host OS may be too old. Follow the installation [instructions](https://docs.docker.com/install/) to get the latest version. 

Building the docker image: 

    $ cd /path/to/c2rust/docker
    $ ../scripts/docker_build.sh


The `docker_build.sh` script takes two optional arguments:
1. the name of the base image (`ubuntu:bionic` by default)
2. the name of the provisioning script (`provision_deb.sh` by default)

Creating a container:

    $ ./docker_run.sh

The `docker_run.sh` scripts takes the image name as an optional argument:

    $ ./docker_run.sh immunant/c2rust:ubuntu-xenial-20190131

Stopping and starting containers:

    $ docker start c2rust
    $ docker stop c2rust

Connect to a running container:

    $ ./docker_exec.sh

Delete c2rust container (force stop if running)

    $ docker rm -f c2rust

## Warning: the following commands delete data

removing all containers:
```
docker rm `docker ps -aq`
```

pruning all images:
```
docker system prune
# remove *all* images, not just unused ones
docker system prune -a
```
