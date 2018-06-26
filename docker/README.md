# C2Rust Vagrant environment

Building LLVM will require more than the default 2GB of RAM on macOS.
4GB should be sufficient.

To build and run (tested on macOS 11.13 with Docker CE 18.03):
```
cd $C2Rust/docker
# create image
docker build -t c2rust .
# create container
docker run --name c2rust --hostname docker -it -v $PWD/..:/home/docker/C2Rust c2rust
# start start container
docker start c2rust
docker stop c2rust
# connect to running container
docker exec -it c2rust /bin/bash
# delete container (force stop if running)
docker rm -f c2rust

```

## Warning: the following commands delete data

removing all containers:
```
docker rm `docker ps -aq`
```

pruning all images:
```
docker prune system
# remove all images, not just unused ones
docker prune system -a
```
