# C2Rust Vagrant environment

- build and run (tested on macOS with Docker CE)
```
cd $C2Rust/docker
# create image
docker build -t c2rust-img .
# create container
docker run --name c2rust-cont -it -v $PWD/..:/home/docker/C2Rust c2rust-img
# start start container
docker start c2rust-cont
docker stop c2rust-cont
# connect to running container
docker exec -it c2rust-cont /bin/bash
# delete container (force stop if running)
docker rm -f c2rust-cont

```
- **careful**: remove all containers 
```
docker rm `docker ps -aq`
```