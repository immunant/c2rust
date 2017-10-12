# C2Rust Vagrant environment
- Ubuntu 16.04.3 LTS (GNU/Linux 4.4.0-62-generic x86_64)
- CMake 3.7.2
- clang version 3.9.1-4ubuntu3~16.04.2 (tags/RELEASE_391/rc2)
- Ninja 1.5.1

## Running with Virtualbox
`vagrant up`

## Running with VMWare Fusion
Requires paid plug in https://www.vagrantup.com/vmware/index.html

1. install plugin
`vagrant plugin install vagrant-vmware-fusion` 

2. install license
`vagrant plugin license vagrant-vmware-fusion /path/to/license.lic`

3. start vagrant
`vagrant up --provider vmware_fusion`
