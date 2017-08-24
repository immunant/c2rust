# C2Rust Vagrant environment
- Ubuntu 14.04.5 LTS (GNU/Linux 3.13.0-32-generic x86_64)
- CMake 3.7.2
- Clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4) 

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
