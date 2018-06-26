# C2Rust Vagrant environment
- Ubuntu 17.10 (GNU/Linux 4.13.0-21-generic x86_64)
- CMake 3.9.1
- Ninja 1.7.2

## Installing Prerequisites

Download a copy of vagrant from https://www.vagrantup.com/downloads.html. Vagrant supports a range of virtualization engines. We recommend you use either [VirtualBox](https://www.virtualbox.org/wiki/Downloads) or on the VMWare editions, e.g., [VMWare Workstation Player](https://www.vmware.com/products/workstation-player/workstation-player-evaluation.html).

## Running with Virtualbox
`vagrant up`

## Running with VMWare Fusion
Requires paid plug-in. See https://www.vagrantup.com/vmware/index.html

1. install plugin
`vagrant plugin install vagrant-vmware-fusion` 

2. install license
`vagrant plugin license vagrant-vmware-fusion /path/to/license.lic`

3. start vagrant
`vagrant up --provider vmware_fusion`
