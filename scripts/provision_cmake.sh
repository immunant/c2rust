#!/bin/bash

if [[ "$EUID" -ne 0 ]]
  then echo "Please run as root"
  exit
fi

# exit early if cmake in path is the one to be installed
if type "cmake" > /dev/null 2>&1; then
  # CMAKE_VERSION=$(cmake --version)
  cmake --version | grep -q "3.9.1" && { echo "Cmake requirements already met. Nothing to do."; exit 0; }
fi

CMAKE_URL="https://cmake.org/files/v3.9/cmake-3.9.1-Linux-x86_64.sh"
CMAKE_SCRIPT=${CMAKE_URL##*/}
CMAKE_PREFIX=/opt/$CMAKE_SCRIPT
CMAKE_SHA256="6fdaa13c978e3e9d0b23a17bb16e68d344a459217df0378b0785674f206562fc  $CMAKE_SCRIPT"

WORK_DIR=`mktemp -d` && cd $WORK_DIR

# deletes the temp directory
function cleanup {
  rm -rf "$WORK_DIR"
  echo "Deleted temp working directory $WORK_DIR"
}

# register cleanup function to be called on the EXIT signal
trap cleanup EXIT

echo "Downloading $CMAKE_SCRIPT"
wget --quiet $CMAKE_URL || { echo >&2 "Cmake download failed. Aborting."; exit 1; }

# check integrity of the downloaded script
echo $CMAKE_SHA256 > cmake_sha256sum.txt
sha256sum -c cmake_sha256sum.txt 2>&1 | grep OK || { echo >&2 "Checksum mismatch. Aborting."; exit 1; }

chmod u+x $CMAKE_SCRIPT
mkdir -p $CMAKE_PREFIX
./$CMAKE_SCRIPT --prefix=$CMAKE_PREFIX --skip-license || { echo >&2 "Cmake installation failed. Aborting."; exit 1; }

# on ubuntu, call update alternatives
source /etc/os-release
if [ "$NAME" == "Ubuntu" ]; then
  update-alternatives --remove-all cmake 
  update-alternatives --install /usr/bin/cmake cmake "$CMAKE_PREFIX/bin/cmake" 1 --slave /usr/bin/ccmake ccmake "$CMAKE_PREFIX/bin/ccmake" --slave /usr/bin/cpack cpack "$CMAKE_PREFIX/bin/cpack" --slave /usr/bin/ctest ctest "$CMAKE_PREFIX/bin/ctest" --slave /usr/bin/cmake-gui cmake-gui "$CMAKE_PREFIX/bin/cmake-gui"
fi
