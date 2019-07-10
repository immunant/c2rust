#!/bin/bash

USER=${1:-docker}

docker exec --user $USER -it c2rust /bin/bash -c "export COLUMNS=`tput cols`; export LINES=`tput lines`; exec bash"
