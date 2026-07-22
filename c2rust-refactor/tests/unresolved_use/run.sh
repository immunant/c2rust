#!/bin/sh
set -e

$refactor reorganize_definitions -- old.rs $rustflags
