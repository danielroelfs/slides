#!/bin/bash 

# from: https://github.com/astefanutti/decktape

docker run --rm -t -v `pwd`:/slides -v ~:/home/user astefanutti/decktape --size 1440x900 --pause 2000 index.html  211013_DanielRoelfs_Shared_genetic_determinants_brain_functional_connectome.pdf
