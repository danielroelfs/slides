#!/bin/bash 

# from: https://github.com/astefanutti/decktape

docker run --rm -t -v `pwd`:/slides -v ~:/home/user astefanutti/decktape --size 1920x1080 --pause 2000 index.html index.pdf
