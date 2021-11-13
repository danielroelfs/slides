#!/bin/bash 

# from: https://github.com/astefanutti/decktape

docker run --rm -t -v `pwd`:/slides -v ~:/home/user astefanutti/decktape --size 1440x900 --pause 5000 index.html  imaging-group-phd-day-slides.pdf
