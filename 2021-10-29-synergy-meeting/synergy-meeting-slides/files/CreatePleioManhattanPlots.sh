#!/bin/bash

diagnoses=`ls ~/Downloads/network_genetics/fuma/fuma*_div100k.csv | xargs basename | awk -F _ '{print $5}'`

for i in `echo "${diagnoses}" | sort -u`
do

  Rscript PlotPleioFDRManhattans.R --diagnosis ${i}
  
done