#!/bin/bash

diagnoses='ADHD ANX ASD BIP MDD PTSD SCZ ED'

for i in `echo "${diagnoses}" | sort -u`
do

  Rscript PlotPleioFDRManhattan_both_features.R --diagnosis ${i}
  
done