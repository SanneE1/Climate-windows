#!/bin/bash

# bash submit_Climwin_master.sh $climate $SpeciesInput

vitalrate=$1
climate=$2
SpeciesInput=$3

dependency=$(
  qsub -terse \
    Analysis/Climwin/Slidingwindow/Sliding_submit.sh \
    $vitalrate \
    $climate \
    $SpeciesInput
)

qsub -hold_jid $dependency \
  Analysis/Climwin/Slidingwindow/Merge_sliding_submit.sh \
    /work/evers/Surv_Climwin-$dependency \
    $vitalrate \
    $climate \
    $SpeciesInput \