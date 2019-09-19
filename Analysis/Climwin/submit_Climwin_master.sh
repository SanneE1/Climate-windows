#!/bin/bash

# bash submit_Climwin_master.sh $climate $SpeciesInput

climate=$1
SpeciesInput=$2

dependency=$(
  qsub -terse \
    Analysis/Climwin/Survival_Climwin_submit.sh \
    $climate \
    $SpeciesInput
)

qsub -hold_jid $dependency \
  Analysis/Climwin/Merge_arrays_submit.sh \
    /work/evers/Surv_Climwin-$dependency \
    $climate \
    $SpeciesInput \