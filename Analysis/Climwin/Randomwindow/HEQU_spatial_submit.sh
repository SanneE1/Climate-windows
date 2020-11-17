#!/bin/bash

#$ -S /bin/bash

#$ -wd /work/$USER

#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#Specify job name
#$ -N Random

#Resources
# max running time

# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-2000

#needed when submitting a non-parallel job
#$ -binding linear:1

#create a single output directory per job
output_dir="/work/$USER/$JOB_NAME-$JOB_ID"
mkdir -p "$output_dir"

module load foss/2018b R/3.5.1

Climate=$1
SpeciesInput=$2
Results_sliding=$3
winner=$4
vitalrate=$(basename "$Results_sliding" .rds | cut -d _ -f2)
output="$output_dir"/${JOB_NAME}_month_${vitalrate}_${JOB_ID}_$SGE_TASK_ID.rds


Rscript "$HOME"/Biome/Analysis/Climwin/Randomwindow/HEQU_spatial_random.R \
"$Climate" \
"$SpeciesInput" \
"$Results_sliding" \
"$winner" \
"$output"
