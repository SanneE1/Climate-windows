#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#Specify job name
#$ -N Climwin

#Resources
# max running time


# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-4

#needed when submitting a non-parallel job
#$ -binding linear:1

#create a single output directory per job
output_dir="/work/$USER/$JOB_NAME-$JOB_ID"
mkdir -p "$output_dir"


module load foss/2018b R/3.5.1

vitalrate=$3
climate=$1
SpeciesInput=$2
output="$output_dir"/${JOB_NAME}_${vitalrate}_month_${JOB_ID}_$SGE_TASK_ID.rds


Rscript "$HOME"/Biome/Analysis/Climwin/Slidingwindow/HEQU_spatial_slidingwindow.R \
  "$vitalrate" \
  "$climate" \
  "$SpeciesInput" \
  "$output"
