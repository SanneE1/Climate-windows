#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#Specify job name
#$ -N Merge_Climwin

#Resources
# max running time
#$ -l h_rt=900

# memory per core (hard limit)
#$ -l h_vmem=8G

#needed when submitting a non-parallel job
#$ -binding linear:1

module load foss/2018b R/3.5.1

## The master submit will pass the output directory that belongs to the first job to output
output_dir=$1

Rscript "$HOME"/Biome/Analysis/Climwin/Randomwindow/Merge_random.R \
  "$output_dir"
