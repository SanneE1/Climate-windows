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


## Need a different output. Want it to reflect the jobname and climate basename of the dependend job
output=$1
## The jobID for which I want to merge



Rscript $HOME/Biome/Analysis/Climwin/Merge_arrays.R \
  $output \
