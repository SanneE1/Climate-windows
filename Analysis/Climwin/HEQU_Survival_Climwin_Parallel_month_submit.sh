#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#Specify job name
#$ -N Surv_Climwin

#Resources
# max running time
#$ -l h_rt=720:00:00

# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-12

#needed when submitting a non-parallel job
#$ -binding linear:1

module load foss/2018b R/3.5.1

climate=$1
SpeciesInput=$2
output=/work/$USER/${JOB_NAME}_$(basename $climate .csv | cut -d _ -f1)_$(basename $climate .csv | cut -d _ -f3)_${JOB_ID}_$SGE_TASK_ID.rds

 
Rscript $HOME/Biome/Analysis/Climwin/HEQU_Survival_Climwin_Parallel_month.r \
  $climate \
  $SpeciesInput \
  $output