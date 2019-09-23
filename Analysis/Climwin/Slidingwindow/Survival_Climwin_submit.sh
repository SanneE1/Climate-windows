#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#Specify job name
#$ -N Surv_Climwin

#Resources
# max running time


# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-30

#needed when submitting a non-parallel job
#$ -binding linear:1

#create a single output directory per job
output_dir="/work/$USER/$JOB_NAME-$JOB_ID"
mkdir -p "$output_dir"


module load foss/2018b R/3.5.1

vital_rate=$1
climate=$2
SpeciesInput=$3
cdata=$(basename $climate .csv | cut -d _ -f3)
species=$(basename $SpeciesInput .csv | cut -d _ -f1)
output="$output_dir"/${JOB_NAME}_${species}_${cdata}_${JOB_ID}_$SGE_TASK_ID.rds


Rscript $HOME/Biome/Analysis/Climwin/Slidingwindow/Survival_Climwin.R \
  --climate-data-format=$cdata \
  --species-used=$species \ 
  $vital_rate
  $climate \
  $SpeciesInput \
  $output
