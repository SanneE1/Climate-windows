#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#Specify job name
#$ -N Surv_Random

#Resources
# max running time
#$ -l h_rt=4:00:00

# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-100

#needed when submitting a non-parallel job
#$ -binding linear:1

#create a single output directory per job
output_dir="/work/$USER/$JOB_NAME-$JOB_ID"
mkdir -p "$output_dir"

module load foss/2018b R/3.5.1

climate=$1
SpeciesInput=$2
Results_sliding=$3
cdate=$(basename $climate .csv | cut -d _ -f3)
species=$(basename $SpeciesInput .csv | cut -d _ -f1)
output="$output_dir"/${JOB_NAME}_${species}_${cdate}_${JOB_ID}_$SGE_TASK_ID.rds

 
Rscript $HOME/Biome/Analysis/Climwin/Randomwindow/Survival_Random.R \
  --climate-data-format=$cdate \
  $climate \
  $SpeciesInput \
  $Results_sliding \
  $output