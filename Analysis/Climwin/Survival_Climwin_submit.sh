#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#Specify job name
#$ -N Surv_Climwin

#Resources
# max running time
#$ -l h_rt=720:00:00

# memory per core (hard limit)
#$ -l h_vmem=8G

# Array numbers 
#$ -t 1-2

#needed when submitting a non-parallel job
#$ -binding linear:1

module load foss/2018b R/3.5.1

climate=$1
SpeciesInput=$2
cdate=$(basename $climate .csv | cut -d _ -f3)
species=$(basename $SpeciesInput .csv | cut -d _ -f1)
output=/work/$USER/$JOB_NAME-$JOB_ID/${JOB_NAME}_${species}_${cdate}_${JOB_ID}_$SGE_TASK_ID.rds


Rscript $HOME/Biome/Analysis/Climwin/Survival_Climwin.R \
  --climate-data-format=$cdate \
  --species-used=$species \
  $climate \
  $SpeciesInput \
  $output