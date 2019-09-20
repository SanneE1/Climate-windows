#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#Specify job name
#$ -N Growth_Baseline

#Resources
# max running time
#$ -l h_rt=10:00:00

# memory per core (hard limit)
#$ -l h_vmem=8G

#needed when submitting a non-parallel job
#$ -binding linear:1

module load foss/2018b R/3.5.1


Rscript $HOME/Biome/Analysis/Climwin/HEQU_Growth_Baseline.R
