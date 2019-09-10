#!/bin/bash
 
#$ -S /bin/bash
 
#$ -wd /work/$USER
 
#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#Specify job name
#$ -N HEQU_Surv_Climwin

#Resources
# max running time
#$ -l h_rt=720:00:00

# memory per core (hard limit)
#$ -l h_vmem=8G

#CPU cores (pe =parallel environment, smp = semetric multi processing)
#$ -pe smp 20-40

module load foss/2018b R/3.5.1
 
Rscript $HOME/Biome/Analysis/Climwin/HEQU_Survival_Climwin_Parallel_month.r