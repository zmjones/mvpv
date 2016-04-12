#PBS -l nodes=5
#PBS -l walltime=24:00:00
#PBS -l pmem=16gb
#PBS -j oe
#PBS -A open
cd work/mvpv
module load R

R CMD BATCH --no-restore --no-save R/analysis.R /dev/stdout
