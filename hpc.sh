#PBS -l nodes=20
#PBS -l walltime=24:00:00
#PBS -l pmem=24gb
#PBS -j oe
#PBS -A open
cd work/mvpv
module load R

mkdir -p results
Rscript R/analysis.R 20
