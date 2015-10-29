#PBS -l nodes=16
#PBS -l walltime=24:00:00
#PBS -l pmem=32gb
#PBS -j oe
cd work/mvpv
module load R

R --no-restore --no-save < install_deps.R > install_deps.Rout

make 
mkdir -p output
R --no-restore --no-save < R/data.R --args 1981 2010 > output/data.Rout
R --no-restore --save < R/analysis.R --args 16 > output/analysis.Rout


