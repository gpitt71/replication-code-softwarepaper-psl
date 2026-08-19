#!/usr/bin/env bash
#SBATCH --job-name=psl-main
#SBATCH --partition=modi_long
#SBATCH --nodelist=n003
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=128G
#SBATCH --time=24:00:00
#SBATCH --output=psl-main-%j.out
#SBATCH --error=psl-main-%j.err

set -euo pipefail

# Slurm sets this to the directory from which `sbatch` was called.
cd "${SLURM_SUBMIT_DIR:?Submit this job from the project root directory}"

mkdir -p data results logs

test -f _targets_main_comparison.R
test -f scripts/run_main_comparison_crew_local.R

export MAIN_COMPARISON_CREW_WORKERS="${MAIN_COMPARISON_CREW_WORKERS:-2}"

# Prevent each crew worker from starting its own pool of BLAS/OpenMP threads.
export R_DATATABLE_NUM_THREADS=1
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

echo "Project directory: ${PWD}"
echo "Node: ${SLURMD_NODENAME:-unknown}"
echo "Crew workers: ${MAIN_COMPARISON_CREW_WORKERS}"
echo "Job ID: ${SLURM_JOB_ID:-unknown}"

Rscript scripts/run_main_comparison_crew_local.R
