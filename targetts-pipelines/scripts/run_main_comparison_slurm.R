if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Install the targets package first: install.packages('targets')")
}
if (!requireNamespace("crew", quietly = TRUE)) {
  stop("Install the crew package first: install.packages('crew')")
}
if (!requireNamespace("crew.cluster", quietly = TRUE)) {
  stop("Install crew.cluster before using the Slurm controller.")
}

Sys.setenv(
  MAIN_COMPARISON_USE_CREW = "false",
  MAIN_COMPARISON_USE_SLURM = "true",
  R_DATATABLE_NUM_THREADS = "1",
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

if (!nzchar(Sys.getenv("MAIN_COMPARISON_SLURM_WORKERS", unset = ""))) {
  Sys.setenv(MAIN_COMPARISON_SLURM_WORKERS = "4")
}
if (!nzchar(Sys.getenv("MAIN_COMPARISON_SLURM_PARTITION", unset = ""))) {
  Sys.setenv(MAIN_COMPARISON_SLURM_PARTITION = "long")
}

targets::tar_make(
  script = "_targets_main_comparison.R",
  store = "_targets_main_comparison"
)
print(targets::tar_crew(store = "_targets_main_comparison"))
