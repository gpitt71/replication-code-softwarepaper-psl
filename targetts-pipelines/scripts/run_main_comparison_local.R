if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Install the targets package first: install.packages('targets')")
}

# Locate targetts-pipelines from this runner's path, so the command works from
# any current working directory.
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(file_argument)) {
  stop("Run this file with Rscript, not source().")
}
runner_file <- normalizePath(
  sub("^--file=", "", file_argument[[1]]),
  mustWork = TRUE
)
project_directory <- dirname(dirname(runner_file))
setwd(project_directory)

if (!file.exists("_targets_main_comparison.R")) {
  stop("Could not find _targets_main_comparison.R in ", project_directory)
}

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("../data", recursive = TRUE, showWarnings = FALSE)

Sys.setenv(
  MAIN_COMPARISON_USE_CREW = "false",
  MAIN_COMPARISON_USE_SLURM = "false",
  R_DATATABLE_NUM_THREADS = "1",
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

selected <- Sys.getenv("MAIN_COMPARISON_TARGETS", unset = "")
if (nzchar(selected)) {
  targets::tar_make(
    names = tidyselect::any_of(trimws(strsplit(
      Sys.getenv("MAIN_COMPARISON_TARGETS"), ",", fixed = TRUE
    )[[1]])),
    script = "_targets_main_comparison.R",
    store = "_targets_main_comparison"
  )
} else {
  targets::tar_make(
    script = "_targets_main_comparison.R",
    store = "_targets_main_comparison"
  )
}
