if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Install the targets package first: install.packages('targets')")
}
if (!requireNamespace("crew", quietly = TRUE)) {
  stop("Install the crew package first: install.packages('crew')")
}

Sys.setenv(
  MAIN_COMPARISON_USE_CREW = "true",
  MAIN_COMPARISON_USE_SLURM = "false",
  R_DATATABLE_NUM_THREADS = "1",
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  BLIS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

if (!nzchar(Sys.getenv("MAIN_COMPARISON_CREW_WORKERS", unset = ""))) {
  Sys.setenv(MAIN_COMPARISON_CREW_WORKERS = "2")
}

selected <- Sys.getenv("MAIN_COMPARISON_TARGETS", unset = "")
selected_names <- if (nzchar(selected)) {
  trimws(strsplit(selected, ",", fixed = TRUE)[[1]])
} else {
  character(0)
}

if (length(selected_names)) {
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
print(targets::tar_crew(store = "_targets_main_comparison"))
