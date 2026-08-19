library(targets)

tar_option_set(
  packages = c(
    "poissonsuperlearner",
    "pammtools",
    "mgcv",
    "survival",
    "Epi",
    "xtable",
    "data.table"
  )
)

# -------------------------------------------------------------------------
# Benchmark settings
# -------------------------------------------------------------------------

sample_sizes <- c(
  1000L,
  2500L,
  5000L,
  10000L,
  25000L,
  50000L
)

n_reps <- 10L

xnames <- c(
  "sex",
  "age",
  "diabetes_duration",
  "value_SBP",
  "value_LDL",
  "value_HBA1C",
  "value_Smoking",
  "value_Motion",
  "value_Albuminuria",
  "eGFR"
)

nodes <- 0:20

form_surv <- stats::as.formula(paste(
  "survival::Surv(time_cvd, status_cvd) ~",
  paste(xnames, collapse = " + ")
))

form_pamm <- stats::as.formula(paste(
  "ped_status ~ interval +",
  paste(xnames, collapse = " + ")
))

gam_terms <- c(
  "sex",
  "s(age)",
  "s(diabetes_duration)",
  "s(value_SBP)",
  "s(value_LDL)",
  "s(value_HBA1C)",
  "value_Smoking",
  "value_Motion",
  "value_Albuminuria",
  "s(eGFR)"
)

form_pamm_gam <- stats::as.formula(paste(
  "ped_status ~ 0 + interval +",
  paste(gam_terms, collapse = " + ")
))

form_epi <- stats::as.formula(paste(
  "~ interval +",
  paste(xnames, collapse = " + ")
))


# -------------------------------------------------------------------------
# Simulation
# -------------------------------------------------------------------------
simulate_case <- function(case) {
  
  n <- case$sample_size[[1]]
  seed <- case$seed[[1]]
  
  dtrain <- poissonsuperlearner::simulateStenoT1(
    n,
    seed = seed,
    scenario = "alpha",
    competing_risks = TRUE
  )
  
  list(
    data = dtrain,
    sample_size = n,
    seed = seed
  )
}


# -------------------------------------------------------------------------
# Helper for timing output
# -------------------------------------------------------------------------

timing_result <- function(tm, sim, method, repetition) {
  
  data.frame(
    method = method,
    sample_size = sim$sample_size,
    repetition = repetition,
    seed = sim$seed,
    user = unname(tm[["user.self"]]),
    system = unname(tm[["sys.self"]]),
    elapsed = unname(tm[["elapsed"]])
  )
}


# -------------------------------------------------------------------------
# Poisson Super Learner
# -------------------------------------------------------------------------

benchmark_psl <- function(sim, repetition) {

  dtrain <- sim$data

  # Use every processor made available to this R process. The targets are
  # deliberately benchmarked sequentially, so this does not oversubscribe
  # the machine or contaminate elapsed-time comparisons between methods.
  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(percent = 100L)

  gc()

  tm <- system.time({

    l_glm <- Learner_glmnet(
      covariates = xnames,
      lambda = 0,
      cross_validation = FALSE
    )

    fit_psl <- fit_learner(
      dtrain,
      learner = l_glm,
      status = "status_cvd",
      event_time = "time_cvd",
      nodes = nodes
    )

  })

  timing_result(
    tm,
    sim,
    method = "poissonsuperlearner",
    repetition = repetition
  )
}


# -------------------------------------------------------------------------
# Poisson Super Learner GAM
# -------------------------------------------------------------------------

benchmark_psl_gam <- function(sim,
                              repetition) {

  dtrain <- sim$data

  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(percent = 100L)

  gc()

  tm <- system.time({

    l_gam <- poissonsuperlearner::Learner_gam(
      covariates = gam_terms,
      intercept = FALSE
    )

    fit_psl_gam <- poissonsuperlearner::fit_learner(
      dtrain,
      learner = l_gam,
      status = "status_cvd",
      event_time = "time_cvd",
      nodes = nodes
    )

  })

  timing_result(
    tm,
    sim,
    method = "poissonsuperlearner_gam",
    repetition = repetition
  )
}


# -------------------------------------------------------------------------
# pammtools
# -------------------------------------------------------------------------

benchmark_pammtools <- function(sim,
                                repetition) {

  dtrain <- sim$data

  gc()

  tm <- system.time({

    ped <- pammtools::as_ped(
      data = dtrain,
      formula = form_surv,
      id = "id",
      cut = nodes,
      combine = FALSE
    )

    fit_pamm <- lapply(
      ped,
      function(dat) {
        glm(
          form_pamm,
          data = dat,
          family = poisson(),
          offset = offset
        )
      }
    )

  })

  timing_result(
    tm,
    sim,
    method = "pammtools",
    repetition = repetition
  )
}


# -------------------------------------------------------------------------
# pammtools GAM with GCV smoothing-parameter selection
# -------------------------------------------------------------------------

benchmark_pammtools_gam_gcv <- function(sim,
                                        repetition) {

  dtrain <- sim$data

  gc()

  tm <- system.time({

    ped <- pammtools::as_ped(
      data = dtrain,
      formula = form_surv,
      id = "id",
      cut = nodes,
      combine = FALSE
    )

    fit_pamm_gam_gcv <- lapply(
      ped,
      function(dat) {
        mgcv::bam(
          form_pamm_gam,
          data = dat,
          family = poisson(),
          offset = offset,
          method = "GCV.Cp"
        )
      }
    )

  })

  timing_result(
    tm,
    sim,
    method = "pammtools_gam_gcv",
    repetition = repetition
  )
}


# -------------------------------------------------------------------------
# Epi
# -------------------------------------------------------------------------

benchmark_epi <- function(sim,
                          repetition) {

  dtrain <- as.data.frame(sim$data)
  dtrain$status_cvd <- factor(
    dtrain$status_cvd,
    levels = 0:2,
    labels = c("event_free", "cvd", "death")
  )

  gc()

  tm <- system.time({

    L <- Epi::Lexis(
      exit = list(fu = time_cvd),
      exit.status = status_cvd,
      id = id,
      data = dtrain,
      notes = FALSE
    )

    Ls <- Epi::splitLexis(
      L,
      breaks = nodes,
      time.scale = "fu"
    )

    Ls$interval <- factor(Ls$fu)

    fit_epi_1 <- Epi::glm.Lexis(
      Ls,
      formula = form_epi,
      from = "event_free",
      to = "cvd",
      verbose = FALSE
    )

    fit_epi_2 <- Epi::glm.Lexis(
      Ls,
      formula = form_epi,
      from = "event_free",
      to = "death",
      verbose = FALSE
    )

  })

  timing_result(
    tm,
    sim,
    method = "Epi",
    repetition = repetition
  )
}


# -------------------------------------------------------------------------
# targets pipeline
# -------------------------------------------------------------------------

list(

  # One row for every sample size x repetition.
  tar_target(
    benchmark_case,
    data.frame(
      sample_size = sample_sizes,
      seed = 100000L + seq_along(sample_sizes)
    ),
    iteration = "vector"
  ),

  # One simulated dataset for every sample size x repetition.
  tar_target(
    sim_data,
    simulate_case(benchmark_case),
    pattern = map(benchmark_case),
    iteration = "list"
  ),
  #repetition target
  tar_target(
    repetition,
    seq_len(n_reps),
    iteration = "vector"
  ),

  # The three packages are evaluated on exactly the same sim_data branches.
  tar_target(
    timing_psl,
    benchmark_psl(sim_data, repetition),
    pattern = cross(sim_data, repetition)
  ),
  
  
  
  tar_target(
    timing_pammtools,
    benchmark_pammtools(sim_data, repetition),
    pattern = cross(sim_data, repetition)
  ),
  
  tar_target(
    timing_epi,
    benchmark_epi(sim_data, repetition),
    pattern = cross(sim_data, repetition)
  ),

  # Combine all individual benchmark results.
  tar_target(
    timings,
    rbind(
      timing_psl,
      timing_pammtools,
      timing_epi
    )
  ),

  # Summary across the 10 repetitions.
  tar_target(
    timing_summary,
    {
      dt <- data.table::as.data.table(timings)

      dt[
        ,
        .(
          mean_elapsed = mean(elapsed),
          sd_elapsed = sd(elapsed),
          median_elapsed = median(elapsed),
          min_elapsed = min(elapsed),
          max_elapsed = max(elapsed)
        ),
        by = .(
          method,
          sample_size
        )
      ][
        order(sample_size, method)
      ]
    }
  ),
  
  tar_target(
    timing_summary_xtable,
    {
      tab <- data.table::copy(timing_summary)
      
      tab[
        ,
        method := factor(
          method,
          levels = c(
            "poissonsuperlearner",
            "pammtools",
            "Epi"
          ),
          labels = c(
            "\\code{poissonsuperlearner}",
            "\\code{pammtools}",
            "\\code{Epi}"
          )
        )
      ]
      
      data.table::setnames(
        tab,
        c(
          "method",
          "sample_size",
          "mean_elapsed",
          "sd_elapsed",
          "median_elapsed",
          "min_elapsed",
          "max_elapsed"
        ),
        c(
          "Method",
          "Sample size",
          "Mean",
          "SD",
          "Median",
          "Min",
          "Max"
        )
      )
      
      tab[
        ,
        c("Mean", "SD", "Median", "Min", "Max") :=
          lapply(
            .SD,
            \(x) round(x, 3)
          ),
        .SDcols = c(
          "Mean",
          "SD",
          "Median",
          "Min",
          "Max"
        )
      ]
      
      xt <- xtable::xtable(
        tab,
        caption = "Computation times in seconds across 10 repeated model fits.",
        label = "tab:computation_times",
        align = c(
          "l",
          "l",
          "r",
          "r",
          "r",
          "r",
          "r",
          "r"
        ),
        digits = c(
          0,  # row names (not printed)
          0,  # Method
          0,  # Sample size
          2,  # Mean
          2,  # SD
          2,  # Median
          2,  # Min
          2   # Max
        )
      )
      
      capture.output(print(
        xt,
        include.rownames = FALSE,
        caption.placement = "top",
        sanitize.text.function = identity,
        hline.after = c(
          -1,  # top of table
          0,   # below column names
          3,
          6,
          9,
          12,
          15,
          18  # bottom
        )
      ))
      
      # capture.output(
      #   print(
      #     xt,
      #     include.rownames = FALSE,
      #     caption.placement = "top",
      #     booktabs = TRUE,
      #     sanitize.text.function = identity
      #   )
      # )
    }
  )
  
)

# For printing afterwards
# cat(
#   targets::tar_read(timing_summary_xtable),
#   sep = "\n"
# )
