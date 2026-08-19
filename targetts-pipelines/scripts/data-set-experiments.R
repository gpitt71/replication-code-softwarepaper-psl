# Data preparation and learner-library definitions for the four applied-data
# experiments. This file is sourced by ../_targets_data_sets.R.

replication_root <- normalizePath(
  file.path(getwd(), ".."),
  winslash = "/",
  mustWork = TRUE
)

metabric_file <- file.path(
  replication_root,
  "data",
  "metabric_clinical_and_expression_data.csv"
)

dataset_specifications <- function() {
  list(
    mgus2 = list(
      id = "id",
      event_time = "psl_time",
      status = "psl_status",
      covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
      gam_numeric = c("age", "dxyr", "hgb", "creat", "mspike"),
      hal2_knots = c(15L, 6L),
      hal3_knots = c(10L, 4L, 2L)
    ),
    rotterdam = list(
      id = "pid",
      event_time = "dtime",
      status = "death",
      covariates = c(
        "year", "age", "meno", "size", "grade", "nodes", "pgr", "er",
        "hormon", "chemo"
      ),
      gam_numeric = c("year", "age", "nodes", "pgr", "er"),
      hal2_knots = c(10L, 4L),
      hal3_knots = c(8L, 3L, 1L)
    ),
    pbc = list(
      id = "id",
      event_time = "time",
      status = "psl_status",
      covariates = c("age", "sex", "edema", "bili", "albumin"),
      gam_numeric = c("age", "bili", "albumin"),
      hal2_knots = c(15L, 6L),
      hal3_knots = c(10L, 4L, 2L)
    ),
    metabric = list(
      id = "Patient_ID",
      event_time = "Survival_time",
      status = "psl_status",
      covariates = c(
        "Cohort", "Age_at_diagnosis", "Chemotherapy", "Radiotherapy",
        "Lymph_nodes_examined_positive", "Lymph_node_status", "Cancer_type",
        "ER_status", "PR_status", "HER2_status",
        "HER2_status_measured_by_SNP6", "PAM50",
        "Nottingham_prognostic_index", "Integrative_cluster", "ESR1", "ERBB2",
        "PGR", "TP53", "PIK3CA", "GATA3", "FOXA1", "MLPH"
      ),
      gam_numeric = c(
        "Age_at_diagnosis", "Lymph_nodes_examined_positive",
        "Nottingham_prognostic_index", "ESR1", "ERBB2", "PGR", "TP53",
        "PIK3CA", "GATA3", "FOXA1", "MLPH"
      ),
      hal2_knots = c(6L, 2L),
      hal3_knots = c(5L, 1L, 1L)
    )
  )
}

load_datasets <- function(metabric_path = metabric_file) {
  d1 <- data.table::as.data.table(survival::mgus2)
  d1[, psl_time := data.table::fifelse(pstat == 1, ptime, futime)]
  d1[, psl_status := data.table::fifelse(
    pstat == 1,
    1L,
    data.table::fifelse(death == 1, 2L, 0L)
  )]
  d1[, sex := as.factor(sex)]

  d2 <- data.table::as.data.table(survival::rotterdam)
  d2[, c("meno", "size", "grade", "hormon", "chemo") := lapply(.SD, factor),
     .SDcols = c("meno", "size", "grade", "hormon", "chemo")]

  d3 <- data.table::as.data.table(survival::pbc)
  d3[, psl_status := status]
  d3 <- d3[, .SD, .SDcols = c(
    "id", "time", "psl_status", "age", "sex", "edema", "bili", "albumin"
  )]
  d3[, c("sex", "edema") := lapply(.SD, factor),
     .SDcols = c("sex", "edema")]

  d4 <- data.table::fread(metabric_path)
  factor_columns <- c(
    "Cohort", "Chemotherapy", "Radiotherapy", "Lymph_node_status",
    "Cancer_type", "ER_status", "PR_status", "HER2_status",
    "HER2_status_measured_by_SNP6", "PAM50", "Integrative_cluster"
  )
  d4[, (factor_columns) := lapply(.SD, factor), .SDcols = factor_columns]
  d4[, psl_status := data.table::fcase(
    Vital_status == "Living", 0L,
    Vital_status == "Died of Disease", 1L,
    Vital_status == "Died of Other Causes", 2L,
    default = NA_integer_
  )]

  list(mgus2 = d1, rotterdam = d2, pbc = d3, metabric = d4)
}

prepare_dataset <- function(dataset_name, datasets, specifications) {
  spec <- specifications[[dataset_name]]
  keep <- c(spec$id, spec$event_time, spec$status, spec$covariates)
  dat <- data.table::copy(datasets[[dataset_name]])[, ..keep]
  complete <- stats::complete.cases(dat) & dat[[spec$event_time]] > 0
  attrition <- data.frame(
    dataset = dataset_name,
    n_raw = nrow(dat),
    n_complete = sum(complete),
    n_excluded = sum(!complete)
  )
  dat <- droplevels(as.data.frame(dat[complete]))
  stopifnot(
    nrow(dat) > 0L,
    !anyNA(dat),
    all(dat[[spec$event_time]] > 0),
    all(dat[[spec$status]] %in% 0:max(dat[[spec$status]]))
  )
  list(name = dataset_name, data = dat, spec = spec, attrition = attrition)
}

xgboost_configurations <- function(dataset_name) {
  nround_scale <- switch(
    dataset_name,
    pbc = 0.75,
    mgus2 = 0.90,
    rotterdam = 1,
    metabric = 1,
    1
  )
  list(
    xgb_stumps = list(
      params = list(
        eta = 0.08, max_depth = 1L, min_child_weight = 10,
        subsample = 0.8, colsample_bytree = 0.8, alpha = 0.5, lambda = 1,
        nthread = 1L
      ),
      nrounds = ceiling(220 * nround_scale)
    ),
    xgb_shallow = list(
      params = list(
        eta = 0.05, max_depth = 2L, min_child_weight = 8,
        subsample = 0.8, colsample_bytree = 0.8, alpha = 0.25, lambda = 1,
        nthread = 1L
      ),
      nrounds = ceiling(320 * nround_scale)
    ),
    xgb_interactions = list(
      params = list(
        eta = 0.04, max_depth = 3L, min_child_weight = 12,
        subsample = 0.75, colsample_bytree = 0.7, alpha = 0.5, lambda = 1.5,
        nthread = 1L
      ),
      nrounds = ceiling(400 * nround_scale)
    )
  )
}

make_learner_library <- function(dataset_name, spec) {
  xgb <- xgboost_configurations(dataset_name)
  gam_terms <- ifelse(
    spec$covariates %in% spec$gam_numeric,
    sprintf("s(%s, k = 5, bs = 'cr')", spec$covariates),
    spec$covariates
  )
  xgb_learner <- function(config) {
    poissonsuperlearner::Learner_xgboost(
      covariates = spec$covariates,
      cross_validation = TRUE,
      params = config$params,
      nrounds = config$nrounds,
      nfold = 3L,
      early_stopping_rounds = 15L,
      verbose = 0L
    )
  }

  list(
    glm = poissonsuperlearner::Learner_glmnet(
      covariates = spec$covariates, lambda = 0, cross_validation = FALSE
    ),
    lasso = poissonsuperlearner::Learner_glmnet(
      covariates = spec$covariates, cross_validation = TRUE,
      alpha = 1, nfolds = 3L
    ),
    ridge = poissonsuperlearner::Learner_glmnet(
      covariates = spec$covariates, cross_validation = TRUE,
      alpha = 0, nfolds = 3L
    ),
    gam = poissonsuperlearner::Learner_gam(
      covariates = gam_terms, method = "fREML", discrete = TRUE, nthreads = 1L
    ),
    xgb_stumps = xgb_learner(xgb$xgb_stumps),
    xgb_shallow = xgb_learner(xgb$xgb_shallow),
    xgb_interactions = xgb_learner(xgb$xgb_interactions),
    hal_degree_2 = poissonsuperlearner::Learner_hal(
      covariates = spec$covariates, max_degree = 2L,
      num_knots = spec$hal2_knots, nfolds = 3L,
      lambda.min.ratio = 0.05, maxit = 50000L
    ),
    hal_degree_3 = poissonsuperlearner::Learner_hal(
      covariates = spec$covariates, max_degree = 3L,
      num_knots = spec$hal3_knots, nfolds = 3L,
      lambda.min.ratio = 0.05, maxit = 50000L
    )
  )
}

fit_dataset_superlearner <- function(prepared) {
  set.seed(20260817L + match(
    prepared$name,
    c("mgus2", "rotterdam", "pbc", "metabric")
  ))
  started <- Sys.time()
  fit <- poissonsuperlearner::Superlearner(
    data = prepared$data,
    id = prepared$spec$id,
    status = prepared$spec$status,
    event_time = prepared$spec$event_time,
    learners = make_learner_library(prepared$name, prepared$spec),
    number_of_nodes = 8L,
    nfold = 3L,
    verbose = TRUE
  )
  list(
    dataset = prepared$name,
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    fit = fit
  )
}

configuration_table <- function(specifications) {
  data.table::rbindlist(lapply(names(specifications), function(nm) {
    spec <- specifications[[nm]]
    xgb <- xgboost_configurations(nm)
    data.table::rbindlist(c(
      lapply(names(xgb), function(learner) {
        cfg <- xgb[[learner]]
        data.table::data.table(
          dataset = nm, learner = learner,
          configuration = paste0(
            "depth=", cfg$params$max_depth, "; eta=", cfg$params$eta,
            "; rounds=", cfg$nrounds, "; min_child_weight=",
            cfg$params$min_child_weight, "; subsample=", cfg$params$subsample,
            "; colsample=", cfg$params$colsample_bytree
          )
        )
      }),
      list(
        data.table::data.table(
          dataset = nm, learner = "hal_degree_2",
          configuration = paste0("max_degree=2; num_knots=", paste(spec$hal2_knots, collapse = ","))
        ),
        data.table::data.table(
          dataset = nm, learner = "hal_degree_3",
          configuration = paste0("max_degree=3; num_knots=", paste(spec$hal3_knots, collapse = ","))
        )
      )
    ))
  }))
}
