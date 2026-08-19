library(targets)

use_crew <- identical(
  tolower(Sys.getenv("MAIN_COMPARISON_USE_CREW", unset = "false")),
  "true"
)
use_slurm <- identical(
  tolower(Sys.getenv("MAIN_COMPARISON_USE_SLURM", unset = "false")),
  "true"
)

if (use_slurm) {
  if (!requireNamespace("crew", quietly = TRUE)) {
    stop("Install the crew package first: install.packages('crew')")
  }
  if (!requireNamespace("crew.cluster", quietly = TRUE)) {
    stop("Install crew.cluster before using the Slurm controller.")
  }

  dir.create("logs/main-comparison-slurm", recursive = TRUE, showWarnings = FALSE)
  dir.create("logs/main-comparison-slurm-scripts", recursive = TRUE,
             showWarnings = FALSE)

  controller <- crew.cluster::crew_controller_slurm(
    name = "main_comparison",
    workers = as.integer(Sys.getenv(
      "MAIN_COMPARISON_SLURM_WORKERS", unset = "4"
    )),
    tls = crew::crew_tls(mode = "none"),
    seconds_idle = as.integer(Sys.getenv(
      "MAIN_COMPARISON_SLURM_SECONDS_IDLE", unset = "120"
    )),
    seconds_launch = as.integer(Sys.getenv(
      "MAIN_COMPARISON_SLURM_SECONDS_LAUNCH", unset = "3600"
    )),
    tasks_max = as.integer(Sys.getenv(
      "MAIN_COMPARISON_SLURM_TASKS_MAX", unset = "1"
    )),
    garbage_collection = TRUE,
    options_cluster = crew.cluster::crew_options_slurm(
      script_directory = "logs/main-comparison-slurm-scripts",
      log_output = file.path(
        "logs", "main-comparison-slurm", "%x_%A.out"
      ),
      log_error = file.path(
        "logs", "main-comparison-slurm", "%x_%A.err"
      ),
      memory_gigabytes_required = as.numeric(Sys.getenv(
        "MAIN_COMPARISON_SLURM_MEMORY_GB", unset = "48"
      )),
      cpus_per_task = as.integer(Sys.getenv(
        "MAIN_COMPARISON_SLURM_CPUS", unset = "1"
      )),
      time_minutes = as.numeric(Sys.getenv(
        "MAIN_COMPARISON_SLURM_TIME_MINUTES", unset = "1440"
      )),
      partition = Sys.getenv(
        "MAIN_COMPARISON_SLURM_PARTITION", unset = "long"
      ),
      n_tasks = 1L
    )
  )
} else if (use_crew) {
  if (!requireNamespace("crew", quietly = TRUE)) {
    stop("Install the crew package first: install.packages('crew')")
  }

  dir.create("logs/main-comparison-crew-local", recursive = TRUE,
             showWarnings = FALSE)
  controller <- crew::crew_controller_local(
    name = "main_comparison_local",
    workers = as.integer(Sys.getenv(
      "MAIN_COMPARISON_CREW_WORKERS", unset = "2"
    )),
    seconds_idle = 10,
    tasks_max = Inf,
    garbage_collection = TRUE,
    options_local = crew::crew_options_local(
      log_directory = "logs/main-comparison-crew-local",
      log_join = TRUE
    )
  )
} else {
  controller <- NULL
}

tar_option_set(
  controller = controller,
  packages = c(
    "poissonsuperlearner",
    "survival",
    "riskRegression",
    "prodlim",
    "data.table",
    "randomForestSRC"
  ),
  seed = 42L,
  error = "continue"
)

pipeline <- list(

  # mgus2
  tar_target(
    mgus_analysis,
    {
      data <- data.table::as.data.table(survival::mgus2)
      data[, sex := as.factor(sex)]
      data <- data[complete.cases(data)]
      set.seed(42)
      train <- data[, sample(.I, size = floor(0.8 * .N))]
      list(data = data, train = train)
    }
  ),
  tar_target(mgus_train_times, seq(0, max(mgus_analysis$data[mgus_analysis$train][["futime"]]), by = 24)),
  tar_target(mgus_test_times, seq(0, max(mgus_analysis$data[-mgus_analysis$train][["futime"]]), by = 24)),
  tar_target(mgus_xgb1_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), cross_validation = TRUE, params = list(max_depth = 2, eta = 0.05, min_child_weight = 10, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(mgus_xgb2_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), cross_validation = TRUE, params = list(max_depth = 3, eta = 0.05, min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(mgus_xgb3_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), cross_validation = TRUE, params = list(max_depth = 5, eta = 0.03, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(mgus_glm_learner, poissonsuperlearner::Learner_glmnet(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), lambda = 0, cross_validation = FALSE)),
  tar_target(mgus_gam_learner, poissonsuperlearner::Learner_gam(covariates = c("s(age)", "sex", "s(dxyr)", "s(hgb)", "s(creat)", "s(mspike)"))),
  tar_target(mgus_hal2_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), max_degree = 2L, num_knots = c(100L, 50L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(mgus_hal3_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"), max_degree = 3L, num_knots = c(80L, 50L, 5L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(
    mgus_xgb1_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_xgb1_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "xgb1", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_xgb2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_xgb2_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "xgb2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_xgb3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_xgb3_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "xgb3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_glm_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_glm_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "glm", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_gam_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_gam_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "gam", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_hal2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_hal2_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "hal2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_hal3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        mgus_analysis$data[mgus_analysis$train],
        learner = mgus_hal3_learner,
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "hal3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_psl_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::Superlearner(
        mgus_analysis$data[mgus_analysis$train],
        learners = list(mgus_xgb1_learner, mgus_xgb2_learner, mgus_xgb3_learner, mgus_glm_learner, mgus_gam_learner, mgus_hal2_learner, mgus_hal3_learner),
        id = "id", status = "death", event_time = "futime",
        nodes = mgus_train_times, nfold = 10,
        variable_transformation = list("age ~ floor(age)", "dxyr ~ floor(dxyr)", "mspike ~ floor(mspike)", "hgb ~ floor(hgb)", "creat ~ floor(creat)"),
        verbose = FALSE
      ))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "psl", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_cox_fit,
    {
      tm <- system.time(model <- survival::coxph(survival::Surv(futime, death) ~ age + sex + dxyr + hgb + creat + mspike, data = mgus_analysis$data[mgus_analysis$train], x = TRUE))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "cox", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_rfsrc_fit,
    {
      tm <- system.time(model <- randomForestSRC::rfsrc(Surv(futime, death) ~ age + sex + dxyr + hgb + creat + mspike, data = mgus_analysis$data[mgus_analysis$train]))
      list(model = model, timing = data.table::data.table(dataset = "mgus2", method = "rfsrc", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    mgus_score,
    {
      test_data <- mgus_analysis$data[-mgus_analysis$train]
      riskRegression::Score(
        list(
        cox_bmk = riskRegression::predictRisk(mgus_cox_fit$model, newdata = test_data, times = mgus_test_times),
        rfsrc_bmk = riskRegression::predictRisk(mgus_rfsrc_fit$model, newdata = test_data, times = mgus_test_times),
        psl = riskRegression::predictRisk(mgus_psl_fit$model, newdata = test_data, times = mgus_test_times),
        discrete_psl = riskRegression::predictRisk(mgus_psl_fit$model, newdata = test_data, model = "discrete_sl", times = mgus_test_times),
        glm = riskRegression::predictRisk(mgus_glm_fit$model, newdata = test_data, times = mgus_test_times),
        gam = riskRegression::predictRisk(mgus_gam_fit$model, newdata = test_data, times = mgus_test_times),
        xgb1 = riskRegression::predictRisk(mgus_xgb1_fit$model, newdata = test_data, times = mgus_test_times),
        xgb2 = riskRegression::predictRisk(mgus_xgb2_fit$model, newdata = test_data, times = mgus_test_times),
        xgb3 = riskRegression::predictRisk(mgus_xgb3_fit$model, newdata = test_data, times = mgus_test_times),
        hal2 = riskRegression::predictRisk(mgus_hal2_fit$model, newdata = test_data, times = mgus_test_times),
        hal3 = riskRegression::predictRisk(mgus_hal3_fit$model, newdata = test_data, times = mgus_test_times)
        ),
        formula = Surv(futime, death) ~ 1, data = test_data, cause = 1,
        metrics = "brier", summary = "ibs", times = mgus_test_times,
        conf.int = FALSE
      )
    }
  ),
  tar_target(
    mgus_maximum_time_score,
    {
      result <- data.table::as.data.table(mgus_score$Brier$score)
      result[times == max(times), .(
        dataset = "mgus2",
        model = as.character(model),
        maximum_time = times,
        integrated_brier_score = IBS
      )]
    }
  ),

  # rotterdam
  tar_target(
    rotterdam_analysis,
    {
      data <- data.table::as.data.table(survival::rotterdam)
      factor_names <- c("meno", "size", "grade", "hormon", "chemo")
      data[, (factor_names) := lapply(.SD, as.factor), .SDcols = factor_names]
      data <- data[complete.cases(data)]
      train <- which(data[["year"]] < 1992)
      list(data = data, train = train)
    }
  ),
  tar_target(rotterdam_train_times, seq(0, max(rotterdam_analysis$data[rotterdam_analysis$train][["dtime"]]), by = 365)),
  tar_target(rotterdam_test_times, seq(0, max(rotterdam_analysis$data[-rotterdam_analysis$train][["dtime"]]), by = 365)),
  tar_target(rotterdam_xgb1_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), cross_validation = TRUE, params = list(max_depth = 2, eta = 0.05, min_child_weight = 10, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(rotterdam_xgb2_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), cross_validation = TRUE, params = list(max_depth = 3, eta = 0.05, min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(rotterdam_xgb3_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), cross_validation = TRUE, params = list(max_depth = 5, eta = 0.03, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(rotterdam_glm_learner, poissonsuperlearner::Learner_glmnet(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), lambda = 0, cross_validation = FALSE)),
  tar_target(rotterdam_gam_learner, poissonsuperlearner::Learner_gam(covariates = c("s(age)", "meno", "size", "grade", "s(nodes)", "s(pgr)", "s(er)", "hormon", "chemo"))),
  tar_target(rotterdam_hal2_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), max_degree = 2L, num_knots = c(100L, 50L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(rotterdam_hal3_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo"), max_degree = 3L, num_knots = c(80L, 50L, 5L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(
    rotterdam_xgb1_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_xgb1_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "xgb1", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_xgb2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_xgb2_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "xgb2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_xgb3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_xgb3_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "xgb3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_glm_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_glm_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "glm", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_gam_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_gam_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "gam", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_hal2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_hal2_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "hal2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_hal3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learner = rotterdam_hal3_learner,
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "hal3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_psl_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::Superlearner(
        rotterdam_analysis$data[rotterdam_analysis$train],
        learners = list(rotterdam_xgb1_learner, rotterdam_xgb2_learner, rotterdam_xgb3_learner, rotterdam_glm_learner, rotterdam_gam_learner, rotterdam_hal2_learner, rotterdam_hal3_learner),
        id = "pid", status = "death", event_time = "dtime",
        nodes = rotterdam_train_times, nfold = 10,
        variable_transformation = list("age ~ floor(age)", "nodes ~ floor(nodes)", "pgr ~ floor(pgr)", "er ~ floor(er)"),
        verbose = FALSE
      ))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "psl", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_cox_fit,
    {
      tm <- system.time(model <- survival::coxph(survival::Surv(dtime, death) ~ age + meno + size + grade + nodes + pgr + er + hormon + chemo, data = rotterdam_analysis$data[rotterdam_analysis$train], x = TRUE))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "cox", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_rfsrc_fit,
    {
      tm <- system.time(model <- randomForestSRC::rfsrc(Surv(dtime, death) ~ age + meno + size + grade + nodes + pgr + er + hormon + chemo, data = rotterdam_analysis$data[rotterdam_analysis$train]))
      list(model = model, timing = data.table::data.table(dataset = "rotterdam", method = "rfsrc", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    rotterdam_score,
    {
      test_data <- rotterdam_analysis$data[-rotterdam_analysis$train]
      riskRegression::Score(
        list(
        cox_bmk = riskRegression::predictRisk(rotterdam_cox_fit$model, newdata = test_data, times = rotterdam_test_times),
        rfsrc_bmk = riskRegression::predictRisk(rotterdam_rfsrc_fit$model, newdata = test_data, times = rotterdam_test_times),
        psl = riskRegression::predictRisk(rotterdam_psl_fit$model, newdata = test_data, times = rotterdam_test_times),
        discrete_psl = riskRegression::predictRisk(rotterdam_psl_fit$model, newdata = test_data, model = "discrete_sl", times = rotterdam_test_times),
        glm = riskRegression::predictRisk(rotterdam_glm_fit$model, newdata = test_data, times = rotterdam_test_times),
        gam = riskRegression::predictRisk(rotterdam_gam_fit$model, newdata = test_data, times = rotterdam_test_times),
        xgb1 = riskRegression::predictRisk(rotterdam_xgb1_fit$model, newdata = test_data, times = rotterdam_test_times),
        xgb2 = riskRegression::predictRisk(rotterdam_xgb2_fit$model, newdata = test_data, times = rotterdam_test_times),
        xgb3 = riskRegression::predictRisk(rotterdam_xgb3_fit$model, newdata = test_data, times = rotterdam_test_times),
        hal2 = riskRegression::predictRisk(rotterdam_hal2_fit$model, newdata = test_data, times = rotterdam_test_times),
        hal3 = riskRegression::predictRisk(rotterdam_hal3_fit$model, newdata = test_data, times = rotterdam_test_times)
        ),
        formula = Surv(dtime, death) ~ 1, data = test_data, cause = 1,
        metrics = "brier", summary = "ibs", times = rotterdam_test_times,
        conf.int = FALSE
      )
    }
  ),
  tar_target(
    rotterdam_maximum_time_score,
    {
      result <- data.table::as.data.table(rotterdam_score$Brier$score)
      result[times == max(times), .(
        dataset = "rotterdam",
        model = as.character(model),
        maximum_time = times,
        integrated_brier_score = IBS
      )]
    }
  ),

  # pbc
  tar_target(
    pbc_analysis,
    {
      data <- data.table::as.data.table(survival::pbc)
      data <- data[, .SD, .SDcols = c("id", "time", "status", "age", "sex", "edema", "bili", "albumin")]
      data[, c("sex", "edema") := lapply(.SD, as.factor), .SDcols = c("sex", "edema")]
      data[, death := as.integer(status == 2L)]
      data <- data[complete.cases(data)]
      set.seed(42)
      train <- data[, sample(.I, size = floor(0.8 * .N))]
      list(data = data, train = train)
    }
  ),
  tar_target(pbc_train_times, seq(0, max(pbc_analysis$data[pbc_analysis$train][["time"]]), by = 365)),
  tar_target(pbc_test_times, seq(0, max(pbc_analysis$data[-pbc_analysis$train][["time"]]), by = 365)),
  tar_target(pbc_xgb1_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "edema", "bili", "albumin"), cross_validation = TRUE, params = list(max_depth = 2, eta = 0.05, min_child_weight = 10, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(pbc_xgb2_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "edema", "bili", "albumin"), cross_validation = TRUE, params = list(max_depth = 3, eta = 0.05, min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(pbc_xgb3_learner, poissonsuperlearner::Learner_xgboost(covariates = c("age", "sex", "edema", "bili", "albumin"), cross_validation = TRUE, params = list(max_depth = 5, eta = 0.03, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(pbc_glm_learner, poissonsuperlearner::Learner_glmnet(covariates = c("age", "sex", "edema", "bili", "albumin"), lambda = 0, cross_validation = FALSE)),
  tar_target(pbc_gam_learner, poissonsuperlearner::Learner_gam(covariates = c("s(age)", "sex", "edema", "s(bili)", "s(albumin)"))),
  tar_target(pbc_hal2_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "sex", "edema", "bili", "albumin"), max_degree = 2L, num_knots = c(100L, 50L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(pbc_hal3_learner, poissonsuperlearner::Learner_hal(covariates = c("age", "sex", "edema", "bili", "albumin"), max_degree = 3L, num_knots = c(80L, 50L, 5L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(
    pbc_xgb1_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_xgb1_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "xgb1", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_xgb2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_xgb2_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "xgb2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_xgb3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_xgb3_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "xgb3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_glm_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_glm_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "glm", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_gam_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_gam_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "gam", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_hal2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_hal2_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "hal2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_hal3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        pbc_analysis$data[pbc_analysis$train],
        learner = pbc_hal3_learner,
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "hal3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_psl_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::Superlearner(
        pbc_analysis$data[pbc_analysis$train],
        learners = list(pbc_xgb1_learner, pbc_xgb2_learner, pbc_xgb3_learner, pbc_glm_learner, pbc_gam_learner, pbc_hal2_learner, pbc_hal3_learner),
        id = "id", status = "death", event_time = "time",
        nodes = pbc_train_times, nfold = 10,
        variable_transformation = list("age ~ floor(age)", "bili ~ floor(bili)", "albumin ~ floor(albumin)"),
        verbose = FALSE
      ))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "psl", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_cox_fit,
    {
      tm <- system.time(model <- survival::coxph(survival::Surv(time, death) ~ age + sex + edema + bili + albumin, data = pbc_analysis$data[pbc_analysis$train], x = TRUE))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "cox", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_rfsrc_fit,
    {
      tm <- system.time(model <- randomForestSRC::rfsrc(Surv(time, death) ~ age + sex + edema + bili + albumin, data = pbc_analysis$data[pbc_analysis$train]))
      list(model = model, timing = data.table::data.table(dataset = "pbc", method = "rfsrc", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    pbc_score,
    {
      test_data <- pbc_analysis$data[-pbc_analysis$train]
      riskRegression::Score(
        list(
        cox_bmk = riskRegression::predictRisk(pbc_cox_fit$model, newdata = test_data, times = pbc_test_times),
        rfsrc_bmk = riskRegression::predictRisk(pbc_rfsrc_fit$model, newdata = test_data, times = pbc_test_times),
        psl = riskRegression::predictRisk(pbc_psl_fit$model, newdata = test_data, times = pbc_test_times),
        discrete_psl = riskRegression::predictRisk(pbc_psl_fit$model, newdata = test_data, model = "discrete_sl", times = pbc_test_times),
        glm = riskRegression::predictRisk(pbc_glm_fit$model, newdata = test_data, times = pbc_test_times),
        gam = riskRegression::predictRisk(pbc_gam_fit$model, newdata = test_data, times = pbc_test_times),
        xgb1 = riskRegression::predictRisk(pbc_xgb1_fit$model, newdata = test_data, times = pbc_test_times),
        xgb2 = riskRegression::predictRisk(pbc_xgb2_fit$model, newdata = test_data, times = pbc_test_times),
        xgb3 = riskRegression::predictRisk(pbc_xgb3_fit$model, newdata = test_data, times = pbc_test_times),
        hal2 = riskRegression::predictRisk(pbc_hal2_fit$model, newdata = test_data, times = pbc_test_times),
        hal3 = riskRegression::predictRisk(pbc_hal3_fit$model, newdata = test_data, times = pbc_test_times)
        ),
        formula = Surv(time, death) ~ 1, data = test_data, cause = 1,
        metrics = "brier", summary = "ibs", times = pbc_test_times,
        conf.int = FALSE
      )
    }
  ),
  tar_target(
    pbc_maximum_time_score,
    {
      result <- data.table::as.data.table(pbc_score$Brier$score)
      result[times == max(times), .(
        dataset = "pbc",
        model = as.character(model),
        maximum_time = times,
        integrated_brier_score = IBS
      )]
    }
  ),

  # metabric
  tar_target(
    metabric_data_file,
    {
      path <- "data/metabric_clinical_and_expression_data.csv"
      if (!file.exists(path)) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        utils::download.file(
          url = paste0(
            "https://bioinformatics-core-shared-training.github.io/",
            "Bitesize-R/data/metabric_clinical_and_expression_data.csv"
          ),
          destfile = path,
          mode = "wb"
        )
      }
      path
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    metabric_analysis,
    {
      data <- data.table::fread(metabric_data_file)
      excluded <- c("Tumour_size", "Tumour_stage", "Mutation_count",
                    "Neoplasm_histologic_grade", "3-gene_classifier",
                    "Survival_status", "Cellularity")
      data <- data[, .SD, .SDcols = setdiff(names(data), excluded)]
      factor_names <- c("Chemotherapy", "Radiotherapy", "Cancer_type",
                        "ER_status", "PR_status", "HER2_status",
                        "HER2_status_measured_by_SNP6", "PAM50",
                        "Integrative_cluster")
      data[, (factor_names) := lapply(.SD, as.factor), .SDcols = factor_names]
      data[, status := data.table::fcase(
        Vital_status == "Living", 0L,
        Vital_status == "Died of Disease", 1L,
        Vital_status == "Died of Other Causes", 2L,
        default = NA_integer_
      )]
      data <- data[complete.cases(data)]
      train <- which(data[["Cohort"]] < 4)
      list(data = data, train = train)
    }
  ),
  tar_target(
    metabric_covariates,
    setdiff(names(metabric_analysis$data), c("Patient_ID", "Survival_time", "Survival_status", "Vital_status", "status", "Cohort"))
  ),
  tar_target(
    metabric_factor_covariates,
    c("Chemotherapy", "Radiotherapy", "Cancer_type", "ER_status", "PR_status", "HER2_status", "HER2_status_measured_by_SNP6", "PAM50", "Integrative_cluster")
  ),
  tar_target(metabric_numeric_covariates, setdiff(metabric_covariates, metabric_factor_covariates)),
  tar_target(
    metabric_benchmark_formula,
    stats::as.formula(paste("prodlim::Hist(Survival_time, status) ~", paste(metabric_covariates, collapse = " + ")))
  ),
  tar_target(
    metabric_rfsrc_formula,
    stats::as.formula(paste("Surv(Survival_time, status) ~", paste(metabric_covariates, collapse = " + ")))
  ),
  tar_target(metabric_train_times, seq(0, max(metabric_analysis$data[metabric_analysis$train][["Survival_time"]]), by = 12)),
  tar_target(metabric_test_times, seq(0, max(metabric_analysis$data[-metabric_analysis$train][["Survival_time"]]), by = 12)),
  tar_target(metabric_xgb1_learner, poissonsuperlearner::Learner_xgboost(covariates = metabric_covariates, cross_validation = TRUE, params = list(max_depth = 2, eta = 0.05, min_child_weight = 10, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(metabric_xgb2_learner, poissonsuperlearner::Learner_xgboost(covariates = metabric_covariates, cross_validation = TRUE, params = list(max_depth = 3, eta = 0.05, min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(metabric_xgb3_learner, poissonsuperlearner::Learner_xgboost(covariates = metabric_covariates, cross_validation = TRUE, params = list(max_depth = 5, eta = 0.03, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8), nrounds = 500, verbose = 0)),
  tar_target(metabric_glm_learner, poissonsuperlearner::Learner_glmnet(covariates = metabric_covariates, lambda = 0, cross_validation = FALSE)),
  tar_target(metabric_gam_learner, poissonsuperlearner::Learner_gam(covariates = c(paste0("s(", metabric_numeric_covariates, ")"), metabric_factor_covariates))),
  tar_target(metabric_hal2_learner, poissonsuperlearner::Learner_hal(covariates = metabric_covariates, max_degree = 2L, num_knots = c(15L, 15L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(metabric_hal3_learner, poissonsuperlearner::Learner_hal(covariates = metabric_covariates, max_degree = 3L, num_knots = c(15L, 10L, 5L), nfolds = 5L, lambda.min.ratio = 0.01)),
  tar_target(
    metabric_xgb1_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_xgb1_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "xgb1", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_xgb2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_xgb2_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "xgb2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_xgb3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_xgb3_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "xgb3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_glm_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_glm_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "glm", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_gam_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_gam_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "gam", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_hal2_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_hal2_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "hal2", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_hal3_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::fit_learner(
        metabric_analysis$data[metabric_analysis$train],
        learner = metabric_hal3_learner,
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "hal3", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_psl_fit,
    {
      tm <- system.time(model <- poissonsuperlearner::Superlearner(
        metabric_analysis$data[metabric_analysis$train],
        learners = list(metabric_xgb1_learner, metabric_xgb2_learner, metabric_xgb3_learner, metabric_glm_learner, metabric_gam_learner, metabric_hal2_learner, metabric_hal3_learner),
        id = "Patient_ID", status = "status", event_time = "Survival_time",
        nodes = metabric_train_times, nfold = 10,
        variable_transformation = NULL,
        verbose = FALSE
      ))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "psl", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_csc_fit,
    {
      tm <- system.time(model <- riskRegression::CSC(metabric_benchmark_formula, data = metabric_analysis$data[metabric_analysis$train]))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "csc", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_rfsrc_fit,
    {
      tm <- system.time(model <- randomForestSRC::rfsrc(metabric_rfsrc_formula, data = metabric_analysis$data[metabric_analysis$train]))
      list(model = model, timing = data.table::data.table(dataset = "metabric", method = "rfsrc", user = unname(tm[["user.self"]]), system = unname(tm[["sys.self"]]), elapsed = unname(tm[["elapsed"]])))
    }
  ),
  tar_target(
    metabric_score,
    {
      test_data <- metabric_analysis$data[-metabric_analysis$train]
      riskRegression::Score(
        list(
        csc_bmk = riskRegression::predictRisk(metabric_csc_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        rfsrc_bmk = riskRegression::predictRisk(metabric_rfsrc_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        psl = riskRegression::predictRisk(metabric_psl_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        discrete_psl = riskRegression::predictRisk(metabric_psl_fit$model, newdata = test_data, model = "discrete_sl", times = metabric_test_times, cause = 1),
        glm = riskRegression::predictRisk(metabric_glm_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        gam = riskRegression::predictRisk(metabric_gam_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        xgb1 = riskRegression::predictRisk(metabric_xgb1_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        xgb2 = riskRegression::predictRisk(metabric_xgb2_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        xgb3 = riskRegression::predictRisk(metabric_xgb3_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        hal2 = riskRegression::predictRisk(metabric_hal2_fit$model, newdata = test_data, times = metabric_test_times, cause = 1),
        hal3 = riskRegression::predictRisk(metabric_hal3_fit$model, newdata = test_data, times = metabric_test_times, cause = 1)
        ),
        formula = prodlim::Hist(Survival_time, status) ~ 1, data = test_data, cause = 1,
        metrics = "brier", summary = "ibs", times = metabric_test_times,
        conf.int = FALSE
      )
    }
  ),
  tar_target(
    metabric_maximum_time_score,
    {
      result <- data.table::as.data.table(metabric_score$Brier$score)
      result[times == max(times), .(
        dataset = "metabric",
        model = as.character(model),
        maximum_time = times,
        integrated_brier_score = IBS
      )]
    }
  ),
  tar_target(
    computation_times,
    data.table::rbindlist(list(
      mgus_xgb1_fit$timing, mgus_xgb2_fit$timing, mgus_xgb3_fit$timing,
      mgus_glm_fit$timing, mgus_gam_fit$timing, mgus_hal2_fit$timing,
      mgus_hal3_fit$timing, mgus_psl_fit$timing, mgus_cox_fit$timing,
      mgus_rfsrc_fit$timing,
      rotterdam_xgb1_fit$timing, rotterdam_xgb2_fit$timing,
      rotterdam_xgb3_fit$timing, rotterdam_glm_fit$timing,
      rotterdam_gam_fit$timing, rotterdam_hal2_fit$timing,
      rotterdam_hal3_fit$timing, rotterdam_psl_fit$timing,
      rotterdam_cox_fit$timing, rotterdam_rfsrc_fit$timing,
      pbc_xgb1_fit$timing, pbc_xgb2_fit$timing, pbc_xgb3_fit$timing,
      pbc_glm_fit$timing, pbc_gam_fit$timing, pbc_hal2_fit$timing,
      pbc_hal3_fit$timing, pbc_psl_fit$timing, pbc_cox_fit$timing,
      pbc_rfsrc_fit$timing
    )),
    retrieval = "main",
    deployment = "main"
  ),
  tar_target(
    maximum_time_scores,
    data.table::rbindlist(list(
      mgus_maximum_time_score,
      rotterdam_maximum_time_score,
      pbc_maximum_time_score
    )),
    retrieval = "main",
    deployment = "main"
  ),
  tar_target(
    integrated_brier_scores_latex,
    {
      dataset_order <- c("mgus2", "rotterdam", "pbc")
      dataset_labels <- c(
        mgus2 = "MGUS2",
        rotterdam = "Rotterdam",
        pbc = "PBC"
      )
      model_order <- c(
        "cox_bmk", "rfsrc_bmk",
        "psl", "discrete_psl",
        "glm", "gam", "xgb1", "xgb2", "xgb3", "hal2", "hal3"
      )
      model_labels <- c(
        cox_bmk = "Cox proportional hazards",
        rfsrc_bmk = "Random survival forest",
        psl = "Poisson Super Learner",
        discrete_psl = "Discrete Poisson Super Learner",
        glm = "GLMnet",
        gam = "GAM",
        xgb1 = "XGBoost 1",
        xgb2 = "XGBoost 2",
        xgb3 = "XGBoost 3",
        hal2 = "HAL (two-way)",
        hal3 = "HAL (three-way)"
      )

      scores <- data.table::copy(maximum_time_scores)
      scores[, `:=`(
        model = as.character(model),
        dataset = as.character(dataset)
      )]
      wide <- data.table::dcast(
        scores,
        model ~ dataset,
        value.var = "integrated_brier_score"
      )
      for (dataset in dataset_order) {
        if (!dataset %in% names(wide)) {
          wide[, (dataset) := NA_real_]
        }
      }

      row_lines <- character(length(model_order))
      for (index in seq_along(model_order)) {
        model_id <- model_order[[index]]
        values <- vapply(dataset_order, function(dataset) {
          value <- wide[model == model_id][[dataset]]
          if (!length(value) || is.na(value[[1L]])) {
            "--"
          } else {
            formatC(value[[1L]], format = "f", digits = 3L)
          }
        }, character(1L))
        row_lines[[index]] <- paste0(
          paste(c(model_labels[[model_id]], values), collapse = " & "),
          " \\\\"
        )
      }
      row_lines <- append(row_lines, "\\hline", after = 4L)
      row_lines <- append(row_lines, "\\hline", after = 2L)

      table_lines <- c(
        "\\begin{table}[htbp]",
        "\\centering",
        "\\caption{Integrated Brier scores at the maximum evaluation time.}",
        "\\label{tab:integrated-brier-scores}",
        "\\begin{tabular}{lccc}",
        "\\hline",
        paste0(
          "Model & ",
          paste(dataset_labels[dataset_order], collapse = " & "),
          " \\\\"
        ),
        "\\hline",
        row_lines,
        "\\hline",
        "\\end{tabular}",
        "\\end{table}"
      )

      path <- "results/main_comparison_integrated_brier_scores.tex"
      writeLines(table_lines, path)
      cat(paste(table_lines, collapse = "\n"), "\n")
      path
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    computation_times_csv,
    {
      path <- "results/main_comparison_computation_times.csv"
      data.table::fwrite(computation_times, path)
      path
    },
    format = "file",
    deployment = "main"
  ),
  tar_target(
    maximum_time_scores_csv,
    {
      path <- "results/main_comparison_maximum_time_scores.csv"
      data.table::fwrite(maximum_time_scores, path)
      path
    },
    format = "file",
    deployment = "main"
  )
)

pipeline[!startsWith(
  vapply(pipeline, function(target) target$name, character(1)),
  "metabric_"
)]
