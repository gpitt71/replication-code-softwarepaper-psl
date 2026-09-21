library(poissonsuperlearner)
library(riskRegression)
library(survival)
library(prodlim)
library(pammtools)
library(Epi)


# Section 2.1 - Illustration

dtrain <- simulateStenoT1(5000,
                          seed = 1,
                          scenario = "beta",
                          competing_risks = TRUE)

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

l_glm <- Learner_glmnet(
  covariates = xnames,
  lambda = 0,
  cross_validation = FALSE
)

bl <- fit_learner(
  data = dtrain,
  learner = l_glm,
  status = "status_cvd",
  event_time = "time_cvd",
  nodes = seq(1, 20)
)

bl$data_info

length(bl$learner_fit)

dtest <- simulateStenoT1(100,
                         seed = 2,
                         scenario = "beta",
                         competing_risks = TRUE)

bl_pred <- predict(bl,
                   times = c(5, 10),
                   newdata = dtest)

bl_pred[, .SD, .SDcols = c(
  "pwch_1",
  "pwch_2",
  "survival_function",
  "absolute_risk"
)]

predictRisk(bl,
            times = c(5, 10),
            newdata = dtest)[1:4, ]

l_glmnet <- Learner_glmnet(
  covariates = xnames,
  alpha = 0,
  cross_validation = TRUE
)

xnames_gam <- c(
  "sex",
  "s(age)",
  "s(value_SBP, k=3)",
  "s(eGFR, k=2)"
)

l_gam <- Learner_gam(
  covariates = xnames_gam
)

l_intercept_glmnet <- Learner_glmnet(
  covariates = NULL,
  lambda = 0,
  cross_validation = FALSE
)

l_intercept_gam <- Learner_gam(
  covariates = NULL
)

l_hal <- Learner_hal(
  covariates = xnames,
  cross_validation = TRUE,
  # hal parameters
  max_degree = 2L,
  num_knots = c(50, 30),
  # glmnet parameters
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

l_xgb <- Learner_xgboost(
  covariates = xnames,
  cross_validation = TRUE,
  params = list(
    max_depth = 3,
    eta = 0.05,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)


# Section 3.3 - Illustration

l_lasso <- Learner_glmnet(
  covariates = xnames,
  alpha = 0,
  cross_validation = TRUE
)

l_gam <- Learner_gam(
  covariates = c(
    "sex",
    "s(age)",
    "s(diabetes_duration)",
    "value_SBP",
    "value_LDL",
    "value_HBA1C",
    "value_Smoking",
    "value_Motion",
    "value_Albuminuria",
    "s(eGFR)"
  )
)

cause1_library = list(
  lasso = l_lasso,
  gam = l_gam,
  xgb = l_xgb
)

l_glm <- Learner_glmnet(
  covariates = xnames,
  lambda = 0,
  cross_validation = FALSE
)

cause2_library <- list(l_glm, l_lasso)

psl_fit <- Superlearner(
  data = dtrain,
  id = "id",
  status = "status_cvd",
  event_time = "time_cvd",
  learners = list(cause1_library, cause2_library),
  nodes = seq(1, 20, by = 5),
  nfold = 5,
  verbose = TRUE
)

psl_fit$cross_validation_deviance

names(psl_fit$superlearner$cause_1)

summary(psl_fit)

sl_pred <- predict(psl_fit, times = c(5, 10), newdata = dtest)

sl_pred[, .SD, .SDcols = c(
  "id",
  "time_cvd",
  "pwch_1",
  "pwch_2",
  "survival_function",
  "absolute_risk"
)]

predictRisk(psl_fit,
            times = c(5, 10),
            model = "discrete_sl",
            newdata = dtest)[1:4, ]

csc_bmk <- CSC(
  Hist(time_cvd, status_cvd) ~ sex + age + diabetes_duration + value_SBP +
    value_LDL + value_HBA1C + value_Smoking + value_Motion + value_Albuminuria +
    eGFR,
  data = dtrain
)

score_metrics = Score(
  list(csc_bmk = csc_bmk, glm = bl, psl = psl_fit),
  times = c(5, 10),
  data = dtest,
  formula = Hist(time_cvd, status_cvd) ~ 1,
  summary = "Brier"
)

score_metrics$Brier

learners <- list(
  glm0 = Learner_glmnet(
    covariates = xnames,
    lambda = 0,
    cross_validation = FALSE
  ),
  glm1 = Learner_glmnet(
    covariates = c("sex", "age"),
    lambda = 0,
    cross_validation = FALSE
  ),
  glm2 = Learner_glmnet(
    covariates = c("value_Albuminuria", "eGFR"),
    lambda = 0,
    cross_validation = FALSE
  )
)

fit <- Superlearner(
  data = dtrain,
  id = "id",
  status = "status_cvd",
  event_time = "time_cvd",
  learners = learners,
  number_of_nodes = 5,
  nfold = 5,
  variable_transformation = list("age ~ floor(age)")
)

summary(fit)


# Appendix A - Usage compared to other packages for super learning

event.SL.library <- cens.SL.library <- c(
  "survSL.km",
  "survSL.coxph",
  "survSL.expreg",
  "survSL.gam"
)

fit <- survSuperLearner(
  time = dtrain[, time_cvd],
  event = dtrain[, status_cvd],
  X = dtrain[, .SD, .SDcols = xnames],
  newX = dtest[, .SD, .SDcols = xnames],
  new.times = seq(0, 20, .5),
  event.SL.library = event.SL.library,
  cens.SL.library = cens.SL.library,
  verbose = FALSE
)


# Appendix B - Usage of the predict method

predictRisk(
  psl_fit,
  times = c(5, 10),
  model = c("lasso", "learner_1"),
  newdata = dtest
)[1:4, ]

predictRisk(
  psl_fit,
  times = c(5, 10),
  model = c(3, 1),
  newdata = dtest
)[1:4, ]


# Appendix C.1 - Code for the comparison with competing software

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

dtrain <- poissonsuperlearner::simulateStenoT1(
  1000,
  seed = 1,
  scenario = "alpha",
  competing_risks = TRUE
)

system.time({
  l_glm <- Learner_glmnet(
    covariates = xnames,
    lambda = 0,
    cross_validation = FALSE
  )

  bl <- fit_learner(
    dtrain,
    learner = l_glm,
    status = "status_cvd",
    event_time = "time_cvd",
    nodes = nodes
  )
})

system.time({
  form_surv <- as.formula(
    paste("Surv(time_cvd, status_cvd) ~", paste(xnames, collapse = " + "))
  )

  ped <- as_ped(
    data = dtrain,
    formula = form_surv,
    cut = nodes,
    id = "id"
  )

  form_pamm <- as.formula(
    paste("ped_status ~ interval +", paste(xnames, collapse = " + "))
  )

  fit_pamm <- glm(
    form_pamm,
    data = ped,
    family = poisson(),
    offset = offset
  )
})

system.time({
  dtrain_epi <- as.data.frame(dtrain)

  states <- c("Alive", "CVD", "Death")

  dtrain_epi$exit_state <- factor(ifelse(
    dtrain_epi$status_cvd == 1,
    "CVD",
    ifelse(dtrain_epi$status_cvd == 2, "Death", "Alive")
  ), levels = states)

  L <- Lexis(
    entry = list(fu = rep(0, nrow(dtrain_epi))),
    exit = list(fu = dtrain_epi$time_cvd),
    entry.status = factor(rep("Alive", nrow(dtrain_epi)), levels = states),
    exit.status = dtrain_epi$exit_state,
    id = dtrain_epi$id,
    data = dtrain_epi
  )

  Ls <- splitLexis(L, breaks = nodes, time.scale = "fu")

  Ls$interval <- factor(Ls$fu)

  form_epi <- as.formula(
    paste("~ interval +", paste(xnames, collapse = " + "))
  )

  fit_epi_1 <- glm.Lexis(
    Ls,
    formula = form_epi,
    from = "Alive",
    to = "CVD",
    verbose = FALSE
  )

  fit_epi_2 <- glm.Lexis(
    Ls,
    formula = form_epi,
    from = "Alive",
    to = "Death",
    verbose = FALSE
  )
})


# Appendix C.2 - Learner specifications for the predictive comparison

# MGUS2

xgb1 <- Learner_xgboost(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  cross_validation = TRUE,
  params = list(
    max_depth = 2,
    eta = 0.05,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb2 <- Learner_xgboost(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  cross_validation = TRUE,
  params = list(
    max_depth = 3,
    eta = 0.05,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb3 <- Learner_xgboost(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  cross_validation = TRUE,
  params = list(
    max_depth = 5,
    eta = 0.03,
    min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

glm <- Learner_glmnet(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  lambda = 0,
  cross_validation = FALSE
)

gam <- Learner_gam(
  covariates = c(
    "s(age)", "sex", "s(dxyr)", "s(hgb)",
    "s(creat)", "s(mspike)"
  )
)

hal2 <- Learner_hal(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  max_degree = 2L,
  num_knots = c(100L, 50L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

hal3 <- Learner_hal(
  covariates = c("age", "sex", "dxyr", "hgb", "creat", "mspike"),
  max_degree = 3L,
  num_knots = c(80L, 50L, 5L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

variable_transformation = list(
  "age ~ floor(age)",
  "dxyr ~ floor(dxyr)",
  "mspike ~ floor(mspike)",
  "hgb ~ floor(hgb)",
  "creat ~ floor(creat)"
)

# Rotterdam

xgb1 <- Learner_xgboost(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  cross_validation = TRUE,
  params = list(
    max_depth = 2,
    eta = 0.05,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb2 <- Learner_xgboost(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  cross_validation = TRUE,
  params = list(
    max_depth = 3,
    eta = 0.05,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb3 <- Learner_xgboost(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  cross_validation = TRUE,
  params = list(
    max_depth = 5,
    eta = 0.03,
    min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

glm <- Learner_glmnet(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  lambda = 0,
  cross_validation = FALSE
)

gam <- Learner_gam(
  covariates = c(
    "s(age)", "meno", "size", "grade", "s(nodes)",
    "s(pgr)", "s(er)", "hormon", "chemo"
  )
)

hal2 <- Learner_hal(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  max_degree = 2L,
  num_knots = c(100L, 50L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

hal3 <- Learner_hal(
  covariates = c(
    "age", "meno", "size", "grade", "nodes",
    "pgr", "er", "hormon", "chemo"
  ),
  max_degree = 3L,
  num_knots = c(80L, 50L, 5L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

variable_transformation = list(
  "age ~ floor(age)",
  "nodes ~ floor(nodes)",
  "pgr ~ floor(pgr)",
  "er ~ floor(er)"
)

# PBC

xgb1 <- Learner_xgboost(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  cross_validation = TRUE,
  params = list(
    max_depth = 2,
    eta = 0.05,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb2 <- Learner_xgboost(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  cross_validation = TRUE,
  params = list(
    max_depth = 3,
    eta = 0.05,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

xgb3 <- Learner_xgboost(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  cross_validation = TRUE,
  params = list(
    max_depth = 5,
    eta = 0.03,
    min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  nrounds = 500,
  verbose = 0
)

glm <- Learner_glmnet(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  lambda = 0,
  cross_validation = FALSE
)

gam <- Learner_gam(
  covariates = c(
    "s(age)", "sex", "edema", "s(bili)", "s(albumin)"
  )
)

hal2 <- Learner_hal(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  max_degree = 2L,
  num_knots = c(100L, 50L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

hal3 <- Learner_hal(
  covariates = c("age", "sex", "edema", "bili", "albumin"),
  max_degree = 3L,
  num_knots = c(80L, 50L, 5L),
  nfolds = 5L,
  lambda.min.ratio = 0.01
)

variable_transformation = list(
  "age ~ floor(age)",
  "bili ~ floor(bili)",
  "albumin ~ floor(albumin)"
)
