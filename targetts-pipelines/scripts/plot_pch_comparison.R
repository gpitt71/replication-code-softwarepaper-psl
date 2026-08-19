library(targets)
library(ggplot2)
library(data.table)

# -------------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------------

project_dir <- "C:/Users/pwt887/Documents/github/replication-code-softwarepaper-psl"

# This should be the same store used in tar_make().
store <- file.path(
  project_dir,
  "targetts-pipelines",
  "_targets_pch_comparison"
)

figure_dir <- file.path(project_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)


# -------------------------------------------------------------------------
# Read benchmark output
# -------------------------------------------------------------------------

timings <- targets::tar_read(
  timings)

setDT(timings)

# -------------------------------------------------------------------------
# Labels and ordering
# -------------------------------------------------------------------------

timings[
  ,
  method_label := factor(
    method,
    levels = c(
      "poissonsuperlearner",
      "pammtools",
      "Epi"
    ),
    labels = c(
      "PSL GLM",
      "pammtools GLM",
      "Epi GLM"
    )
  )
]

timings[
  ,
  sample_size_label := factor(
    sample_size,
    levels = sort(unique(sample_size)),
    labels = paste0("n = ", sort(unique(sample_size)))
  )
]


# -------------------------------------------------------------------------
# Boxplot
# -------------------------------------------------------------------------

p <- ggplot(
  timings,
  aes(
    x = method_label,
    y = elapsed
  )
) +
  geom_boxplot() +
  facet_wrap(
    ~ sample_size_label,
    ncol = 3
  ) +
  labs(
    x = NULL,
    y = "Computation time (seconds)"
  )

print(p)


# -------------------------------------------------------------------------
# Save figure
# -------------------------------------------------------------------------

ggsave(
  filename = file.path(
    figure_dir,
    "pch_computation_time_boxplot.pdf"
  ),
  plot = p,
  width = 12,
  height = 7
)

ggsave(
  filename = file.path(
    figure_dir,
    "pch_computation_time_boxplot.png"
  ),
  plot = p,
  width = 12,
  height = 7,
  dpi = 300
)
