library(modelsummary)

model <- readRDS("./output/models/linear_reg.rds")

model |>
  modelsummary(
    output = "output/tables/linear_reg_table.md",
    gof_map = c("nobs", "r.squared")
  )
