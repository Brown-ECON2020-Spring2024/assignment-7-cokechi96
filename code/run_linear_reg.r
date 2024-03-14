library(data.table)
library(estimatr)

data <- fread("./output/data/gapminder.csv")

model <- lm_robust(lifeExp ~ gdpPercap + pop, data = data)

saveRDS(model, "./output/models/linear_reg.rds")
