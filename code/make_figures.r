library(ggplot2)
library(data.table)

data <- fread("./output/data/gapminder.csv")

rand_countries <- sample(unique(data$country), 10)

## Plot
p1 <- data[country %in% rand_countries] |>
  ggplot(aes(
     x = year,
     y = country,
     fill = lifeExp
  )) +
  geom_tile() +
  scale_fill_viridis_c()

ggsave("./output/figures/ten_country_plot.pdf", p1, width = 4, height = 6, units = "in")
