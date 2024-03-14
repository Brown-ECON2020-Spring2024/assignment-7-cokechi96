library(testdat)
library(testthat)
library(data.table)
library(estimatr)

## Import test-relevant data
data <- fread("../output/data/gapminder.csv")
model <- readRDS("../output/models/linear_reg.rds")

#source("../code/run_linear_reg.r")
## Add unit tests as specified by question

test_that("estimated coefficient for gdpPercap is reasonable", {
    expect_equal(model$coefficients[[2]], 0.00076, tolerance = 0.001)
})

test_that("number of observations equal 1704", {
    expect_equal(nobs(model), 1703)
})

test_that("year and country create a unique id", {
    expect_unique(data = data, c(year, country))
})

test_that("gdpPercap observations are between 0 and 1e6", {
    expect_range(data = data, gdpPercap, 0, 1e6)
})

test_that("the continent encompasses the 5 main continents", {
    expect_values(data = data, continent, c("Asia", "Europe", 
     "Africa", "Americas", "Oceania"))
})
