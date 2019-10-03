library(tibble)
library(gfiscamutils)
library(herringutils)
context("Test the get_surv_ind() function")

load(here::here("R/sysdata.rda"))

surv_type <- tribble(
  ~gear,   ~gearname, ~qind,
  4,   "Surface",     1,
  5,      "Dive",     2)

st <- get_surv_ind(list(model),
                   "Central Coast",
                   surv_type)

test_that("Survey index data frame is structured correctly", {
  expect_true(all(names(st) == c("year",
                                 "value",
                                 "area",
                                 "group",
                                 "sex",
                                 "wt",
                                 "timing",
                                 "region",
                                 "gear",
                                 "qind")))
  expect_equal(as.character(unique(st$region)), "Central Coast")
  expect_equal(unique(st$gear), c("Surface", "Dive"))
  expect_equal(class(st$region), "factor")
  expect_error(get_surv_ind(model,
                            "Central Coast",
                            surv_type))
  expect_error(get_surv_ind(list(model),
                            c("A", "Central Coast"),
                            surv_type))
})
