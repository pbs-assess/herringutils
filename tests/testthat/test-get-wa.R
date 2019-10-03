library(tibble)
library(gfiscamutils)
library(herringutils)

context("Test the get_wa() function")

load(here::here("R/sysdata.rda"))

gear <- tribble(
  ~gear,   ~gearname,
  1,     "Other",
  2,     "RoeSN",
  3,     "RoeGN")

wa <- get_wa(list(model),
             "Central Coast",
             gear)

test_that("Weight-at-age data frame is structured correctly", {
  expect_true(all(c("year",
                    "area",
                    "group",
                    "sex",
                    "region",
                    "gear") %in% names(wa)))
  expect_equal(as.character(unique(wa$region)), "Central Coast")
  expect_equal(unique(wa$gear), "Other")
  expect_equal(class(wa$region), "factor")
  expect_error(get_wa(model,
                      "Central Coast",
                      gear))
  expect_error(get_wa(list(model),
                      c("A", "Central Coast"),
                      gear))
})
