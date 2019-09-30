context("Test the get_catch() function")

load(here::here("R/sysdata.rda"))

gear <- tribble(
  ~gear,   ~gearname,
  1,     "Other",
  2,     "RoeSN",
  3,     "RoeGN")

ct <- get_catch(list(model),
                "Central Coast",
                gear)

test_that("Catch data frame is structured correctly", {
  expect_equal(names(ct), c("year",
                            "area",
                            "group",
                            "sex",
                            "type",
                            "value",
                            "region",
                            "gear"))
  expect_equal(as.character(unique(ct$region)), "Central Coast")
  expect_equal(unique(ct$gear), c("Other", "RoeSN", "RoeGN"))
  expect_equal(class(ct$region), "factor")
  expect_error(get_catch(model,
                         "Central Coast",
                         gear))
  expect_error(get_catch(list(model),
                         c("A", "Central Coast"),
                         gear))
})
