context("Test the get_pa() function")

load(here::here("R/sysdata.rda"))

gear <- tribble(
  ~gear,   ~gearname,
  1,     "Other",
  2,     "RoeSN",
  3,     "RoeGN")

pa <- get_pa(list(model),
             "Central Coast",
             gear)

test_that("Proportions-at-age data frame is structured correctly", {
  expect_true(all(c("year",
                    "area",
                    "group",
                    "sex",
                    "region",
                    "gear") %in% names(pa)))
  expect_equal(as.character(unique(pa$region)), "Central Coast")
  expect_equal(unique(pa$gear), c("Other", "RoeSN", "RoeGN"))
  expect_equal(class(pa$region), "factor")
  expect_error(get_pa(model,
                      "Central Coast",
                      gear))
  expect_error(get_pa(list(model),
                      c("A", "Central Coast"),
                      gear))
})
