context("test-multiIndicator")

test_that("checking if the multi indicator stuff works", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )

  result <- getCube.filterAndAggregateByOptions.v2(optionSet)

  expect_equal(nrow(result$data), 73)
})
