context("test-agrByMonthAndCohort")

test_that("Get data which is aggregated by month and cohort", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010, 2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE
  )
  filteredData <- getCube.filteredByOptions(optionSet)
  expect_type(filteredData, "list")

  expect_equal(filteredData[1,]$cohort, 2009)

  aggregatedData <- getCube.aggregate.cohort(filteredData, optionSet)
  expect_equal(nrow(aggregatedData), 73 * 3)
  optionSet$indicator <- 'Earnings from wages or salary (mean)'
  filteredData <- getCube.filteredByOptions(optionSet)
  expect_type(filteredData, "list")

  expect_equal(filteredData[1,]$cohort, 2009)

  aggregatedData <- getCube.aggregate.cohort(filteredData, optionSet)
  expect_equal(nrow(aggregatedData), 73 * 3)

  about <- checkCube.about(filteredData, optionSet)
  expect_true(about$multiCohort == TRUE)
})

test_that("The primary function works", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010, 2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE
  )
  result <- getCube.filterAndAggregateByOptions(optionSet)
  data <- result$data
  expect_type(data, "list")

  expect_equal(data[1,]$cohort, 2009)
  expect_equal(nrow(data), 73 * 3)
})

test_that("The primary function works", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE
  )
  result <- getCube.filterAndAggregateByOptions(optionSet)
  data <- result$data
  expect_type(data, "list")

  expect_equal(data[1,]$cohort, 2011)
  expect_equal(nrow(data), 73)
})
