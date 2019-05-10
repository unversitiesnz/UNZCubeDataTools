context("test-agrByMonthAndCohort")

test_that("The filter function does not break", {
  optionSet = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010, 2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE,
    dimCohort = TRUE
  )
  filterFunctionTest <- getCube.selector(optionSet)
  indexArray <- filterFunctionTest(datacube.v2, optionSet)
  datacube.v2[indexArray, ]
  filteredData <- getCube.filteredByOptions.v2(optionSet)
})

test_that("Get data which is aggregated by month and cohort", {
  optionSet = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010, 2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE,
    dimCohort = TRUE
  )
  filteredData <- getCube.filteredByOptions.v2(optionSet)
  expect_type(filteredData, "list")

  expect_equal(filteredData[1,]$cohort, 2009)

  aggregatedData <- getCube.aggregate.v2(filteredData, optionSet)
  expect_equal(nrow(aggregatedData), 73 * 3)
  optionSet$indicator <- 'Earnings from wages or salary (mean)'
  filteredData <- getCube.filteredByOptions.v2(optionSet)
  expect_type(filteredData, "list")

  expect_equal(filteredData[1,]$cohort, 2009)

  aggregatedData <- getCube.aggregate.v2(filteredData, optionSet)
  expect_equal(nrow(aggregatedData), 73 * 3)

  #about <- checkCube.about(filteredData, optionSet)
  #expect_true(about$multiCohort == TRUE)
})

test_that("The primary function works", {
  optionSet = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010, 2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE,
    dimCohort = TRUE
  )
  result <- getCube.filterAndAggregateByOptions.v2(optionSet)
  data <- result$data
  expect_type(data, "list")

  expect_equal(data[1,]$cohort, 2009)
  expect_equal(nrow(data), 73 * 3)
})

test_that("The primary function works", {
  optionSet = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2011),
    indicator = "Overseas",
    multiCohort = TRUE,
    young_grad = TRUE,
    dimCohort = TRUE
  )
  result <- getCube.filterAndAggregateByOptions.v2(optionSet)
  data <- result$data
  expect_type(data, "list")

  expect_equal(data[1,]$cohort, 2011)
  expect_equal(nrow(data), 73)
})
