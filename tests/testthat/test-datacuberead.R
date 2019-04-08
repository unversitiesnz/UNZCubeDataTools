context("test-datacuberead")
###### test objects  ######
optionSet.dataset1.dom = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas",
  young_grad = TRUE
)
optionSet.dataset1.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas",
  young_grad = TRUE
)
optionSet.dataset2.dom = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas",
  young_grad = TRUE
)
optionSet.dataset2.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas",
  young_grad = TRUE
)
###### data rows select #######

check_that.selectExact <- function(result, run_info = "") {
  expect_false(anyNA(result), label = "there are NA selectors (overseas)", info = run_info)
  expect_true(any(result == TRUE), info = run_info)
  expect_type(result, "logical")
  expect_equal(length(result), nrow(datacube.v2), info = run_info)
  expect_equal(sum(result, na.rm = TRUE), 73, info = run_info)

}

test_that("data selector - dataset1 - domestic", {
  optionSet = optionSet.dataset1.dom

  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)

  optionSet <- list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "On a benefit",
    young_grad = TRUE
  )
  testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(testData,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
})
test_that("data selector - dataset1 - international", {
  optionSet <- optionSet.dataset1.int
  result <- getCube.dataset1.int.select(datacube.v2,optionSet)
  check_that.selectExact(result)
})
test_that("data selector - dataset2 - domestic", {
  optionSet <- optionSet.dataset2.dom
  result <- getCube.dataset2.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
})

test_that("data selector - dataset2 - international", {
  optionSet <- optionSet.dataset2.int
  result <- getCube.dataset2.int.select(datacube.v2,optionSet)
  check_that.selectExact(result)
})

########### Choose data selection function ########

test_that("get data filter function", {
  optionSet = optionSet.dataset1.dom
  selectFunction <- getCube.selector(optionSet)
  expect_type(selectFunction, "closure")
  # don't know how to check what function is returned, test by using it?

  result <- selectFunction(datacube.v2,optionSet)
  check_that.selectExact(result, "optionSet dataset1 dom")
  # option 2
  optionSet = optionSet.dataset2.dom
  selectFunction <- getCube.selector(optionSet)
  expect_type(selectFunction, "closure")
  # don't know how to check what function is returned, test by using it?

  result <- selectFunction(datacube.v2,optionSet)
  check_that.selectExact(result, "optionSet dataset2 dom")
  #otpion 3
  optionSet = optionSet.dataset1.int
  selectFunction <- getCube.selector(optionSet)
  expect_type(selectFunction, "closure")
  # don't know how to check what function is returned, test by using it?
  testData <- getCube.forIndicator(optionSet)
  result <- selectFunction(testData,optionSet)
  check_that.selectExact(result, "optionSet dataset1 int")

  #otpion 4
  optionSet = optionSet.dataset2.int
  selectFunction <- getCube.selector(optionSet)
  expect_type(selectFunction, "closure")
  # don't know how to check what function is returned, test by using it?

  result <- selectFunction(datacube.v2,optionSet)
  check_that.selectExact(result, "optionSet dataset2 int")
})

###### choose dataset #####

test_that("I can choose the right dataset", {
  optionSet = optionSet.dataset1.dom
  resultData <- getCube.forIndicator(optionSet)
  expect_type(resultData, "list")
  expect_equal(ncol(resultData),12)
})

###### get filtered data ######

test_that("I get the right data", {
  optionSet = optionSet.dataset1.dom
  result <- getCube.filteredByOptions(optionSet)
  expect_equal(nrow(result), 73)
})

#### test income ind filter #####

test_that("data selector - dataset1 - domestic (income)", {

  optionSet <- list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Earnings from wages or salary (mean)",
    young_grad = TRUE
  )

  testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(testData,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
  data_result <- getCube.forIndicator(optionSet)
  expect_equal(nrow(data_result), 253076)

  result2 <- getCube.filteredByOptions(optionSet)
  expect_equal(nrow(result2), 73)
  result3 <- getCube.filterAndAggregateByOptions(optionSet)$data
  expect_equal(nrow(result3), 73) # I expect income missing due to suppression is to blame, can live with, should sort out.
})

########## Handle missing data ############

test_that("data selector - dataset1 - domestic (income)", {

  optionSet <- list(
    indicator = "Overseas",
    cohort = NA,
    dom = TRUE,
    sex = 2,
    ethnicity = 2,
    subsector = "University",
    studyLevel = 1,
    fieldOfStudy = 8,
    young_grad = TRUE
  )


  result2 <- getCube.filteredByOptions(optionSet)
  # expect_equal(nrow(result2), 0)
  expect_true(anyNA(result2$num))


  result3 <- getCube.filterAndAggregateByOptions(optionSet)$data
  #expect_equal(nrow(result3), 0)

  # data no longer missing, would need a better way to test
})
