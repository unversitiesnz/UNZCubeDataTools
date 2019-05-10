context("test-datacubereadagregate")

# define options
# Stakeholder Request 15: I think you should be able to select more than one option for ‘Sub Sector’, ‘Ethnicity’, ‘Level of Study’, and ‘Field of Study’.
optionSet.dataset1.dom = list(
  dom = 1,
  sex = 1,
  eth = c(1, 2),
  studyLevel = c(4, 6),
  subsector = c("University"),
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas",
  young_grad = 1
)
optionSet.dataset1.int = list(
  dom = 0,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas",
  young_grad = 1
)
optionSet.dataset2.dom = list(
  dom = 1,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas",
  young_grad = 1
)
optionSet.dataset2.int = list(
  dom = 0,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas",
  young_grad = 1
)

#test
test_that("The read function can support more than one option", {

})
check_that.selectExact <- function(result, run_info = "") {
  expect_false(anyNA(result), label = "there are NA selectors (overseas)", info = run_info)
  expect_true(any(result == TRUE), info = run_info)
  expect_type(result, "logical")
  expect_equal(length(result), nrow(datacube.v2), info = run_info)


}
test_that("data selector - dataset1 - domestic", {
  optionSet = optionSet.dataset1.dom
  # testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 2 * 2)
  optionSet <- list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Benefit",
    young_grad = 1
  )
  # testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
})
# possibly add a test for each different dom/cohort combo

test_that("data selector - summury - sex - domestic", {
  optionSet = optionSet.dataset1.dom = list(
    dom = 1,
    sex = 1,
    eth = c(1, 2),
    studyLevel = c(4, 6),
    subsector = c("University", "non-University"),
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )
  # testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 438)
  optionSet <- list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "On a benefit",
    young_grad = 1
  )
  # testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
})


test_that("data selector - all (subtotals) - domestic", {
  optionSet = optionSet.dataset1.dom = list(
    dom = 1,
    sex = -1,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )

  #testData <- getCube.forIndicator(optionSet)
  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 2)
  optionSet <- list(
    dom = 1,
    sex = 1,
    eth = -1,
    studyLevel = 3,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "On a benefit",
    young_grad = 1
  )

  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

  optionSet = optionSet.dataset1.dom = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = -1,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )

  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 6)

  optionSet = optionSet.dataset1.dom = list(
    dom = 1,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = -1,
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )

  result <- getCube.dataset1.dom.select(datacube.v2,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 2)
})

test_that("All and other options play nice", {
  optionSet = list(
    dom = 1,
    sex = 1,
    eth = c(-1),
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )

  result <- getCube.dataset1.dom.select(datacube.v2, optionSet)
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

  optionSet = list(
    dom = 1,
    sex = 1,
    eth = c(-1, 1),
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas",
    young_grad = 1
  )

  result <- getCube.dataset1.dom.select(datacube.v2, optionSet)
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

})
