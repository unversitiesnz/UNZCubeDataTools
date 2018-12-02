context("test-datacubereadagregate")

# define options
# Stakeholder Request 15: I think you should be able to select more than one option for ‘Sub Sector’, ‘Ethnicity’, ‘Level of Study’, and ‘Field of Study’.
optionSet.dataset1.dom = list(
  dom = TRUE,
  sex = 1,
  eth = c(1, 2),
  studyLevel = c(4, 6),
  subsector = c("University", "non-University"),
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas"
)
optionSet.dataset1.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas"
)
optionSet.dataset2.dom = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas"
)
optionSet.dataset2.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Overseas"
)

#test
test_that("The read function can support more than one option", {

})
check_that.selectExact <- function(result, run_info = "") {
  expect_false(anyNA(result), label = "there are NA selectors (overseas)", info = run_info)
  expect_true(any(result == TRUE), info = run_info)
  expect_type(result, "logical")
  expect_equal(length(result), nrow(datacube.overseas), info = run_info)


}
test_that("data selector - dataset1 - domestic", {
  optionSet = optionSet.dataset1.dom

  result <- getCube.dataset1.dom.select(datacube.overseas,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 146 * 2 * 2)
  optionSet <- list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Benefit"
  )
  result <- getCube.dataset1.dom.select(datacube.benefit,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
})
# possibly add a test for each different dom/cohort combo

test_that("data selector - summury - sex - domestic", {
  optionSet = optionSet.dataset1.dom = list(
    dom = TRUE,
    sex = 1,
    eth = c(1, 2),
    studyLevel = c(4, 6),
    subsector = c("University", "non-University"),
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )

  result <- getCube.dataset1.dom.select(datacube.overseas,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 146 * 2 * 2)
  optionSet <- list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Benefit"
  )
  result <- getCube.dataset1.dom.select(datacube.benefit,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
})


test_that("data selector - all (subtotals) - domestic", {
  optionSet = optionSet.dataset1.dom = list(
    dom = TRUE,
    sex = NA,
    eth = 1,
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )

  result <- getCube.dataset1.dom.select(datacube.overseas,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 2)
  optionSet <- list(
    dom = TRUE,
    sex = 1,
    eth = NA,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Benefit"
  )
  result <- getCube.dataset1.dom.select(datacube.benefit,optionSet)
  expect_false(anyNA(result), label = "there are NA selectors (benefit)")
  expect_true(any(result == TRUE))
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

  optionSet = optionSet.dataset1.dom = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = NA,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )

  result <- getCube.dataset1.dom.select(datacube.overseas,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 7)

  optionSet = optionSet.dataset1.dom = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 4,
    subsector = NA,
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )

  result <- getCube.dataset1.dom.select(datacube.overseas,optionSet)
  check_that.selectExact(result)
  expect_equal(sum(result, na.rm = TRUE), 73 * 2)
})

test_that("All and other options play nice", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = c(NA),
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )
  result <- getCube.dataset1.dom.select(datacube.overseas, optionSet)
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = c(NA, 1),
    studyLevel = 4,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )
  result <- getCube.dataset1.dom.select(datacube.overseas, optionSet)
  expect_equal(sum(result, na.rm = TRUE), 73 * 5)

})
