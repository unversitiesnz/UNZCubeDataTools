context("test-aboutdata")

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

optionSet.ag = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = c(4,6),
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas"
)

optionSet.su = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 4,
  subsector = "non-University",
  fieldOfStudy = 1,
  cohort = NA,
  indicator = "Overseas"
)

optionSet.ch = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 4,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Employer change"
)
paste('test_that("about data", {
  data <- getCube.filteredByOptions(optionSet)
  about <- checkCube.about(data, optionSet)
  expect_false(about$suppression)
  expect_false(about$aggregation)

  data <- getCube.filteredByOptions(optionSet.ag)
  about <- checkCube.about(data, optionSet.ag)
  expect_false(about$suppression)
  expect_true(about$aggregation)

  data <- getCube.filteredByOptions(optionSet.su)
  about <- checkCube.about(data, optionSet.su)
  expect_true(about$suppression)
  expect_false(about$aggregation)
})

test_that("region change about data", {
  data <- getCube.filteredByOptions(optionSet.ch)
  about <- checkCube.about(data, optionSet.ch)
  expect_false(about$suppression)
  expect_false(about$aggregation)
})


########## Handle missing data ############

test_that("About - check for data missing", {

  optionSet <- list(
    indicator = "Overseas",
    cohort = NA,
    dom = TRUE,
    sex = 2,
    ethnicity = 2,
    subsector = "University",
    studyLevel = 2,
    fieldOfStudy = 8
  )

  data <- getCube.filteredByOptions(optionSet)
  about <- checkCube.about(data, optionSet)
  expect_false(about$hasData)
})
')
