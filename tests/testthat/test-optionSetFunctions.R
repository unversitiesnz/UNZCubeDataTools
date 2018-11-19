context("test-optionSetFunctions")

optionSet.dataset1.dom = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Overseas"
)

test_that("Get a printable string for the dataset", {
  result <- optionSetToString(optionSet.dataset1.dom)
  #print(result)
  expect_type(result, "character")
})
