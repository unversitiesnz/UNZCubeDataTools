context("test-dataCubeReadByCarAgre")

# define options
# Stakeholder Request 15: I think you should be able to select more than one option for ‘Sub Sector’, ‘Ethnicity’, ‘Level of Study’, and ‘Field of Study’.

#test
test_that("The read function can support more than one option", {
  optionSet = list(
    dom = 1,
    sex = c(1, 2),
    eth = c(1),
    studyLevel = c(4),
    subsector = c("University"),
    fieldOfStudy = NA,
    cohort = 2009,
    young_grad = 1,
    indicator = "Overseas"
  )
  filteredData <- getCube.filteredByOptions.v2(optionSet)
  aggregate(x = filteredData$overseas_denom, by = list(month = filteredData$month), FUN=sum, na.rm = FALSE)
})


test_that("The read function can support more than one option", {
  optionSet = list(
    dom = 1,
    sex = -1,
    eth = c(1),
    studyLevel = c(4),
    subsector = c("University"),
    fieldOfStudy = NA,
    cohort = 2009,
    young_grad = 1,
    indicator = "Overseas"
  )
  filteredData <- getCube.filteredByOptions.v2(optionSet)
  expect_equal(nrow(filteredData), 73 * 2)
  aggData <- getCube.aggregate.v2(filteredData, optionSet)
  expect_equal(nrow(aggData), 73)
})

