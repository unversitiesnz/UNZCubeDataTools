context("test-dataCubeReadByCarAgre")

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
  optionSet = list(
    dom = TRUE,
    sex = c(1, 2),
    eth = c(1),
    studyLevel = c(4),
    subsector = c("University"),
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )
  filteredData <- getCube.filteredByOptions(optionSet)
  aggregate(x = filteredData$denom, by = list(month = filteredData$month), FUN=sum, na.rm = FALSE)
})


test_that("The read function can support more than one option", {
  optionSet = list(
    dom = TRUE,
    sex = NA,
    eth = c(1),
    studyLevel = c(4),
    subsector = c("University"),
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Overseas"
  )
  filteredData <- getCube.filteredByOptions(optionSet)
  expect_equal(nrow(filteredData), 73 * 2)
  aggData <- getCube.aggregate(filteredData, optionSet)
  expect_equal(nrow(aggData), 73)
})

