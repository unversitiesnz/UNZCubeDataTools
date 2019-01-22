context("test-incomeDataFunctions")

optionSet.dataset1.dom = list(
  dom = TRUE,
  sex = 1,
  eth = c(1),
  studyLevel = c(4),
  subsector = c("University"),
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset1.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset2.dom = list(
  dom = TRUE,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset2.int = list(
  dom = FALSE,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Earnings from wages or salary (mean)"
)

test_that("income works!", {
  result <- getCube.filteredByOptions(optionSet.dataset1.dom)
  expect_equal(nrow(result), 73)
  result2 <- getCube.filteredByOptions(optionSet.dataset1.int)
  expect_equal(nrow(result2), 73)
  result3 <- getCube.filteredByOptions(optionSet.dataset2.dom)
  expect_equal(nrow(result3), 73)
  result4 <- getCube.filteredByOptions(optionSet.dataset2.int)
  expect_equal(nrow(result4), 73)
})

test_that("income aggregates", {
  optionSet = list(
    dom = TRUE,
    sex = c(1,2),
    eth = c(1),
    studyLevel = c(4),
    subsector = c("University"),
    fieldOfStudy = NA,
    cohort = 2009,
    indicator = "Earnings from wages or salary (mean)"
  )
  result <- getCube.filteredByOptions(optionSet)

  result2 <- getCube.filterAndAggregateByOptions(optionSet)$data
  expect_equal(nrow(result2), 73)

})
