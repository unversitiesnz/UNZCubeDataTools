context("test-incomeDataFunctions")

optionSet.dataset1.dom = list(
  dom = 1,
  sex = 1,
  eth = c(1),
  studyLevel = c(4),
  subsector = c("University"),
  fieldOfStudy = NA,
  young_grad = 1,
  cohort = 2009,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset1.int = list(
  dom = 0,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = NA,
  cohort = 2009,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset2.dom = list(
  dom = 1,
  sex = 1,
  eth = 1,
  studyLevel = 6,
  subsector = "University",
  young_grad = 1,
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Earnings from wages or salary (mean)"
)
optionSet.dataset2.int = list(
  dom = 0,
  sex = NA,
  eth = NA,
  studyLevel = 6,
  subsector = "University",
  fieldOfStudy = 6,
  cohort = NA,
  indicator = "Earnings from wages or salary (mean)"
)

test_that("income works!", {
  result <- getCube.filteredByOptions.v2(optionSet.dataset1.dom)
  expect_equal(nrow(result), 73)
  result2 <- getCube.filteredByOptions.v2(optionSet.dataset1.int)
  expect_equal(nrow(result2), 73)
  result3 <- getCube.filteredByOptions.v2(optionSet.dataset2.dom)
  expect_equal(nrow(result3), 73)
  result4 <- getCube.filteredByOptions.v2(optionSet.dataset2.int)
  expect_equal(nrow(result4), 73)
})

test_that("income aggregates", {
  optionSet = list(
    dom = 1,
    sex = c(1,2),
    eth = c(1),
    studyLevel = c(3),
    subsector = "University",
    fieldOfStudy = NA,
    young_grad = 1,
    cohort = 2009,
    indicator = "Earnings from wages or salary (mean)"
  )
  result <- getCube.filteredByOptions.v2(optionSet)

  result2 <- getCube.filterAndAggregateByOptions.v2(optionSet)$data
  expect_equal(nrow(result2), 73)

})
