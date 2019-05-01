context("test-multiIndicator")

test_that("checking if the multi indicator stuff works", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = 6,
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    young_grad = TRUE
  )

  result <- getCube.filterAndAggregateByOptions.v2(optionSet)

  expect_equal(nrow(result$data), 73) # a row is being dropped.
})

testFunction <- function(data) {
  print(typeof(data))
  print(data)
  return(10)
}

test_that("agregate multi indicator data", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = c(2,3,4),
    subsector = "University",
    fieldOfStudy = NA,
    cohort = 2009,
    young_grad = TRUE
  )

  filtered.data <- getCube.filteredByOptions.v2(optionSet)

  #library(dplyr)
  # proportionIndicators <- indicator_names.v2[!indicator_names.v2 == "wns_income"]
  #proportionIndicators <- unique(indicator_names.v2)
  #num_titles <- paste(proportionIndicators, "num", sep="_")
  #denom_titles <- paste(proportionIndicators, "denom", sep="_")
  #titles <- append(num_titles, denom_titles)
  #distinct(titles)
  #aggregate(x = filtered.data[,titles], by = list(month = filtered.data$month), FUN=testFunction)
  #apply(filtered.data[,titles], MARGIN = 2, FUN = testFunction)


  result <- getCube.aggregate.v2(filtered.data, optionSet)
  #result <- getCube.filterAndAggregateByOptions.v2(optionSet)

  expect_equal(nrow(result), 73)

  result2 <- getCube.filterAndAggregateByOptions.v2(optionSet)
  expect_equal(nrow(result2$data), 73)
})

test_that("Dim Cohort", {
  optionSet = list(
    dom = TRUE,
    sex = 1,
    eth = 1,
    studyLevel = c(2,3,4),
    subsector = "University",
    fieldOfStudy = NA,
    cohort = c(2009, 2010),
    young_grad = TRUE,
    dimCohort = TRUE
  )

  filteredData <- getCube.filteredByOptions.v2(optionSet)

  expect_equal(nrow(filteredData), 73 * 2 * 3)

  filteredData2 <- getCube.aggregate.v2(filteredData, optionSet)

  expect_equal(nrow(filteredData2), 73 * 2)
})
