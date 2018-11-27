getCube.dataset1.dom.select <- function(indicator.data, optionSet) {
  return ( (indicator.data$sex %in% optionSet$sex | is.na(optionSet$sex)) &
            indicator.data$domestic %in% optionSet$dom &
            indicator.data$cohort %in% optionSet$cohort & !is.na(indicator.data$cohort) &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | is.na(optionSet$subsector))  &
            is.na(indicator.data$ter_com_NZSCED) &
            (indicator.data$ethnicity %in% optionSet$eth | is.na(optionSet$eth)) &
            indicator.data$dataset == "dataset1" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | is.na(optionSet$studyLevel)))
}
getCube.dataset1.int.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == FALSE &
            indicator.data$cohort %in% optionSet$cohort &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | is.na(optionSet$subsector)) &
            indicator.data$dataset == "dataset1" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | is.na(optionSet$studyLevel)))
}
getCube.dataset2.dom.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &
            (indicator.data$sex %in% optionSet$sex | is.na(optionSet$sex)) &
            (indicator.data$ethnicity %in% optionSet$eth | is.na(optionSet$eth))  &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | is.na(optionSet$subsector)) &
            (indicator.data$ter_com_NZSCED %in% optionSet$fieldOfStudy | is.na(optionSet$fieldOfStudy)) &
            indicator.data$dataset == "dataset2" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | is.na(optionSet$studyLevel)))
}
getCube.dataset2.int.select <-function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &

            (indicator.data$ter_com_subsector %in% optionSet$subsector | is.na(optionSet$subsector)) &
            (indicator.data$ter_com_NZSCED %in% optionSet$fieldOfStudy | is.na(optionSet$fieldOfStudy)) &
            indicator.data$dataset == "dataset2" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | is.na(optionSet$studyLevel)))
}

getCube.selector <-function(optionSet) {
  if(is.na(optionSet$cohort)) {
    datasetName <- "dataset2"
  } else {
    datasetName <- "dataset1"
  }
  return (switch(datasetName,
         "dataset1" = switch(as.character(optionSet$dom), "TRUE" = getCube.dataset1.dom.select, "FALSE" = getCube.dataset1.int.select),
         "dataset2" = switch(as.character(optionSet$dom), "TRUE" = getCube.dataset2.dom.select, "FALSE" = getCube.dataset2.int.select)))
  #getCube.dataset1.dom.select
}

getCube.forIndicator <- function(optionSet) {
  switch(optionSet$indicator, "Overseas" = datacube.overseas,
                    "Benefit" = datacube.benefit,
                    "Job seekers" = datacube.job_seeker,
                    "Further University Study" = datacube.uni,
                    "University Study at a Higher Level" = datacube.uni_hi,
                    "Further Study" = datacube.prog,
                    "Wage and Salary Employed" = datacube.wns,
                    "W&S Income (Mean)" = datacube.wns_income,
                    "W&S Income (Median)" = datacube.wns_income
        )
}

#' Get the dataset based on the options passed to it.
#'
#' @param optionSet A list/vector with the following fields: list(dom = bool, sex = int, eth = int, studyLevel = int, subsector = string, fieldOfStudy = int, cohort = int, indicator = string).
#' @return The data rows that relate to optionsSet.
#' @examples
#' getCube.filteredByOptions(optionSet)
getCube.filteredByOptions <- function(optionSet) {
  filterFunction <- getCube.selector(optionSet)
  indicator.data <- getCube.forIndicator(optionSet)
  indicator.data[filterFunction(indicator.data, optionSet),]
}

getCube.aggregate <- function(filtered.data, optionSet) {
  aggregate(x = filtered.data[,c("num", "denom")], by = list(month = filtered.data$month), FUN=sum, na.rm = TRUE)
}

getCube.filterAndAggregateByOptions <- function(optionSet) {
  filteredData <- getCube.filteredByOptions(optionSet)
  getCube.aggregate(filteredData)
}
