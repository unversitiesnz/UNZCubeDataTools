library(dplyr)

getCube.dataset1.dom.select <- function(indicator.data, optionSet) {
  #print(anyNA(optionSet$eth))
  return ( (indicator.data$sex %in% optionSet$sex | anyNA(optionSet$sex)) &
            indicator.data$domestic %in% optionSet$dom &
            indicator.data$cohort %in% optionSet$cohort &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | anyNA(optionSet$subsector))  &
            ((indicator.data$ethnicity %in% optionSet$eth) | anyNA(optionSet$eth)) &
            indicator.data$dataset == "dataset1" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | anyNA(optionSet$studyLevel)))
}
getCube.dataset1.int.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == FALSE &
            indicator.data$cohort %in% optionSet$cohort &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | anyNA(optionSet$subsector)) &
            indicator.data$dataset == "dataset1" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | anyNA(optionSet$studyLevel)))
}
getCube.dataset2.dom.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &
            (indicator.data$sex %in% optionSet$sex | anyNA(optionSet$sex)) &
            (indicator.data$ethnicity %in% optionSet$eth | anyNA(optionSet$eth))  &
            (indicator.data$ter_com_subsector %in% optionSet$subsector | anyNA(optionSet$subsector)) &
            (indicator.data$ter_com_NZSCED %in% optionSet$fieldOfStudy | anyNA(optionSet$fieldOfStudy)) &
            indicator.data$dataset == "dataset2" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | anyNA(optionSet$studyLevel)))
}
getCube.dataset2.int.select <-function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &

            (indicator.data$ter_com_subsector %in% optionSet$subsector | anyNA(optionSet$subsector)) &
            (indicator.data$ter_com_NZSCED %in% optionSet$fieldOfStudy | anyNA(optionSet$fieldOfStudy)) &
            indicator.data$dataset == "dataset2" &
            (indicator.data$ter_com_qual_type %in% optionSet$studyLevel | anyNA(optionSet$studyLevel)))
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
                    "Employer Change" = datacube.wns_pbn_ch,
                    "Region Change" = datacube.wns_reg_ch,
                    "District Change" = datacube.wns_ta_ch,
                    "W&S or Self Employed" = datacube.wns_sei,
                    "Wage and Salary Employed" = datacube.wns,
                    "W&S Income (Mean)" = datacube.wns_income,
                    "W&S Income (Median)" = datacube.wns_income,
                    "NEET" = datacube.neet
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
  if (optionSet$indicator == "W&S Income (Mean)") {
    filtered.data %>%
      filter(!is.na(mean)) %>%
      group_by(month) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(mean, num))
  } else if (optionSet$indicator == "W&S Income (Median)") {
    filtered.data %>%
      filter(!is.na(median)) %>%
      group_by(month) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(median, num))
  } else {
    aggregate(x = filtered.data[,c("num", "denom")], by = list(month = filtered.data$month), FUN=sum, na.rm = TRUE)
  }
}

# given the data, indicate weather suppression has been applied, and if aggregation is required.
checkCube.about <- function(data, optionSet) {
  return (list(
    hasData = (
      nrow(data) > 0
    ),
    suppression = (
      (
        optionSet$indicator %in% c("Overseas", "Benefit", "Job seekers", "Further University Study", "University Study at a Higher Level", "Further Study", "W&S or Self Employed", "Wage and Salary Employed", "NEET") &&
        anyNA(data$num)
      ) ||
      (
        optionSet$indicator %in% c("Employer Change", "Region Change", "District Change") &&
        anyNA(data[-c(0), "num"])
      ) ||
      (
        optionSet$indicator == "W&S Income (Mean)" &&
          anyNA(data[, "mean"])
      )
    ),
    aggregation = (nrow(data) > 73)
  ))
}

getCube.filterAndAggregateByOptions <- function(optionSet) {
  filteredData <- getCube.filteredByOptions(optionSet)
  #decide if suppression was applied here?
  #return as list(data=data, containsSuppression=True/False)
  about <- checkCube.about(filteredData, optionSet)
  if (about$hasData) {
    aggregatedData <- getCube.aggregate(filteredData, optionSet)
    return (list(data=aggregatedData, about=about))
  } else {
    return (list(data=filteredData, about=about))
  }
}
