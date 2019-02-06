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
  datacube[[optionSet$indicator]]
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
  if (optionSet$indicator == indicator_names$wns_mean) {
    filtered.data %>%
      filter(!is.na(mean)) %>%
      group_by(month) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(mean, num))
  } else if (optionSet$indicator == indicator_names$wns_median) {
    filtered.data %>%
      filter(!is.na(median)) %>%
      group_by(month) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(median, num))
  } else {
    aggregate(x = filtered.data[,c("num", "denom")], by = list(month = filtered.data$month), FUN=sum, na.rm = TRUE)
  }
}

getCube.aggregate.cohort <- function(filtered.data, optionSet) {
  if (optionSet$indicator == indicator_names$wns_mean) {
    filtered.data %>%
      filter(!is.na(mean)) %>%
      group_by(month, cohort) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(mean, num))
  } else if (optionSet$indicator == indicator_names$wns_median) {
    filtered.data %>%
      filter(!is.na(median)) %>%
      group_by(month, cohort) %>%
      summarise(base_num = sum(num), weighted_value =  weighted.mean(median, num))
  } else {
    aggregate(x = filtered.data[,c("num", "denom")], by = list(month = filtered.data$month, cohort = filtered.data$cohort), FUN=sum, na.rm = TRUE)
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
        optionSet$indicator %in% c(indicator_names$overseas, indicator_names$all_benefit, indicator_names$job_seeker_benefit, indicator_names$enrolled_university, indicator_names$enrolled_higher_university, indicator_names$enrolled_tertiary_study, indicator_names$employed, indicator_names$employed_wns, indicator_names$neet) &&
        anyNA(data$num)
      ) ||
      (
        optionSet$indicator %in% c(indicator_names$employer_change, indicator_names$regional_mobility, indicator_names$district_mobility) &&
        anyNA(data[-c(0), "num"])
      ) ||
      (
        optionSet$indicator == indicator_names$wns_mean &&
          anyNA(data[, "mean"])
      )
    ),
    aggregation = (nrow(data) > 73),
    multiCohort = !is.null(optionSet$multiCohort) && optionSet$multiCohort
  ))
}

getCube.filterAndAggregateByOptions <- function(optionSet) {
  filteredData <- getCube.filteredByOptions(optionSet)
  #decide if suppression was applied here?
  #return as list(data=data, containsSuppression=True/False)
  about <- checkCube.about(filteredData, optionSet)
  if (about$hasData) {
    if(about$multiCohort) {
      aggregatedData <- getCube.aggregate.cohort(filteredData, optionSet)
    } else {
      aggregatedData <- getCube.aggregate(filteredData, optionSet)
    }

    return (list(data=aggregatedData, about=about))
  } else {
    return (list(data=filteredData, about=about))
  }
}
