getCube.dataset1.dom.select <- function(indicator.data, optionSet) {
  return (indicator.data$sex == optionSet$sex & !is.na(indicator.data$sex) &
            indicator.data$domestic == optionSet$dom & 
            indicator.data$cohort == optionSet$cohort & !is.na(indicator.data$cohort) &
            indicator.data$ter_com_subsector == optionSet$subsector &
            is.na(indicator.data$ter_com_NZSCED) &
            indicator.data$ethnicity == optionSet$eth & !is.na(indicator.data$ethnicity) &
            indicator.data$dataset == "dataset1" &
            indicator.data$ter_com_qual_type == optionSet$studyLevel)
}
getCube.dataset1.int.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == FALSE & 
            indicator.data$cohort == optionSet$cohort &
            indicator.data$ter_com_subsector == optionSet$subsector &
            indicator.data$dataset == "dataset1" &
            indicator.data$ter_com_qual_type == optionSet$studyLevel)
}
getCube.dataset2.dom.select <- function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &
            indicator.data$sex == optionSet$sex &
            indicator.data$ethnicity == optionSet$eth & 
            indicator.data$ter_com_subsector == optionSet$subsector &
            indicator.data$ter_com_NZSCED == optionSet$fieldOfStudy &
            indicator.data$dataset == "dataset2" &
            indicator.data$ter_com_qual_type == optionSet$studyLevel)
}
getCube.dataset2.int.select <-function(indicator.data, optionSet) {
  return (indicator.data$domestic == optionSet$dom &
            
            indicator.data$ter_com_subsector == optionSet$subsector &
            indicator.data$ter_com_NZSCED == optionSet$fieldOfStudy &
            indicator.data$dataset == "dataset2" &
            indicator.data$ter_com_qual_type == optionSet$studyLevel)
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
                    "University Study at a Lower Level" = datacube.uni_lo, 
                    "Further Study" = datacube.prog
        )
}

getCube.filteredByOptions <- function(optionSet) {
  filterFunction <- getCube.selector(optionSet)
  indicator.data <- getCube.forIndicator(optionSet)
  indicator.data[filterFunction(indicator.data, optionSet),]
}