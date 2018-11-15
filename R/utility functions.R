#testFile = "dataspec-test2.csv"
ChangeNamesOnData <- function (indicator.data) {
  names(indicator.data)[names(indicator.data) == 'snz_sex_code'] <- 'sex'
  names(indicator.data)[names(indicator.data) == 'demo_eth'] <- 'ethnicity'
  #names(indicator.data)[names(indicator.data) == 'ter_com_qual_type_2010'] <- 'completed.qual.type'
  subset(indicator.data, select = -c(ind))
  #indicator.data
}
#testData.overseas = ChangeNamesOnData(read.csv("test-os.csv"))
#testData.custody = ChangeNamesOnData(read.csv("test-cus.csv", na.strings=c("S")))
#testData.benefit = ChangeNamesOnData(read.csv("test-ben.csv", na.strings=c("S")))
#datacube = read.csv("data-cube.csv")
cube = readRDS("cube.rds")
datacube.benefit = ChangeNamesOnData(cube[cube$ind == "total_da_onben_post",])
datacube.job_seeker = ChangeNamesOnData(cube[cube$ind == "JS_all_da_post",])
datacube.overseas = ChangeNamesOnData(cube[cube$ind == "OS_da_post",])
datacube.prog = ChangeNamesOnData(cube[cube$ind == "ter_post_da_prog",])
datacube.uni = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni",])
datacube.uni_hi = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni_hi",])
datacube.uni_lo = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni_lo",])

# set options

domesticOptions = list("Domestic"=TRUE, "International"=FALSE)
indicatorOptions = list("Overseas", "Benefit", "Job seekers", "Further Study", "Further University Study", "University Study at a Higher Level", "University Study at a Lower Level")
sexOptions = list("Male" = 1, "Female" = 2)
# macron issue! https://github.com/tidyverse/tibble/issues/87 & https://github.com/tidyverse/dplyr/issues/1950
ethnicityOptions = list("European"=1, "M\u101ori"=2, "Pasifika"=3, "Asian"=4, "Other"=5)
studyLevelOptions = list("Level 1-4 Certificates" = 2,
"Certificates and Diploma Level 5-7" = 3,
"Bachelor Degrees" = 4,
"Honours, postgrad dipl" = 5,
"Masters degrees" = 6,
"Doctoral degrees" = 7
)
fieldOfStudyOptions = list(
  "01 Natural and Physical Sciences" = 1,
  "02 Information Technology" = 2,
  "03 Engineering and Related Technologies" = 3,
  "04 Architecture and Building" = 4,
  "05 Agriculture, Environmental and Related Studies" = 5,
  "06 Health" = 6,
  "07 Education" = 7,
  "08 Management and Commerce" = 8,
  "09 Society and Culture" = 9,
  "10 Creative Arts" = 10,
  "11 Food, Hospitality and Personal Services" = 11,
  "12 Mixed Field Programmes" = 12
)


sexToId <- function(sex) {
  switch (sex,
    "Male" = 1, "Female" = 2
  )
}
domesticToId <- function(domestic) {
  switch (domestic,
          "Domestic" = 1, "International" = 0
  )
}
ethnicityToId <- function(ethnicity) {
  switch (ethnicity,
          "European" = 1, "M\u101ori" = 2, "Pasifika" = 3, "Asian" = 4, "Middle Eastern/Latin American/African" = 5, "Other" = 6
  )
}
#indicator = "Overseas"
#dom = 1
#eth = 1
#sex = 1


getOverseasData <- function(sex, dom, eth, indicator, cohort, subsector, fieldOfStudy, studyLevel = 4) {

  optionSet = list(
    dom = as.logical(dom),
    sex = as.numeric(sex),
    eth = as.numeric(eth),
    studyLevel =as.numeric(studyLevel),
    subsector = subsector,
    fieldOfStudy = as.numeric(fieldOfStudy),
    cohort = as.numeric(cohort),
    indicator = indicator
  )
  indicator.data.2 <- getCube.filteredByOptions(optionSet)
  if (nrow(indicator.data.2) == 0)
    return(NULL)

  indicator.data.2$prop <- indicator.data.2$num / indicator.data.2$denom
  #indicator.data.2 <- subset(indicator.data.2, select = -c(num, denom))
  #attach(indicator.data.2)
  #selectedData = aggregate(x = indicator.data.2, by = list(month), FUN=mean, na.rm = TRUE)
  #detach(indicator.data.2)
  selectedData = indicator.data.2
  selectedData
}



getOverseasData(1, TRUE, 1, "Overseas", "2011", "University", 5)

