ChangeNamesOnData <- function (indicator.data) {
  names(indicator.data)[names(indicator.data) == 'snz_sex_code'] <- 'sex'
  names(indicator.data)[names(indicator.data) == 'demo_eth'] <- 'ethnicity'
  subset(indicator.data, select = -c(ind))
}
if (!exists("dataDir")) {
  dataDir = '.';
}
#cube = readRDS(file.path(dataDir, "cube.rds"))
load(file.path(dataDir, "datacube.RData"))
load(file.path(dataDir, "cubehelpers.RData"))

datacube <- list(
  datacube.overseas,
  datacube.benefit,
  datacube.job_seeker,
  datacube.prog,
  datacube.uni,
  datacube.uni_hi,
  datacube.wns_sei,
  datacube.wns,
  datacube.neet,
  datacube.wns_income,
  datacube.wns_income,
  datacube.wns_reg_ch,
  datacube.wns_ta_ch,
  datacube.wns_pbn_ch
)

indicator_names = list(
  overseas = "Overseas",
  all_benefit = "On a benefit",
  job_seeker_benefit = "Job Seeker benefit",
  enrolled_tertiary_study = "Enrolled in tertiary study",
  enrolled_university = "Enrolled at university",
  enrolled_higher_university = "Enrolled in higher level studies at university",
  employed = "Employed",
  employed_wns = "Employed on wages or salaries",
  neet = "NEET (Not in Employment, Education or Training)",
  wns_mean = "Earnings from wages or salary (mean)",
  wns_median = "Earnings from wages or salary (median)",
  regional_mobility = "Regional mobility",
  district_mobility = "District mobility",
  employer_change = "Employer change"
)

names(datacube) <- indicator_names

# cleaning
#remove(cube)

#cube_income = readRDS(file.path(dataDir, "cube_inc.rds"))
#datacube.wns_income = ChangeNamesOnData(cube_income)
