ChangeNamesOnData <- function (indicator.data) {
  names(indicator.data)[names(indicator.data) == 'snz_sex_code'] <- 'sex'
  names(indicator.data)[names(indicator.data) == 'demo_eth'] <- 'ethnicity'
  subset(indicator.data, select = -c(ind))
}
cube = readRDS("cube.rds")
datacube.benefit = ChangeNamesOnData(cube[cube$ind == "total_da_onben_post",])
datacube.job_seeker = ChangeNamesOnData(cube[cube$ind == "JS_all_da_post",])
datacube.overseas = ChangeNamesOnData(cube[cube$ind == "OS_da_post",])
datacube.prog = ChangeNamesOnData(cube[cube$ind == "ter_post_da_prog",])
datacube.uni = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni",])
datacube.uni_hi = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni_hi",])
datacube.uni_lo = ChangeNamesOnData(cube[cube$ind == "ter_post_enr_uni_lo",])

# cleaning
remove(cube)
