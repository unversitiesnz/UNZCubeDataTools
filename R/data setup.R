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

# cleaning
#remove(cube)

#cube_income = readRDS(file.path(dataDir, "cube_inc.rds"))
#datacube.wns_income = ChangeNamesOnData(cube_income)
