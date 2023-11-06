## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ospsuite.parameteridentification)
knitr::opts_knit$set(
  root.dir = ".." # setting up to package root folder so that the relative paths match between the vignettes and the tests
)

## ----include=FALSE, eval=TRUE-------------------------------------------------
# Some results in this vignette take too much time to calculate, so they are cached in the "cached.RData" file. 
# Whenever the R code in the vignette uses these cached results, there's a block with `eval=FALSE` showing how
# they were calculated, and a block with `echo=FALSE` reusing the cached results.
load("vignettes/cached.RData")

## ----include=TRUE, eval=TRUE--------------------------------------------------
piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE

## ----include=TRUE, eval=TRUE--------------------------------------------------
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Aciclovir.pkml"))
names(simulations) <- "Aciclovir"

## ----include=TRUE, eval=TRUE--------------------------------------------------
parameters <- c(PIParameters$new(parameters = ospsuite::getParameter(
    path = "Aciclovir|Lipophilicity", 
    container = simulations$Aciclovir
  )))

## ----include=TRUE, eval=TRUE--------------------------------------------------
parameters[[1]]$minValue <- -10
parameters[[1]]$maxValue <- 10

## ----include=TRUE, eval=TRUE--------------------------------------------------
filePath <- "tests/data/AciclovirLaskinData.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Laskin 1982.Group A"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  filePath <- "tests/data/AciclovirLaskinData.xlsx"
#  dataConfiguration <- loadDataImporterConfiguration("tests/data/dataImporter_configuration.xml")
#  observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)

## ----include=TRUE, eval=TRUE--------------------------------------------------
outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulations$Aciclovir
))
outputMapping$addObservedDataSets(observedData$`AciclovirLaskinData.Laskin 1982.Group A`)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  task <- ParameterIdentification$new(
#    simulations = simulations,
#    parameters = parameters,
#    outputMappings = outputMappings,
#    configuration = piConfiguration
#  )
#  taskResults <- task$run()

## ----include=TRUE, echo=FALSE, eval=TRUE--------------------------------------
task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMappings,
  configuration = piConfiguration
)
taskResults <- task$run()

## ----include=TRUE, eval=TRUE, fig.width=10, fig.height=6, fig.align='center'----
task$plotResults()

## ----include=FALSE, eval=TRUE-------------------------------------------------
task$configuration$printEvaluationFeedback <- FALSE

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  profile <- task$calculateOFVProfiles()
#  task$plotOFVProfiles(profile)[[1]]

## ----include=TRUE, echo=FALSE, eval=TRUE, fig.width=6, fig.height=4, fig.align='center'----
task$plotOFVProfiles(cachedAciclovirProfile)[[1]]

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  profile <- task$calculateOFVProfiles(lower = -10, upper = 10, totalEvaluations = 200)
#  task$plotOFVProfiles(profile)[[1]]

## ----include=TRUE, echo=FALSE, eval=TRUE, fig.width=6, fig.height=4, fig.align='center'----
task$plotOFVProfiles(cachedAciclovirFullProfile)[[1]]

## ----include=TRUE, eval=TRUE--------------------------------------------------
piConfiguration <- PIConfiguration$new()
piConfiguration$printEvaluationFeedback <- TRUE
simulations <- c(loadSimulation("tests/dev/Models/Simulations/Smith1981 iv 5mg Midazolam.pkml"))
names(simulations) <- "Midazolam"

## ----include=TRUE, eval=TRUE--------------------------------------------------
getAllParametersMatching("**|kcat", simulations$Midazolam)

## ----include=FALSE, eval=TRUE-------------------------------------------------
piConfiguration$printEvaluationFeedback <- FALSE

## ----include=TRUE, message=FALSE, eval=TRUE-----------------------------------
parameterInputData <- list(
  list(path = "Midazolam|Lipophilicity", min = -10, max = 10, start = 3.9),
  list(path = "Midazolam-CYP3A4-Patki et al. 2003 rCYP3A4|kcat", min = 0, max = 3200, start = 320)
)
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(
      path = parameterInputData[[idx]]$path,
      container = simulation
    ))
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}
filePath <- "tests/data/Midazolam_Smith_1981.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "Smith1981"
dataConfiguration$namingPattern <- "{Source}.{Sheet}"
observedData <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
outputMapping <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Midazolam|Plasma (Peripheral Venous Blood)",
  container = simulations$Midazolam
))
outputMapping$addObservedDataSets(observedData$Midazolam_Smith_1981.Smith1981)
outputMapping$scaling <- "lin"
outputMappings <- c(outputMapping)

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMapping,
  configuration = piConfiguration
)
taskResults <- task$run()

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  grid <- task$gridSearch(lower = c(3, 0), upper = c(4, 5), totalEvaluations = 2000)
#  task$plotOFVGrid(grid)

## ----include=TRUE, echo=FALSE, eval=TRUE, fig.width=10, fig.height=6, fig.align='center'----
task$plotGrid(cachedMidazolamGrid)

## ----include=TRUE, message=FALSE, eval=TRUE-----------------------------------
simulations <- c(
  "IV250" = loadSimulation("tests/dev/Models/Simulations/Chu1992 iv 250mg Clarithromycin.pkml"),
  "PO250" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 250mg Clarithromycin.pkml"),
  "PO250MD" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 250mg md Clarithromycin.pkml"),
  "PO500" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 500mg Clarithromycin.pkml"),
  "PO500MD" = loadSimulation("tests/dev/Models/Simulations/Chu1993 po 500mg md Clarithromycin.pkml")
)
piConfiguration <- PIConfiguration$new()

parameterInputData <- list(
  list(path = "Clarithromycin-CYP3A4-fit|kcat", min = 0, max = 100, start = 10),
  list(path = "Neighborhoods|Kidney_pls_Kidney_ur|Clarithromycin|Renal Clearances-fitted|Specific clearance", min = 0, max = 100, start = 10),
  list(path = "Clarithromycin|Specific intestinal permeability (transcellular)", min = 0, max = 1, start = 0.01)
)
parameters <- vector("list", length = length(parameterInputData))
for (idx in seq_along(parameterInputData)) {
  modelParams <- list()
  for (simulation in simulations) {
    modelParams <- c(modelParams, ospsuite::getParameter(
      path = parameterInputData[[idx]]$path,
      container = simulation
    ))
  }
  parameters[[idx]] <- PIParameters$new(parameters = modelParams)
  parameters[[idx]]$minValue <- parameterInputData[[idx]]$min
  parameters[[idx]]$maxValue <- parameterInputData[[idx]]$max
  parameters[[idx]]$startValue <- parameterInputData[[idx]]$start
}

# Observed data is loaded from two different files
# because IV data is reported in µmol/L, and PO data is reported in µg/ml
filePath <- "tests/data/Clarithromycin_Chu_1992.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- "IV250"
dataConfiguration$namingPattern <- "{Sheet}"
observedData_IV <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
filePath <- "tests/data/Clarithromycin_Chu_1993.xlsx"
dataConfiguration <- createImporterConfigurationForFile(filePath = filePath)
dataConfiguration$sheets <- c("PO250", "PO250MD", "PO500", "PO500MD")
dataConfiguration$namingPattern <- "{Sheet}"
observedData_PO <- loadDataSetsFromExcel(xlsFilePath = filePath, importerConfigurationOrPath = dataConfiguration)
observedData <- c(observedData_IV, observedData_PO)

outputMappings <- vector("list", length = length(simulations))
for (idx in seq_along(simulations)) {
  outputMappings[[idx]] <- PIOutputMapping$new(quantity = getQuantity("Organism|PeripheralVenousBlood|Clarithromycin|Plasma (Peripheral Venous Blood)",
                                                                      container = simulations[[idx]]
  ))
  outputMappings[[idx]]$addObservedDataSets(observedData[[names(simulations)[[idx]]]])
  outputMappings[[idx]]$scaling <- "lin"
}

task <- ParameterIdentification$new(
  simulations = simulations,
  parameters = parameters,
  outputMappings = outputMappings,
  configuration = piConfiguration
)

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  taskResults <- task$run()
#  task$plotOFVProfiles(task$calculateOFVProfiles())[[1]]

## ----include=TRUE, echo=FALSE, eval=TRUE, fig.width=6, fig.height=4, fig.align='center'----
task$plotOFVProfiles(cachedClarithromycinSuboptimalProfiles)[[1]]

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  grid <- task$gridSearch(totalEvaluations = 1000, setStartingPoint = TRUE)
#  best <- dplyr::slice_min(grid, ofv, with_ties = FALSE)

## ----include=TRUE, message=FALSE, eval=FALSE----------------------------------
#  task$configuration$algorithm <- "DEoptim"
#  task$configuration$algorithmOptions <- list(itermax = 1000, steptol = 10)
#  taskResults <- task$run()

## ----include=TRUE, eval=FALSE-------------------------------------------------
#  task$plotOFVProfiles(task$calculateOFVProfiles())

## ----include=TRUE, echo=FALSE, eval=TRUE, fig.width=6, fig.height=4, fig.align='center'----
task$plotOFVProfiles(cachedClarithromycinOptimalProfiles)[[1]]
task$plotOFVProfiles(cachedClarithromycinOptimalProfiles)[[2]]
task$plotOFVProfiles(cachedClarithromycinOptimalProfiles)[[3]]

