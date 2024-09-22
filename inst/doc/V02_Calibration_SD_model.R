## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  fig.width = 6,
  fig.asp = 0.68,
  out.width = "70%",
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(airGRiwrm)

## -----------------------------------------------------------------------------
data(Severn)
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"
griwrm <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
Precip <- ConvertMeteoSD(griwrm, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
str(InputsModel)

## -----------------------------------------------------------------------------
IndPeriod_Run <- seq(
  which(InputsModel[[1]]$DatesR == (InputsModel[[1]]$DatesR[1] + 365*24*60*60)), # Set aside warm-up period
  length(InputsModel[[1]]$DatesR) # Until the end of the time series
)
IndPeriod_WarmUp <- seq(1, IndPeriod_Run[1] - 1)

## -----------------------------------------------------------------------------
RunOptions <- CreateRunOptions(
  InputsModel,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run
)

## ----InputsCrit---------------------------------------------------------------
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, ],
  AprioriIds = c(
      "54057" = "54032",
      "54032" = "54001",
      "54001" = "54095"
  ),
  transfo = "sqrt",
  k = 0.15
)
str(InputsCrit)

## ----CalibOption--------------------------------------------------------------
CalibOptions <- CreateCalibOptions(InputsModel)

## ----Calibration--------------------------------------------------------------
OutputsCalib <- suppressWarnings(
  Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions))

## ----RunModel-----------------------------------------------------------------
OutputsModels <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = extractParam(OutputsCalib)
)

## ----fig.height = 5, fig.width = 8--------------------------------------------
plot(OutputsModels, Qobs = Qobs[IndPeriod_Run,])

## -----------------------------------------------------------------------------
Qm3s <- attr(OutputsModels, "Qm3s")
plot(Qm3s[1:150,])

