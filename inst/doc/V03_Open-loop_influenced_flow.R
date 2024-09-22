## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.asp = 0.68,
  out.width = "70%",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(airGRiwrm)

## -----------------------------------------------------------------------------
data(Severn)
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"

## -----------------------------------------------------------------------------
nodes$model[nodes$gauge_id == "54002"] <- NA
nodes$model[nodes$gauge_id == "54001"] <- NA
griwrmV03 <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
griwrmV03

## ----diagram------------------------------------------------------------------
plot(griwrmV03)

## -----------------------------------------------------------------------------
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Precip <- ConvertMeteoSD(griwrmV03, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrmV03, PotEvapTot)
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))

## -----------------------------------------------------------------------------
QobsInputs <- Qobs[, c("54001", "54002")]

## -----------------------------------------------------------------------------
IM_OL <- CreateInputsModel(griwrmV03, DatesR, Precip, PotEvap, QobsInputs)

## -----------------------------------------------------------------------------
IndPeriod_Run <- seq(
  which(DatesR == (DatesR[1] + 365*24*60*60)), # Set aside warm-up period
  length(DatesR) # Until the end of the time series
)
IndPeriod_WarmUp = seq(1,IndPeriod_Run[1]-1)
RunOptions <- CreateRunOptions(IM_OL,
                               IndPeriod_WarmUp = IndPeriod_WarmUp,
                               IndPeriod_Run = IndPeriod_Run)
InputsCrit <- CreateInputsCrit(IM_OL,
                               FUN_CRIT = ErrorCrit_KGE2,
                               RunOptions = RunOptions, Obs = Qobs[IndPeriod_Run,],
                               AprioriIds = c("54057" = "54032", "54032" = "54029"),
                               transfo = "sqrt", k = 0.15
)
CalibOptions <- CreateCalibOptions(IM_OL)

## ----Calibration--------------------------------------------------------------
OC_OL <- suppressWarnings(
  Calibration(IM_OL, RunOptions, InputsCrit, CalibOptions))
ParamV03 <- sapply(griwrmV03$id, function(x) {OC_OL[[x]]$Param})

## ----RunModel-----------------------------------------------------------------
OM_OL <- RunModel(
  IM_OL,
  RunOptions = RunOptions,
  Param = ParamV03
)

## ----fig.height = 5, fig.width = 8--------------------------------------------
plot(OM_OL, Qobs = Qobs[IndPeriod_Run, ], which = "Regime")

## -----------------------------------------------------------------------------
Qm3s <- attr(OM_OL, "Qm3s")
plot(Qm3s[1:150, ])

