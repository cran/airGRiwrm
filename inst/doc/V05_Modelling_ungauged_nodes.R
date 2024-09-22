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

## ----network, echo = FALSE----------------------------------------------------
plot.mermaid("
graph LR
id95[54095]
id01[54001]
id29[54029]

id95 -->| 42 km| id01

subgraph Shared parameters from node 54032
id01 -->| 45 km| 54032
id29 -->| 32 km| 54032
end

54032 -->| 15 km| 54057
54002 -->| 43 km| 54057

classDef UpUng fill:#eef
classDef UpGau fill:#aaf
classDef IntUng fill:#efe
classDef IntGau fill:#afa
classDef DirInj fill:#faa

class id29 UpUng
class 54057,54032 IntGau
class id01 IntUng
class id95,54002 UpGau
")

## ----griwrm-------------------------------------------------------------------
data(Severn)
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"
nodes$model[nodes$gauge_id %in% c("54029", "54001")] <- "Ungauged"
griwrmV05 <- CreateGRiwrm(
  nodes,
  list(id = "gauge_id", down = "downstream_id", length = "distance_downstream")
)
griwrmV05

## ----plot_network-------------------------------------------------------------
plot(griwrmV05)

## ----obs----------------------------------------------------------------------
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Precip <- ConvertMeteoSD(griwrmV05, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrmV05, PotEvapTot)

## ----InputsModel--------------------------------------------------------------
IM_U <- CreateInputsModel(griwrmV05, DatesR, Precip, PotEvap)

## ----RunOptions---------------------------------------------------------------
IndPeriod_Run <- seq(
  which(DatesR == (DatesR[1] + 365*24*60*60)), # Set aside warm-up period
  length(DatesR) # Until the end of the time series
)
IndPeriod_WarmUp = seq(1,IndPeriod_Run[1]-1)
RunOptions <- CreateRunOptions(IM_U,
                               IndPeriod_WarmUp = IndPeriod_WarmUp,
                               IndPeriod_Run = IndPeriod_Run)
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
InputsCrit <- CreateInputsCrit(IM_U,
                               FUN_CRIT = ErrorCrit_KGE2,
                               RunOptions = RunOptions, Obs = Qobs[IndPeriod_Run,],
                               AprioriIds = c("54057" = "54032", "54032" = "54095"),
                               transfo = "sqrt", k = 0.15
)
CalibOptions <- CreateCalibOptions(IM_U)

## ----Calibration--------------------------------------------------------------
OC_U <- suppressWarnings(
  Calibration(IM_U, RunOptions, InputsCrit, CalibOptions))

## ----param--------------------------------------------------------------------
ParamV05 <- sapply(griwrmV05$id, function(x) {OC_U[[x]]$Param})
dfParam <- do.call(
  rbind,
  lapply(ParamV05, function(x)
    if (length(x)==4) {return(c(NA, x))} else return(x))
)
colnames(dfParam) <- c("velocity", paste0("X", 1:4))
knitr::kable(round(dfParam, 3))

## ----RunModel-----------------------------------------------------------------
OutputsModels <- RunModel(
  IM_U,
  RunOptions = RunOptions,
  Param = ParamV05
)

## ----plot, fig.height = 5, fig.width = 8--------------------------------------
plot(OutputsModels, Qobs = Qobs[IndPeriod_Run,], which = c("Regime", "CumFreq"))

