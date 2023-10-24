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
Severn$BasinsInfo

## -----------------------------------------------------------------------------
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$distance_downstream <- nodes$distance_downstream #je ne comprends pas cette ligne, elle semble inutile
nodes$model <- "RunModel_GR4J"

## -----------------------------------------------------------------------------
griwrm <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
griwrm

## ----diagram------------------------------------------------------------------
plot(griwrm)

## -----------------------------------------------------------------------------
BasinsObs <- Severn$BasinsObs
str(BasinsObs)

## ----warning=FALSE, message=FALSE---------------------------------------------

DatesR <- BasinsObs[[1]]$DatesR

PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))

## -----------------------------------------------------------------------------
Precip <- ConvertMeteoSD(griwrm, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)

## -----------------------------------------------------------------------------
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)

