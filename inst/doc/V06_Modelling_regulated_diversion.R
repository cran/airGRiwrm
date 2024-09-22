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
data(Severn)

## -----------------------------------------------------------------------------
Qobs <- cbind(sapply(Severn$BasinsObs, function(x) {x$discharge_spec}))
Qobs <- Qobs[, Severn$BasinsInfo$gauge_id]
Qobs_m3s <- t(apply(Qobs, 1, function(r) r * Severn$BasinsInfo$area * 1E3 / 86400))
apply(Qobs_m3s[, c("54029", "54001")], 2, quantile, probs = seq(0,1,0.1), na.rm = TRUE)

## -----------------------------------------------------------------------------

nodes_div <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes_div$model <- "RunModel_GR4J"
nodes_div <- rbind(nodes_div, data.frame(gauge_id = "54001",
                                         downstream_id = "54029",
                                         distance_downstream = 25,
                                         model = "Diversion",
                                         area = NA))
renameCols <- list(id = "gauge_id", down = "downstream_id", length = "distance_downstream")
griwrmV06 <- CreateGRiwrm(nodes_div, renameCols)
plot(griwrmV06)

## -----------------------------------------------------------------------------
data(Severn)
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"
griwrm <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Precip <- ConvertMeteoSD(griwrm, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)

## -----------------------------------------------------------------------------
Qdiv <- matrix(rep(0, length(DatesR)), ncol = 1)
colnames(Qdiv) <- "54001"
Qmin <- matrix(rep(11.5 * 86400, length(DatesR)), ncol = 1)
colnames(Qmin) <- "54001"
IM_div <- CreateInputsModel(griwrmV06, DatesR, Precip, PotEvap, Qinf = Qdiv, Qmin = Qmin)

## -----------------------------------------------------------------------------
sv <- CreateSupervisor(IM_div, TimeStep = 1L)

## -----------------------------------------------------------------------------
#' @param sv the Supervisor environment
logicFunFactory <- function(sv) {
  #' @param Y Flow measured at "54002" the previous time step
  function(Y) {
    Qnat <- Y
    #  We need to remove the diverted flow to compute the natural flow at "54002"
    lastU <- sv$controllers[[sv$controller.id]]$U
    if (length(lastU) > 0) {
      Qnat <- max(0, Y + lastU)
    }
    return(-max(5.3 * 86400 - Qnat, 0))
  }
}

## -----------------------------------------------------------------------------
CreateController(sv,
                 ctrl.id = "Low flow support",
                 Y = "54029",
                 U = "54001",
                 FUN = logicFunFactory(sv))

## -----------------------------------------------------------------------------
# Running simulation on year 2003
IndPeriod_Run <- which(
  DatesR >= as.POSIXct("2003-03-01", tz = "UTC") &
    DatesR <= as.POSIXct("2004-01-01", tz = "UTC")
)
IndPeriod_WarmUp = seq(IndPeriod_Run[1] - 366,IndPeriod_Run[1] - 1)
RunOptions <- CreateRunOptions(IM_div,
                               IndPeriod_WarmUp = IndPeriod_WarmUp,
                               IndPeriod_Run = IndPeriod_Run)
ParamV02 <- readRDS(system.file("vignettes", "ParamV02.RDS", package = "airGRiwrm"))

## -----------------------------------------------------------------------------
ParamV02$`54029` <- c(1, ParamV02$`54029`)

## -----------------------------------------------------------------------------
OM_div <- RunModel(sv, RunOptions = RunOptions, Param = ParamV02)

## -----------------------------------------------------------------------------
OM_nat <- RunModel(IM_div, RunOptions = RunOptions, Param = ParamV02)

## ----fig.asp=1----------------------------------------------------------------
dfQdiv <- data.frame(DatesR = OM_div[[1]]$DatesR,
                     Diverted_flow = OM_div$`54001`$Qdiv_m3 / 86400)

oldpar <- par(mfrow=c(3,1), mar = c(2.5,4,1,1))
plot.Qm3s(dfQdiv)

# Plot natural and influenced flow at station "54001" and "54029"
thresholds <- c("54001" = 11.5, "54029" = 5.3)
lapply(names(thresholds), function(id) {
  df <- cbind(attr(OM_div, "Qm3s")[, c("DatesR", id)],
                   attr(OM_nat, "Qm3s")[, id])
  names(df) <- c("DatesR",
                      paste(id, "with low-flow support"),
                      paste(id, "natural flow"))
  plot.Qm3s(df, ylim = c(0,50), lty = c("solid", "dashed"))
  abline(h = thresholds[id], col = "green", lty = "dotted")
})
par(oldpar)

