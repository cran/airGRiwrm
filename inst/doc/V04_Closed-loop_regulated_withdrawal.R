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

## ----load_cache---------------------------------------------------------------
data(Severn)
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"

## ----updated_nodes------------------------------------------------------------
nodes <- rbind(
  nodes,
  data.frame(
    gauge_id = c("Irrigation1", "Irrigation2"),
    downstream_id = c("54001", "54032"),
    distance_downstream = c(35, 10),
    model = NA,
    area = NA
  )
)

nodes


## ----griwm--------------------------------------------------------------------
griwrmV04 <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
plot(griwrmV04)

## ----monthly_water_need-------------------------------------------------------
# Formatting climatic data for CreateInputsModel (See vignette V01_Structure_SD_model for details)
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
Precip <- ConvertMeteoSD(griwrmV04, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrmV04, PotEvapTot)

# Calculation of the water need at the sub-basin scale
dailyWaterNeed <- PotEvap - Precip
dailyWaterNeed <- cbind(as.data.frame(DatesR), dailyWaterNeed[,c("54001", "54032")])
monthlyWaterNeed <- SeriesAggreg(dailyWaterNeed, "%Y%m", rep("mean",2))
monthlyWaterNeed <- SeriesAggreg(dailyWaterNeed, "%m", rep("q80",2))
monthlyWaterNeed[monthlyWaterNeed < 0] <- 0
monthlyWaterNeed$DatesR <- as.numeric(format(monthlyWaterNeed$DatesR,"%m"))
names(monthlyWaterNeed)[1] <- "month"
monthlyWaterNeed <- monthlyWaterNeed[order(monthlyWaterNeed$month),]
monthlyWaterNeed

## -----------------------------------------------------------------------------
irrigationObjective <- monthlyWaterNeed
# Conversion in m3/day
irrigationObjective$"54001" <- monthlyWaterNeed$"54001" * 15 * 1E3
irrigationObjective$"54032" <- monthlyWaterNeed$"54032" * 30 * 1E3
# Irrigation period between March and September
irrigationObjective[-seq(3,9),-1] <- 0
# Conversion in m3/s
irrigationObjective[,c(2,3)] <- round(irrigationObjective[,c(2,3)] / 86400, 1)
irrigationObjective$total <- rowSums(irrigationObjective[,c(2,3)])
irrigationObjective

## -----------------------------------------------------------------------------
# Application of the 50% irrigation system efficiency on the water demand
irrigationObjective[,seq(2,4)] <- irrigationObjective[,seq(2,4)] / 0.5
# Display result in m3/s
irrigationObjective

## ----abstraction restriction rule---------------------------------------------
restriction_rule <- data.frame(quantile_natural_flow = c(.05, .3, 0.5, 0.7),
                               abstraction_rate = c(0.1, 0.15, 0.20, 0.24))

## -----------------------------------------------------------------------------
quant_m3s32 <- quantile(
  Qobs[,"54032"] * griwrmV04[griwrmV04$id == "54032", "area"] / 86.4,
  restriction_rule$quantile_natural_flow,
  na.rm = TRUE
)
restriction_rule_m3s <- data.frame(
  threshold_natural_flow = quant_m3s32,
  abstraction_rate = restriction_rule$abstraction_rate
)

matplot(restriction_rule$quantile_natural_flow,
        cbind(restriction_rule_m3s$threshold_natural_flow,
              restriction_rule$abstraction_rate * restriction_rule_m3s$threshold_natural_flow,
              max(irrigationObjective$total)),
        log = "x", type = "l",
        main = "Quantiles of flow on the Severn at Saxons Lode (54032)",
        xlab = "quantiles", ylab = "Flow (m3/s)",
        lty = 1, col = rainbow(3, rev = TRUE)
        )
legend("topleft", legend = c("Natural flow", "Abstraction limit", "Irrigation max. objective"),
       col = rainbow(3, rev = TRUE), lty = 1)

## -----------------------------------------------------------------------------
# A function to enclose the parameters in the function (See: http://adv-r.had.co.nz/Functional-programming.html#closures)
getAvailableAbstractionEnclosed <- function(restriction_rule_m3s) {
  function(Qnat) approx(restriction_rule_m3s$threshold_natural_flow,
                        restriction_rule_m3s$abstraction_rate,
                        Qnat,
                        rule = 2)
}
# The function with the parameters inside it :)
getAvailableAbstraction <- getAvailableAbstractionEnclosed(restriction_rule_m3s)
# You can check the storage of the parameters in the function with
as.list(environment(getAvailableAbstraction))

## -----------------------------------------------------------------------------
restriction_rotation <- matrix(c(5,7,6,4,2,1,3,3,1,2,4,6,7,5), ncol = 2)
m <- do.call(
  rbind,
  lapply(seq(0,7), function(x) {
    b <- restriction_rotation <= x
    rowSums(b)
  })
)
# Display the planning of restriction
image(1:ncol(m), 1:nrow(m), t(m), col = heat.colors(3, rev = TRUE),
      axes = FALSE, xlab = "week day", ylab = "number of restriction days",
      main = "Number of closed irrigation perimeters")
axis(1, 1:ncol(m), unlist(strsplit("SMTWTFS", "")))
axis(2, 1:nrow(m), seq(0,7))
for (x in 1:ncol(m))
  for (y in 1:nrow(m))
    text(x, y, m[y,x])

## -----------------------------------------------------------------------------
# Flow time series are needed for all direct injection nodes in the network
# even if they may be overwritten after by a controller
QinfIrrig <- data.frame(Irrigation1 = rep(0, length(DatesR)),
                        Irrigation2 = rep(0, length(DatesR)))

# Creation of the GRiwrmInputsModel object
IM_Irrig <- CreateInputsModel(griwrmV04, DatesR, Precip, PotEvap, QinfIrrig)

## -----------------------------------------------------------------------------
sv <- CreateSupervisor(IM_Irrig, TimeStep = 7L)

## -----------------------------------------------------------------------------
fIrrigationFactory <- function(supervisor,
                               irrigationObjective,
                               restriction_rule_m3s,
                               restriction_rotation) {
  function(Y) {
    # Y is in m3/day and the basin's area is in km2
    # Calculate the objective of irrigation according to the month of the current days of simulation
    month <- as.numeric(format(supervisor$ts.date, "%m"))
    U <- irrigationObjective[month, c(2,3)] # m3/s
    meanU <- mean(rowSums(U))
    if (meanU > 0) {
      # calculate the naturalized flow from the measured flow and the abstracted flow of the previous week
      lastU <- supervisor$controllers[[supervisor$controller.id]]$U # m3/day
      Qnat <- (Y - rowSums(lastU)) / 86400 # m3/s
      # Maximum abstracted flow available
      Qrestricted <- mean(
        approx(restriction_rule_m3s$threshold_natural_flow,
               restriction_rule_m3s$abstraction_rate,
               Qnat,
               rule = 2)$y * Qnat
      )
      # Total for irrigation
      QIrrig <- min(meanU, Qrestricted)
      # Number of days of irrigation
      n <- floor(7 * (1 - QIrrig / meanU))
      # Apply days off
      U[restriction_rotation[seq(nrow(U)),] <= n] <- 0
    }
    return(-U * 86400) # withdrawal is a negative flow in m3/day on an upstream node
  }
}

## -----------------------------------------------------------------------------
fIrrigation <- fIrrigationFactory(supervisor = sv,
                                  irrigationObjective = irrigationObjective,
                                  restriction_rule_m3s = restriction_rule_m3s,
                                  restriction_rotation = restriction_rotation)

## -----------------------------------------------------------------------------
str(as.list(environment(fIrrigation)))

## -----------------------------------------------------------------------------
CreateController(sv,
                 ctrl.id = "Irrigation",
                 Y = "54032",
                 U = c("Irrigation1", "Irrigation2"),
                 FUN = fIrrigation)

## -----------------------------------------------------------------------------
IndPeriod_Run <- seq(
  which(DatesR == (DatesR[1] + 365*24*60*60)), # Set aside warm-up period
  length(DatesR) # Until the end of the time series
)
IndPeriod_WarmUp = seq(1,IndPeriod_Run[1]-1)
RunOptions <- CreateRunOptions(IM_Irrig,
                               IndPeriod_WarmUp = IndPeriod_WarmUp,
                               IndPeriod_Run = IndPeriod_Run)
ParamV02 <- readRDS(system.file("vignettes", "ParamV02.RDS", package = "airGRiwrm"))

## -----------------------------------------------------------------------------
OM_Irrig <- RunModel(sv, RunOptions = RunOptions, Param = ParamV02)

## -----------------------------------------------------------------------------
Qm3s <- attr(OM_Irrig, "Qm3s")
Qm3s <- Qm3s[Qm3s$DatesR > "2003-02-25" & Qm3s$DatesR < "2003-10-05",]
oldpar <- par(mfrow=c(2,1), mar = c(2.5,4,1,1))
plot(Qm3s[, c("DatesR", "54095", "54001", "54032")], main = "", xlab = "")
plot(Qm3s[, c("DatesR", "Irrigation1", "Irrigation2")], main = "", xlab = "", legend.x = "bottomright")
par(oldpar)

