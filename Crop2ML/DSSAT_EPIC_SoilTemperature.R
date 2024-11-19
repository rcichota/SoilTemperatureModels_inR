#' The EPIC soil temperature model from DSSAT in R
#' 
#' @description Collection of functions for running the EPIC soil temperature model from DSSAT framework.
#'              This was coverted into R using the Crop2ML framework.
#'
#' @param siteData data.frame with information about the site fro which the simulations will be run for.
#'        This includes: 
#' @param soilGeneralData data.frame with information about the soil type. Includes
#' @param soilLayerData data.frame with parameters for the soil profile (by layer). This includes:
#' @param weatherData data.frame with daily weather data. Need to include.
#' @param simulationInputData list of parameters specific for running this model
#'
#' @return a data.frame with the simulated values for soil temperature
#'
#' @example
#'
#' @export
# Blame to RCichota

source(file.path(dirname(sys.frame(1)$ofile), "stemp_epic.r"))

DSSAT_EPIC_SoilTEmp <- function(siteData, soilGeneralData, soilLayerData, weatherData, simulationInputData)
{
  # initialise a series of variable to collect output of interest
  simulatedDate <- c()
  simulatedDepth <- c()
  simulatedSoilTempAve <- c()
  simulatedSoilTempMin <- c()
  simulatedSoilTempMax <- c()
  
  # define some variables specific to a given run/simulation
  firstDay <- 1
  lastDay <- 10
  soilWaterContent <- simulationInputData$soilWaterContent
  biomassAmount <- simulationInputData$biomassAmount
  waterSwitch <- simulationInputData$waterSwitch
  mulchAmount <- simulationInputData$mulchAmount
  irrigationAmount <- simulationInputData$irrigationAmount
  
  # set model initialisation
  initialState <- init_stemp_epic(
    NL=nrow(soilLayerData),
    ISWWAT=waterSwitch,
    BD=soilLayerData$SLBDM,
    DLAYR=soilLayerData$THICK,
    DS=soilLayerData$SLLB,
    DUL=soilLayerData$SLDUL,
    LL=soilLayerData$SLLL,
    NLAYR=nrow(soilLayerData),
    TAMP=siteData$TAMP,
    RAIN=weatherData$RAIN[1],
    SW=soilWaterContent,
    TAVG=weatherData$T2M[1],
    TMAX=weatherData$TMAX[1],
    TMIN=weatherData$TMIN[1],
    TAV=siteData$TAV,
    DEPIR=irrigationAmount[1],
    BIOMAS=biomassAmount,
    MULCHMASS=mulchAmount,
    SNOW=weatherData$SNOW[1])
  
  # collect some variables that are needed on CalculateModel
  previousCUMDPT <- initialState$CUMDPT
  previousDSMID <- initialState$DSMID
  previousTDL <- initialState$TDL
  previousTMA <- initialState$TMA
  previousNDays <- initialState$NDays
  previousWetDay <- initialState$WetDay
  previousX2_PREV <- initialState$X2_PREV
  previousSRFTEMP <- initialState$SRFTEMP
  previousST <- initialState$ST
  
  # run model for the duration of the weather file
  for (day in 1:nrow(weatherData))
  {
    onProcessState <- model_stemp_epic(
      BD=soilLayerData$SLBDM,
      RAIN=weatherData$RAIN[day],
      NDays=previousNDays,
      DEPIR=irrigationAmount[day],
      TMIN=weatherData$TMIN[day],
      WetDay=previousWetDay,
      DUL=soilLayerData$SLDUL,
      BIOMAS=biomassData,
      DS=soilLayerData$SLLB,
      TAMP=siteData$TAMP,
      DLAYR =soilLayerData$THICK,
      MULCHMASS=mulchAmount,
      LL=soilLayerData$SLLL,
      TDL=previousTDL,
      X2_PREV=previousX2_PREV,
      SW=soilWaterData,
      DSMID=previousDSMID,
      NLAYR=nrow(soilLayerData),
      TMAX=weatherData$TMAX[day],
      TAV=siteData$TAV,
      SNOW=weatherData$SNOW[day],
      TMA=previousTMA,
      TAVG=weatherData$T2M[day],
      SRFTEMP=previousSRFTEMP,
      ST=previousST,
      NL=nrow(soilLayerData),
      ISWWAT=doWater,
      CUMDPT=previousCUMDPT)

    # collect some variables that are needed for next iteration
    previousCUMDPT <- onProcessState$CUMDPT
    previousDSMID <- onProcessState$DSMID
    previousTDL <- onProcessState$TDL
    previousTMA <- onProcessState$TMA
    previousNDays <- onProcessState$NDays
    previousWetDay <- onProcessState$WetDay
    previousX2_PREV <- onProcessState$X2_PREV
    previousSRFTEMP <- onProcessState$SRFTEMP
    previousST <- onProcessState$ST

    # set date in correct format
    todaysDate <- as.Date(as.numeric(substr(weatherData$DATE[day], 5, 7)) - 1, as.Date(paste0(substr(weatherData$DATE[day], 1, 4), "-01-01")))

    # collect output data
    simulatedDate <- c(simulatedDate, rep(as.character(todaysDate), nrow(soilLayerData) + 1))
    simulatedDepth <- c(simulatedDepthTop, c(0.0, previousDSMID))
    simulatedSoilTempAve <- c(simulatedSoilTempAve, c(round(onProcessState$SRFTEMP, 6), round(onProcessState$ST, 6)))
    simulatedSoilTempMin <- c(simulatedSoilTempMin, rep("na", nrow(soilLayerData) + 1))
    simulatedSoilTempMax <- c(simulatedSoilTempMax, rep("na", nrow(soilLayerData) + 1))
  }
  
  # prepare the output table
  outputData <- data.frame(simulatedDate, simulatedDepth,simulatedSoilTempAve, simulatedSoilTempMin, simulatedSoilTempMax)
  colnames(outputData) <- c("Date", "Depth", "Temp_Avg", "Temp_Min", "Temp_Max")

  return(outputData)
}
