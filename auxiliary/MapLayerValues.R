#' Auxiliary soil functions
#'
#' @description Collection of functions to re-map values from one array to another with different number of elements.
#'              These are aimed at re-mapping values in arrays describing in a soil profile (collection of layers).
#'              This comprises two basic arrays, one containing the values for the thickness of each soil layer, and
#'              another with the respective values of a given parameter or property (e.g. water content, temperature).
#'              Generally these are set at initialisation based on real soil descriptions or user preferences. However
#'              some functions require specific layering, so the values might need to be re-mapped to the new structure
#'              and then back. This is what the functions in this file do. Note that variables come in two 'flavours',
#'              the values may represent either amounts (e.g. the amount of nitrogen in each layer, in kg/m2) or as
#'              concentrations (e.g. amount of nitrogen per unit of soil mass in each layer, in kg/kg). These have to
#'              be treated differently to map them correctly (ensure mass balance is maintained). Which is why there
#'              are two mapping functions here, one for each case; plus one to eventually interpolate the values.
#'              Note that if the sum of thicknesses (total soil depth) in the new structure is different than the one
#'              in the original layering, the values are truncated (if new soil depth is smaller than the original)
#'              or the value of the last layer is repeated (as concentration) to fill in the values needed.
#'
#' @param fromValues array with the values for the thickness of each layer in the original structure
#' @param fromThickness array with the original values of the variable to be re-mapped
#' @param toThickness array with the thickness of the soil layers in the new structure
#'
#' @return array with the values of the given variable values mapped to the new array structure
#'
#' @example
#' originalSoilThickness <- c(50, 150, 300, 500)
#' originalSoilWater <- c(25, 60, 90, 175)
#' newSoilThickness <- c(50, 50, 50, 50, 100, 100, 100, 100, 100, 100, 100, 100)
#' newSoilWater <-  MapAmountsByLayer(originalSoilWater, originalSoilThickness, newSoilThickness)
#'  this should return:
#' newSoilWater => c(25, 20, 20, 20, 30, 30, 30, 35, 35, 35, 35, 35)
#'
#' @export
# Blame to RCichota


MapAmountsByLayer <- function(fromValues, fromThickness, toThickness)
{
  # get new arrays with the accumulate values of the original arrays
  cumFromThickness <- fromThickness[1]
  cumFromValues <- fromValues[1]
  for (z in 2:length(fromThickness))
  {
    cumFromThickness <- c(cumFromThickness, cumFromThickness[z-1]+fromThickness[z])
    cumFromValues <- c(cumFromValues, cumFromValues[z-1]+fromValues[z])
    # working with cumulative values ensure that a simple linear interpolation is enough
    #  to get the value at any depth. To estimate value by layer, one needs to get the
    #  cumulative values at the beginning and the end of 'new' layer and subtract them...
  }

  # interpolate new values
  numNewLayers <- length(toThickness)
  newValues <- rep(0.0, numNewLayers)
  layerTop <- 0
  layerBottom <- toThickness[1]
  for (z in 1:numNewLayers)
  {
    cumNewValueTop <- linearInterpolation(layerTop, cumFromThickness, cumFromValues)
    cumNewValueBottom <- linearInterpolation(layerBottom, cumFromThickness, cumFromValues)
    
    newValues[z] <- cumNewValueBottom - cumNewValueTop
    
    if (z < numNewLayers)
    {
      layerTop <- layerBottom
      layerBottom <- layerBottom + toThickness[z+1]
    }
  }

  return (newValues)
}


MapConcentrationByLayer <- function(fromValues, fromThickness, toThickness)
{
  # convert values from concentration to amounts
  baseValues <- fromValues*fromThickness
  
  # re-map amount values to new thickness  
  newValues = MapAmountsByLayer(baseValues, fromThickness, toThickness);
  
  # convert new amounts into concentration
  return(newValues/toThickness)
}

# returns the value of y corresponding to a given x, using linear interpolation on X and Y arrays
# Note that the arrays are assumed to be given in ascending order (i.e. with cumulative values)
linearInterpolation <- function(givenX, xValsArray, yValsArray)
{
  # find where x lies in the x array
  pos <- which.min(replace(xValsArray - givenX, (xValsArray - givenX) < 0, NA))
  if (identical(pos, integer(0)))
  {  # empty index returned (passed the array length), use last value
    pos <- length(xValsArray)
  }

  # find the matching y (interpolated)
  if (pos==1)  # first element, 0th value is assumed zero
  {
    newY  <- givenX * yValsArray[pos] / xValsArray[pos]
  } else
  {
    newY <- yValsArray[pos - 1] + (givenX - xValsArray[pos - 1]) * (yValsArray[pos] - yValsArray[pos - 1]) / (xValsArray[pos] - xValsArray[pos - 1])
  }

  return(newY)
}
