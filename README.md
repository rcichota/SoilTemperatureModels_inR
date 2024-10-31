# SoilTemperatureModels_inR

Code to run soil temperature models from Crop2ML in R environment  

This takes a number of soil temperature models that have been (re)coded in R language using Crop2ML. Original models come from a variety of modelling platforms/frameworks (DSSAT, APSIM, Bioma, etc.) and were isolated to only account for soil temperature processes.  
In this project the various models are set to run in an RStudio project in order to simulate a range of scenarios designed to primarily test whether Crop2ML converted the various models appropriately (i.e. the results from re-runs match the set of reference values, produced in the native code). As the scenarios cover a range of climatic and edaphic conditions, the results will also be used for a sensitivity analysis.

The code for the various soil temperature models were sourced from: https://github.com/AgriculturalModelExchangeInitiative/Crop2ML

Team:
 Rogerio Cichota, Jingjing Zhang
 Plant and Food Research