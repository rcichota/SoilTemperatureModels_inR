library(gsubfn)

init_soiltemperatureswat <- function (VolumetricWaterContent,
         LayerThickness,
         LagCoefficient,
         AirTemperatureAnnualAverage,
         BulkDensity,
         SoilProfileDepth){
    SoilTemperatureByLayers<- vector()
    SoilTemperatureByLayers <-  rep(0.0,length(LayerThickness))
    for( i in seq(0, length(LayerThickness)-1, 1)){
        SoilTemperatureByLayers[i+1] <- as.double(15)
    }
    return(list("SoilTemperatureByLayers" = SoilTemperatureByLayers))
}

model_soiltemperatureswat <- function (VolumetricWaterContent,
         SurfaceSoilTemperature,
         LayerThickness,
         LagCoefficient,
         SoilTemperatureByLayers,
         AirTemperatureAnnualAverage,
         BulkDensity,
         SoilProfileDepth){
    #'- Name: SoilTemperatureSWAT -Version: 001, -Time step: 1
    #'- Description:
    #'            * Title: SoilTemperatureSWAT model
    #'            * Authors: simone.bregaglio
    #'            * Reference: http://bioma.jrc.ec.europa.eu/ontology/JRC_MARS_biophysical_domain.owl
    #'            * Institution: University Of Milan
    #'            * ExtendedDescription: Strategy for the calculation of soil temperature with SWAT method. Reference: Neitsch,S.L., Arnold, J.G., Kiniry, J.R., Williams, J.R., King, K.W. Soil and Water Assessment Tool. Theoretical documentation. Version 2000. http://swatmodel.tamu.edu/media/1290/swat2000theory.pdf
    #'            * ShortDescription: Strategy for the calculation of soil temperature with SWAT method
    #'- inputs:
    #'            * name: VolumetricWaterContent
    #'                          ** description : Volumetric soil water content
    #'                          ** inputtype : variable
    #'                          ** variablecategory : exogenous
    #'                          ** datatype : DOUBLEARRAY
    #'                          ** len : 
    #'                          ** max : 0.8
    #'                          ** min : 0
    #'                          ** default : 0.25
    #'                          ** unit : m3 m-3
    #'            * name: SurfaceSoilTemperature
    #'                          ** description : Average surface soil temperature
    #'                          ** inputtype : variable
    #'                          ** variablecategory : auxiliary
    #'                          ** datatype : DOUBLE
    #'                          ** max : 60
    #'                          ** min : -60
    #'                          ** default : 25
    #'                          ** unit : degC
    #'            * name: LayerThickness
    #'                          ** description : Soil layer thickness
    #'                          ** inputtype : parameter
    #'                          ** parametercategory : constant
    #'                          ** datatype : DOUBLEARRAY
    #'                          ** len : 
    #'                          ** max : 3
    #'                          ** min : 0.005
    #'                          ** default : 0.05
    #'                          ** unit : m
    #'            * name: LagCoefficient
    #'                          ** description : Lag coefficient that controls the influence of the previous day's temperature on the current day's temperature
    #'                          ** inputtype : parameter
    #'                          ** parametercategory : constant
    #'                          ** datatype : DOUBLE
    #'                          ** max : 1
    #'                          ** min : 0
    #'                          ** default : 0.8
    #'                          ** unit : dimensionless
    #'            * name: SoilTemperatureByLayers
    #'                          ** description : Soil temperature of each layer
    #'                          ** inputtype : variable
    #'                          ** variablecategory : state
    #'                          ** datatype : DOUBLEARRAY
    #'                          ** len : 
    #'                          ** max : 60
    #'                          ** min : -60
    #'                          ** default : 15
    #'                          ** unit : degC
    #'            * name: AirTemperatureAnnualAverage
    #'                          ** description : Annual average air temperature
    #'                          ** inputtype : parameter
    #'                          ** parametercategory : constant
    #'                          ** datatype : DOUBLE
    #'                          ** max : 50
    #'                          ** min : -40
    #'                          ** default : 15
    #'                          ** unit : degC
    #'            * name: BulkDensity
    #'                          ** description : Bulk density
    #'                          ** inputtype : parameter
    #'                          ** parametercategory : constant
    #'                          ** datatype : DOUBLEARRAY
    #'                          ** len : 
    #'                          ** max : 1.8
    #'                          ** min : 0.9
    #'                          ** default : 1.3
    #'                          ** unit : t m-3
    #'            * name: SoilProfileDepth
    #'                          ** description : Soil profile depth
    #'                          ** inputtype : parameter
    #'                          ** parametercategory : constant
    #'                          ** datatype : DOUBLE
    #'                          ** max : 50
    #'                          ** min : 0
    #'                          ** default : 3
    #'                          ** unit : m
    #'- outputs:
    #'            * name: SoilTemperatureByLayers
    #'                          ** description : Soil temperature of each layer
    #'                          ** datatype : DOUBLEARRAY
    #'                          ** variablecategory : state
    #'                          ** len : 
    #'                          ** max : 60
    #'                          ** min : -60
    #'                          ** unit : degC
    SoilProfileDepthmm <- SoilProfileDepth * 1000
    TotalWaterContentmm <- as.double(0)
    for( i in seq(0, length(LayerThickness)-1, 1)){
        TotalWaterContentmm <- TotalWaterContentmm + (VolumetricWaterContent[i+1] * LayerThickness[i+1])
    }
    TotalWaterContentmm <- TotalWaterContentmm * 1000
    MaximumDumpingDepth <- as.double(0)
    DumpingDepth <- as.double(0)
    ScalingFactor <- as.double(0)
    DepthBottom <- as.double(0)
    RatioCenter <- as.double(0)
    DepthFactor <- as.double(0)
    DepthCenterLayer <- LayerThickness[1] * 1000 / 2
    MaximumDumpingDepth <- 1000 + (2500 * BulkDensity[1] / (BulkDensity[1] + (686 * exp(-5.63 * BulkDensity[1]))))
    ScalingFactor <- TotalWaterContentmm / ((0.356 - (0.144 * BulkDensity[1])) * SoilProfileDepthmm)
    DumpingDepth <- MaximumDumpingDepth * exp(log(500 / MaximumDumpingDepth) * ((1 - ScalingFactor) / (1 + ScalingFactor)) ^ 2)
    RatioCenter <- DepthCenterLayer / DumpingDepth
    DepthFactor <- RatioCenter / (RatioCenter + exp(-0.867 - (2.078 * RatioCenter)))
    SoilTemperatureByLayers[1] <- LagCoefficient * SoilTemperatureByLayers[1] + ((1 - LagCoefficient) * (DepthFactor * (AirTemperatureAnnualAverage - SurfaceSoilTemperature) + SurfaceSoilTemperature))
    for( i in seq(1, length(LayerThickness)-1, 1)){
        DepthBottom <- DepthBottom + (LayerThickness[(i - 1)+1] * 1000)
        DepthCenterLayer <- DepthBottom + (LayerThickness[i+1] * 1000 / 2)
        MaximumDumpingDepth <- 1000 + (2500 * BulkDensity[i+1] / (BulkDensity[i+1] + (686 * exp(-5.63 * BulkDensity[i+1]))))
        ScalingFactor <- TotalWaterContentmm / ((0.356 - (0.144 * BulkDensity[i+1])) * SoilProfileDepthmm)
        DumpingDepth <- MaximumDumpingDepth * exp(log(500 / MaximumDumpingDepth) * ((1 - ScalingFactor) / (1 + ScalingFactor)) ^ 2)
        RatioCenter <- DepthCenterLayer / DumpingDepth
        DepthFactor <- RatioCenter / (RatioCenter + exp(-0.867 - (2.078 * RatioCenter)))
        SoilTemperatureByLayers[i+1] <- LagCoefficient * SoilTemperatureByLayers[i+1] + ((1 - LagCoefficient) * (DepthFactor * (AirTemperatureAnnualAverage - SurfaceSoilTemperature) + SurfaceSoilTemperature))
    }
    return (SoilTemperatureByLayers)
}