##Fehyl-Buska data

FBData <- initializeDataFrame()
Lipid1 <- "FehylBuska_pH0_3picrophilus.xlsx" ##Placeholder. Automating Lipid incorporation is harder than I thought it would be
FBData <- addNewRow(FBData, 1, 58, 75, 0.3, "FehylBuska_pH0_3picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 2, 58, 75, 0.5, "FehylBuska_pH0_5picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 3, 58, 75, 0.7, "FehylBuska_pH0_7picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 4, 58, 75, 0.9, "FehylBuska_pH0_9picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 5, 58, 75, 1.1, "FehylBuska_pH1_1picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 6, 53, 75, 0.7, "FehylBuska_Temp53_picrophilus.xlsx", Lipid1, "xlsx")
FBData <- addNewRow(FBData, 7, 63, 75, 0.7, "FehylBuska_Temp63_picrophilus.xlsx", Lipid1, "xlsx")
write.csv(FBData, file = "FehylBuskaGrowthData.csv")
FBDataAppended <-  read.csv("FBDataWithGDGT.csv", stringsAsFactors = FALSE)
FBpHData <- subset(FBDataAppended, Temp == 58)
FBTempData <- subset(FBDataAppended, pH == 0.7)

FBpHQuadPlot <- QuadPlot(FBpHData, "pH", "Fehyl-Buska_pH_experiments", RIAxisMin = 0, RIAxisMax = 3.5, RateAxisMax = 30)
FBTempQuadPlot <- QuadPlot(FBTempData, "Temp", "Fehyl-Buska_Temp_experiments", RIAxisMin = 0, RIAxisMax = 3.5, RateAxisMax = 30)
FBTempAllQuadPlot <- QuadPlot(FBDataAppended, "Temp", "Fehyl-Buska_Temp_all", RIAxisMin = 0, RIAxisMax = 3.5, RateAxisMax = 30)
FBpHAllQuadPlot <- QuadPlot(FBDataAppended, "pH", "Fehyl-Buska_pH_all", RIAxisMin = 0, RIAxisMax = 3.5, RateAxisMax = 30)


#Elling Data
EllingData <- initializeDataFrame()
Lipid1 <- "EarlyGrowthNMaritimus.xlsx" ##Place holder for automatic lipid inclusion
EllingData<- addNewRow(EllingData, 1, 28, "EG", 7.5, "EarlyGrowthNMaritimus.xlsx", Lipid1,"xlsx")
EllingData<- addNewRow(EllingData, 2, 28, "LG", 7.5, "LateGrowthNMaritimus.xlsx", Lipid1,"xlsx")
EllingData<- addNewRow(EllingData, 3, 28, "ES", 7.5, "EarlyStationaryNMaritimus.xlsx",Lipid1, "xlsx")
EllingData<- addNewRow(EllingData, 4, 28, "LS", 7.5, "LateStationaryNMaritimus.xlsx", Lipid1,"xlsx")
write.csv(EllingData, file = "EllingGrowthData.csv")
EllingDataAppended <- read.csv("EllingGrowthData.csv", stringsAsFactors = FALSE)
EllingDataAppended$Phase <- as.factor(EllingDataAppended$Phase)
EllingDataAppended$Phase <- factor(EllingDataAppended$Phase, levels(EllingDataAppended$Phase)[c(1,3,2,4)])


EllingPhaseQuadPlot <- QuadPlot(EllingDataAppended, "Phase", "EllingGrowthExperiments", RIAxisMin = 1.5, RIAxisMax = 3.5, RateAxisMin = 45, RateAxisMax = 65)

#Hurley Data
HurleyData <- read.csv("HurleyDataCompressed.csv", stringsAsFactors = FALSE) 
HurleyQuadPlot <- QuadPlot(HurleyData, "Dilution", "HurleyChemostatExperiments", RIAxisMin = 2.5, RIAxisMax = 3.5, RateAxisMin = 20, RateAxisMax = 75, calculateRates = FALSE)
