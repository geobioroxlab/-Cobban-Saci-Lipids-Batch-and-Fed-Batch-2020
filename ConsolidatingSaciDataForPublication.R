require(data.table)
require(readxl)
require(ggplot2)
require(reshape2)
require(vegan)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
library(car)
library(EnvStats)
require(userfriendlyscience)

dataframe1combined <- read.csv("dataframe1combined.csv")
AZData <- read.csv("AZData.csv")

#Create red blue color pallette for n colors
mypal <- colorRampPalette( brewer.pal( 6 , "RdBu" ) )
##Define graphing constants
RIaxisMin = 1.5
RIaxisMax = 5.0
RateAxisMin = 0
RateAxisMax = 120
SacrificeMin = 0.4
SacrificeMax = 2.3
PointSize<- 5
strokeSize <- 3
LogODMin <- -8
LogODMax <- 0.8
TimeMin <- 0
TimeMax <- 200
minorAxisLength <- 0.15
axisTickSize <- 2
GrowthCurvePointSize = 15
myTheme = list(theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(face = "bold", size = 40), axis.text = element_text(size = 36, colour = "black"),strip.text = element_text(size = 36, colour = "black"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside"))

dataframe1combined$Rate <- lapply(dataframe1combined$GrowthCurves, makeGrowthRates)
#Performed to allow for reading of the rates as a vector not a list
dataframe1combined$Rate <- unlist(dataframe1combined$Rate)
#Consolidating data and running linear models
TempData <- subset(dataframe1combined, pH == 3.00 & RPM == 200 & Sampler =="ABC")
aScat <- ggplot(TempData, aes(x = as.factor(Temp), y = 1/Rate)) +
  labs(x = "Temperature", y = "Doubling Time (hours)")+
  geom_point(stat = "identity", color = "goldenrod", size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RateAxisMin, RateAxisMax )) + myTheme +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))

pHData <- subset(dataframe1combined, Temp == 70 & RPM == 200 & Sampler=="ABC")
bScat <- ggplot(pHData, aes(x = as.factor(pH), y = 1/Rate)) +
  labs(x = "pH", y = "Doubling Time (hours)")+
  geom_point(stat = "identity", color = "dodgerblue",  size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RateAxisMin, RateAxisMax))+ myTheme +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))



shakingData <-  subset(dataframe1combined, Temp == 70 & pH == 3.00& Sampler=="YZ")
cScat <- ggplot(shakingData, aes(x = RPM, y = 1/Rate)) +
  labs(x = "RPM", y = "Doubling Time (hours)/hr")+
  geom_point(stat = "identity", color = "darkorchid",  size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RateAxisMin, RateAxisMax)) + myTheme+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))




dScat <- ggplot(TempData, aes(x = as.factor(Temp), y = RI)) +
  labs(x = "Temperature", y = "RI")+
  geom_point(stat = "identity", color = "goldenrod", size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) +
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.ticks.y=element_line(color = "black", size = axisTickSize))+
  myTheme +theme(plot.margin = margin(5.5, 5.5, 5.5, 45))


eScat <- ggplot(pHData, aes(x = as.factor(pH), y = RI)) +
  labs(x = "pH", y = "RI")+
  geom_point(stat = "identity", color = "dodgerblue",  size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) + myTheme +
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))

fScat <- ggplot(shakingData, aes(x = RPM, y = RI)) +
  labs(x = "RPM", y = "RI")+
  geom_point(stat = "identity", color = "darkorchid",  size = PointSize, shape = 1, stroke = strokeSize)+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) +myTheme+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))


##Set up all O2 sparge control experiments

#Required datafames/manipulations
#dOdf(Primary data frame containing most all data)
dOdf <- read.csv("dOdf.csv")
#An experiment number variable to make the next functions work.
dOdf$Experiment = 1

#GrowthCurvedOData(Contains average growth curve from each experiment)
#DataframedO2(Contains GDGT melted data)
normalizedGDGTs <- c("GDGT.0.Normalized", "GDGT.1.Normalized", "GDGT.2.Normalized", "GDGT.3.Normalized","GDGT.3.iso.Normalized", "GDGT.4.Normalized", "GDGT.4.iso.Normalized", "GDGT.5.Normalized", "GDGT.5.iso.Normalized","GDGT.6.Normalized", "GDGT.7.Normalized", "GDGT.8.Normalized")
GrowthCurvedOData <- AverageGrowthCurve(dOdf, 3, 70, 20,1,1)
GrowthCurvedOData <- rbind(GrowthCurvedOData, AverageGrowthCurve(dOdf, 3, 70, 2, 1,2))
GrowthCurvedOData <- rbind(GrowthCurvedOData, AverageGrowthCurve(dOdf, 3, 70, 0.2,1,3))
GrowthCurvedOData <- rbind(GrowthCurvedOData, AverageGrowthCurve(dOdf, 3, 70, 0.5,1,4))
GrowthCurvedOData <- rbind(GrowthCurvedOData, AverageGrowthCurve(dOdf, 3, 70, 1,1,5 ))
##0.22 is the representation of the serially transferred 0.2% experiment in the case of this data. 
GrowthCurvedOData <- rbind(GrowthCurvedOData, AverageGrowthCurve(dOdf, 3, 70, 0.22,1, 6))
dataframedO2 <- melt(dOdf, id = c("ID","Temp", "RPM", "pH", "Replicate", "GrowthCurves","LipidPeaks", "Rate", "RI", "Phase", "SacrificeOD", "Experiment", normalizedGDGTs))


mydOPlot = ggplot(GrowthCurvedOData, aes(x = timeElapsed, y = (mean), color = as.factor(ID))) +
  geom_line() +
  geom_point()+ myTheme+ theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA)) +
  geom_errorbar(aes(x = timeElapsed, ymin = mean-se, ymax = mean+se))+
  scale_color_manual(breaks = c("1", "2","5", "4", "6","3"),labels = c("20%", "2%","1%", "0.5%" ,"0.2%*","0.2%"), name = "Experimental Conditions", values = c("dodgerblue1","dodgerblue2","darkorchid1", "dodgerblue4","darkorchid2","dodgerblue3"))+
  xlab("Time Since Start of Experiment")+ ylab("OD600")
mydOPlot

myLogdOPlot = ggplot(GrowthCurvedOData, aes(x = timeElapsed, y = log(mean), color = as.factor(ID))) +
  geom_line() +
  geom_point()+ myTheme+ theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA), axis.ticks = element_line(color = "black", size = axisTickSize)) +
  geom_errorbar(aes(x = timeElapsed, ymin = log(mean-se), ymax = log(mean+se)))+
  scale_color_manual(breaks = c("1", "2","5", "4", "6","3"),labels = c("20%", "2%","1%", "0.5%" ,"0.2%*","0.2%"), name = "Experimental Conditions", values = c("dodgerblue1","dodgerblue2","darkorchid1", "dodgerblue4","darkorchid2","dodgerblue3"))+
  xlab("Time Since Start of Experiment")+ ylab("Log(OD600)") + coord_cartesian(ylim = c(LogODMin, LogODMax), xlim = c(TimeMin, TimeMax))
myLogdOPlot

dataframedOLateLog <- subset(dataframedO2, Phase == "LL")
dOGDGTLateLog <- ggplot(dataframedOLateLog, aes(x=as.factor(Replicate), y= value, fill=forcats::fct_rev(variable))) +  
  facet_wrap(~RPM, nrow = 1, strip.position = "bottom")+
  
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right") + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4","GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0"))+
  labs(x="dO", y =  "Relative Abundance") 

dOGDGTLateLog

dataframedOEarlyStationary <- subset(dataframedO2, Phase =="ES")
dOGDGTEarlyStationary <- ggplot(dataframedOEarlyStationary, aes(x=as.factor(Replicate), y= value, fill=forcats::fct_rev(variable))) +  
  facet_wrap(~RPM, nrow = 1, strip.position = "bottom")+
  
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right") + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4","GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0"))+
  labs(x="dO", y =  "Relative Abundance") 

dOGDGTEarlyStationary

#Produce a 
n <-ggplot(data = dOdf, aes(x = 1/Rate, y = RI, color=Phase)) + 
  labs(x = "Doubling Time (Hours)", y ="RI", shape = expression("O"["2"]*"%)"))+
  geom_point(size = 5, aes(shape = as.factor(RPM),text =sprintf("dO: %s", dOdf$RPM))) +
  scale_shape_manual(values = (c(16,17,18,2,1,15)), labels = c("0.2", "0.2*", "0.5", "1", "2", "20"))+
  geom_smooth(method = "lm", size =2) + myTheme + theme(legend.position=c(0.9, 0.8), legend.text = element_text(size = 40), legend.title = element_text(size = 40), legend.key = element_rect(colour = "white"), strip.background = element_blank()) + coord_cartesian(ylim = c(RIaxisMin,RIaxisMax), xlim = c(RateAxisMin, RateAxisMax))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.ticks.y=element_line(color = "black", size = axisTickSize))


n

##Summarize just the late log data
dODataLL <- subset(dOdf, Phase=="LL")
dORateSummary <- getRateSummaryStats(dODataLL, dODataLL$RPM)
dORISummaryLL <- getRISummaryStats(dODataLL, dODataLL$RPM)


doRatePlot <- ggplot(dORateSummary, aes(x = as.factor(summarystat), y = mean, fill = as.factor(summarystat))) +
  labs(x = "dO (%)", y = "Doublings/hr")+
  geom_bar(stat = "identity", fill = "darkorange")+
  geom_errorbar(aes(ymin = mean-se, ymax=mean+se)) +
  coord_cartesian(ylim = c(RateAxisMin, RateAxisMax)) + myTheme
doRatePlot

#Summarize Early stationary data
dODataES <- subset(dOdf, Phase=="ES")
dORISummaryES <- getRISummaryStats(dODataES, dODataES$RPM)

##Combine Late Log and Early Stationary data for GDGT 
##Don't need to do this for rate because the growth curve is the same and we are using average rates
dORISummaryLL$Phase <- "LL"
dORISummaryES$Phase <- "ES"
dORISummary <- rbind(dORISummaryES, dORISummaryLL)


dORIPlot <- ggplot(dORISummary, aes(x = (summarystat), y = mean, fill = Phase)) +
  labs(x = "dO(%)", y = "RI")+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax=mean+se), position = "dodge")+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) + myTheme + theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA))
dORIPlot

dORIScatter <- ggplot(dOdf, aes(x = log10(RPM), y = RI, color = Phase, shape = as.factor(RPM))) +
  labs(x = expression(bold("Log(O"["2"]*"%)")), y = "RI") +
  geom_point(size = PointSize, stroke = strokeSize)+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) + myTheme + theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA)) +
  scale_x_continuous(sec.axis = sec_axis(trans = (~10^.),name = element_blank(), breaks = c(0.2, 0.5, 1, 2, 20), labels = c("", "", "", "",""))) +
  scale_shape_manual(values = c(1,19,1,1,1,1))+ 
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))

dORIScatter
dORateScatter <- ggplot(dOdf, aes(x = log10(RPM), y = 1/Rate, shape = as.factor(RPM))) +
  labs(x = element_blank(), y = element_blank()) + 
  geom_point(size = 5, color = "darkgreen", stroke = strokeSize, show.legend = FALSE)+
  coord_cartesian(ylim = c(RateAxisMin, RateAxisMax)) + myTheme + theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA)) +
  scale_x_continuous(breaks = c(-0.5, 0.0, 0.5, 1.0), labels = c("","","","") ,sec.axis = sec_axis(trans = (~10^.),name = expression(bold("Sparge Gas (O"["2"]*"%)")), breaks = c(0.2, 0.5, 1, 2, 20), labels = c(0.2, 0.5, 1, 2, 20)))+
  scale_shape_manual(values = c(1,19,1,1,1,1))+
  theme(axis.title.x.bottom  =element_blank(),axis.text.x.bottom =element_blank(),axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length = unit(minorAxisLength, "cm"))+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))

dORateScatter

grid.arrange(dORateScatter, dORIScatter, ncol = 1)

grid.arrange(aScat,bScat,cScat,dORateScatter,dScat,eScat,fScat, dORIScatter, ncol=4)


dOGDGT <- ggplot(dataframedO2, aes(x=Replicate, y= value, fill=forcats::fct_rev(variable))) +  
  facet_wrap(~RPM+Phase, nrow = 1, strip.position = "bottom", labeller = )+
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right") + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4","GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("8", "7", "6", "5-iso", "5","4-iso", "4", "3-iso", "3", "2", "1", "0"))+
  labs(x=expression(bold("Sparge Gas (O"["2"]*"%)")), y =  "Relative Abundance") + scale_x_discrete()+
  theme(plot.margin = margin(30, 30, 5.5, 5.5))
dOGDGT
dOdf$GrowthCurves <- paste(dOdf$GrowthCurves)
dOdf$SacrificeOD <- lapply(dOdf$GrowthCurves, getSacrificeOD)



dataframe2 <- melt(dataframe1combined, id = c("Sampler","Temp", "RPM", "pH", "Replicate", "GrowthCurves","RI", "Rate", "SacrificeOD", normalizedGDGTs))
TempGDGTData <- subset(dataframe2, pH == 3.00 & RPM == 200)
tgdgt <- ggplot(TempGDGTData, aes(x=Replicate, y= value, fill=forcats::fct_rev(variable))) + facet_wrap(~Temp, nrow = 1, strip.position = "bottom") + 
  geom_bar(stat ='identity', position = "fill", show.legend = FALSE) + myTheme +theme(legend.position="right", strip.background = element_blank()) + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  
   labs(x="Temperature", y =  "Relative Abundance") +
  scale_x_discrete(labels=c("1", "2", "3", "4", "5"))+
  theme(plot.margin = margin(5.5, 5.5, 30, 5.5))
        
tgdgt
pHGDGTData <- subset(dataframe2, RPM==200 & Temp==70)
pHgdgt <- ggplot(pHGDGTData, aes(x=Replicate, y= value, fill=forcats::fct_rev(variable))) + facet_wrap(~pH, nrow = 1, strip.position = "bottom") + 
  geom_bar(stat ='identity', position = "fill", show.legend = FALSE) + myTheme +theme(legend.position="right", strip.background = element_blank()) + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  labs(x="pH") + theme(axis.text.y = element_blank(), axis.title.y = element_blank())+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5"))+
  theme(plot.margin = margin(30, 30, 30, 5.5))
pHgdgt
ShakingGDGTData <- subset(dataframe2, Sampler == "YZ"& Temp==70 &pH ==3.00)
Shakinggdgt <- ggplot(ShakingGDGTData, aes(x=Replicate, y= value, fill=forcats::fct_rev(variable))) + facet_wrap(~RPM, nrow = 1, strip.position = "bottom") + 
  geom_bar(stat ='identity', position = "fill", show.legend = FALSE) + myTheme +theme(legend.position="right", strip.background = element_blank()) + 
  #scale_fill_grey(name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  labs(x="Shaking Speed", y =  "Relative Abundance") +
  scale_x_discrete(labels=c("1", "2", "3", "4", "5"))+
  theme(plot.margin = margin(30, 5.5, 30, 30))
Shakinggdgt

grid.arrange(arrangeGrob(tgdgt, pHgdgt, ncol = 2), Shakinggdgt,dOGDGT, nrow =3)



##Start Data for publication graphs
TempData <- subset(dataframe1combined, pH == 3.00 & RPM == 200 & Sampler =="ABC")
TempSummary <- getRateSummaryStats(TempData, TempData$Temp)


pHData <- subset(dataframe1combined, Temp == 70 & RPM == 200 & Sampler=="ABC")



shakingData <-  subset(dataframe1combined, Temp == 70 & pH == 3.00& Sampler=="YZ")


ShakingScatter <- ggplot(shakingData, aes(x = (RPM), y = RI)) +
  labs(x = "RPM", y = "RI") +
  geom_point(size = 5,  color = "darkorchid")+
  coord_cartesian(ylim = c(RIaxisMin, RIaxisMax)) + myTheme +
  theme(legend.position = "right", legend.key = element_rect(fill = "transparent", color = NA)) 
ShakingScatter


o <-ggplot(data = TempData, aes(x = 1/Rate, y = RI)) + 
  labs(x = "Doubling Time (Hours)", y ="RI")+
  scale_x_continuous(position = "top", limits = c(RateAxisMin, RateAxisMax))+
  geom_point(size = 5, color= "goldenrod" ,aes(shape= as.factor(Temp))) +
  scale_shape_manual(values = c(16, 17, 15, 1))+
  geom_smooth(method = "lm", size =2, color ="goldenrod") + myTheme + ylim(RIaxisMin,RIaxisMax) +
  theme(legend.position=c(0.9,0.9),strip.background = element_blank(), legend.key = element_rect(fill = "white"), legend.text = element_text(size = 40), legend.title = element_text(size =40)) + labs(shape="Temperature (°C)")+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(axis.ticks.y=element_line(color = "black", size = axisTickSize))
o

p<- ggplot(data = pHData, aes(x = 1/Rate, y = RI)) + 
  labs(x = "Doubling Time (Hours)", y ="RI")+  scale_x_continuous(position = "top", limits = c(RateAxisMin,RateAxisMax))+
  geom_point(size = 5, color= "dodgerBlue" ,aes(shape= as.factor(pH))) +
  geom_smooth(method = "lm", size =2) + myTheme + ylim(RIaxisMin,RIaxisMax) +
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white")) + labs(shape="pH")+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))
p

q <- ggplot(data = shakingData, aes(x = 1/Rate, y = RI)) + 
  labs(x = "Doubling Time (Hours)", y ="RI", shape = "Shaking Speed")+  scale_x_continuous(position = "top", limits = c(RateAxisMin, RateAxisMax))+
  geom_point(size = 5, color= "darkorchid4", aes(shape = as.factor(RPM))) +
  geom_smooth(method = "lm", size =2) + myTheme + ylim(RIaxisMin,RIaxisMax) +
  scale_shape_manual(values = (c(16,17,18,2,1,15,6,0))) +
  theme(legend.position=c(0.9,0.9),strip.background = element_blank(), legend.key = element_rect(fill = "white")) + labs(shape="RPM")+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))
q
r<- ggplot(data = AZData, aes(x = 1/Rate, y = RI))+
  labs(x = "Doubling Time (Hours)", y ="RI")+
  geom_point(size = PointSize, color= "red") +
  geom_smooth(method = "lm", size =2) + myTheme +xlim(RateAxisMin, RateAxisMax)+ ylim(RIaxisMin,RIaxisMax) +
  theme(legend.position=c(0.9, 0.9),strip.background = element_blank(), legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_line(color = "black", size = axisTickSize))

r

grid.arrange(o,p,q,n,r,ncol=3 )

ShakingLong <- melt(shakingData, id = c("Temp", "RPM", "pH", "Replicate", "GrowthCurves","RI", "Rate", "SacrificeOD", "Sampler", normalizedGDGTs))

YZGDGT <- ggplot(ShakingLong, aes(x=as.factor(Replicate), y= value, fill=forcats::fct_rev(variable))) +  
  facet_wrap(~RPM, nrow = 1, strip.position = "bottom", labeller = label_value)+
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right") + 
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8","GDGT-7","GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso","GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0"))+
  labs(x="Shaking Speed", y =  "Relative Abundance") 
YZGDGT

TempGDGTData <- melt(TempData, id = c("Temp", "RPM", "pH", "Replicate", "GrowthCurves","RI", "Rate", "SacrificeOD", "Sampler", normalizedGDGTs))
tgdgt <- ggplot(TempGDGTData, aes(x=as.factor(Replicate), y= value, fill=forcats::fct_rev(variable))) + facet_wrap(~Temp, nrow = 1, strip.position = "bottom") + 
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right", strip.background = element_blank()) + 
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  labs(x="Temperature (°C)", y =  "Relative Abundance") 
tgdgt

pHGDGTData <- melt(pHData, id = c("Temp", "RPM", "pH", "Replicate", "GrowthCurves","RI", "Rate", "SacrificeOD", "Sampler", normalizedGDGTs))

pHgdgt <- ggplot(pHGDGTData, aes(x=as.factor(Replicate), y= value, fill=forcats::fct_rev(variable))) + facet_wrap(~pH, nrow = 1, strip.position = "bottom") + 
  geom_bar(stat ='identity', position = "fill") + myTheme +theme(legend.position="right", strip.background = element_blank()) + 
  scale_fill_manual(values = mypal(12), name = NULL, labels=c("GDGT-8", "GDGT-7", "GDGT-6", "GDGT-5-iso", "GDGT-5","GDGT-4-iso", "GDGT-4", "GDGT-3-iso", "GDGT-3", "GDGT-2", "GDGT-1", "GDGT-0")) +
  labs(x="pH", y =  "Relative Abundance") 
pHgdgt

pHGrowthCurveData <- AverageGrowthCurve(pHData, 2, 70, 200,1,1)
pHGrowthCurveData <- rbind(pHGrowthCurveData, AverageGrowthCurve(pHData, 4, 70, 200, 1,3))
pHGrowthCurveData <- rbind(pHGrowthCurveData, AverageGrowthCurve(pHData, 3, 70, 200,1,2))

myLogpHplot = ggplot(pHGrowthCurveData, aes(x = timeElapsed, y = log2(mean), group = as.factor(ID), shape = as.factor(ID))) +
  geom_line(color="dodgerblue", size=2) +
  geom_point(color="dodgerblue", size=GrowthCurvePointSize)+ myTheme +
  geom_errorbar(aes(x = timeElapsed, ymin = log2(mean)-Log2SE, ymax = log2(mean)+Log2SE), width = 4, size = 1)+
  scale_shape_manual(breaks = c("1", "3", "2"),labels = c("70  200  2", "70  200  3", "70  200  4"
  ),name="Experimental Conditions", values = c(16, 17, 15))+
  xlab(element_blank())+ ylab("Log(OD600)") + myTheme + theme(legend.position = "right",strip.background = element_blank(), legend.key = element_rect(fill = "white"), axis.ticks = element_line(color = "black", size = axisTickSize))+
  scale_y_continuous(sec.axis = sec_axis(trans = (~2^.),name = "OD600", breaks = c(0.01, 0.1, 0.5, 1, 2), labels = c(0.01, 0.1, 0.5, 1, 2))) +
  coord_cartesian(ylim = c(LogODMin, LogODMax), xlim = c(TimeMin, TimeMax-60))
myLogpHplot

TempGrowthCurveData <- AverageGrowthCurve(TempData, 3.00, 65, 200,1,1)
TempGrowthCurveData <- rbind(TempGrowthCurveData, AverageGrowthCurve(TempData, 3.00, 70, 200, 1,2))
TempGrowthCurveData <- rbind(TempGrowthCurveData, AverageGrowthCurve(TempData, 3.00, 75, 200,1,3))
TempGrowthCurveData <- rbind(TempGrowthCurveData, AverageGrowthCurve(TempData, 3.00, 80, 200,1,4))


myLogTempPlot = ggplot(TempGrowthCurveData, aes(x = timeElapsed, y = log2(mean), group = as.factor(ID), shape = as.factor(ID))) +
  geom_line(color="goldenrod", size=2) +
  geom_point(color="goldenrod", size=GrowthCurvePointSize)+ myTheme +
  geom_errorbar(aes(x = timeElapsed, ymin = log2(mean)-Log2SE, ymax = log2(mean)+Log2SE), width = 2)+
  scale_shape_manual(breaks = c("1", "2", "3", "4"),labels = c("65  200  3", "70  200  3", "75  200  3", "80 200 3"
  ),name="Experimental Conditions", values = c(16, 17, 15, 1))+
  xlab(element_blank())+ ylab(element_blank()) + myTheme + theme(legend.position = "right",strip.background = element_blank(), legend.key = element_rect(fill = "white"), axis.ticks = element_line(color = "black", size = axisTickSize))+
  scale_y_continuous(sec.axis = sec_axis(trans = (~2^.),name = "OD600", breaks = c(0.01, 0.1, 0.5, 1, 2), labels = c(0.01, 0.1, 0.5, 1, 2))) +
  theme(axis.text.x = element_blank())+
  coord_cartesian(ylim = c(LogODMin, LogODMax), xlim = c(TimeMin, TimeMax))
myLogTempPlot

#Put all of the average growth curves for each piece of shaking data in a list
ShakingGrowthCurveData <- AverageGrowthCurve(shakingData, 3.00, 70, 0,1,1)
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 50, 1,2))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 61, 1,3))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 75, 1,4))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 97, 1,5))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 125, 1,6))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 200, 1,7))
ShakingGrowthCurveData <- rbind(ShakingGrowthCurveData, AverageGrowthCurve(shakingData, 3.00, 70, 300, 1,8))


myLogShakingPlot <- ggplot(ShakingGrowthCurveData, aes(x = timeElapsed, y = log2(mean), group = as.factor(ID), shape = as.factor(ID))) +
  geom_line(color="darkorchid4", size=2) +
  geom_point(color="darkorchid4", size=GrowthCurvePointSize)+ myTheme +
  geom_errorbar(aes(x = timeElapsed, ymin = log2(mean)-Log2SE, ymax = log2(mean)+Log2SE), width = 4, size = 1)+
  scale_shape_manual(breaks = c("1", "2", "3", "4","5","6","7", "8"),labels = c("70  0  3", "70  50  3", "70  61  3", "70 75 3", "70 97 3", "70, 125, 3", "70 200 3", "70 300 3"
  ),name="Experimental Conditions", values = c(16,17,18,2,1,15,6,0))+
  theme(legend.position = "right",strip.background = element_blank(), legend.key = element_rect(fill = "white"), axis.ticks = element_line(color = "black", size = axisTickSize))+
  scale_y_continuous(sec.axis = sec_axis(trans = (~2^.),name = "OD600", breaks = c(0.01, 0.1, 0.5, 1, 2), labels = c(0.01, 0.1, 0.5, 1, 2))) +
  xlab(element_blank()) +ylab("Log(OD600)")+theme(axis.text.x = element_blank())+  coord_cartesian(ylim = c(LogODMin, LogODMax), xlim = c(TimeMin, TimeMax-60))
myLogShakingPlot

grid.arrange(myLogShakingPlot,myLogTempPlot, myLogpHplot,myLogdOPlot, ncol=2)


#Rate vs RI Regressions

##Run All ANOVA's for each GDGT composition. 

Anova(lm(GDGT.0.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.1.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.2.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.3.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.3.iso.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.4.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.4.iso.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.5.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.5.iso.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.6.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.7.Normalized~Hours, data = AZData), type = "II")
Anova(lm(GDGT.8.Normalized~Hours, data = AZData), type = "II")

dOExperiments <- dOdf
dOExperiments$dO <- dOExperiments$RPM
Anova(lm(GDGT.0.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.1.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.2.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.3.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.3.iso.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.4.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.4.iso.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.5.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.5.iso.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.6.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.7.Normalized~dO, data = dOExperiments), type = "II")
Anova(lm(GDGT.8.Normalized~dO, data = dOExperiments), type = "II")

batchdata <- dataframe1combined
Anova(lm(GDGT.0.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.1.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.2.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.3.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.3.iso.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.4.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.4.iso.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.5.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.5.iso.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.6.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.7.Normalized~Temp+RPM+pH, data = batchdata), type = "II")
Anova(lm(GDGT.8.Normalized~Temp+RPM+pH, data = batchdata), type = "II")

##Seperate data based on which dataset it is and pull out all the GDGT values to plot in NMDS
batchGDGTData <- batchdata[which(colnames(batchdata)=="GDGT.0"):which(colnames(batchdata) =="GDGT.8")]
batchNMDS <- metaMDS(batchGDGTData, distance = "bray", k =2)
batchNMDS_X_Y <- data.frame(batchNMDS$points)
ggplot(batchNMDS_X_Y, aes(x=MDS1, y = MDS2, shape = as.factor(batchdata$Temp))) + geom_point(size = 5) + labs(shape="Temperature") +scale_shape_manual(values = c(16, 17, 15, 1)) + myTheme+
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.ticks.y=element_line(color = "black", size = axisTickSize))

ggplot(batchNMDS_X_Y, aes(x=MDS1, y = MDS2, shape = as.factor(batchdata$pH))) + geom_point(size = 5) + labs(shape="pH")+ myTheme+
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.ticks.y=element_line(color = "black", size = axisTickSize))

ggplot(batchNMDS_X_Y, aes(x=MDS1, y = MDS2, shape = as.factor(batchdata$RPM))) + geom_point(size=5) + labs(shape="RPM") +scale_shape_manual(values = (c(16,17,18,2,1,15,6,0))) +myTheme+
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.ticks.y=element_line(color = "black", size = axisTickSize))


dOGDGTData <- dOdf[which(colnames(dOdf)=="GDGT.0"):which(colnames(dOdf) =="GDGT.8")]
oNMDS <- metaMDS(dOGDGTData, distance = "bray", k=2)
oNMDS_X_Y <- data.frame(oNMDS$points)
ggplot(oNMDS_X_Y, aes(x=MDS1, y = MDS2, color = as.factor(dOdf$Phase),shape = as.factor(dOdf$RPM))) + geom_point(size = 5) + labs(shape="O2", color = "Phase") + scale_shape_manual(values = (c(16,17,18,2,1,15)), labels = c("0.2", "0.2*", "0.5", "1", "2", "20"))+ myTheme+
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.ticks.y=element_line(color = "black", size = axisTickSize))

AZGDGTData <-  AZData[which(colnames(AZData)=="GDGT.0"):which(colnames(AZData) =="GDGT.8")]
AZNMDS <-  metaMDS(AZGDGTData, distance = "bray", k=2)
AZNMDS_X_Y <- data.frame(AZNMDS$points)
ggplot(AZNMDS_X_Y, aes(x=MDS1, y = MDS2, shape = as.factor(AZData$Hours))) + geom_point(size = 5) + labs(shape="Time")+ myTheme+
  theme(legend.position="right",strip.background = element_blank(),legend.key = element_rect(fill = "white"))+
  theme(axis.ticks = element_line(color = "black", size = axisTickSize),axis.ticks.length.x = unit(minorAxisLength, "cm"))+
  theme(legend.text = element_text(size = 40), legend.title = element_text(size =40), axis.ticks.y=element_line(color = "black", size = axisTickSize))

