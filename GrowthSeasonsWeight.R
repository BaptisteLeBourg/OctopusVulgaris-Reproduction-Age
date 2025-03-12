# In this script we will assess the relationship between the number of increments
# and the weight in octopuses and notably check if it differs between sexes and 
# hatching season.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)

# Let's load the octopus maturity and age data.
DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Let's find the octopuses whose age was considered as inaccurate (coefficient of
# variation higher than 10) and remove them from the data.
which(DataOctopus$CV>10)
DataOctopus<-DataOctopus[-c(71,204),]

# We have to chose which model should be plotted on the figure. Lets start by 
# running each model: linear, logaritmic, exponential, power and degree 2 and 
# 3 polynomial
modelLm<-lm(Weight ~ MeanAge*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelLog<-lm(Weight ~ log(MeanAge)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelExp<-lm(log(Weight) ~ MeanAge*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPower<-lm(log(Weight) ~ log(MeanAge)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPoly1<-lm(Weight ~ poly(MeanAge, degree = 2)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPoly2<-lm(Weight ~ poly(MeanAge, degree = 3)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))

# Let's summarize each model and check their R-squared values.
summary(modelLm)
summary(modelLog)
summary(modelExp)
summary(modelPower)
summary(modelPoly1)
summary(modelPoly2)

# The power regression has the highest R-squared value. This is the one we will
# show on the plot. Let's see which factors influence the relationship between 
# the number of increments and the weight.
Anova(modelPower,type="III")

# As the interaction between the sex and the hatching season influences the 
# relationship, we will show the males and the females separately on the plot.
# Consequently, we will have to compute the linear models separately for each
# sex and hatching season. Let's start by extracting the data from the dataset.
Autumn <- droplevels(subset(DataOctopus,HatchingSeason=="Autumn"))
AutumnFemales <- droplevels(subset(Autumn,Sex=="F"))
AutumnMales <- droplevels(subset(Autumn,Sex=="M"))

Winter <- droplevels(subset(DataOctopus,HatchingSeason=="Winter"))
WinterFemales<-droplevels(subset(Winter,Sex=="F"))
WinterMales<-droplevels(subset(Winter,Sex=="M"))

Spring <- droplevels(subset(DataOctopus,HatchingSeason=="Spring"))
SpringFemales <- droplevels(subset(Spring,Sex=="F"))
SpringMales <- droplevels(subset(Spring,Sex=="M"))

Summer <- droplevels(subset(DataOctopus,HatchingSeason=="Summer"))
SummerFemales <- droplevels(subset(Summer,Sex=="F"))
SummerMales <- droplevels(subset(Summer,Sex=="M"))

# Let's run the model for each group.
ModelAutumnFemales<-lm(log(Weight) ~ log(MeanAge),AutumnFemales)
ModelAutumnMales<-lm(log(Weight) ~ log(MeanAge),AutumnMales)
ModelWinterFemales<-lm(log(Weight) ~ log(MeanAge),WinterFemales)
ModelWinterMales<-lm(log(Weight) ~ log(MeanAge),WinterMales)
ModelSpringFemales<-lm(log(Weight) ~ log(MeanAge),SpringFemales)
ModelSpringMales<-lm(log(Weight) ~ log(MeanAge),SpringMales)
ModelSummerFemales<-lm(log(Weight) ~ log(MeanAge),SummerFemales)
ModelSummerMales<-lm(log(Weight) ~ log(MeanAge),SummerMales)

# Let's plot the relationships.
AAllComp <- ggplot(data=DataOctopus, aes(x=MeanAge,y=Weight))
AAllComp <- AAllComp + geom_point(size=1.25,aes(col=HatchingSeason,fill=HatchingSeason,shape=Sex))

table(DataOctopus$Sex)
AAllComp <- AAllComp + scale_shape_manual(values=c(1,2),labels=c("Female (n = 247)","Male (n = 255)"))

table(DataOctopus$HatchingSeason)
AAllComp <- AAllComp + scale_color_manual(values = c("Winter" ="#1E88E5","Spring"="forestgreen","Summer" = "#D81B60","Autumn" = "#E6AD02"),labels=c("Winter (n = 126)","Spring (n = 118)","Summer (n = 112)","Autumn (n = 146)"))
AAllComp <- AAllComp + scale_fill_manual(values = c("Winter" ="#1E88E5","Spring"="forestgreen","Summer" = "#D81B60","Autumn" = "#E6AD02"),labels=c("Winter (n = 126)","Spring (n = 118)","Summer (n = 112)","Autumn (n = 146)"))

myfun <- function(x) exp(ModelAutumnFemales$coefficients[1]+ModelAutumnFemales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Autumn (n = 146)",linetype="Female (n = 247)"),lwd=1,col="#E6AD02",xlim=c(min(AutumnFemales$MeanAge,na.rm=T),max(AutumnFemales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelAutumnMales$coefficients[1]+ModelAutumnMales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Autumn (n = 146)",linetype="Male (n = 255)"),lwd=1,col="#E6AD02",xlim=c(min(AutumnMales$MeanAge,na.rm=T),max(AutumnMales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelWinterFemales$coefficients[1]+ModelWinterFemales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Winter (n = 126)",linetype="Female (n = 247)"),lwd=1,col="#1E88E5",xlim=c(min(WinterFemales$MeanAge,na.rm=T),max(WinterFemales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelWinterMales$coefficients[1]+ModelWinterMales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Winter (n = 126)",linetype="Male (n = 255)"),lwd=1,col="#1E88E5",xlim=c(min(WinterMales$MeanAge,na.rm=T),max(WinterMales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSpringFemales$coefficients[1]+ModelSpringFemales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Spring (n = 118)",linetype="Female (n = 247)"),lwd=1,col="forestgreen",xlim=c(min(SpringFemales$MeanAge,na.rm=T),max(SpringFemales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSpringMales$coefficients[1]+ModelSpringMales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Spring (n = 118)",linetype="Male (n = 255)"),lwd=1,col="forestgreen",xlim=c(min(SpringMales$MeanAge,na.rm=T),max(SpringMales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSummerFemales$coefficients[1]+ModelSummerFemales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Summer (n = 112)",linetype="Female (n = 247)"),lwd=1,col="#D81B60",xlim=c(min(SummerFemales$MeanAge,na.rm=T),max(SummerFemales$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSummerMales$coefficients[1]+ModelSummerMales$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Summer (n = 112)",linetype="Male (n = 255)"),lwd=1,col="#D81B60",xlim=c(min(SummerMales$MeanAge,na.rm=T),max(SummerMales$MeanAge,na.rm=T)))


AAllComp<-AAllComp+theme_classic(base_size=17.5)+xlab("Number of increments")+ylab("Body weight (kg)")
AAllComp<-AAllComp+theme(panel.border=element_rect(fill=NA),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))
AAllComp <- AAllComp+ scale_y_continuous(breaks=c(0,2,4,6,8),limits=c(0,8),labels= c("0","2","4","6","8"))
AAllComp <- AAllComp + scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700),limits=c(200,700))
AAllComp <- AAllComp+ guides(shape= guide_legend(override.aes = list(size=3),title="Sex",order=1),
                             linetype= guide_legend(override.aes = list(colour="black"),title="Sex",order=1),
                             fill = guide_legend(override.aes = list(fill = c("#1E88E5","forestgreen","#D81B60","#E6AD02"),
                             shape = 22,size=6,color=c("black","black","black","black")),title="Hatching season"),
                             color=F,order=2) + 
  theme(legend.key.width = unit(3, "line"))


dev.new(width=10.6, height=6, unit="cm")
AAllComp

