# In this script we will assess the relationship between the number of increments
# and the ventral mantle length in octopuses. We will also check if this 
# relationship differs between sexes and hatching season.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)

# Let's load the octopus maturity and age data.
DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Let's find the octopuses whose age was considered as inaccurate (coefficient of
# variation higher than 10) and remove them from the data.
which(DataOctopus$CV>10)
DataOctopus<-DataOctopus[-c(71,204),]

# There were some octopuses for which mantle length data were not available. Let's
# remove them.
DataOctopus <- DataOctopus[!is.na(DataOctopus$VentralMantleLength),]


# We have to chose which model should be plotted on the figure. Lets start by 
# running each model: linear, logaritmic, exponential, power and degree 2 and 
# 3 polynomial
modelLm<-lm(VentralMantleLength ~ MeanAge*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelLog<-lm(VentralMantleLength ~ log(MeanAge)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelExp<-lm(log(VentralMantleLength) ~ MeanAge*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPower<-lm(log(VentralMantleLength) ~ log(MeanAge)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPoly1<-lm(VentralMantleLength ~ poly(MeanAge, degree = 2)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))
modelPoly2<-lm(VentralMantleLength ~ poly(MeanAge, degree = 3)*Sex*HatchingSeason,DataOctopus,contrasts=list(Sex="contr.poly",HatchingSeason="contr.poly"))

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

# The hatching season is the only factor influencing the relationship between the
# number of increments and the ventral mantle length. Consequently, we will show
# have to compute the linear models separately for each hatching season. 
# Let's start by extracting the data from the dataset.
Autumn <- droplevels(subset(DataOctopus,HatchingSeason=="Autumn"))
Winter <- droplevels(subset(DataOctopus,HatchingSeason=="Winter"))
Spring <- droplevels(subset(DataOctopus,HatchingSeason=="Spring"))
Summer <- droplevels(subset(DataOctopus,HatchingSeason=="Summer"))

# Let's run the model for each group.
ModelAutumn<-lm(log(VentralMantleLength) ~ log(MeanAge),Autumn)
ModelWinter<-lm(log(VentralMantleLength) ~ log(MeanAge),Winter)
ModelSpring<-lm(log(VentralMantleLength) ~ log(MeanAge),Spring)
ModelSummer<-lm(log(VentralMantleLength) ~ log(MeanAge),Summer)


# Let's plot the relationships.
AAllComp<-ggplot(data=DataOctopus, aes(x=MeanAge,y=VentralMantleLength))
AAllComp <- AAllComp + geom_point(size=1.25,aes(col=HatchingSeason,fill=HatchingSeason,shape=Sex))

table(DataOctopus$Sex)
AAllComp <- AAllComp + scale_shape_manual(values=c(1,2),labels=c("Female (n = 239)","Male (n = 248)"))

table(DataOctopus$HatchingSeason)
AAllComp <- AAllComp + scale_color_manual(values = c("Winter" ="#1E88E5","Spring"="forestgreen","Summer" = "#D81B60","Autumn" = "#E6AD02"),labels=c("Winter (n = 124)","Spring (n = 117)","Summer (n = 104)","Autumn (n = 142)"))
AAllComp <- AAllComp + scale_fill_manual(values = c("Winter" ="#1E88E5","Spring"="forestgreen","Summer" = "#D81B60","Autumn" = "#E6AD02"),labels=c("Winter (n = 124)","Spring (n = 117)","Summer (n = 104)","Autumn (n = 142)"))

myfun <- function(x) exp(ModelAutumn$coefficients[1]+ModelAutumn$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Autumn (n = 142)"),lwd=1,col="#E6AD02",xlim=c(min(Autumn$MeanAge,na.rm=T),max(Autumn$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelWinter$coefficients[1]+ModelWinter$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Winter (n = 124)"),lwd=1,col="#1E88E5",xlim=c(min(Winter$MeanAge,na.rm=T),max(Winter$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSpring$coefficients[1]+ModelSpring$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Spring (n = 117)"),lwd=1,col="forestgreen",xlim=c(min(Spring$MeanAge,na.rm=T),max(Spring$MeanAge,na.rm=T)))

myfun <- function(x) exp(ModelSummer$coefficients[1]+ModelSummer$coefficients[2]*log(x))
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Summer (n = 104)"),lwd=1,col="#D81B60",xlim=c(min(Summer$MeanAge,na.rm=T),max(Summer$MeanAge,na.rm=T)))


AAllComp<-AAllComp+theme_classic(base_size=17.5)+xlab("Number of increments")+ylab("Ventral mantle length (cm)")
AAllComp<-AAllComp+theme(panel.border=element_rect(fill=NA),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))
AAllComp <- AAllComp+ scale_y_continuous(breaks=c(5,10,15,20),limits=c(5,20),labels= c("5","10","15","20"))
AAllComp <- AAllComp + scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700),limits=c(200,700))
AAllComp <- AAllComp+ guides(shape= guide_legend(override.aes = list(size=3),title="Sex",order=1),
                               fill = guide_legend(override.aes = list(fill = c("#1E88E5","forestgreen","#D81B60","#E6AD02"),
                                                                       shape = 22,size=6,color=c("black","black","black","black")),title="Hatching season"),
                               color=F,order=2) + 
  theme(legend.key.width = unit(3, "line"))


dev.new(width=10.6, height=6, unit="cm")
AAllComp

