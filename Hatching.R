# In this script, we are making a plot displaying the hatching frequencies for
# each month and each year. This plot does not appear in the paper but shows that
# octopuses hatch throughout the year.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)

# Let's load the octopus maturity and age data.
DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Let's find the octopuses whose age was considered as inaccurate (coefficient of
# variation higher than 10) and remove them from the data.
which(DataOctopus$CV>10)
DataOctopus<-DataOctopus[-c(71,204),]

# Here is the script for the plot.
HM<-ggplot(data=DataOctopus, aes(x=as.factor(HatchingMonth)))
HM<-HM+geom_bar(aes(y = (..count..)/sum(..count..)*100),col="black",fill="grey90")
HM<-HM+facet_grid(~HatchingYear,scales="free_x",space="free_x")
HM<-HM+theme_classic(base_size=17.5)+xlab("Hatching month")+ylab("Frequency (%)")
HM<-HM+theme(panel.spacing = unit(0,"cm"),panel.border=element_rect(fill=NA),
             axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))+ylim(0,8)

# let's add sample numbers at the top of the bars.
table(DataOctopus$HatchingMonth,DataOctopus$HatchingYear)
dat_text <- data.frame(label = c("n = 2",  
                                 "n = 3",
                                 "n = 5",
                                 "n = 10",
                                 "n = 16",
                                 "n = 22",
                                 "n = 27",
                                 "n = 27",
                                 "n = 30",
                                 "n = 30",
                                 "n = 29",
                                 "n = 27",
                                 "n = 23",
                                 "n = 27",
                                 "n = 31",
                                 "n = 27",
                                 "n = 32",
                                 "n = 28",
                                 "n = 22",
                                 "n = 21",
                                 "n = 9",
                                 "n = 9",
                                 "n = 11",
                                 "n = 10",
                                 "n = 16",
                                 "n = 7",
                                 "n = 1"),
                       HatchingYear = c(rep("2021",7),
                                        rep("2022",12),
                                        rep("2023",8)),
                       x = c(c(1:7),c(1:12),c(1:8)),
                       y = rep(8,27))
dat_text

HM<-HM+geom_text(
  data=dat_text,
  size    = 3,   fontface="bold",
  mapping = aes(x = x, y = y, label = label))

dev.new(width=10.6, height=6, unit="cm")
HM

