# This script focuses on the estimation of the reproduction period. However, as 
# the sampling was not random, proportions of maturity stages in the samples may 
# not reflect the actual proportions in the total landings as some weight classes 
# may be over- or under-represented in the sampled octopuses. Consequently, the 
# actual proportion of each maturity stage in the landings was estimated. To do
# so, the number of octopuses landed each month, for each weight class and for 
# each sex were initially estimated using landing data and the biometry data 
# collected by the Regional Fisheries Committee of Brittany.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)
library(dplyr)
library(ggpubr)

# Let's load the octopus maturity and age data.
DataBiometry<-read.table("Data-Biometry.csv",dec=".",sep="\t",header=T,quote = "",fill=T,na.strings=c("","NA"))

DataLandings<-read.table("Data-landings.csv",dec=".",sep="\t",header=T,quote = "",fill=T,na.strings=c("","NA"))

DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Note that for the maturity and age data, we will use only octopuses from the 
# fisheries landings as we want to determine the proportions of each maturity 
# stage in the landings. Consequently, let's remove the samples that were not 
# part of the landings in the maturity and age data. Those are the one with no
# weight class. We will also removes females for which no maturity stage was 
# available because of misidentification of their oviducal glands.
DataOctopus <- DataOctopus[!is.na(DataOctopus$WeightClass),]
DataOctopus <- DataOctopus[!is.na(DataOctopus$Maturity),]

# Now, we will estimate the numbers of landed octopuses for each weight class, 
# for each sampling month and for each sex. To do so, we will create a dataset
# providing the numbers of sampled octopuses for biometry analyses in each weight
# class, each sampling month and each sex. We will also include the total weight
# of the sampled octopues and the sex ratio.

# Let's compute the numbers of octopuses sampled for biometry analyses (n) in 
# each weight class (i), each sampling month (j) and each sex (k).
nijk<-aggregate(Weight~WeightClass*Sex*YearAndMonth,DataBiometry,"length")

# We will rename the weight columns into nijk, like in the formula in the paper.
names(nijk)[names(nijk) == "Weight"] <- "nijk"

# Let's compute the numbers of octopuses sampled for biometry analyses (n) in 
# each weight class (i) and each sampling month (j). It will allow to compute the sex 
# ratio later.
nij<-aggregate(Weight~WeightClass*YearAndMonth,DataBiometry,"length")

# We will rename the weight columns into nij, like in the formula in the paper.
names(nij)[names(nij) == "Weight"] <- "nij"

# Let's compute the total weight of the octopuses sampled for biometry analyses 
# (w) in each weight class (i) and each sampling month (j) and each sex (k).
wijk<-aggregate(Weight~WeightClass*Sex*YearAndMonth,DataBiometry,"sum")

# We will rename the weight columns into wijk, like in the formula in the paper.
names(wijk)[names(wijk) == "Weight"] <- "wijk"

# Let's combine all the previous data.
DataPopulation <- nijk %>% left_join(nij)
DataPopulation <- DataPopulation %>% left_join(wijk)

# Now let's combine this dataset with the landings data.
DataPopulation <- DataPopulation %>% left_join(DataLandings)

# For illustration, we will rename the landings columns into Wij, like in the
# formula in the paper.
names(DataPopulation)[names(DataPopulation) == "Landings"] <- "Wij"

# Now we can compute the number of landed octopuses (N) for each weight class (i),
# each month (j) and each sex (k).
DataPopulation$Nijk<-DataPopulation$nijk*(DataPopulation$nijk/DataPopulation$nij)*DataPopulation$Wij/DataPopulation$wijk

# We will keep several columns and separate the sexes in two different datasets.
DataPopulation<-DataPopulation[,c(1,2,3,7,10)]

# Now we will use the previously created datasets and the data on maturity to
# compute the numbers of each maturity stage (l) for each month (i) and each sex 
# (k) in the landings (N). We start by creating a dataset that includes the 
# numbers of octopuses in the data on age and maturity (nprime) in each weight 
# class (i), each month (j), each sex (k) and each maturity stage (l). 
nprimeijkl<-aggregate(Weight~WeightClass*Sex*Maturity*YearAndMonth,DataOctopus,"length")

# We will rename the weight columns into nprimeijkl, like in the formula in the paper.
names(nprimeijkl)[names(nprimeijkl) == "Weight"] <- "nprimeijkl"

# Then, we compute the total number of octopuses for which maturity has been 
# assessed (nprime) in each weight class (i), each month (j) and each sex (k).
nprimeijk<-aggregate(Weight~WeightClass*Sex*YearAndMonth,DataOctopus,"length")

# We will rename the weight columns into nprimeijk, like in the formula in the paper.
names(nprimeijk)[names(nprimeijk) == "Weight"] <- "nprimeijk"

# We combine these data and then combine them with the data of the estimated 
# numbers of octopuses in the landings (Nijk).
DataMaturity <- nprimeijkl %>% left_join(nprimeijk)
DataMaturity<-DataMaturity %>% left_join(DataPopulation)

# Now let's estimate the number of each maturity stage (l) in the landed octopuses 
# (N) for each weight class (i), each month (j) and each sex (l).
DataMaturity$Nijkl <- DataMaturity$nprimeijkl * DataMaturity$Nijk / DataMaturity$nprimeijk

# As we do not assess the maturity for each weight class, the results of 
# all weight classes are summed to get the number of each maturity stage (l) 
# in the landed octopuses (N) for each each month (j) and each sex (k).
DataMaturity<-aggregate(Nijkl~Maturity*Sex*YearAndMonth*Month,DataMaturity,"sum")

# We will rename the weight columns into Njkl, like in the formula in the paper.
names(DataMaturity)[names(DataMaturity) == "Nijkl"] <- "Njkl"

# Note that we included the variable month in the previous computation. This is
# because we want to show maturity each month regardless of the year in the plots.
# Consequently, we will sum the results for each month.
DataMaturity<-aggregate(Njkl~Maturity*Sex*Month,DataMaturity,"sum")

# Let's separate the females and the males to do the plots.
Female<-droplevels(subset(DataMaturity,Sex=="F"))
Male<-droplevels(subset(DataMaturity,Sex=="M"))

# Now we can plot the results. Let's start with the females.
ReprodF<-ggplot(Female) +
  aes(x = as.factor(Month),y=Njkl, fill = factor(Maturity)) +
  geom_bar(position = position_fill(reverse = TRUE), stat="identity",col="black")

ReprodF <- ReprodF + scale_y_continuous(labels=scales::percent_format(suffix=""))
table(DataOctopus$Maturity)
ReprodF <- ReprodF + scale_fill_manual(values = c("Immature" = "white", "Maturation" = "grey","Mature" ="black"),labels=c("Immature (n = 103)", "Maturation (n = 126)","Mature (n = 217)"))
ReprodF<-ReprodF+theme_classic(base_size=17.5)+xlab("Sampling month")+ylab("Frequency (%)")
ReprodF<-ReprodF+theme(panel.spacing = unit(0,"cm"),panel.border=element_rect(fill=NA),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))
ReprodF<-ReprodF+ guides(fill = guide_legend(title=""))

# let's add sample numbers at the top of the bars.
table(DataOctopus$Sex,DataOctopus$Month)
dat_text <- data.frame(label = c("n = 31",  
                                 "n = 15",
                                 "n = 24",
                                 "n = 13",
                                 "n = 16",
                                 "n = 13",
                                 "n = 15",
                                 "n = 15",
                                 "n = 15",
                                 "n = 16",
                                 "n = 19",
                                 "n = 24"),
                       Maturity=rep("Immature",12),
                       x = c(1:12),
                       y = rep(1.05,12))
dat_text

ReprodF<-ReprodF+geom_text(
  data=dat_text,
  size    = 3,
  fontface="bold",
  mapping = aes(x = x, y = y, label = label))

# Then, let's plot the results for the males.
ReprodM<-ggplot(Male) +
  aes(x = as.factor(Month),y=Njkl, fill = factor(Maturity)) +
  geom_bar(position = position_fill(reverse = TRUE), stat="identity",col="black")

ReprodM <- ReprodM + scale_y_continuous(labels=scales::percent_format(suffix=""))
table(DataOctopus$Maturity)
ReprodM <- ReprodM + scale_fill_manual(values = c("Immature" = "white", "Maturation" = "grey","Mature" ="black"),labels=c("Immature (n = 103)", "Maturation (n = 126)","Mature (n = 217)"))
ReprodM<-ReprodM+theme_classic(base_size=17.5)+xlab("Sampling month")+ylab("Frequency (%)")
ReprodM<-ReprodM+theme(panel.spacing = unit(0,"cm"),panel.border=element_rect(fill=NA),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))
ReprodM<-ReprodM+ guides(fill = guide_legend(title=""))

# let's add sample numbers at the top of the bars.
table(DataOctopus$Sex,DataOctopus$Month)
dat_text <- data.frame(label = c("n = 29",  
                                 "n = 17",
                                 "n = 24",
                                 "n = 12",
                                 "n = 16",
                                 "n = 19",
                                 "n = 17",
                                 "n = 17",
                                 "n = 17",
                                 "n = 16",
                                 "n = 24",
                                 "n = 22"),
                       Maturity=rep("Immature",12),
                       x = c(1:12),
                       y = rep(1.05,12))
dat_text

ReprodM<-ReprodM+geom_text(
  data=dat_text,
  size = 3,
  fontface="bold",
  mapping = aes(x = x, y = y, label = label))

# Let's combine the two plots.
dev.new(width=10.6, height=6, unit="cm")
ggarrange(ReprodF, ReprodM,font.label = list(size = 20),labels = c("a", "b"),
          common.legend = T, legend = "top")


