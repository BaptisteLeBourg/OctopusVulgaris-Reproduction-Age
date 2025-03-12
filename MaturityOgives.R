# In this script, we will determine the weight, length and age at maturity in 
# octopuses from Brittany using maturity ogives.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)
library(sizeMat)
library(dplyr)
library(ggpubr)

# Let's load the octopus maturity and age data.
DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Let's select the samples that were caught during the reproduction period (from
# March to June)
DataOctopus<-droplevels(subset(DataOctopus,
                                 Month=="3"|
                                 Month=="4"|
                                 Month=="5"|
                                 Month=="6"))

# Let's separate the sexes in two datasets.
Female<-droplevels(subset(DataOctopus,Sex=="F"))
Male<-droplevels(subset(DataOctopus,Sex=="M"))

# Let's check if there are octopuses whose age was considered as inaccurate 
# (coefficient of variation higher than 10) and create a supplementary data frame
# where they are removed.

which(Female$CV>10) # one sample has an inaccurate age.
FemaleAge<-Female[-c(53),]

which(Male$CV>10) # no samples with an inaccurate age.

#
table(DataOctopus$Sex)

# Let's assess the weights at maturity.
# For females.
Maturity_weight_F <- gonad_mature(data = Female,
                             varNames = c("Weight", "Maturity"), 
                             inmName =  c("Immature","Maturation"), 
                             matName = c("Mature"), 
                             method = "fq", 
                             niter = 1000)

Maturity_weight_F %>% print() 

# For males
Maturity_weight_M <- gonad_mature(data = Male,
                                 varNames = c("Weight", "Maturity"), 
                                 inmName =  c("Immature","Maturation"), 
                                 matName = c("Mature"), 
                                 method = "fq", 
                                 niter = 1000)

Maturity_weight_M %>% print() 

# Let's prepare the data for the plot.
DataPlotWeight<-Maturity_weight_F$out %>% as_tibble() %>% mutate(sex = "Female") %>%
  bind_rows(Maturity_weight_M$out %>% as_tibble() %>% mutate(sex = "Male"))

# Let's do the plot.  
PlotWeight <-  ggplot(DataPlotWeight,aes(x = x, y = fitted, linetype = sex))+
  geom_line(linewidth = 1)+
  geom_line(aes(y = CIlower,group = sex), linetype = "dotted", size = .5)+
  geom_line(aes(y = CIupper,group = sex), linetype = "dotted", size = .5)+
  geom_segment(aes(x = -Inf, xend = 2.5583 , y = 0.5, yend = 0.5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 2.5583, xend = 2.5583 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  geom_segment(aes(x = -Inf, xend = 0.939 , y = 0.5, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 0.939, xend = 0.939 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(breaks=c(0,2,4,6,8),limits=c(0,8),labels= c("0","2","4","6","8"))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),labels= c("0","25","50","75","100"))+
  theme_classic(base_size=17.5)+
  theme(panel.border=element_rect(fill=NA),legend.key.width = unit(3, "line"),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))+
  scale_linetype_manual(values = c(scales::linetype_pal()(2)),labels=c("Females (n = 80)", "Males (n = 84)"),name="")+
  labs(x = "Body weight (kg)", y = "Frequency (%)")

# Let's assess the ventral mantle lengths at maturity.
# For females.
Maturity_length_F <- gonad_mature(data = Female,
                                varNames = c("VentralMantleLength", "Maturity"), 
                                inmName =  c("Immature","Maturation"), 
                                matName = c("Mature"), 
                                method = "fq", 
                                niter = 1000)

Maturity_length_F %>% print() 

# For males.
Maturity_length_M <- gonad_mature(data = Male,
                                varNames = c("VentralMantleLength", "Maturity"), 
                                inmName =  c("Immature","Maturation"), 
                                matName = c("Mature"), 
                                method = "fq", 
                                niter = 1000)

Maturity_length_M %>% print() 

# Let's prepare the data for the plot.
DataPlotLength<-Maturity_length_F$out %>% as_tibble() %>% mutate(sex = "Female") %>%
  bind_rows(Maturity_length_M$out %>% as_tibble() %>% mutate(sex = "Male"))

# Let's do the plot.  
PlotLength <-  ggplot(DataPlotLength,aes(x = x, y = fitted, linetype = sex))+
  geom_line(linewidth = 1)+
  geom_line(aes(y = CIlower,group = sex), linetype = "dotted", size = .5)+
  geom_line(aes(y = CIupper,group = sex), linetype = "dotted", size = .5)+
  geom_segment(aes(x = -Inf, xend = 13.2496 , y = 0.5, yend = 0.5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 13.2496, xend = 13.2496 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  geom_segment(aes(x = -Inf, xend = 8.5236 , y = 0.5, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 8.5236, xend = 8.5236 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(breaks=c(5,10,15,20),limits=c(5,20),labels= c("5","10","15","20"))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),labels= c("0","25","50","75","100"))+
  theme_classic(base_size=17.5)+
  theme(panel.border=element_rect(fill=NA),legend.key.width = unit(3, "line"),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))+
  scale_linetype_manual(values = c(scales::linetype_pal()(2)),labels=c("Females (n = 80)", "Males (n = 84)"),name="")+
  labs(x = "Ventral mantle length (cm)", y = "Frequency (%)")


# Let's assess the ventral mantle lengths at maturity.
# For females. Note that we use another data frame here as one age was considered
# as inaccurate (CV > 10)
Maturity_age_F <- gonad_mature(data = FemaleAge,
                              varNames = c("MeanAge", "Maturity"), 
                              inmName =  c("Immature","Maturation"), 
                              matName = c("Mature"), 
                              method = "fq", 
                              niter = 1000)

Maturity_age_F %>% print() 

# For males.
Maturity_age_M <- sizeMat::gonad_mature(data = Male,
                              varNames = c("MeanAge", "Maturity"), 
                              inmName =  c("Immature","Maturation"), 
                              matName = c("Mature"), 
                              method = "fq", 
                              niter = 1000)

Maturity_age_M %>% print() 


# Let's prepare the data for the plot.
DataPlotAge<-Maturity_age_F$out %>% as_tibble() %>% mutate(sex = "Female") %>%
  bind_rows(Maturity_age_M$out %>% as_tibble() %>% mutate(sex = "Male"))

# Let's do the plot.
PlotAge <-  ggplot(DataPlotAge,aes(x = x, y = fitted, linetype = sex))+
  geom_line(linewidth = 1)+
  geom_line(aes(y = CIlower,group=sex), linetype = "dotted", size = .5)+
  geom_line(aes(y = CIupper,group=sex), linetype = "dotted", size = .5)+
  geom_segment(aes(x = -Inf, xend = 451.258 , y = 0.5, yend = 0.5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 451.258, xend = 451.258 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  geom_segment(aes(x = -Inf, xend = 325.7432 , y = 0.5, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 325.7432, xend = 325.7432 , y = -Inf, yend = .5), color ="black", size =.2, linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700),limits=c(200,700))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(0,1),labels= c("0","25","50","75","100"))+
  theme_classic(base_size=17.5)+
  theme(panel.border=element_rect(fill=NA),legend.key.width = unit(3, "line"),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))+
  scale_linetype_manual(values = c(scales::linetype_pal()(2)),labels=c("Females (n = 80)", "Males (n = 84)"),name="")+
  labs(x = "Number of increments", y = "Frequency (%)")

# Let's put together the three plots.
dev.new(width=10.6, height=10, unit="cm")

blank<- ggplot() + theme_void()
ggarrange(
ggarrange(PlotWeight, PlotLength,font.label = list(size = 20),labels = c("a", "b"),
          common.legend = T, legend = "top",ncol=2),
ggarrange(blank,PlotAge,blank,ncol=3,font.label = list(size = 20),labels = c("","c",""),
          common.legend = T, legend = "none",widths = c(0.5, 1, 0.5)),
nrow=2, legend="top",heights=c(1,0.9)
)


