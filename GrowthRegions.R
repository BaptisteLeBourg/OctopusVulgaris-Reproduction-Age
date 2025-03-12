# In this script, we will compare the growth of octopuses from Southern Brittany
# with other regions. It will be done by assessing the relationships between the
# numbers of increments and the weight.

# Let's start by loading the necessary packages.
library(ggplot2)
library(Rcmdr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Let's load the octopus maturity and age data.
DataOctopus<-read.table("Data-Maturity-Age.csv",dec=".",sep="\t",header=T,row.names=1,quote = "",fill=T,na.strings=c("","NA"))

# Let's find the octopuses whose age was considered as inaccurate (coefficient of
# variation higher than 10) and remove them from the data.
which(DataOctopus$CV>10)
DataOctopus<-DataOctopus[-c(71,204),]

# We will now asses the relationship between the number of increments and the 
# weight of octopuses. We have to chose which model should be plotted on the 
# figure. Lets start by running each model: linear, logarthimic, exponential, 
# power and degree 2 and 3 polynomial.
modelLm<-lm(Weight ~ MeanAge,DataOctopus)
modelLog<-lm(Weight ~ log(MeanAge),DataOctopus)
modelExp<-lm(log(Weight) ~ MeanAge,DataOctopus)
modelPower<-lm(log(Weight) ~ log(MeanAge),DataOctopus)
modelPoly1<-lm(Weight ~ poly(MeanAge, degree = 2),DataOctopus)
modelPoly2<-lm(Weight ~ poly(MeanAge, degree = 3),DataOctopus)

# Let's summary each model and check their R-squared values.
summary(modelLm)
summary(modelLog)
summary(modelExp)
summary(modelPower)
summary(modelPoly1)
summary(modelPoly2)

# The power regression has the highest R-squared value. This is the one we will
# show on the plot.

# Now we will do the plot. First we want to do a little map that will be embedded
# in the plot and that will show the locations of the different studies.

# Let's create a data frame with the coordinates for each study.
Latitude<- c(21,28,35.5,39.88,47.71)
Longitude<- c(-17,-15.65,-4.9,8.25,-3.99)
Location<-c("Mauritania","Gran Canaria","Morroco","Sardinia","Brittany")
map<-data.frame(Latitude,Longitude,Location)

# We will now convert the coordinates to the right projection for the map.
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=aeqd +lon_0=0 +lat_0=34 +datum=WGS84 +units=m +no_defs"

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-20, 20, 20, -20, -20), # x-coordinates (longitudes) of points A,B,C,D
    c(19, 19, 49, 49, 19)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)

laeabb <- st_transform(bb, crs = crsLAEA)

b <- st_bbox(laeabb)

help_sam = function(data,
                    src.proj = CRS("+proj=longlat +datum=WGS84 +no_defs"),
                    dst.proj = CRS("+proj=aeqd +lon_0=0 +lat_0=34 +datum=WGS84 +units=m +no_defs")) {
  require(sp)
  as.data.frame(
    spTransform(
      SpatialPointsDataFrame(
        coords = data.frame(Longitude = map$Longitude,
                            Latitude = map$Latitude),
        data = data.frame(Location = map$Location,
                          Xlon = map$Longitude,
                          Ylat = map$Latitude),
        proj4string = src.proj), dst.proj))
  
}
mapConvert <- help_sam(map)

# We can now do the map.
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

PlotMap <- ggplot(world) + geom_sf(fill = "gray75",color="gray75") + coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"]))
PlotMap <- PlotMap + geom_point(data=mapConvert, aes(x=coords.x1, y=coords.x2,fill=Location),shape=22,size=3)

PlotMap <- PlotMap + scale_fill_manual(name='Sampling region',
                             breaks=c('Mauritania', 'Gran Canaria', 'Morroco','Sardinia','Brittany'),
                             values=c('Mauritania'='#E6AD02', 'Gran Canaria'='#1E88E5', 'Morroco'='#D81B60','Sardinia'='forestgreen','Brittany'='black'))

PlotMap <- PlotMap + theme_linedraw(base_size=10) + theme(panel.grid.major = element_blank(),
                                                    axis.title.x=element_blank(),
                                                    axis.title.y=element_blank(),
                                                    axis.text.x=element_blank(), 
                                                    axis.ticks.x=element_blank(), 
                                                    axis.text.y=element_blank(), 
                                                    axis.ticks.y=element_blank(),
                                                    legend.position="none")

PlotMap <- PlotMap + scale_y_continuous(breaks = seq(20, 50, len = 4))



# Now, we will plot the relationships between the weight and the number of 
# increments for the different regions. The relationships for each region were
# taken from the literature and were multiplied by 10^-3 to obtain a result in 
# kilograms instead of grams.
AAllComp<-ggplot(data=DataOctopus, aes(x=MeanAge,y=Weight))

myfun <- function(x) (29.56*exp(0.014*x))*10^-3 # Morroco, Faiki et al., 2023
AAllComp <- AAllComp + stat_function(fun = myfun,lwd=1,aes(colour="Morroco"),xlim=c(129,382))

myfun <- function(x) (-7351.6+1607.7*log(x))*10^-3 # Gran Canaria, Hernández-López et al., 2001
AAllComp <- AAllComp + stat_function(fun = myfun,lwd=1,aes(colour="Gran Canaria"),xlim=c(96.8095,398))

myfun <- function(x) (37.34*exp(0.010*x)*10^-3) # Sardinia, Cucu et al., 2013
AAllComp <- AAllComp + stat_function(fun = myfun,lwd=1,lty=1,aes(colour="Sardinia"),xlim=c(61,465))

myfun <- function(x) (2.53*10^-4*x^3.02)*10^-3 # Mauritania, Perales-Raya et al., 2010. Equation computed with the data provided by the Table 1 in the publication.
AAllComp <- AAllComp + stat_function(fun = myfun,lwd=1,lty=1,aes(colour="Mauritania"),xlim=c(74,243))

myfun <- function(x) exp(modelPower$coefficients[1]+modelPower$coefficients[2]*log(x)) # Brittany, this study.
AAllComp <- AAllComp + stat_function(fun = myfun,aes(colour="Brittany"),lwd=1,xlim=c(min(DataOctopus$MeanAge,na.rm=T),max(DataOctopus$MeanAge,na.rm=T)))

# We also add the individual points for the data from this study
AAllComp <- AAllComp + geom_point(size=0.75,colour="black")

AAllComp<-AAllComp+theme_classic(base_size=17.5)+xlab("Number of increments")+ylab("Body weight (kg)")
AAllComp<-AAllComp+theme(panel.border=element_rect(fill=NA),axis.title = element_text(face="bold"),axis.text = element_text(face="bold"))
AAllComp <- AAllComp+ scale_y_continuous(breaks=c(0,2,4,6,8),limits=c(0,8),labels= c("0","2","4","6","8"))
AAllComp <- AAllComp + scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700),limits=c(0,700))
AAllComp <- AAllComp + scale_color_manual(name='Sampling region',
                                          breaks=c('Mauritania', 'Gran Canaria', 'Morroco','Sardinia','Brittany'),
                                          values=c('Mauritania'='#E6AD02', 'Gran Canaria'='#1E88E5', 'Morroco'='#D81B60','Sardinia'='forestgreen','Brittany'='black'))

# We can now embed the map in the plot.
AAllComp <- AAllComp + annotation_custom(grob=ggplotGrob(PlotMap),xmin=-480,ymin=4.1)

dev.new(width=10.6, height=6, unit="cm")
AAllComp

# Here are the references used for the relationships between the numbers of 
# increments and the weight in regions other than Brittany.

# Cuccu, D., Mereu, M., Cau, A., Pesci, P., Cau, A. (2013). Reproductive 
#      development versus estimated age and size in a wild Mediterranean 
#      population of Octopus vulgaris (Cephalopoda: Octopodidae). Journal of the
#      Marine Biological Association of the United Kingdom 93, 843-849. 
#      https://doi.org/10.1017/S0025315412000203

# Faiki, A., Chairi, H., Idrissi, M.M., Bensbai, J. (2023). Study of the growth 
#      of Octopus vulgaris in the Moroccan Mediterranean Sea by direct age 
#      estimation through the analysis of upper beaks. Journal of the Marine 
#      Biological Association of the United Kingdom 103, e33. 
#      https://doi.org/10.1017/S0025315423000218

# Hernández-López, J.L., Castro-Hernández, J.J., Hernández-García, V. (2001). 
#      Age determined from the daily deposition of concentric rings on common 
#      octopus (Octopus vulgaris) beaks. Fishery Bulletin 99, 679-684.

# Perales-Raya, C., Bartolomé, A., García-Santamaría, M.T., Pascual-Alayón, P.,
#      Almansa, E. (2010). Age estimation obtained from analysis of octopus 
#      (Octopus vulgaris Cuvier, 1797) beaks: improvements and comparisons. 
#      Fisheries Research 106, 171-176. https://doi.org/10.1016/j.fishres.2010.05.003