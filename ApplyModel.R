library(plyr)
library(dplyr)

setwd("~/git/NHOPD/")
##----- load synthetic population ----- ##
pop <- read.csv("data/2010_ver1_33/2010_ver1_33_synth_people.txt") 
hh <- read.csv("data/2010_ver1_33/2010_ver1_33_synth_households.txt")

pophh <- merge(pop, hh, by.x="sp_hh_id", by.y="sp_id")
rm(pop, hh)

##----- load model for comparison ----- ##
load("Model.RData")


## ----- make same variable types ----- ##
## AGE2
pophh$AGE2 <- cut(pophh$age, breaks=c(0, 17, 25, 34, 49, 64, 150)) 
to.vals <-  c("(1): Less than 18", "(2): 18 to 25", "(3): 26 to 34", "(4): 35 to 49", "(5): 50 to 64", "(6): 65 or older")
pophh$AGE2 <- mapvalues(pophh$AGE2, from=levels(pophh$AGE2), to= to.vals)

## NEWRACE2
#NEWRACE2(1) NonHisp White
#NEWRACE2(2) NonHisp Black/Afr Am  
#NEWRACE2(3) NonHisp Native Am/AK Native  
#NEWRACE2(4) NonHisp Native HI/Other Pac Isl  
#NEWRACE2(5) NonHisp Asian       
#NEWRACE2(6) NonHisp more than one race  
#NEWRACE2(7) Hispanic

## Recoded detailed race code
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or American
# .Indian or Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races 

to.vals.race <- c("(1) NonHisp White", "(2) NonHisp Black/Afr Am", "(3) NonHisp Native Am/AK Native", 
                  "(3) NonHisp Native Am/AK Native", "(3) NonHisp Native Am/AK Native", "(5) NonHisp Asian",
                  "(3) NonHisp Native Am/AK Native", "(6) NonHisp more than one race", "(6) NonHisp more than one race")
pophh$NEWRACE2 <- mapvalues(pophh$race, from=c(1:9), to=to.vals.race)

## sex
pophh$IRSEX <- mapvalues(pophh$sex, from=c(1:2), to=c("(1) Male", "(2) Female"))

##income
to.vals.income <- c("(1) Less than $10,000 (Including Loss)", "(2) $10,000 - $19,999",
                    "(3) $20,000 - $29,999","(4) $30,000 - $39,999", "(5) $40,000 - $49,999",
                    "(6) $50,000 - $74,999", "(7) $75,000 or more") 
 
pophh$IRFAMIN3 <-cut(pophh$hh_income, breaks=c(-1000000, 10000, 20000, 30000, 40000, 50000, 75000, 10000000) )
pophh$IRFAMIN3 <- mapvalues(pophh$IRFAMIN, from=levels(pophh$IRFAMIN), to=to.vals.income)

##hh size
to.vals.hhsize <- c("(1) One person in household","(2) Two people in household","(3) Three people in household",
                    "(4) Four people in household", "(5) Five people in household", "(6) 6 or more people in household")
pophh$IRHHSIZ2 <- pophh$hh_size
pophh$IRHHSIZ2[pophh$IRHHSIZ2>6] <- 6
pophh$IRHHSIZ2 <- mapvalues(pophh$IRHHSIZ2, from=1:6, to=to.vals.hhsize)



## ----- make predictions using model ----- ##
pophh$drug.use.prob <- predict(model, newdata=pophh, type = 'response')


## ---- make plots ------ ##
require(maptools)
require(sp)
require(raster)
require(rgeos)
require(rgdal)
library(argparse)
require(fields)


#Loading Mapping Data
nh.shp <- readShapePoly("data/tl_2013_33_cousub/tl_2013_33_cousub.shp")
sp.data <- SpatialPoints(pophh[,c("longitude", "latitude")])
bin.indicators <- over(sp.data, nh.shp, returnList=FALSE)
indicators <- bin.indicators$COUSUBFP

plot(nh.shp, col=1:260)
num.users <- sapply(split(pophh$drug.use.prob, indicators), sum, na.rm=TRUE)
use.rate <- sapply(split(pophh$drug.use.prob, indicators), mean, na.rm=TRUE)

colorBins <- function(x, num.bins, scale=TRUE){
  colors <- topo.colors(num.bins)
  if(scale){ breaks <- seq(0,1, length.out = num.bins-1)}else{
    breaks <- num.bins -1
  }
  x.cut <- cut(x, breaks = breaks, ordered_result=TRUE)
  x.colors <- colors[as.factor(x.cut)]
  rng <- range(x, na.rm=TRUE)
  breaks <- rng[1] + seq(0, 1, length=num.bins)*(diff(rng))
  out <- list(colors=colors, x.colors=x.colors, breaks=breaks)
}

#cols1 <- colorBins(log(num.users+1), num.bins=20, scale=FALSE)
#plot(nh.shp, col=cols1$x.colors)

cols2 <- colorBins(use.rate, num.bins=20, scale=FALSE) 
png("estimated-use-rate.png")
plot(nh.shp, col=cols2$x.colors,  main = "Estimated Use Rate")
image.plot( zlim=range(use.rate, na.rm=TRUE), legend.only=TRUE, col=cols2$colors)
dev.off()
