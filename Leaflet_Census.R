rm(list=ls())
library(ggplot2); library(maptools); library(gpclib); library(sp); library(rgdal)
gpclibPermit()

library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(raster)

pophh <- readRDS("~/git/NHOPD/merged_pop.Rdata")

# note that you can use county names in the tigris package but 
# not in the acs.fetch function from the acs package so I'm using
# fips numbers here.

# grab the spatial data (tigris)
countiesNH <- c(15,17)
tractsNH <- tracts(state = c('NH'), county = countiesNH)

tracts = rbind(tractsNH,makeUniqueIDs=TRUE)

#find which tract each individual is in
#to_type <- CRS(proj4string(tracts))
#to_type <- CRS("+proj=longlat +ellps=WGS84 +no_defs")


sp.data <- SpatialPoints(pophh[,c("longitude", "latitude")])
proj4string(sp.data) <- CRS("+proj=longlat +ellps=WGS84 +no_defs")
tracts <- spTransform(tracts@polygons, proj4string(sp.data)) #transform to use same projection
bin.indicators <- over(sp.data, tracts, returnList=FALSE)



api.key.install(key="219baafada66bd76bc9fea7f30f76adb7f53bda7")

# create a geographic set to grab tabular data (acs)

geoNH <-geo.make(state=c("NH"),
                   county=c(countiesNH), tract="*")
geo = geoNH

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto". Here are the
# variables we want with pretty and auto results.
#"Household Income: Total:" ("B19001_001")
#"Household Income: $200,000 or more" ("B19001_017")


# the resulting "income" object is not a data.frame it's a list
# to see what's available

names(attributes(income))
##  [1] "endyear"        "span"           "acs.units"      "currency.year" 
##  [5] "modified"       "geography"      "acs.colnames"   "estimate"      
##  [9] "standard.error" "class"
attr(income, "acs.colnames")
##  [1] "Household Income: Total:"              
##  [2] "Household Income: Less than $10,000"   
##  [3] "Household Income: $10,000 to $14,999"  
##  [4] "Household Income: $15,000 to $19,999"  
##  [5] "Household Income: $20,000 to $24,999"  
##  [6] "Household Income: $25,000 to $29,999"  
##  [7] "Household Income: $30,000 to $34,999"  
##  [8] "Household Income: $35,000 to $39,999"  
##  [9] "Household Income: $40,000 to $44,999"  
## [10] "Household Income: $45,000 to $49,999"  
## [11] "Household Income: $50,000 to $59,999"  
## [12] "Household Income: $60,000 to $74,999"  
## [13] "Household Income: $75,000 to $99,999"  
## [14] "Household Income: $100,000 to $124,999"
## [15] "Household Income: $125,000 to $149,999"
## [16] "Household Income: $150,000 to $199,999"
## [17] "Household Income: $200,000 or more"

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

income_df <- dplyr::select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)





income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
income_merged <- income_merged[income_merged$ALAND>0,]


library(leaflet)
popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)


#schoolShape = readShapeSpatial('~/Desktop/tl_2015_sd/tl_2015_us_sde.shp', 
#                               proj4string = CRS("+proj=longlat +datum=WGS84"))
#ZipShape = readShapeSpatial('~/Desktop/ZipDisplay/ZipDisplay2015-10.shp', 
#                            proj4string = CRS("+proj=longlat +datum=WGS84"))


#names(schoolShape)
#NH_MA = schoolShape[is.element(schoolShape$STATEFP,c(33,25) ), ]

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  #addPolygons(data = NH_MA, fill = FALSE, color = "blue", weight =2) %>%
  #addPolygons(data = ZipShape, fill = FALSE, color = "blue", weight =2) %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.4, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 
map3

library(htmlwidgets)
saveWidget(map3, file="~/Desktop/Code/map3.html", selfcontained=FALSE)



                 