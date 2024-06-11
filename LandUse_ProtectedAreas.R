library(raster)
library(sp)
library(maptools)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(plyr)
library(rgeos)
library(classInt)
library(rcompanion)
library(reshape2)
library(qpcR)
library(plotKML)
library(elliplot)
library(RColorBrewer)
library(rlist)
library(ncdf4)
library(ncdf4.helpers)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(lattice)
library(rasterVis)
library(logr)
library(rdtLite)
library(VoCC)

#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = FALSE)

setwd("./Gobernanza_AreasProtegidas/")


# shapefiles
wdpa0 <- readOGR("./SpatialDataPAs/WDPA_Amer0.shp")
wdpa1 <- readOGR("./SpatialDataPAs/WDPA_Amer1.shp")
wdpa2 <- readOGR("./SpatialDataPAs/WDPA_Amer2.shp")

areas_wdpa0 <- raster::area(wdpa0)
areas_wdpa0 <- areas_wdpa0/1000000
wdpa0@data$Area_km2 <- areas_wdpa0

areas_wdpa1 <- raster::area(wdpa1)
areas_wdpa1 <- areas_wdpa1/1000000
wdpa1@data$Area_km2 <- areas_wdpa1

areas_wdpa2 <- raster::area(wdpa2)
areas_wdpa2 <- areas_wdpa2/1000000
wdpa2@data$Area_km2 <- areas_wdpa2

wdpa0 <- wdpa0[,c(1,2,7)]
wdpa1 <- wdpa1[,c(2,4,31)]
wdpa2 <- wdpa2[,c(2,4,31)]

wdpa.all <- rbind(wdpa0, wdpa1, wdpa2)

# water 0 (NA)
# native vegetation 1-14 (1)
# crops 15-30 (2)
# urban 31 (3)
# barren 32 (NA)

states_26_SSP1 <- nc_open("./RCP_2.6-SSP1/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2030.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_26_SSP1,'PFT1')
pft2 <-ncvar_get(states_26_SSP1,'PFT2')
pft3 <-ncvar_get(states_26_SSP1,'PFT3')
pft4 <-ncvar_get(states_26_SSP1,'PFT4')
pft5 <-ncvar_get(states_26_SSP1,'PFT5')
pft6 <-ncvar_get(states_26_SSP1,'PFT6')
pft7 <-ncvar_get(states_26_SSP1,'PFT7')
pft8 <-ncvar_get(states_26_SSP1,'PFT8')
pft9 <-ncvar_get(states_26_SSP1,'PFT9')
pft10 <-ncvar_get(states_26_SSP1,'PFT10')
pft11 <-ncvar_get(states_26_SSP1,'PFT11')
pft12 <-ncvar_get(states_26_SSP1,'PFT12')
pft13 <-ncvar_get(states_26_SSP1,'PFT13')
pft14 <-ncvar_get(states_26_SSP1,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp126_2030 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp126_2030 <- reclassify(ssp126_2030, c(-Inf,0,0,0,Inf,1))
ssp126_2030.nat <- calc(ssp126_2030, sum)
ssp126_2030.nat <- reclassify(ssp126_2030.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_26_SSP1,'PFT15')
pft16 <-ncvar_get(states_26_SSP1,'PFT16')
pft17 <-ncvar_get(states_26_SSP1,'PFT17')
pft18 <-ncvar_get(states_26_SSP1,'PFT18')
pft19 <-ncvar_get(states_26_SSP1,'PFT19')
pft20 <-ncvar_get(states_26_SSP1,'PFT20')
pft21 <-ncvar_get(states_26_SSP1,'PFT21')
pft22 <-ncvar_get(states_26_SSP1,'PFT22')
pft23 <-ncvar_get(states_26_SSP1,'PFT23')
pft24 <-ncvar_get(states_26_SSP1,'PFT24')
pft25 <-ncvar_get(states_26_SSP1,'PFT25')
pft26 <-ncvar_get(states_26_SSP1,'PFT26')
pft27 <-ncvar_get(states_26_SSP1,'PFT27')
pft28 <-ncvar_get(states_26_SSP1,'PFT28')
pft29 <-ncvar_get(states_26_SSP1,'PFT29')
pft30 <-ncvar_get(states_26_SSP1,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# crops
ssp126_2030 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp126_2030 <- reclassify(ssp126_2030, c(-Inf,0,0,0,Inf,1))
ssp126_2030.crop <- calc(ssp126_2030, sum)
ssp126_2030.crop <- reclassify(ssp126_2030.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_26_SSP1,'PFT31')
pft31 <- raster(pft31)
ssp126_2030.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp126_3states_2030 <- stack(ssp126_2030.nat, ssp126_2030.crop, ssp126_2030.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp126_3states_2030) <- crs.geo
extent(ssp126_3states_2030) <- c(-180,180,-90,90) # set extent

plot(ssp126_3states_2030)

tmp <-getwd()
outfile <- "ssp126_3states_2030.nc"
writeRaster(ssp126_3states_2070, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
            longname="nat_crop_urban", xname="lon", yname="lat")

#
states_26_SSP1 <- nc_open("./RCP_2.6-SSP1/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2070.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_26_SSP1,'PFT1')
pft2 <-ncvar_get(states_26_SSP1,'PFT2')
pft3 <-ncvar_get(states_26_SSP1,'PFT3')
pft4 <-ncvar_get(states_26_SSP1,'PFT4')
pft5 <-ncvar_get(states_26_SSP1,'PFT5')
pft6 <-ncvar_get(states_26_SSP1,'PFT6')
pft7 <-ncvar_get(states_26_SSP1,'PFT7')
pft8 <-ncvar_get(states_26_SSP1,'PFT8')
pft9 <-ncvar_get(states_26_SSP1,'PFT9')
pft10 <-ncvar_get(states_26_SSP1,'PFT10')
pft11 <-ncvar_get(states_26_SSP1,'PFT11')
pft12 <-ncvar_get(states_26_SSP1,'PFT12')
pft13 <-ncvar_get(states_26_SSP1,'PFT13')
pft14 <-ncvar_get(states_26_SSP1,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp126_2070 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp126_2070 <- reclassify(ssp126_2070, c(-Inf,0,0,0,Inf,1))
ssp126_2070.nat <- calc(ssp126_2070, sum)
ssp126_2070.nat <- reclassify(ssp126_2070.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_26_SSP1,'PFT15')
pft16 <-ncvar_get(states_26_SSP1,'PFT16')
pft17 <-ncvar_get(states_26_SSP1,'PFT17')
pft18 <-ncvar_get(states_26_SSP1,'PFT18')
pft19 <-ncvar_get(states_26_SSP1,'PFT19')
pft20 <-ncvar_get(states_26_SSP1,'PFT20')
pft21 <-ncvar_get(states_26_SSP1,'PFT21')
pft22 <-ncvar_get(states_26_SSP1,'PFT22')
pft23 <-ncvar_get(states_26_SSP1,'PFT23')
pft24 <-ncvar_get(states_26_SSP1,'PFT24')
pft25 <-ncvar_get(states_26_SSP1,'PFT25')
pft26 <-ncvar_get(states_26_SSP1,'PFT26')
pft27 <-ncvar_get(states_26_SSP1,'PFT27')
pft28 <-ncvar_get(states_26_SSP1,'PFT28')
pft29 <-ncvar_get(states_26_SSP1,'PFT29')
pft30 <-ncvar_get(states_26_SSP1,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp126_2070 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp126_2070 <- reclassify(ssp126_2070, c(-Inf,0,0,0,Inf,1))
ssp126_2070.crop <- calc(ssp126_2070, sum)
ssp126_2070.crop <- reclassify(ssp126_2070.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_26_SSP1,'PFT31')
pft31 <- raster(pft31)
ssp126_2070.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp126_3states_2070 <- stack(ssp126_2070.nat, ssp126_2070.crop, ssp126_2070.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp126_3states_2070) <- crs.geo
extent(ssp126_3states_2070) <- c(-180,180,-90,90) # set extent

plot(ssp126_3states_2070)

tmp <-getwd()
outfile <- "ssp126_3states_2070.nc"
writeRaster(ssp126_3states_2070, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")

x <- terra::extract(ssp126_3states_2070, wdpa.all, 'mean')
ssp126_3states_2070.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp126_3states_2070.wdpa)

write.csv(ssp126_3states_2070.wdpa, file="ssp126_3states_2070.wdpa.csv")

save.image("SSP126_LU.RData")

dif <- ssp126_3states_2070 - ssp126_3states_2030

levelplot(dif)

# ssp345
states_45_ssp3 <- nc_open("./RCP_4.5-SSP3/GCAM_Demeter_LU_ssp3_rcp45_modelmean_2030.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_45_ssp3,'PFT1')
pft2 <-ncvar_get(states_45_ssp3,'PFT2')
pft3 <-ncvar_get(states_45_ssp3,'PFT3')
pft4 <-ncvar_get(states_45_ssp3,'PFT4')
pft5 <-ncvar_get(states_45_ssp3,'PFT5')
pft6 <-ncvar_get(states_45_ssp3,'PFT6')
pft7 <-ncvar_get(states_45_ssp3,'PFT7')
pft8 <-ncvar_get(states_45_ssp3,'PFT8')
pft9 <-ncvar_get(states_45_ssp3,'PFT9')
pft10 <-ncvar_get(states_45_ssp3,'PFT10')
pft11 <-ncvar_get(states_45_ssp3,'PFT11')
pft12 <-ncvar_get(states_45_ssp3,'PFT12')
pft13 <-ncvar_get(states_45_ssp3,'PFT13')
pft14 <-ncvar_get(states_45_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp326_2030 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp326_2030 <- reclassify(ssp326_2030, c(-Inf,0,0,0,Inf,1))
ssp326_2030.nat <- calc(ssp326_2030, sum)
ssp326_2030.nat <- reclassify(ssp326_2030.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_45_ssp3,'PFT15')
pft16 <-ncvar_get(states_45_ssp3,'PFT16')
pft17 <-ncvar_get(states_45_ssp3,'PFT17')
pft18 <-ncvar_get(states_45_ssp3,'PFT18')
pft19 <-ncvar_get(states_45_ssp3,'PFT19')
pft20 <-ncvar_get(states_45_ssp3,'PFT20')
pft21 <-ncvar_get(states_45_ssp3,'PFT21')
pft22 <-ncvar_get(states_45_ssp3,'PFT22')
pft23 <-ncvar_get(states_45_ssp3,'PFT23')
pft24 <-ncvar_get(states_45_ssp3,'PFT24')
pft25 <-ncvar_get(states_45_ssp3,'PFT25')
pft26 <-ncvar_get(states_45_ssp3,'PFT26')
pft27 <-ncvar_get(states_45_ssp3,'PFT27')
pft28 <-ncvar_get(states_45_ssp3,'PFT28')
pft29 <-ncvar_get(states_45_ssp3,'PFT29')
pft30 <-ncvar_get(states_45_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp326_2030 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp326_2030 <- reclassify(ssp326_2030, c(-Inf,0,0,0,Inf,1))
ssp326_2030.crop <- calc(ssp326_2030, sum)
ssp326_2030.crop <- reclassify(ssp326_2030.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_45_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp326_2030.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp326_3states_2030 <- stack(ssp326_2030.nat, ssp326_2030.crop, ssp326_2030.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp326_3states_2030) <- crs.geo
extent(ssp326_3states_2030) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp326_3states_2030.nc"
writeRaster(ssp326_3states_2030, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp326_3states_2030, wdpa.all, 'mean')
ssp326_3states_2030.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp326_3states_2030.wdpa)

write.csv(ssp326_3states_2030.wdpa, file="ssp326_3states_2030.wdpa.csv")



# 2050
states_45_ssp3 <- nc_open("./RCP_4.5-SSP3/GCAM_Demeter_LU_ssp3_rcp45_modelmean_2050.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_45_ssp3,'PFT1')
pft2 <-ncvar_get(states_45_ssp3,'PFT2')
pft3 <-ncvar_get(states_45_ssp3,'PFT3')
pft4 <-ncvar_get(states_45_ssp3,'PFT4')
pft5 <-ncvar_get(states_45_ssp3,'PFT5')
pft6 <-ncvar_get(states_45_ssp3,'PFT6')
pft7 <-ncvar_get(states_45_ssp3,'PFT7')
pft8 <-ncvar_get(states_45_ssp3,'PFT8')
pft9 <-ncvar_get(states_45_ssp3,'PFT9')
pft10 <-ncvar_get(states_45_ssp3,'PFT10')
pft11 <-ncvar_get(states_45_ssp3,'PFT11')
pft12 <-ncvar_get(states_45_ssp3,'PFT12')
pft13 <-ncvar_get(states_45_ssp3,'PFT13')
pft14 <-ncvar_get(states_45_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp326_2050 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp326_2050 <- reclassify(ssp326_2050, c(-Inf,0,0,0,Inf,1))
ssp326_2050.nat <- calc(ssp326_2050, sum)
ssp326_2050.nat <- reclassify(ssp326_2050.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_45_ssp3,'PFT15')
pft16 <-ncvar_get(states_45_ssp3,'PFT16')
pft17 <-ncvar_get(states_45_ssp3,'PFT17')
pft18 <-ncvar_get(states_45_ssp3,'PFT18')
pft19 <-ncvar_get(states_45_ssp3,'PFT19')
pft20 <-ncvar_get(states_45_ssp3,'PFT20')
pft21 <-ncvar_get(states_45_ssp3,'PFT21')
pft22 <-ncvar_get(states_45_ssp3,'PFT22')
pft23 <-ncvar_get(states_45_ssp3,'PFT23')
pft24 <-ncvar_get(states_45_ssp3,'PFT24')
pft25 <-ncvar_get(states_45_ssp3,'PFT25')
pft26 <-ncvar_get(states_45_ssp3,'PFT26')
pft27 <-ncvar_get(states_45_ssp3,'PFT27')
pft28 <-ncvar_get(states_45_ssp3,'PFT28')
pft29 <-ncvar_get(states_45_ssp3,'PFT29')
pft30 <-ncvar_get(states_45_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp326_2050 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp326_2050 <- reclassify(ssp326_2050, c(-Inf,0,0,0,Inf,1))
ssp326_2050.crop <- calc(ssp326_2050, sum)
ssp326_2050.crop <- reclassify(ssp326_2050.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_45_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp326_2050.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp326_3states_2050 <- stack(ssp326_2050.nat, ssp326_2050.crop, ssp326_2050.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp326_3states_2050) <- crs.geo
extent(ssp326_3states_2050) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp326_3states_2050.nc"
writeRaster(ssp326_3states_2050, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp326_3states_2050, wdpa.all, 'mean')
ssp326_3states_2050.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp326_3states_2050.wdpa)

write.csv(ssp326_3states_2050.wdpa, file="ssp326_3states_2050.wdpa.csv")


# 2070
states_45_ssp3 <- nc_open("./RCP_4.5-SSP3/GCAM_Demeter_LU_ssp3_rcp45_modelmean_2070.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_45_ssp3,'PFT1')
pft2 <-ncvar_get(states_45_ssp3,'PFT2')
pft3 <-ncvar_get(states_45_ssp3,'PFT3')
pft4 <-ncvar_get(states_45_ssp3,'PFT4')
pft5 <-ncvar_get(states_45_ssp3,'PFT5')
pft6 <-ncvar_get(states_45_ssp3,'PFT6')
pft7 <-ncvar_get(states_45_ssp3,'PFT7')
pft8 <-ncvar_get(states_45_ssp3,'PFT8')
pft9 <-ncvar_get(states_45_ssp3,'PFT9')
pft10 <-ncvar_get(states_45_ssp3,'PFT10')
pft11 <-ncvar_get(states_45_ssp3,'PFT11')
pft12 <-ncvar_get(states_45_ssp3,'PFT12')
pft13 <-ncvar_get(states_45_ssp3,'PFT13')
pft14 <-ncvar_get(states_45_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp326_2070 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp326_2070 <- reclassify(ssp326_2070, c(-Inf,0,0,0,Inf,1))
ssp326_2070.nat <- calc(ssp326_2070, sum)
ssp326_2070.nat <- reclassify(ssp326_2070.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_45_ssp3,'PFT15')
pft16 <-ncvar_get(states_45_ssp3,'PFT16')
pft17 <-ncvar_get(states_45_ssp3,'PFT17')
pft18 <-ncvar_get(states_45_ssp3,'PFT18')
pft19 <-ncvar_get(states_45_ssp3,'PFT19')
pft20 <-ncvar_get(states_45_ssp3,'PFT20')
pft21 <-ncvar_get(states_45_ssp3,'PFT21')
pft22 <-ncvar_get(states_45_ssp3,'PFT22')
pft23 <-ncvar_get(states_45_ssp3,'PFT23')
pft24 <-ncvar_get(states_45_ssp3,'PFT24')
pft25 <-ncvar_get(states_45_ssp3,'PFT25')
pft26 <-ncvar_get(states_45_ssp3,'PFT26')
pft27 <-ncvar_get(states_45_ssp3,'PFT27')
pft28 <-ncvar_get(states_45_ssp3,'PFT28')
pft29 <-ncvar_get(states_45_ssp3,'PFT29')
pft30 <-ncvar_get(states_45_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp326_2070 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp326_2070 <- reclassify(ssp326_2070, c(-Inf,0,0,0,Inf,1))
ssp326_2070.crop <- calc(ssp326_2070, sum)
ssp326_2070.crop <- reclassify(ssp326_2070.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_45_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp326_2070.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp326_3states_2070 <- stack(ssp326_2070.nat, ssp326_2070.crop, ssp326_2070.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp326_3states_2070) <- crs.geo
extent(ssp326_3states_2070) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp326_3states_2070.nc"
writeRaster(ssp326_3states_2070, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp326_3states_2070, wdpa.all, 'mean')
ssp326_3states_2070.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp326_3states_2070.wdpa)

write.csv(ssp326_3states_2070.wdpa, file="ssp326_3states_2070.wdpa.csv")


# ssp585
states_85_ssp3 <- nc_open("./RCP_8.5-SSP5/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2030.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_85_ssp3,'PFT1')
pft2 <-ncvar_get(states_85_ssp3,'PFT2')
pft3 <-ncvar_get(states_85_ssp3,'PFT3')
pft4 <-ncvar_get(states_85_ssp3,'PFT4')
pft5 <-ncvar_get(states_85_ssp3,'PFT5')
pft6 <-ncvar_get(states_85_ssp3,'PFT6')
pft7 <-ncvar_get(states_85_ssp3,'PFT7')
pft8 <-ncvar_get(states_85_ssp3,'PFT8')
pft9 <-ncvar_get(states_85_ssp3,'PFT9')
pft10 <-ncvar_get(states_85_ssp3,'PFT10')
pft11 <-ncvar_get(states_85_ssp3,'PFT11')
pft12 <-ncvar_get(states_85_ssp3,'PFT12')
pft13 <-ncvar_get(states_85_ssp3,'PFT13')
pft14 <-ncvar_get(states_85_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp585_2030 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp585_2030 <- reclassify(ssp585_2030, c(-Inf,0,0,0,Inf,1))
ssp585_2030.nat <- calc(ssp585_2030, sum)
ssp585_2030.nat <- reclassify(ssp585_2030.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_85_ssp3,'PFT15')
pft16 <-ncvar_get(states_85_ssp3,'PFT16')
pft17 <-ncvar_get(states_85_ssp3,'PFT17')
pft18 <-ncvar_get(states_85_ssp3,'PFT18')
pft19 <-ncvar_get(states_85_ssp3,'PFT19')
pft20 <-ncvar_get(states_85_ssp3,'PFT20')
pft21 <-ncvar_get(states_85_ssp3,'PFT21')
pft22 <-ncvar_get(states_85_ssp3,'PFT22')
pft23 <-ncvar_get(states_85_ssp3,'PFT23')
pft24 <-ncvar_get(states_85_ssp3,'PFT24')
pft25 <-ncvar_get(states_85_ssp3,'PFT25')
pft26 <-ncvar_get(states_85_ssp3,'PFT26')
pft27 <-ncvar_get(states_85_ssp3,'PFT27')
pft28 <-ncvar_get(states_85_ssp3,'PFT28')
pft29 <-ncvar_get(states_85_ssp3,'PFT29')
pft30 <-ncvar_get(states_85_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp585_2030 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp585_2030 <- reclassify(ssp585_2030, c(-Inf,0,0,0,Inf,1))
ssp585_2030.crop <- calc(ssp585_2030, sum)
ssp585_2030.crop <- reclassify(ssp585_2030.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_85_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp585_2030.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp585_3states_2030 <- stack(ssp585_2030.nat, ssp585_2030.crop, ssp585_2030.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp585_3states_2030) <- crs.geo
extent(ssp585_3states_2030) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp585_3states_2030.nc"
writeRaster(ssp585_3states_2030, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp585_3states_2030, wdpa.all, 'mean')
ssp585_3states_2030.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp585_3states_2030.wdpa)

write.csv(ssp585_3states_2030.wdpa, file="ssp585_3states_2030.wdpa.csv")



# 2050
states_85_ssp3 <- nc_open("./RCP_8.5-SSP5/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2050.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_85_ssp3,'PFT1')
pft2 <-ncvar_get(states_85_ssp3,'PFT2')
pft3 <-ncvar_get(states_85_ssp3,'PFT3')
pft4 <-ncvar_get(states_85_ssp3,'PFT4')
pft5 <-ncvar_get(states_85_ssp3,'PFT5')
pft6 <-ncvar_get(states_85_ssp3,'PFT6')
pft7 <-ncvar_get(states_85_ssp3,'PFT7')
pft8 <-ncvar_get(states_85_ssp3,'PFT8')
pft9 <-ncvar_get(states_85_ssp3,'PFT9')
pft10 <-ncvar_get(states_85_ssp3,'PFT10')
pft11 <-ncvar_get(states_85_ssp3,'PFT11')
pft12 <-ncvar_get(states_85_ssp3,'PFT12')
pft13 <-ncvar_get(states_85_ssp3,'PFT13')
pft14 <-ncvar_get(states_85_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp585_2050 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp585_2050 <- reclassify(ssp585_2050, c(-Inf,0,0,0,Inf,1))
ssp585_2050.nat <- calc(ssp585_2050, sum)
ssp585_2050.nat <- reclassify(ssp585_2050.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_85_ssp3,'PFT15')
pft16 <-ncvar_get(states_85_ssp3,'PFT16')
pft17 <-ncvar_get(states_85_ssp3,'PFT17')
pft18 <-ncvar_get(states_85_ssp3,'PFT18')
pft19 <-ncvar_get(states_85_ssp3,'PFT19')
pft20 <-ncvar_get(states_85_ssp3,'PFT20')
pft21 <-ncvar_get(states_85_ssp3,'PFT21')
pft22 <-ncvar_get(states_85_ssp3,'PFT22')
pft23 <-ncvar_get(states_85_ssp3,'PFT23')
pft24 <-ncvar_get(states_85_ssp3,'PFT24')
pft25 <-ncvar_get(states_85_ssp3,'PFT25')
pft26 <-ncvar_get(states_85_ssp3,'PFT26')
pft27 <-ncvar_get(states_85_ssp3,'PFT27')
pft28 <-ncvar_get(states_85_ssp3,'PFT28')
pft29 <-ncvar_get(states_85_ssp3,'PFT29')
pft30 <-ncvar_get(states_85_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp585_2050 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp585_2050 <- reclassify(ssp585_2050, c(-Inf,0,0,0,Inf,1))
ssp585_2050.crop <- calc(ssp585_2050, sum)
ssp585_2050.crop <- reclassify(ssp585_2050.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_85_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp585_2050.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp585_3states_2050 <- stack(ssp585_2050.nat, ssp585_2050.crop, ssp585_2050.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp585_3states_2050) <- crs.geo
extent(ssp585_3states_2050) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp585_3states_2050.nc"
writeRaster(ssp585_3states_2050, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp585_3states_2050, wdpa.all, 'mean')
ssp585_3states_2050.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp585_3states_2050.wdpa)

write.csv(ssp585_3states_2050.wdpa, file="ssp585_3states_2050.wdpa.csv")


# 2070
states_85_ssp3 <- nc_open("./RCP_8.5-SSP5/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2070.nc", write=TRUE, readunlim=TRUE, verbose = TRUE, auto_GMT = TRUE, suppress_dimvals = FALSE)

# native
pft1 <-ncvar_get(states_85_ssp3,'PFT1')
pft2 <-ncvar_get(states_85_ssp3,'PFT2')
pft3 <-ncvar_get(states_85_ssp3,'PFT3')
pft4 <-ncvar_get(states_85_ssp3,'PFT4')
pft5 <-ncvar_get(states_85_ssp3,'PFT5')
pft6 <-ncvar_get(states_85_ssp3,'PFT6')
pft7 <-ncvar_get(states_85_ssp3,'PFT7')
pft8 <-ncvar_get(states_85_ssp3,'PFT8')
pft9 <-ncvar_get(states_85_ssp3,'PFT9')
pft10 <-ncvar_get(states_85_ssp3,'PFT10')
pft11 <-ncvar_get(states_85_ssp3,'PFT11')
pft12 <-ncvar_get(states_85_ssp3,'PFT12')
pft13 <-ncvar_get(states_85_ssp3,'PFT13')
pft14 <-ncvar_get(states_85_ssp3,'PFT14')

pft1 <- raster(pft1)
pft2 <- raster(pft2)
pft3 <- raster(pft3)
pft4 <- raster(pft4)
pft5 <- raster(pft5)
pft6 <- raster(pft6)
pft7 <- raster(pft7)
pft8 <- raster(pft8)
pft9 <- raster(pft9)
pft10 <- raster(pft10)
pft11 <- raster(pft11)
pft12 <- raster(pft12)
pft13 <- raster(pft13)
pft14 <- raster(pft14)

ssp585_2070 <- stack(pft1, pft2, pft3, pft4, pft5, pft6, pft7, pft8, pft9, pft10, pft11, pft12, pft13, pft14)
ssp585_2070 <- reclassify(ssp585_2070, c(-Inf,0,0,0,Inf,1))
ssp585_2070.nat <- calc(ssp585_2070, sum)
ssp585_2070.nat <- reclassify(ssp585_2070.nat, c(-Inf,0,0,0,Inf,1))

# crops
pft15 <-ncvar_get(states_85_ssp3,'PFT15')
pft16 <-ncvar_get(states_85_ssp3,'PFT16')
pft17 <-ncvar_get(states_85_ssp3,'PFT17')
pft18 <-ncvar_get(states_85_ssp3,'PFT18')
pft19 <-ncvar_get(states_85_ssp3,'PFT19')
pft20 <-ncvar_get(states_85_ssp3,'PFT20')
pft21 <-ncvar_get(states_85_ssp3,'PFT21')
pft22 <-ncvar_get(states_85_ssp3,'PFT22')
pft23 <-ncvar_get(states_85_ssp3,'PFT23')
pft24 <-ncvar_get(states_85_ssp3,'PFT24')
pft25 <-ncvar_get(states_85_ssp3,'PFT25')
pft26 <-ncvar_get(states_85_ssp3,'PFT26')
pft27 <-ncvar_get(states_85_ssp3,'PFT27')
pft28 <-ncvar_get(states_85_ssp3,'PFT28')
pft29 <-ncvar_get(states_85_ssp3,'PFT29')
pft30 <-ncvar_get(states_85_ssp3,'PFT30')

pft15 <- raster(pft15)
pft16 <- raster(pft16)
pft17 <- raster(pft17)
pft18 <- raster(pft18)
pft19 <- raster(pft19)
pft20 <- raster(pft20)
pft21 <- raster(pft21)
pft22 <- raster(pft22)
pft23 <- raster(pft23)
pft24 <- raster(pft24)
pft25 <- raster(pft25)
pft26 <- raster(pft26)
pft27 <- raster(pft27)
pft28 <- raster(pft28)
pft29 <- raster(pft29)
pft30 <- raster(pft30)

# native vegetation
ssp585_2070 <- stack(pft15, pft16, pft17, pft18, pft19, pft20, pft21, pft22, pft23, pft24, pft25, pft26, pft27, pft28, pft29, pft30)
ssp585_2070 <- reclassify(ssp585_2070, c(-Inf,0,0,0,Inf,1))
ssp585_2070.crop <- calc(ssp585_2070, sum)
ssp585_2070.crop <- reclassify(ssp585_2070.crop, c(-Inf,0,0,0,Inf,2))

# urban
pft31 <-ncvar_get(states_85_ssp3,'PFT31')
pft31 <- raster(pft31)
ssp585_2070.urban <- reclassify(pft31, c(-Inf,0,0,0,Inf,3))

# 
ssp585_3states_2070 <- stack(ssp585_2070.nat, ssp585_2070.crop, ssp585_2070.urban)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
projection(ssp585_3states_2070) <- crs.geo
extent(ssp585_3states_2070) <- c(-180,180,-90,90) # set extent


tmp <-getwd()
outfile <- "ssp585_3states_2070.nc"
writeRaster(ssp585_3states_2070, filename=file.path(tmp, outfile), overwrite=TRUE, format="CDF", varname="landcover", varunit="landcover", 
  longname="nat_crop_urban", xname="lon", yname="lat")


x <- terra::extract(ssp585_3states_2070, wdpa.all, 'mean')
ssp585_3states_2070.wdpa <- cbind(as.data.frame(wdpa.all), as.data.frame(x))
head(ssp585_3states_2070.wdpa)

write.csv(ssp585_3states_2070.wdpa, file="ssp585_3states_2070.wdpa.csv")


