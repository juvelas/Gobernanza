
library(raster)
library(sf)
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
library(VoCC)
library(rlist)
library(ncdf4)
library(ncdf4.helpers)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(lattice)
library(rasterVis)

setwd("./Gobernanza_AreasProtegidas/")

# shapefiles
wdpa0 <- readOGR("./SpatialDataPAs/WDPA_America.shp")
areas_wdpa <- raster::area(wdpa0)
areas_wdpa <- areas_wdpa/1000000
wdpa0@data$Area_km2 <- areas_wdpa


# cru-ts datasets
tmin <- brick("cru_ts4.03.1901.2018.tmn.dat.nc")
tmax <- brick("cru_ts4.03.1901.2018.tmx.dat.nc")

# Historical gradient based velocity of climate change by time period

# calculate mean annual monthly temp
yr.tmin.p1 <- sumSeries(tmin, p = "1901-01/1950-12", yr0 = "1901-01-01", l = nlayers(tmin),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.p1 <- sumSeries(tmax, p = "1901-01/1950-12", yr0 = "1901-01-01", l = nlayers(tmax),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmin.p2 <- sumSeries(tmin, p = "1951-01/1990-12", yr0 = "1951-01-01", l = nlayers(tmin),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.p2 <- sumSeries(tmax, p = "1951-01/1990-12", yr0 = "1951-01-01", l = nlayers(tmax),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmin.p3 <- sumSeries(tmin, p = "1991-01/2018-12", yr0 = "1991-01-01", l = nlayers(tmin),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.p3 <- sumSeries(tmax, p = "1991-01/2018-12", yr0 = "1991-01-01", l = nlayers(tmax),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")


# temporal trend and gradient-based velocity
tr.tmin.p1 <- tempTrend(yr.tmin.p1, th=30)
sg.tmin.p1 <- spatGrad(yr.tmin.p1, th = 0.1, projected = FALSE)
gvocc.tmin.p1 <- gVoCC(tr.tmin.p1, sg.tmin.p1)

tr.tmax.p1 <- tempTrend(yr.tmax.p1, th=30)
sg.tmax.p1 <- spatGrad(yr.tmax.p1, th = 0.1, projected = FALSE)
gvocc.tmax.p1 <- gVoCC(tr.tmax.p1, sg.tmax.p1)

tr.tmin.p2 <- tempTrend(yr.tmin.p2, th=30)
sg.tmin.p2 <- spatGrad(yr.tmin.p2, th = 0.1, projected = FALSE)
gvocc.tmin.p2 <- gVoCC(tr.tmin.p2, sg.tmin.p2)

tr.tmax.p2 <- tempTrend(yr.tmax.p2, th=30)
sg.tmax.p2 <- spatGrad(yr.tmax.p2, th = 0.1, projected = FALSE)
gvocc.tmax.p2 <- gVoCC(tr.tmax.p2, sg.tmax.p2)

tr.tmin.p3 <- tempTrend(yr.tmin.p3, th=20)
sg.tmin.p3 <- spatGrad(yr.tmin.p3, th = 0.1, projected = FALSE)
gvocc.tmin.p3 <- gVoCC(tr.tmin.p3, sg.tmin.p3)

tr.tmax.p3 <- tempTrend(yr.tmax.p3, th=20)
sg.tmax.p3 <- spatGrad(yr.tmax.p3, th = 0.1, projected = FALSE)
gvocc.tmax.p3 <- gVoCC(tr.tmax.p3, sg.tmax.p3)


# residence times for PAs

amin.wdpa0.p1 <- resTime(wdpa0, gvocc.tmin.p1[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
amax.wpda0.p1 <- resTime(wdpa0, gvocc.tmax.p1[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))

amin.wdpa0.p2 <- resTime(wdpa0, gvocc.tmin.p2[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
amax.wpda0.p2 <- resTime(wdpa0, gvocc.tmax.p2[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))

amin.wdpa0.p3 <- resTime(wdpa0, gvocc.tmin.p3[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
amax.wpda0.p3 <- resTime(wdpa0, gvocc.tmax.p3[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))


restime.wdpa0.p1 <- cbind(as.data.frame(amin.wdpa0.p1), as.data.frame(amax.wpda0.p1))
colnames(restime.wdpa0.p1) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")

restime.wdpa0.p2 <- cbind(as.data.frame(amin.wdpa0.p2), as.data.frame(amax.wpda0.p2))
colnames(restime.wdpa0.p2) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")

restime.wdpa0.p3 <- cbind(as.data.frame(amin.wdpa0.p3), as.data.frame(amax.wpda0.p3))
colnames(restime.wdpa0.p3) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")

restime.wdpa0.p1["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.p1)<-sapply(restime.wdpa0.p1, is.infinite)
wdpa0.p1.2 <- cbind(wdpa0, restime.wdpa0.p1)

restime.wdpa0.p2["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.p2)<-sapply(restime.wdpa0.p2, is.infinite)
wdpa0.p2.2 <- cbind(wdpa0, restime.wdpa0.p2)

restime.wdpa0.p3["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.p3)<-sapply(restime.wdpa0.p3, is.infinite)
wdpa0.p3.2 <- cbind(wdpa0, restime.wdpa0.p3)


wdpa0.p1.3 <- as.data.frame(wdpa0.p1.2)
wdpa0.p2.3 <- as.data.frame(wdpa0.p2.2)
wdpa0.p3.3 <- as.data.frame(wdpa0.p3.2)

wdpa0.p1.3["Historical"] <- "P1"
wdpa0.p2.3["Historical"] <- "P2"
wdpa0.p3.3["Historical"] <- "P3"

wdpa0.p.3 <- rbind(wdpa0.p1.3, wdpa0.p2.3, wdpa0.p3.3)

write.csv(wdpa0.p.3, file="WDPA_America_ResTime_Historical.csv")

write.csv(wdpa0.p2.3, file="WDPA_America_ResTime_Historical_P2.csv")
write.csv(wdpa0.p3.3, file="WDPA_America_ResTime_Historical_P3.csv")


write.csv(as.data.frame(wdpa0.p1.2), file="WDPA_America_ResTime_p1.csv")
write.csv(as.data.frame(wdpa0.p2.2), file="WDPA_America_ResTime_p2.csv")
write.csv(as.data.frame(wdpa0.p3.2), file="WDPA_America_ResTime_p3.csv")

save.image("Vocc_1901_2018_APs.RData")


# Future projections
# CMIP6 SSP126
tasmin.f.ssp126  <- brick("./CMIP6/tasmin_mon_ens_ssp126_192_ave.nc")
tasmin.f.ssp126 <- tasmin.f.ssp126-273.15
tasmin.f.ssp126 <- tasmin.f.ssp126[[1813:3012]]

tasmax.f.ssp126 <- brick("./CMIP6/tasmax_mon_ens_ssp126_192_ave.nc")
tasmax.f.ssp126 <- tasmax.f.ssp126-273.15
tasmax.f.ssp126 <- tasmax.f.ssp126[[1813:3012]]

tasmin.f.ssp245 <- brick("./CMIP6/tasmin_mon_ens_ssp245_192_ave.nc")
tasmin.f.ssp245 <- tasmin.f.ssp245-273.15
tasmin.f.ssp245 <- tasmin.f.ssp245[[1813:3012]]

tasmax.f.ssp245 <- brick("./CMIP6/tasmax_mon_ens_ssp245_192_ave.nc")
tasmax.f.ssp245 <- tasmax.f.ssp245-273.15
tasmax.f.ssp245 <- tasmax.f.ssp245[[1813:3012]]

tasmin.f.ssp370 <- brick("./CMIP6/tasmin_mon_ens_ssp370_192_ave.nc")
tasmin.f.ssp370 <- tasmin.f.ssp370-273.15
tasmin.f.ssp370 <- tasmin.f.ssp370[[1813:3012]]

tasmax.f.ssp370 <- brick("./CMIP6/tasmax_mon_ens_ssp370_192_ave.nc")
tasmax.f.ssp370 <- tasmax.f.ssp370-273.15
tasmax.f.ssp370 <- tasmax.f.ssp370[[1813:3012]]

tasmin.f.ssp585 <- brick("./CMIP6/tasmin_mon_ens_ssp585_192_ave.nc")
tasmin.f.ssp585 <- tasmin.f.ssp585-273.15
tasmin.f.ssp585 <- tasmin.f.ssp585[[1813:3012]]

tasmax.f.ssp585 <- brick("./CMIP6/tasmax_mon_ens_ssp585_192_ave.nc")
tasmax.f.ssp585 <- tasmax.f.ssp585-273.15
tasmax.f.ssp585 <- tasmax.f.ssp585[[1813:3012]]


# calculate mean annual monthly temp
yr.tmin.ssp126 <- sumSeries(tasmin.f.ssp126, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmin.f.ssp126),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.ssp126 <- sumSeries(tasmax.f.ssp126, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmax.f.ssp126),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmin.ssp245 <- sumSeries(tasmin.f.ssp245, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmin.f.ssp245),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.ssp245 <- sumSeries(tasmax.f.ssp245, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmax.f.ssp245),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmin.ssp370 <- sumSeries(tasmin.f.ssp370, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmin.f.ssp370),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.ssp370 <- sumSeries(tasmax.f.ssp370, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmax.f.ssp370),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmin.ssp585 <- sumSeries(tasmin.f.ssp585, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmin.f.ssp585),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

yr.tmax.ssp585 <- sumSeries(tasmax.f.ssp585, p = "2025-01/2100-12", yr0 = "2001-01-01", l = nlayers(tasmax.f.ssp585),
                     fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")

# temporal trend and gradient-based velocity
tr.tmin.ssp126 <- tempTrend(yr.tmin.ssp126, th=30)
sg.tmin.ssp126 <- spatGrad(yr.tmin.ssp126, th = 0.1, projected = FALSE)
tr.tmax.ssp126 <- tempTrend(yr.tmax.ssp126, th=30)
sg.tmax.ssp126 <- spatGrad(yr.tmax.ssp126, th = 0.1, projected = FALSE)

gvocc.tmax.ssp126 <- gVoCC(tr.tmax.ssp126, sg.tmax.ssp126)
gvocc.tmin.ssp126 <- gVoCC(tr.tmin.ssp126, sg.tmin.ssp126)
gvocc.tmin.ssp126 <- terra::rotate(gvocc.tmin.ssp126, left=TRUE)
gvocc.tmax.ssp126 <- terra::rotate(gvocc.tmax.ssp126, left=TRUE)

tr.tmin.ssp245 <- tempTrend(yr.tmin.ssp245, th=30)
sg.tmin.ssp245 <- spatGrad(yr.tmin.ssp245, th = 0.1, projected = FALSE)
tr.tmax.ssp245 <- tempTrend(yr.tmax.ssp245, th=30)
sg.tmax.ssp245 <- spatGrad(yr.tmax.ssp245, th = 0.1, projected = FALSE)

gvocc.tmax.ssp245 <- gVoCC(tr.tmax.ssp245, sg.tmax.ssp245)
gvocc.tmin.ssp245 <- gVoCC(tr.tmin.ssp245, sg.tmin.ssp245)
gvocc.tmin.ssp245 <- terra::rotate(gvocc.tmin.ssp245, left=TRUE)
gvocc.tmax.ssp245 <- terra::rotate(gvocc.tmax.ssp245, left=TRUE)

tr.tmin.ssp370 <- tempTrend(yr.tmin.ssp370, th=30)
sg.tmin.ssp370 <- spatGrad(yr.tmin.ssp370, th = 0.1, projected = FALSE)
tr.tmax.ssp370 <- tempTrend(yr.tmax.ssp370, th=30)
sg.tmax.ssp370 <- spatGrad(yr.tmax.ssp370, th = 0.1, projected = FALSE)

gvocc.tmax.ssp370 <- gVoCC(tr.tmax.ssp370, sg.tmax.ssp370)
gvocc.tmin.ssp370 <- gVoCC(tr.tmin.ssp370, sg.tmin.ssp370)
gvocc.tmin.ssp370 <- terra::rotate(gvocc.tmin.ssp370, left=TRUE)
gvocc.tmax.ssp370 <- terra::rotate(gvocc.tmax.ssp370, left=TRUE)

tr.tmin.ssp585 <- tempTrend(yr.tmin.ssp585, th=30)
sg.tmin.ssp585 <- spatGrad(yr.tmin.ssp585, th = 0.1, projected = FALSE)
tr.tmax.ssp585 <- tempTrend(yr.tmax.ssp585, th=30)
sg.tmax.ssp585 <- spatGrad(yr.tmax.ssp585, th = 0.1, projected = FALSE)

gvocc.tmax.ssp585 <- gVoCC(tr.tmax.ssp585, sg.tmax.ssp585)
gvocc.tmin.ssp585 <- gVoCC(tr.tmin.ssp585, sg.tmin.ssp585)
gvocc.tmin.ssp585 <- terra::rotate(gvocc.tmin.ssp585, left=TRUE)
gvocc.tmax.ssp585 <- terra::rotate(gvocc.tmax.ssp585, left=TRUE)


# Calculating area internally
# wdpa0
a1 <- resTime(wdpa0, gvocc.tmin.ssp126[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
a2 <- resTime(wdpa0, gvocc.tmax.ssp126[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
restime.wdpa0.ssp126 <- cbind(as.data.frame(a1), as.data.frame(a2))
colnames(restime.wdpa0.ssp126) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")
restime.wdpa0.ssp126["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.ssp126)<-sapply(restime.wdpa0.ssp126, is.infinite)
restime.wdpa0.ssp126 <- cbind(wdpa0, restime.wdpa0.ssp126)
write.csv(as.data.frame(restime.wdpa0.ssp126), file="WDPA_Amer0_ResTime_SSP126.csv")

a1 <- resTime(wdpa0, gvocc.tmin.ssp245[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
a2 <- resTime(wdpa0, gvocc.tmax.ssp245[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
restime.wdpa0.ssp245 <- cbind(as.data.frame(a1), as.data.frame(a2))
colnames(restime.wdpa0.ssp245) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")
restime.wdpa0.ssp245["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.ssp245)<-sapply(restime.wdpa0.ssp245, is.infinite)
restime.wdpa0.ssp245 <- cbind(wdpa0, restime.wdpa0.ssp245)
write.csv(as.data.frame(restime.wdpa0.ssp245), file="WDPA_Amer0_ResTime_SSP245.csv")


a1 <- resTime(wdpa0, gvocc.tmin.ssp370[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
a2 <- resTime(wdpa0, gvocc.tmax.ssp370[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
restime.wdpa0.ssp370 <- cbind(as.data.frame(a1), as.data.frame(a2))
colnames(restime.wdpa0.ssp370) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")
restime.wdpa0.ssp370["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.ssp370)<-sapply(restime.wdpa0.ssp370, is.infinite)
restime.wdpa0.ssp370 <- cbind(wdpa0, restime.wdpa0.ssp370)
write.csv(as.data.frame(restime.wdpa0.ssp370), file="WDPA_Amer0_ResTime_SSP370.csv")

a1 <- resTime(wdpa0, gvocc.tmin.ssp585[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
a2 <- resTime(wdpa0, gvocc.tmax.ssp585[[1]], areapg=as.numeric(as.numeric(levels(wdpa0$Area_km2))[wdpa0$Area_km2]))
restime.wdpa0.ssp585 <- cbind(as.data.frame(a1), as.data.frame(a2))
colnames(restime.wdpa0.ssp585) <- c("ID", "v.tmin", "d.tmin", "resTime.tmin", "ID2", "v.tmax", "d.tmax", "resTime.tmax")
restime.wdpa0.ssp585["NAME"] <- wdpa0@data$NAME
is.na(restime.wdpa0.ssp585)<-sapply(restime.wdpa0.ssp585, is.infinite)
restime.wdpa0.ssp585 <- cbind(wdpa0, restime.wdpa0.ssp585)
write.csv(as.data.frame(restime.wdpa0.ssp585), file="WDPA_Amer0_ResTime_SSP585.csv")

save.image("VoGG_WDPA_America.RData")

# save ncdf gvocc 
writeRaster(gvocc.tmin, "gvocc_tmin.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")

writeRaster(gvocc.tmax, "gvocc_tmax.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")

writeRaster(gvocc.tmin.ssp126, "gvocc_tmin_ssp126.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmin.ssp245, "gvocc_tmin_ssp245.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmin.ssp370, "gvocc_tmin_ssp370.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmin.ssp585, "gvocc_tmin_ssp585.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")


writeRaster(gvocc.tmax.ssp126, "gvocc_tmax_ssp126.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmax.ssp245, "gvocc_tmax_ssp245.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmax.ssp370, "gvocc_tmax_ssp370.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")
writeRaster(gvocc.tmax.ssp585, "gvocc_tmax_ssp585.nc", overwrite=T,
            format="CDF", varname="gvocc", varunit="km/year", xname="X",
            yname="Y", zname="nbands", zunit="numeric")

