library(raster)
library(rgdal)

#log forest cover at different time points 
f51 
f77 
f91 
f00

###TABHET
#load the Data
TABHET<- raster("/Users/lauramoro/Documents/PUERTO_RICO/Tree SDMs/Tree SDMs/TABHET.grd")

#set the CRS of TABHET
crs(TABHET) <- "+proj=longlat +datum=WGS84 +no_defs"
pTABHET <- projectRaster(TABHET, f51, method='ngb')

#habitat suitability and forest cover for TABHET 1951-1977-1991-2000
TABHET_ov_f51 <- overlay(f51, pTABHET, fun = function(r1, r2) { return( r1 * r2) })
TABHET_ov_f51[TABHET_ov_f51<0] <- NA
plot(TABHET_ov_f51)

TABHET_ov_f77 <- overlay(f77, pTABHET, fun = function(r1, r2) { return( r1 * r2) })
TABHET_ov_f77[TABHET_ov_f77<0] <- NA
plot(TABHET_ov_f77)

TABHET_ov_f91<- overlay(f91, pTABHET, fun = function(r1, r2) { return( r1 * r2) })
TABHET_ov_f91[TABHET_ov_f91<0] <- NA
plot(TABHET_ov_f91)

TABHET_ov_f00 <- overlay(f00, pTABHET, fun = function(r1, r2) { return( r1 * r2) })
TABHET_ov_f00[TABHET_ov_f00<0] <- NA
plot(TABHET_ov_f00)

##TABSCH

TABSCH<- raster("/Users/lauramoro/Documents/PUERTO_RICO/Tree SDMs/Tree SDMs/TABSCH.grd")
crs(TABSCH) <- "+proj=longlat +datum=WGS84 +no_defs" 
pTABSCH <- projectRaster(TABSCH, f51, method='ngb')

#habitat suitability and forest cover for TABHET 1951-1977-1991-2000
TABSCH_ov_f51 <- overlay(f51, pTABSCH, fun = function(r1, r2) { return( r1 * r2) })
TABSCH_ov_f51[TABSCH_ov_f51<0] <- NA
plot(TABSCH_ov_f51)

TABSCH_ov_f77 <- overlay(f77, pTABSCH, fun = function(r1, r2) { return( r1 * r2) })
TABSCH_ov_f77[TABSCH_ov_f77<0] <- NA
plot(TABSCH_ov_f77)

TABSCH_ov_f91 <- overlay(f91, pTABSCH, fun = function(r1, r2) { return( r1 * r2) })
TABSCH_ov_f91[TABSCH_ov_f91<0] <- NA
plot(TABSCH_ov_f91)

TABSCH_ov_f00 <- overlay(f00, pTABSCH, fun = function(r1, r2) { return( r1 * r2) })
TABSCH_ov_f00[TABSCH_ov_f00<0] <- NA
plot(TABSCH_ov_f00)


##TABSC

TABRIG<- raster("/Users/lauramoro/Documents/PUERTO_RICO/Tree SDMs/Tree SDMs/TABRIG.gri")
crs(TABRIG) <- "+proj=longlat +datum=WGS84 +no_defs" 
pTABRIG <- projectRaster(TABSCH, f51, method='ngb')

TABRIG_ov_f51 <- overlay(f51, pTABRIG, fun = function(r1, r2) { return( r1 * r2) })
TABRIG_ov_f51[TABRIG_ov_f51<0] <- NA
plot(TABRIG_ov_f51)

TABRIG_ov_f77 <- overlay(f77, pTABRIG, fun = function(r1, r2) { return( r1 * r2) })

TABRIG_ov_f77[TABRIG_ov_f77<0] <- NA
plot(TABRIG_ov_f77)

TABRIG_ov_f91 <- overlay(f91, pTABRIG, fun = function(r1, r2) { return( r1 * r2) })
TABRIG_ov_f91[TABRIG_ov_f91<0] <- NA
plot(TABRIG_ov_f91)

TABRIG_ov_f00 <- overlay(f00, pTABRIG, fun = function(r1, r2) { return( r1 * r2) })
TABRIG_ov_f00[TABRIG_ov_f00<0] <- NA
plot(TABRIG_ov_f00)



