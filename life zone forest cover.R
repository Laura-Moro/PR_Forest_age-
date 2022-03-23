library(raster)
library(rgdal)

# Load Helmer age map
age <- raster("/Users/lauramoro/Documents/PUERTO_RICO/Forest_Age_Helmer/Maps_1951-2000/iitf_jgr113_puertorico_forestage_zone_reprojectedWGS84.tif")

### READ IN DATA
r51 <- raster("/Users/lauramoro/Documents/PUERTO_RICO/Forest_Age_Helmer/Maps_1951-2000/puerto51_sub1_100905_landcov_final.img")

r77 <- raster("/Users/lauramoro/Documents/PUERTO_RICO/Forest_Age_Helmer/Maps_1951-2000/puerto77_sub1_100905_landcov_urbveg_final.img")

r91 <- raster("/Users/lauramoro/Documents/PUERTO_RICO/Forest_Age_Helmer/Maps_1951-2000/pr91_100805_final_quarry_recode2landcov_subset.img")

r00 <- raster("/Users/lauramoro/Documents/PUERTO_RICO/Forest_Age_Helfmer/Maps_1951-2000/pr2000_100805_final_quarry_recode2landcov_subset.img")

### CROP AND REPROJECT
rp51 <- projectRaster(r51, age, method='ngb')
rp77 <- projectRaster(r77, age, method='ngb')
rp91 <- projectRaster(r91, age, method='ngb')
rp00 <- projectRaster(r00, age, method='ngb')

### RECLASSIFY FOREST AREAS
f51 <- rp51 %in% 5
f77 <- rp77 %in% c(5,7)
f91 <- rp91 %in% c(5,7)
f00 <- rp00 %in% c(5,7)

plot <- stack(f51, f77, f91, f00)p





## USING RASTER
### READ LIFE ZONE AND RASTERIZE
lz <- readOGR("/Users/lauramoro/Documents/PUERTO_RICO/Land_Cover_GAP/Data/Lifezones/lifezones_Project.shp")

lzr <- rasterize(lz['ECOZONE'], age)

lzr@data@attributes[[1]]$lz <- as.numeric(as.factor(lzr@data@attributes[[1]]$ECOZONE))

lzr2 <- reclassify(lzr, lzr@data@attributes[[1]][,c("ID","lz")])

# Get results using the polygons to mask different areas
result_raster <- matrix(nrow=length(unique(lzr@data@attributes[[1]]$lz)), ncol=4)
rownames(result_raster) <- sort(unique(lzr@data@attributes[[1]]$ECOZONE))
colnames(result_raster) <- c(1951, 1977, 1991, 2000)

for(i in 1:length(unique(lzr@data@attributes[[1]]$lz))){
  print(i)
  tmp <- f *(lzr2 == i)
  
  # TOTAL NUMBER OF PIXELS
  result_raster[i,] <- cellStats(tmp, sum)
  
  # PROPORTION OF PIXELS
  # result_raster[i,] <- cellStats(tmp, sum) / cellStats((lzr2 == i), sum)
}

plot(c(1951, 1977, 1991, 2000), result_raster[1,], 
     type='l', ylim=c(0, max(result_raster)), lwd=3)


for(i in 2:6){
  lines(c(1951, 1977, 1991, 2000), result_raster[i,], col=i, lwd=3)
}


legend("topleft", legend=rownames(result_raster), col=1:6, lty=1, bty='n', cex=.75, lwd=3)



### USING SHAPEFILES
# Get results using the polygons to mask different areas

lz <- readOGR("/Users/lauramoro/Documents/PUERTO_RICO/Land_Cover_GAP/Data/Lifezones/lifezones_Project.shp")

result <- matrix(nrow=length(unique(lz$ECOZONE)), ncol=4)
rownames(result) <- unique(lz$ECOZONE)
colnames(result) <- c(1951, 1977, 1991, 2000)

for(i in 1:length(unique(lz$ECOZONE))){
  focal_lz <- unique(lz$ECOZONE)[i]
  tmp <- mask(f, lz[lz$ECOZONE == focal_lz,])
  result[i,] <- cellStats(tmp, sum)
}

plot(c(1951, 1977, 1991, 2000), result[1,], 
     type='l', ylim=c(0,max(result)), lwd=3)

for(i in 2:6){
  lines(c(1951, 1977, 1991, 2000), result[i,], col=i, lwd=3)
}

legend("topleft", legend=rownames(result), col=1:6, lty=1, bty='n', cex=.75, lwd=3)

