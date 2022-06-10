## ---- LoadPackages --------

library(terra)
library(magrittr)
library(geodata)
library(sf)


## ---- TemplateRaster --------

Template <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif")

values(Template) <- 1


## ---- DenmarkArea --------

DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd(), version = "4.0") %>%
  terra::project(terra::crs(Template))
Area_DK <- terra::expanse(DK)

## ---- PlotDenmark --------

plot(DK, col = "grey")

## ---- Paragraph-3-Klit-raster --------

# read Paragraph 3

Paragraph3 <-  vect("O:/Nat_BDR-data/Arealanalyse/RAW/BES_NATURTYPER_SHAPE")

Paragraph3 <- Paragraph3[,c("Objekt_id", "Natyp_navn")]

Paragraph3_by_nature <- aggregate(Paragraph3, by='Natyp_navn')

terra::writeVector(Paragraph3_by_nature, "O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp", overwrite = T)

Habs <- terra::vect("O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp")

# read klits

Klits <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/MATRIKELKORT/DK_SHAPE_UTM32-EUREF89/MINIMAKS/TEMA/KLIT.shp")

Klits$Natyp_navn <- "Klit"


Klits <- terra::aggregate(Klits, by = "Natyp_navn") %>% terra::project(terra::crs(Habs))

# joint both polygons

Habs2 <- rbind(Habs, Klits)

# rasterize to take out overlaps

Rast_p3_klit  <- terra::rasterize(Habs2, Template, field = "Natyp_navn")

## ---- Write-Paragraph-3-Klit --------

# Write raw rasters to disk

writeRaster(Rast_p3_klit, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))
Rast_p3_klit_Croped <- terra::mask(Rast_p3_klit, DK)

# Write croped rasters to disk

writeRaster(Rast_p3_klit_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit.tif",
               destination = "RasterizedCOG/Rast_COG_p3_klit.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif",
               destination = "RasterizedCOG/Rast_COG_p3_klit_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotP3klit --------

plot(DK, col = "grey")
plot(Rast_p3_klit_Croped, add =T)


## ---- Rasterize-Natura2000 --------

# Read the layer

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Natura2000 MiljøGIS Maj2022/pg-natura_2000_omraader_natura2000.shp")

# Get the layer

Natura2000 <- Natura2000[,"temanavn"]

# Aggregate to multypolygon

Natura2000 <- terra::aggregate(Natura2000, by = "temanavn")

# Select feature

Natura2000 <- Natura2000[,"temanavn"]

# change feature name

names(Natura2000) <- "Temanavn"

# Add a feature for subsetting

Natura2000$Natura2000 <- "yes"

# Rasterize

Rast_Natura2000  <- terra::rasterize(Natura2000, Template, field = "Natura2000")


# Crop to Denmark

Rast_Natura2000_Croped <- terra::mask(Rast_Natura2000, DK)

## ---- Write-Natura2000 --------

writeRaster(Rast_Natura2000, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_Natura2000_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

# Save as cloud optimized raster

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif",
               destination = "RasterizedCOG/Rast_Natura2000.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif",
               destination = "RasterizedCOG/Rast_Natura2000_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotNatura-2000 --------

plot(DK, col = "grey")
plot(Rast_Natura2000_Croped, add =T)
