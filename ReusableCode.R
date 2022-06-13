## ---- LoadPackages --------

library(terra)
library(magrittr)
library(geodata)
library(sf)
library(tidyverse)


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

## ---- Rasterize-markblokkort --------

# read the poltygons

markblokkort <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Markblokke2021/Markblokke2021.shp")

# Select only permanent grasslands "PGR" and plough area "OMD"

markblokkort <- markblokkort[(markblokkort$MB_TYPE %in% c("OMD", "PGR")),7]

# Transform to multipolygon

markblokkort_Aggregated <- terra::aggregate(markblokkort, by='MB_TYPE')

# Rasterize

Rast_markblokkort  <- terra::rasterize(markblokkort_Aggregated, Template, field = "MB_TYPE")

# Crop to Denmark

Rast_markblokkort_Croped <- terra::mask(Rast_markblokkort, DK)

## ---- Write-markblokkort --------

writeRaster(Rast_markblokkort, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_markblokkort_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort.tif",
               destination = "RasterizedCOG/Rast_markblokkort.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif",
               destination = "RasterizedCOG/Rast_markblokkort_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- Plotmarkblokkort --------

plot(DK, col = "grey")
plot(Rast_markblokkort_Croped, add =T)

## ---- Rasterize-NaturaOgVildtreservater --------

# read the polygons

Wildreserve <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NATUR_VILDT_RESERVATER/NATUR_VILDT_RESERVATER.shp")

# Eliminate the some of the reserves

NaturaOgVildtreservater <- Wildreserve[!(Wildreserve$Beken_navn %in% c("Agerø og Skibsted Fjord", "Agger Tange", "Anholt",
                                                                       "Ertholmene", "Hesselø", "Hirsholmene",
                                                                       "Horsens Nørrestrand",  "Vorsø")),]

# Transform to multipolygon

NaturaOgVildtreservater_Aggregated <- terra::aggregate(NaturaOgVildtreservater, by='Temanavn')

# Rasterize

Rast_NaturaOgVildtreservater  <- terra::rasterize(NaturaOgVildtreservater_Aggregated, Template, field = "Temanavn")

# And crop

Rast_NaturaOgVildtreservater_Croped <- terra::mask(Rast_NaturaOgVildtreservater, DK)

## ---- Write-NaturaOgVildtreservater --------

writeRaster(Rast_NaturaOgVildtreservater, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_NaturaOgVildtreservater_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater.tif",
               destination = "RasterizedCOG/Rast_NaturaOgVildtreservater.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped.tif",
               destination = "RasterizedCOG/Rast_NaturaOgVildtreservater_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))


## ---- PlotNaturaOgVildtreservater --------

plot(DK, col = "grey")
plot(Rast_NaturaOgVildtreservater_Croped, add =T)

## ---- Rasterize-IUCN --------

# Read

IUCN <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/IUCN REVISED FREDNINGER/Fredninger_IUCNKat_2018_25832.shp")
IUCN$IUCN <- "Yes"

IUCN_Aggregated <- terra::aggregate(IUCN, by='IUCN')

Sys.time()
Rast_IUCN  <- terra::rasterize(IUCN_Aggregated, Template, field = "IUCN")
Sys.time()

Rast_IUCN_Croped <- terra::mask(Rast_IUCN, DK)


## ---- Write-IUCN --------

writeRaster(Rast_IUCN, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_IUCN_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN.tif",
               destination = "RasterizedCOG/Rast_IUCN.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN_Croped.tif",
               destination = "RasterizedCOG/Rast_IUCN_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotIUCN --------

plot(DK, col = "grey")
plot(Rast_NaturaOgVildtreservater_Croped, add =T)

## ---- Rasterize-Urort-Skov --------

# Read state owned untouched forest

Urort_Skov <- list.files(path = "O:/Nat_BDR-data/Arealanalyse/RAW/Uroert skov NST Feb2022/", full.names = T, pattern = "shp") %>%
  purrr::map(vect) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Template))

Urort_Skov$Owned <- "State"

# Read private owned untouched forest

private_Urort_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp") %>%
  terra::project(crs(Template))
private_Urort_Skov <- private_Urort_Skov[private_Urort_Skov$tilskudsor== "Privat urørt skov", ]

private_Urort_Skov$Owned <- "Private"

# join geometries

Urort_Skov <- rbind(Urort_Skov, private_Urort_Skov)
Urort_Skov <- Urort_Skov[,"Owned"]

# fix validity problems

Urort_Skov <- Urort_Skov %>% terra::makeValid()

# Transform to multipolygons

Urort_Skov_Aggregated <- terra::aggregate(Urort_Skov, by='Owned')

# Rasterize and crop

Rast_Urort_Skov  <- terra::rasterize(Urort_Skov_Aggregated, Template, field = "Owned")

Rast_Urort_Skov_Croped <- terra::mask(Rast_Urort_Skov, DK)

## ---- Write-Urort-skov --------

writeRaster(Rast_Urort_Skov, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_Urort_Skov_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov.tif",
               destination = "RasterizedCOG/Rast_Urort_Skov.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov_Croped.tif",
               destination = "RasterizedCOG/Rast_Urort_Skov_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))


## ---- PlotUrortSkov --------

plot(DK, col = "grey")
plot(Rast_Urort_Skov_Croped, add =T)

## ---- Rasterize-naturnationalpark --------

# Read state owned untouched forest
National_Parks <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/GIS filer - de 5/GIS filer - de 5/Naturnationalparker.shp") %>%
  terra::project(crs(Template))

National_Parks$ID <- "NationalParks"

National_Parks_Aggregated <- terra::aggregate(National_Parks, by='ID')

Rast_National_Parks  <- terra::rasterize(National_Parks_Aggregated, Template, field = "ID")


Rast_National_Parks_Croped <- terra::mask(Rast_National_Parks, DK)


## ---- Write-naturnationalpark --------

writeRaster(Rast_National_Parks, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_National_Parks_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks.tif",
               destination = "RasterizedCOG/Rast_National_Parks.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks_Croped.tif",
               destination = "RasterizedCOG/Rast_National_Parks_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))


## ---- Rasterize-stoette --------

# Read Private owned untouched forest

stoette_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_Skov <- stoette_Skov[stoette_Skov$tilskudsor== "Privat urørt skov", ]

stoette_Skov$Type <- "Skov"

# Read sammenhaengende

stoette_sammenhaengende <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_sammenhaengende <- stoette_sammenhaengende[stoette_sammenhaengende$tilskudsor== "Sammenhængende arealer", ]
stoette_sammenhaengende$Type <- "sammenhaengende"


# read egekrat

stoette_egekrat <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/EGEKRAT/egekrat.shp")

stoette_egekrat <- stoette_egekrat[stoette_egekrat$vurdering_ %in% c(1,2),]
stoette_egekrat <- stoette_egekrat[stoette_egekrat$sikret %in% c("ja"),]
stoette_egekrat$Type <- "egekrat"

# Join them together

stoette <- list(stoette_Skov, stoette_sammenhaengende, stoette_egekrat) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Template))

# Transform to multipolygon

stoette <- stoette[,"Type"]
stoette_Aggregated <- terra::aggregate(stoette, by='Type')

# Rasterize

Rast_stoette  <- terra::rasterize(stoette_Aggregated, Template, field = "Type")

Rast_stoette_Croped <- terra::mask(Rast_stoette, DK)

## ---- Write-stoette --------

writeRaster(Rast_stoette, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_stoette_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette.tif",
               destination = "RasterizedCOG/Rast_stoette.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette_Croped.tif",
               destination = "RasterizedCOG/Rast_stoette_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- Plotstoette --------

plot(DK, col = "grey")
plot(Rast_stoette_Croped, add =T)


## ---- AllStack --------

All <- c(Rast_Urort_Skov_Croped, Rast_stoette_Croped, Rast_p3_klit_Croped, Rast_markblokkort_Croped, Rast_NaturaOgVildtreservater_Croped, Rast_Natura2000_Croped, Rast_National_Parks_Croped, Rast_IUCN_Croped)


## ---- crosstab --------

Area <- crosstab(All, useNA=T, long=TRUE)

saveRDS(Area, "Area_Total.rds")

## ---- summary-table --------

# First remove all areas that are NA for all categories

Area <- readRDS("Area_Total.rds")
Area2 <- Area[!(rowSums(is.na(Area)) == max(rowSums(is.na(Area)))),]

# Change the names of the columns of Paragraph 3 from Natyp_navn to Habitats_P3 and change numbers for actual names

Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(Rast_p3_klit_Croped)[[1]]) - 1), Habitats_P3 = levels(Rast_p3_klit_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(Paragraph3_DF) %>%
  dplyr::select(-Natyp_navn)

# Change the names of the columns of Natura 2000 from Natura2000 to Natura_2000 and change numbers for actual names

Natura2000_DF <- data.frame(Natura2000 = 0, Natura_2000 = "Yes")

Area2 <- Area2 %>%
  full_join(Natura2000_DF) %>%
  dplyr::select(-Natura2000)

# Change the names of the columns of markblokkort from MB_TYPE to Types_markblokkort and change numbers for actual names

markblokkort_DF <- data.frame(MB_TYPE = 0:(length(levels(Rast_markblokkort_Croped)[[1]]) - 1), Types_markblokkort = levels(Rast_markblokkort_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(markblokkort_DF) %>%
  dplyr::select(-"MB_TYPE") %>%
  mutate(Area_Sq_Mt = 100*Freq,
         Proportion = 100*(Area_Sq_Mt/Area_DK)) %>%
  dplyr::select(-Freq) %>%
  ungroup()

# Change the names of the columns of IUCN from IUCN to IS_IUCN and change numbers for actual names

IUCN_DF <- data.frame(IUCN = 0, IS_IUCN = "Yes")

Area2 <- Area2 %>%
  full_join(IUCN_DF) %>%
  dplyr::select(-IUCN) %>%
  rename(IUCN= IS_IUCN)

# Change the names of the columns of Urort_Skov from Owned to Urort_Skov and change numbers for actual names

Urort_Skov_DF <- data.frame(Owned = 0:(length(levels(Rast_Urort_Skov_Croped)[[1]]) - 1), Urort_Skov = levels(Rast_Urort_Skov_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(Urort_Skov_DF) %>%
  dplyr::select(-Owned)

# Change the names of the columns of stoette from Type to Stoette and change numbers for actual names

stoette_DF <- data.frame(Type  = 0:(length(levels(Rast_stoette_Croped)[[1]]) - 1), Stoette = levels(Rast_stoette_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(stoette_DF) %>%
  dplyr::select(-Type)

# Change the names of the columns of NaturaOgVildtreservater_DF from Temanavn to NaturaOgVildtreservater and change numbers for actual names

NaturaOgVildtreservater_DF <- data.frame(Temanavn  = 0, NaturaOgVildtreservater = "Yes")

Area2 <- Area2 %>%
  full_join(NaturaOgVildtreservater_DF) %>%
  dplyr::select(-Temanavn)

# Change the names of the columns of Naturnationalparker from ID to Naturnationalparker and change numbers for actual names

Naturnationalparker_DF <- data.frame(ID  = 0, Naturnationalparker = "Yes")

Area2 <- Area2 %>%
  full_join(Naturnationalparker_DF) %>%
  dplyr::select(-ID) %>%
  dplyr::relocate(Proportion, .after = everything()) %>%
  dplyr::relocate(Area_Sq_Mt, .after = everything())

# Save dataframe as rds

saveRDS(Area2, "Area_summary.rds")

## ---- table-overlap --------

knitr::kable(Area2, digits = 3, caption = "Number of cells that are shared by different groups of rastercells")

## ---- table-general-overlap-creation --------

# eliminate cells that are not natura 2000 and separate overlapped and not overlapped cells and summarize

Natura2000_Table1a <- Area2 %>%
  dplyr::filter(!is.na(Natura_2000)) %>%
  mutate(Overlaped = case_when(is.na(Habitats_P3) & is.na(Types_markblokkort) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(NaturaOgVildtreservater) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(Natura_2000, Overlaped) %>%
  summarise_if(is.numeric, sum)

Natura2000_Table1_Totals <- Natura2000_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Nautra_2000")

Natura2000_Table1_Appart <- Natura2000_Table1a %>%
  mutate(Class = "Nautra_2000") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Natura_2000) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Natura2000_Table1 <- full_join(Natura2000_Table1_Totals, Natura2000_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not paragraph 3 and separate overlapped and not overlapped cells and summarize


Paragraph3_Table1a <- Area2 %>%
  dplyr::filter(!is.na(Habitats_P3)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(NaturaOgVildtreservater) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes"),
         Paragrah3 = "yes") %>%
  group_by(Paragrah3, Overlaped) %>%
  summarise_if(is.numeric, sum)

Paragraph3_Table1_Totals <- Paragraph3_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Paragraph_3")

Paragraph3_Table1_Appart <- Paragraph3_Table1a %>%
  mutate(Class = "Paragraph_3") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Paragrah3) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Paragraph3_Table1 <- full_join(Paragraph3_Table1_Totals, Paragraph3_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not NaturaOgVildtreservater and separate overlapped and not overlapped cells and summarize

NaturaOgVildtreservater_Table1a <- Area2 %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(Habitats_P3) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(NaturaOgVildtreservater, Overlaped) %>%
  summarise_if(is.numeric, sum)

NaturaOgVildtreservater_Table1_Totals <- NaturaOgVildtreservater_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "NaturaOgVildtreservater")

NaturaOgVildtreservater_Table1_Appart <- NaturaOgVildtreservater_Table1a %>%
  mutate(Class = "NaturaOgVildtreservater") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -NaturaOgVildtreservater) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

NaturaOgVildtreservater_Table1 <- full_join(NaturaOgVildtreservater_Table1_Totals, NaturaOgVildtreservater_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not IUCN and separate overlapped and not overlapped cells and summarize

IUCN_Table1a <- Area2 %>%
  dplyr::filter(!is.na(IUCN)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(Urort_Skov) & is.na(Stoette) & is.na(Habitats_P3) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(IUCN, Overlaped) %>%
  summarise_if(is.numeric, sum)

IUCN_Table1_Totals <- IUCN_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "IUCN")

IUCN_Table1_Appart <- IUCN_Table1a %>%
  mutate(Class = "IUCN") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -IUCN) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

IUCN_Table1 <- full_join(IUCN_Table1_Totals, IUCN_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not Untouched forest and separate overlapped and not overlapped cells and summarize


Urort_Skov_Table1a <- Area2 %>%
  dplyr::filter(!is.na(Urort_Skov)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Stoette) & is.na(Habitats_P3) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes")) %>%
  mutate(Urort_Skov = "Yes") %>%
  group_by(Urort_Skov, Overlaped) %>%
  summarise_if(is.numeric, sum)

Urort_Skov_Table1_Totals <- Urort_Skov_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Urort_Skov")

Urort_Skov_Table1_Appart <- Urort_Skov_Table1a %>%
  mutate(Class = "Urort_Skov") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Urort_Skov) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Urort_Skov_Table1 <- full_join(Urort_Skov_Table1_Totals, Urort_Skov_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not subsidized areas and separate overlapped and not overlapped cells and summarize

Stoette_Table1a <- Area2 %>%
  dplyr::filter(!is.na(Stoette)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) & is.na(Naturnationalparker) ~ "No",
                               TRUE ~ "Yes")) %>%
  mutate(Stoette = "Yes") %>%
  group_by(Stoette, Overlaped) %>%
  summarise_if(is.numeric, sum)

Stoette_Table1_Totals <- Stoette_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Stoette")

Stoette_Table1_Appart <- Stoette_Table1a %>%
  mutate(Class = "Stoette") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Stoette) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Stoette_Table1 <- full_join(Stoette_Table1_Totals, Stoette_Table1_Appart) %>%
  relocate(Class, .before = everything())

# eliminate cells that are not Naturnationalparker and separate overlapped and not overlapped cells and summarize

Naturnationalparker_Table1a <- Area2 %>%
  dplyr::filter(!is.na(Naturnationalparker)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) & is.na(Stoette) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(Naturnationalparker, Overlaped) %>%
  summarise_if(is.numeric, sum)

Naturnationalparker_Table1_Totals <- Naturnationalparker_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Naturnationalparker")

Naturnationalparker_Table1_Appart <- Naturnationalparker_Table1a %>%
  mutate(Class = "Naturnationalparker") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Naturnationalparker) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Naturnationalparker_Table1 <- full_join(Naturnationalparker_Table1_Totals, Naturnationalparker_Table1_Appart) %>%
  relocate(Class, .before = everything())

# add a table for total area

Total_Area__Table1a <- Area2 %>%
  dplyr::filter(!is.na(Naturnationalparker)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) & is.na(Stoette) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(Naturnationalparker, Overlaped) %>%
  summarise_if(is.numeric, sum)

# put them all together

Table1 <- list(Natura2000_Table1, Paragraph3_Table1, NaturaOgVildtreservater_Table1, IUCN_Table1, Urort_Skov_Table1, Stoette_Table1, Naturnationalparker_Table1) %>%
  purrr::reduce(bind_rows)

# Save table 1

readr::write_csv(Table1, "Table1.csv")
saveRDS(Table1, "Table1.rds")

## ---- table-overlap-summarized --------

knitr::kable(Table1, digits = 3, caption = "Areas that are exclusive or overlapped between different groups")
