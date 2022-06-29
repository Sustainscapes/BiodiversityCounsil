## ---- LoadPackages --------

library(terra)
library(magrittr)
library(geodata)
library(sf)
library(tidyverse)
library(mregions)


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
National_Parks <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NNP from NST/Naturnationalparker.shp") %>%
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


## ---- Ownership --------

Ownership <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Archive/ejerskab_20220609.gpkg")
Ownership$Ownership <- ifelse(Ownership$ejerforhold_dni %in% c("fond", "andet"), "Privat",
                              ifelse(Ownership$ejerforhold_dni %in% c("naturstyrelsen", "forsvaret","landbrugsstyrelsen", "kystdirektoratet"), "Statslig", "Kommunal"))

Ownersip_Aggregated <- terra::aggregate(Ownership, by = "Ownership")

Rast_Ownersip  <- terra::rasterize(Ownersip_Aggregated, Template, field = "Ownership")
Rast_Ownersip_Croped <- terra::mask(Rast_Ownersip, DK)

writeRaster(Rast_Ownersip, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ownersip.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_Ownersip_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ownersip_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ownersip.tif",
               destination = "RasterizedCOG/Rast_Ownersip.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

sf::gdal_utils("warp",
               source ="O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ownersip_Croped.tif",
               destination = "RasterizedCOG/Rast_Ownersip_Croped.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- Plotownership --------

plot(DK, col = "grey")
plot(Rast_Ownersip_Croped, add =T)

## ---- AllStack --------

All <- c(Rast_Urort_Skov_Croped, Rast_stoette_Croped, Rast_p3_klit_Croped, Rast_markblokkort_Croped, Rast_NaturaOgVildtreservater_Croped, Rast_Natura2000_Croped, Rast_National_Parks_Croped, Rast_IUCN_Croped)
writeRaster(All, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/All.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))


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


## ---- table-overlap --------

Area2 <- Area2 %>%
  dplyr::arrange(desc(Proportion)) %>%
  mutate(Area_Sq_Km = Area_Sq_Mt/1000000) %>%
  dplyr::select(-Area_Sq_Mt)

# Save dataframe as rds

saveRDS(Area2, "Area_summary.rds")
readr::write_csv(Area2, "Area_summary.csv")
openxlsx::write.xlsx(Area2, "Area_summary.xlsx")

knitr::kable(Area2, digits = 3, caption = "Number of cells that are shared by different groups of rastercells", format.args	= list(big.mark = ','))

## ---- table-general-overlap-creation --------

# eliminate cells that are not natura 2000 and separate overlapped and not overlapped cells and summarize

Area2 <- readRDS("Area_summary.rds")

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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
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


## ---- table-overlap-summarized --------

Area_summary <- readRDS("Area_summary.rds")

ForTotal <- Area_summary %>% dplyr::select(-Types_markblokkort)

ForTotal <- ForTotal[rowSums(is.na(ForTotal)) != 7,]

TotalProtected <- ForTotal %>% summarise_if(is.numeric, sum)

TotalProtected <- ForTotal %>% summarise_if(is.numeric, sum) %>%
  mutate(Class = "Total") %>%
  relocate(Class, .before = everything())

NewTable <- dplyr::arrange(bind_rows(Table1, TotalProtected), desc(Proportion))

knitr::kable(NewTable, digits = 3, caption = "Areas that are exclusive or overlapped between different groups", format.args	= list(big.mark = ','))

# Save table 1

readr::write_csv(NewTable, "Table1.csv")
openxlsx::write.xlsx(NewTable, "Table1.xlsx")
saveRDS(NewTable, "Table1.rds")

## ---- generate-second-table --------

# read in data

Area_summary <- readRDS("Area_summary.rds")

# Total

# Total area

All <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) | !is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Urort_Skov) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All open habitats area

All_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & (!is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Urort_Skov) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker))) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
  relocate(Nature_content, .before = everything())

# All in untouched forest

All_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & (!is.na(Natura_2000) | !is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker))) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
  relocate(Nature_content, .before = everything())

# All in Permanent grasslands

All_PGR <- Area_summary %>%
  dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Urort_Skov) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
  relocate(Nature_content, .before = everything())

# All in Arable lands

All_OMD <- Area_summary %>%
  dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Urort_Skov) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
  relocate(Nature_content, .before = everything())

# All in Lakes

All_SO <- Area_summary %>%
  dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3)| !is.na(IUCN) | !is.na(Urort_Skov) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) & Habitats_P3 == "Sø") %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "All") %>%
  relocate(Nature_content, .before = everything())

# Joind toghether

Final_All <- list(All, All_Open_Nature, All_Urort_Skov, All_PGR, All_OMD,All_SO) %>%
  purrr::reduce(full_join)

######

# Natura_2000 total area

Natura_2000 <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000)) %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Natura_2000 open habitats area

Natura_2000_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & !is.na(Habitats_P3)) %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Natura_2000 in untouched forest

Natura_2000_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & !is.na(Urort_Skov)) %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Natura_2000 in Permanent grasslands

Natura_2000_PGR <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Natura_2000 in Arable lands

Natura_2000_OMD <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Natura_2000 in Lakes

Natura_2000_SO <- Area_summary %>%
  dplyr::filter(!is.na(Natura_2000) & Habitats_P3 == "Sø") %>%
  group_by(Natura_2000) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "Natura 2000") %>%
  dplyr::select(-Natura_2000) %>%
  relocate(Nature_content, .before = everything())

# Joind toghether

Final_Natura_2000 <- list(Natura_2000, Natura_2000_Open_Nature, Natura_2000_Urort_Skov, Natura_2000_PGR, Natura_2000_OMD,Natura_2000_SO) %>%
  purrr::reduce(full_join)

# Total area Paragraph 3

Habitats_P3 <- Area_summary %>%
  dplyr::filter(!is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

# Total area Paragraph 3

Habitats_P3_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Habitats_P3) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

# Paragraph 3 in untouched forest

Habitats_P3_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Habitats_P3) & !is.na(Urort_Skov)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

# Paragraph 3 in Permament grasslands

Habitats_P3_PGR <- Area_summary %>%
  dplyr::filter(Types_markblokkort == "PGR" & is.na(Habitats_P3) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

# Paragraph 3 in Plough areas

Habitats_P3_OMD <- Area_summary %>%
  dplyr::filter(Types_markblokkort == "OMD" & is.na(Habitats_P3) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

# Paragraph 3 in lakes

Habitats_P3_SO <- Area_summary %>%
  dplyr::filter(!is.na(Habitats_P3) & Habitats_P3 == "Sø") %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "Paragraph 3 and klit") %>%
  relocate(Nature_content, .before = everything())

#Join all

Final_Habitats_P3 <- list(Habitats_P3, Habitats_P3_Open_Nature, Habitats_P3_Urort_Skov, Habitats_P3_PGR, Habitats_P3_OMD,Habitats_P3_SO) %>%
  purrr::reduce(full_join)

# NaturaOgVildtreservater total area

NaturaOgVildtreservater <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater)) %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in open areas

NaturaOgVildtreservater_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater) & !is.na(Habitats_P3)) %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in untouched forests

NaturaOgVildtreservater_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater) & !is.na(Urort_Skov)) %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in Permanent grasslands

NaturaOgVildtreservater_PGR <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in Plough areas

NaturaOgVildtreservater_OMD <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in lakes

NaturaOgVildtreservater_SO <- Area_summary %>%
  dplyr::filter(!is.na(NaturaOgVildtreservater) & Habitats_P3 == "Sø") %>%
  group_by(NaturaOgVildtreservater) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "NaturaOgVildtreservater") %>%
  dplyr::select(-NaturaOgVildtreservater) %>%
  relocate(Nature_content, .before = everything())

# Join together

Final_NaturaOgVildtreservater <- list(NaturaOgVildtreservater, NaturaOgVildtreservater_Open_Nature, NaturaOgVildtreservater_Urort_Skov, NaturaOgVildtreservater_PGR, NaturaOgVildtreservater_OMD,NaturaOgVildtreservater_SO) %>%
  purrr::reduce(full_join)

# IUCN Total Area

IUCN <- Area_summary %>%
  dplyr::filter(!is.na(IUCN)) %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# IUCN in open areas

IUCN_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(IUCN) & !is.na(Habitats_P3)) %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# IUCN in untouched forest

IUCN_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(IUCN) & !is.na(Urort_Skov)) %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# IUCN in Permanent grasslands

IUCN_PGR <- Area_summary %>%
  dplyr::filter(!is.na(IUCN) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# IUCN in Plough areas

IUCN_OMD <- Area_summary %>%
  dplyr::filter(!is.na(IUCN) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# IUCN in Lakes

IUCN_SO <- Area_summary %>%
  dplyr::filter(!is.na(IUCN) & Habitats_P3 == "Sø") %>%
  group_by(IUCN) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "IUCN") %>%
  dplyr::select(-IUCN) %>%
  relocate(Nature_content, .before = everything())

# Join all togheter

Final_IUCN <- list(IUCN, IUCN_Open_Nature, IUCN_Urort_Skov, IUCN_PGR, IUCN_OMD,IUCN_SO) %>%
  purrr::reduce(full_join)

# Untouched forest

Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Urort_Skov in open areas

Urort_Skov_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Urort_Skov in untouched forest

Urort_Skov_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & !is.na(Urort_Skov)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Urort_Skov in Permanent grasslands

Urort_Skov_PGR <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Urort_Skov in Plough areas

Urort_Skov_OMD <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Urort_Skov in Lakes

Urort_Skov_SO <- Area_summary %>%
  dplyr::filter(!is.na(Urort_Skov) & Habitats_P3 == "Sø") %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "Urort_Skov") %>%
  relocate(Nature_content, .before = everything())

# Join all togheter

Final_Urort_Skov <- list(Urort_Skov, Urort_Skov_Open_Nature, Urort_Skov_Urort_Skov, Urort_Skov_PGR, Urort_Skov_OMD,Urort_Skov_SO) %>%
  purrr::reduce(full_join)

# Nationalparks


Naturnationalparker  <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Naturnationalparker in open areas

Naturnationalparker_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Naturnationalparker in untouched forest

Naturnationalparker_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker) & !is.na(Urort_Skov)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Naturnationalparker in Permanent grasslands

Naturnationalparker_PGR <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Naturnationalparker in Plough areas

Naturnationalparker_OMD <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Naturnationalparker in Lakes

Naturnationalparker_SO <- Area_summary %>%
  dplyr::filter(!is.na(Naturnationalparker) & Habitats_P3 == "Sø") %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "Naturnationalparker") %>%
  relocate(Nature_content, .before = everything())

# Join all togheter

Final_Naturnationalparker <- list(Naturnationalparker, Naturnationalparker_Open_Nature, Naturnationalparker_Urort_Skov, Naturnationalparker_PGR, Naturnationalparker_OMD,Naturnationalparker_SO) %>%
  purrr::reduce(full_join)

# Subsidies

Stoette  <- Area_summary %>%
  dplyr::filter(!is.na(Stoette)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Area = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Stoette in open areas

Stoette_Open_Nature <- Area_summary %>%
  dplyr::filter(!is.na(Stoette) & !is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Open_Nature = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Stoette in untouched forest

Stoette_Urort_Skov <- Area_summary %>%
  dplyr::filter(!is.na(Stoette) & !is.na(Urort_Skov)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(Urort_Skov = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Stoette in Permanent grasslands

Stoette_PGR <- Area_summary %>%
  dplyr::filter(!is.na(Stoette) & Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(PGR = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Stoette in Plough areas

Stoette_OMD <- Area_summary %>%
  dplyr::filter(!is.na(Stoette) & Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(OMD = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Stoette in Lakes

Stoette_SO <- Area_summary %>%
  dplyr::filter(!is.na(Stoette) & Habitats_P3 == "Sø") %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
  rename(SO = Area_Sq_Km) %>%
  mutate(Nature_content = "Stoette") %>%
  relocate(Nature_content, .before = everything())

# Join all togheter

Final_Stoette <- list(Stoette, Stoette_Open_Nature, Stoette_Urort_Skov, Stoette_PGR, Stoette_OMD,Stoette_SO) %>%
  purrr::reduce(full_join)

# Get everything together and arrange by total area


Total <- list(Final_All, Final_Natura_2000, Final_Habitats_P3, Final_NaturaOgVildtreservater, Final_IUCN, Final_Urort_Skov, Final_Naturnationalparker, Final_Stoette) %>%
  purrr::reduce(bind_rows) %>%
  arrange(desc(Area))

## ---- show-second-table --------

readr::write_csv(Total, "Total.csv")
openxlsx::write.xlsx(Total, "Total.xlsx")
saveRDS(Total, "Total.rds")


knitr::kable(Total, digits = 3, caption = "Total area for protected areas and potential protected areas", format.args	= list(big.mark = ','))

## ---- build-3d-table --------

Ownership <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ownersip_Croped.tif")

Natura2000 <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif")

Paragraph3 <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif")

Vildtreservater <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped.tif")

Urort_skov <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov_Croped.tif")

National_Parks <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks_Croped.tif")

ForTable <- c(Ownership, Natura2000, Paragraph3, Vildtreservater, Urort_skov, National_Parks)

Ownership_Long_Table <- terra::crosstab(ForTable, useNA=T, long=TRUE)

saveRDS(Ownership_Long_Table, "Ownership_Long_Table.rds")

Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(Paragraph3)[[1]]) - 1), Habitats_P3 = levels(Paragraph3)[[1]])

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(Paragraph3_DF) %>%
  dplyr::select(-Natyp_navn)

# Change the names of the columns of Natura 2000 from Natura2000 to Natura_2000 and change numbers for actual names

Natura2000_DF <- data.frame(Natura2000 = 0, Natura_2000 = "Yes")

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(Natura2000_DF) %>%
  dplyr::select(-Natura2000)


# Change the names of the columns of Urort_Skov from Owned to Urort_Skov and change numbers for actual names

Urort_Skov_DF <- data.frame(Owned = 0:(length(levels(Urort_skov)[[1]]) - 1), Urort_Skov = levels(Urort_skov)[[1]])

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(Urort_Skov_DF) %>%
  dplyr::select(-Owned)

# Change the names of the columns of NaturaOgVildtreservater_DF from Temanavn to NaturaOgVildtreservater and change numbers for actual names

NaturaOgVildtreservater_DF <- data.frame(Temanavn  = 0, NaturaOgVildtreservater = "Yes")

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(NaturaOgVildtreservater_DF) %>%
  dplyr::select(-Temanavn)

# Change the names of the columns of Naturnationalparker from ID to Naturnationalparker and change numbers for actual names

Naturnationalparker_DF <- data.frame(ID  = 0, Naturnationalparker = "Yes")

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(Naturnationalparker_DF) %>%
  dplyr::select(-ID)

## Change the names of owntership

Ownership_DF <- data.frame(Ownership  = 0:(length(levels(Ownership)[[1]]) - 1), ownership = levels(Ownership)[[1]])

Ownership_Long_Table <- Ownership_Long_Table %>%
  full_join(Ownership_DF) %>%
  dplyr::select(-Ownership)

Ownership_Table <- Ownership_Long_Table %>%
  mutate(Area_Sq_Mt = 100*Freq, Proportion = 100*(Area_Sq_Mt/Area_DK)) %>%
  dplyr::select(-Freq) %>%
  mutate_at(c("Habitats_P3", "Natura_2000", "Urort_Skov", "NaturaOgVildtreservater", "Naturnationalparker"), ~ifelse(is.na(.x), NA, "Yes")) %>%
  pivot_longer(c("Habitats_P3", "Natura_2000", "Urort_Skov", "NaturaOgVildtreservater","Naturnationalparker")) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value, -Proportion) %>%
  group_by(name, ownership) %>%
  summarise(Area_Sq_Km = sum(Area_Sq_Mt)/1000000) %>%
  mutate(ownership = ifelse(is.na(ownership), "Unknown", ownership)) %>%
  pivot_wider(values_from = Area_Sq_Km, names_from = ownership)

saveRDS(Ownership_Table, "Ownership_Table.rds")
readr::write_csv(Ownership_Table, "Ownership_Table.csv")
openxlsx::write.xlsx(Ownership_Table, "Ownership_Table.xlsx")

## ---- show-3d-table --------

knitr::kable(Ownership_Table, digits = 3, caption = "Total area for areas by ownership in square kilometers", format.args	= list(big.mark = ','))

## ---- build-P3-Ownership-table --------

P3_Ownership <- Ownership_Long_Table %>%
  dplyr::filter(!is.na(Habitats_P3)) %>%
  dplyr::mutate(Status = ifelse(is.na(Natura_2000) & is.na(Urort_Skov) & is.na(NaturaOgVildtreservater) & is.na(Naturnationalparker), "Exclusive", "Overlaps")) %>%
  dplyr::select(Freq, Habitats_P3, ownership, Status) %>%
  dplyr::mutate(Habitats_P3 = ifelse(is.na(Habitats_P3), NA, "Yes")) %>%
  mutate(ownership = ifelse(is.na(ownership), "Unknown", ownership)) %>%
  group_by(Habitats_P3, ownership, Status) %>%
  summarise(Area_Sq_Km = (sum(Freq)*100)/1000000) %>%
  ungroup() %>%
  dplyr::select(-Habitats_P3) %>%
  pivot_wider(names_from = ownership, values_from = Area_Sq_Km)

saveRDS(P3_Ownership, "P3_Ownership.rds")
readr::write_csv(P3_Ownership, "P3_Ownership.csv")
openxlsx::write.xlsx(P3_Ownership, "P3_Ownership.xlsx")

## ---- show-P3-Ownership-table --------

knitr::kable(P3_Ownership, digits = 3, caption = "Total area for paragraph 3 and klit in square kilometers separated by ownership", format.args	= list(big.mark = ','))

## ---- build-Skov-Ownership-table --------

Urort_Skov_Ownership <- Ownership_Long_Table %>%
  dplyr::filter(!is.na(Urort_Skov)) %>%
  dplyr::mutate(Status = ifelse(is.na(Natura_2000) & is.na(Habitats_P3) & is.na(NaturaOgVildtreservater) & is.na(Naturnationalparker), "Exclusive", "Overlaps")) %>%
  dplyr::select(Freq, Urort_Skov, Status) %>%
  group_by(Urort_Skov, Status) %>%
  summarise(Area_Sq_Km = (sum(Freq)*100)/1000000) %>%
  ungroup() %>%
  pivot_wider(names_from = Urort_Skov, values_from = Area_Sq_Km)

saveRDS(Urort_Skov_Ownership, "Urort_Skov_Ownership.rds")
readr::write_csv(Urort_Skov_Ownership, "Urort_Skov_Ownership.csv")
openxlsx::write.xlsx(Urort_Skov_Ownership, "Urort_Skov_Ownership.xlsx")

## ---- show-Skov-Ownership-table --------

knitr::kable(Urort_Skov_Ownership, digits = 3, caption = "Total area for untouched forest in square kilometers separated by ownership", format.args	= list(big.mark = ','))


## ---- sea-of-Denmark --------

SeaOfDenmark <- mregions::mr_shp("Denmark:eez") %>%
  dplyr::filter(geoname == "Danish Exclusive Economic Zone" ) %>%
  dplyr::select(geoname) %>%
  terra::vect() %>%
  terra::project(terra::crs(DK))

Area_Sea_DK <- expanse(SeaOfDenmark)

TemplateSea <- terra::extend(Template, SeaOfDenmark)

## ---- PlotSeaDenmark --------

plot(SeaOfDenmark, col = "blue")

## ---- Natura-2000-sea-raster --------

# read natura2000

Natura2000 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Natura2000 MiljøGIS Maj2022/pg-natura_2000_omraader_natura2000.shp")
Natura2000$Natura_2000 <- "Yes"
Natura2000 <- Natura2000[,c("Natura_2000")]

Natura2000 <- aggregate(Natura2000, by='Natura_2000')
Rast_Natura2000_Sea <- terra::rasterize(Natura2000, TemplateSea)

Natura2000_Croped_Sea <- terra::mask(Rast_Natura2000_Sea, SeaOfDenmark)

# Write croped rasters to disk

writeRaster(Natura2000_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped_Sea.tif",
               destination = "RasterizedCOG/Rast_Natura2000_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotNatura2000Sea --------

plot(SeaOfDenmark, col = "blue")
plot(Natura2000_Croped_Sea, add =T, legend = "bottom")

## ---- Habitatomrade-sea-raster --------

Habitatomrade <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HABITAT_OMRAADER/HABITAT_OMRAADER.shp")

Habitatomrade$Habitatomrade <- "Yes"
Habitatomrade <- Habitatomrade[,c("Habitatomrade")]

Habitatomrade <- aggregate(Habitatomrade, by='Habitatomrade')

Habitatomrade <- terra::rasterize(Habitatomrade, TemplateSea)
Habitatomrade_Croped_Sea <- terra::mask(Habitatomrade, SeaOfDenmark)


# Write raw rasters to disk



# Write croped rasters to disk

writeRaster(Habitatomrade_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatomrade_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatomrade_Croped_Sea.tif",
               destination = "RasterizedCOG/Rast_Habitatomrade_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotHabitatomradeSea --------

plot(SeaOfDenmark, col = "blue")
plot(Habitatomrade_Croped_Sea, add =T, legend = "bottom")

## ---- Habitatnaturtype-sea-raster --------

Habitatnaturtype <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_marine_kortlaeg_2004_2018.shp")

Habitatnaturtype$Habitatnaturtype <- Habitatnaturtype$Naturnavn
Habitatnaturtype <- Habitatnaturtype[,c("Habitatnaturtype")]

Habitatnaturtype <- aggregate(Habitatnaturtype, by='Habitatnaturtype')
Habitatnaturtype <- terra::rasterize(Habitatnaturtype, TemplateSea)
Habitatnaturtype_Croped_Sea <- terra::mask(Habitatnaturtype, SeaOfDenmark)


# Write croped rasters to disk

writeRaster(Habitatnaturtype_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatnaturtype_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatnaturtype_Croped_Sea.tif",
               destination = "RasterizedCOG/Rast_Habitatnaturtype_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotHabitatnaturtypeSea --------

plot(SeaOfDenmark, col = "blue")
plot(Habitatnaturtype_Croped_Sea, add =T, legend = "bottom")

## ---- Ramsar-sea-raster --------

Ramsar <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/RAMSAR/ramsar.shp")

Ramsar$Ramsar <- "Yes"
Ramsar <- Ramsar[,c("Ramsar")]

Ramsar <- aggregate(Ramsar, by='Ramsar')
Ramsar <- terra::rasterize(Ramsar, TemplateSea)
Ramsar_Croped_Sea <- terra::mask(Ramsar, SeaOfDenmark)


# Write raw rasters to disk



# Write croped rasters to disk

writeRaster(Ramsar_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ramsar_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ramsar_Croped_Sea.tif",
               destination = "RasterizedCOG/Rast_Ramsar_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotRamsarSea --------

plot(SeaOfDenmark, col = "blue")
plot(Ramsar_Croped_Sea, add =T, legend = "bottom")

## ---- Havstrategi-standard-sea-raster --------

Havstrategi_standard_1 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HAVSTRATEGI_OMRAADER/download-FF9FC53C-80BB-4A0E-88A2-E6C543CB686/download-FF9FC53C-80BB-4A0E-88A2-E6C543CB686/havstrategi-omr_foraar2016.shp")
Havstrategi_standard_2 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HAVSTRATEGI_OMRAADER/Havstrat_omr Nordso_Bornh_Hoering2021/H¢ringsportalen_GIS_data/Nye_havstrategiomraader_horing_26032021.shp")

Havstrategi_standard <- rbind(Havstrategi_standard_1, Havstrategi_standard_2)

Havstrategi_standard$Havstrategi_standard <- "Yes"
Havstrategi_standard <- Havstrategi_standard[,c("Havstrategi_standard")]

Havstrategi_standard <- aggregate(Havstrategi_standard, by='Havstrategi_standard')
Havstrategi_standard_Croped_Sea <- terra::mask(Havstrategi_standard, SeaOfDenmark)


# Write raw rasters to disk

Havstrategi_standard_Croped_Sea <- terra::rasterize(Havstrategi_standard_Croped_Sea, TemplateSea)

# Write croped rasters to disk

writeRaster(Havstrategi_standard_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_standard_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_standard_Croped_Sea.tif",
               destination = "RasterizedCOG/Havstrategi_standard_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotHavstrategiStandardSea --------

plot(SeaOfDenmark, col = "blue")
plot(Havstrategi_standard_Croped_Sea, add =T, legend = "bottom")

## ---- Havstrategi-streng-sea-raster --------

Havstrategi_streng <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HAVSTRATEGI_OMRAADER/Havstrat_omr Nordso_Bornh_Hoering2021/H¢ringsportalen_GIS_data/Nye_strengt_beskyttede_omraader_horing_26032021.shp")

Havstrategi_streng$Havstrategi_streng <- "Yes"
Havstrategi_streng <- Havstrategi_streng[,c("Havstrategi_streng")]

Havstrategi_streng <- aggregate(Havstrategi_streng, by='Havstrategi_streng')
Havstrategi_streng_Croped_Sea <- terra::mask(Havstrategi_streng, SeaOfDenmark)


# Write raw rasters to disk

Havstrategi_streng_Croped_Sea <- terra::rasterize(Havstrategi_streng_Croped_Sea, TemplateSea)

# Write croped rasters to disk

writeRaster(Havstrategi_streng_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_streng_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_streng_Croped_Sea.tif",
               destination = "RasterizedCOG/Havstrategi_streng_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotHavstrategiStrengSea --------

plot(SeaOfDenmark, col = "blue")
plot(Havstrategi_streng_Croped_Sea, add =T, legend = "bottom")

## ---- Natur-Vildt-Reservater-sea-raster --------

Wildreserve <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NATUR_VILDT_RESERVATER/NATUR_VILDT_RESERVATER.shp")

# Eliminate the some of the reserves

Natur_Vildt_Reservater <- Wildreserve[!(Wildreserve$Beken_navn %in% c("Agerø og Skibsted Fjord", "Agger Tange", "Anholt",
                                                                       "Ertholmene", "Hesselø", "Hirsholmene",
                                                                       "Horsens Nørrestrand",  "Vorsø")),]

Natur_Vildt_Reservater$Natur_Vildt_Reservater <- "Yes"
Natur_Vildt_Reservater <- Natur_Vildt_Reservater[,c("Natur_Vildt_Reservater")]

Natur_Vildt_Reservater <- aggregate(Natur_Vildt_Reservater, by='Natur_Vildt_Reservater')
Natur_Vildt_Reservater <- terra::rasterize(Natur_Vildt_Reservater, TemplateSea)
Natur_Vildt_Reservater_Croped_Sea <- terra::mask(Natur_Vildt_Reservater, SeaOfDenmark)


# Write croped rasters to disk

writeRaster(Natur_Vildt_Reservater_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Natur_Vildt_Reservater_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Natur_Vildt_Reservater_Croped_Sea.tif",
               destination = "RasterizedCOG/Natur_Vildt_Reservater_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotNaturVildtReservaterSea --------

plot(SeaOfDenmark, col = "blue")
plot(Natur_Vildt_Reservater_Croped_Sea, add =T, legend = "bottom")

## ---- Fredninger-sea-raster --------

Fredninger <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/IUCN beskyt hav/Beskyt_omr_hav_IUCN_m_info_2.shp")

Fredninger$Fredninger <- "Yes"
Fredninger <- Fredninger[,c("Fredninger")]

Fredninger <- aggregate(Fredninger, by='Fredninger')
Fredninger <- terra::rasterize(Fredninger, TemplateSea)
Fredninger_Croped_Sea <- terra::mask(Fredninger, SeaOfDenmark)


# Write raw rasters to disk



# Write croped rasters to disk

writeRaster(Fredninger_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fredninger_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fredninger_Croped_Sea.tif",
               destination = "RasterizedCOG/Fredninger_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotFredningerSea --------

plot(SeaOfDenmark, col = "blue")
plot(Fredninger_Croped_Sea, add =T)

## ---- Fuglebeskyt-sea-raster --------

Fuglebeskyt <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Fuglebeskyttelses_omraader/Fuglebeskyttelsesomraader.shp")

Fuglebeskyt$Fuglebeskyt <- "Yes"
Fuglebeskyt <- Fuglebeskyt[,c("Fuglebeskyt")]

Fuglebeskyt <- aggregate(Fuglebeskyt, by='Fuglebeskyt')
Fuglebeskyt <- terra::rasterize(Fuglebeskyt, TemplateSea)
Fuglebeskyt_Croped_Sea <- terra::mask(Fuglebeskyt, SeaOfDenmark)


# Write raw rasters to disk



# Write croped rasters to disk

writeRaster(Fuglebeskyt_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fuglebeskyt_Croped_Sea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))

# save as cloud optimized rasters

sf::gdal_utils("warp",
               source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fuglebeskyt_Croped_Sea.tif",
               destination = "RasterizedCOG/Fuglebeskyt_Croped_Sea.tif",
               options = c(
                 "-of", "COG",
                 "-co", "RESAMPLING=NEAREST",
                 "-co", "TILING_SCHEME=GoogleMapsCompatible",
                 "-co", "COMPRESS=DEFLATE",
                 "-co", "NUM_THREADS=46"
               ))

## ---- PlotFuglebeskytSea --------

plot(SeaOfDenmark, col = "blue")
plot(Fuglebeskyt_Croped_Sea, add =T)


## ---- AllStackSea --------

Rast_Natura2000_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped_Sea.tif")

Rast_Habitatomrade_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatomrade_Croped_Sea.tif")

Rast_Habitatnaturtype_Croped_Sea <-  terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Habitatnaturtype_Croped_Sea.tif")

Rast_Ramsar_Croped_Sea <-  terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Ramsar_Croped_Sea.tif")

Havstrategi_standard_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_standard_Croped_Sea.tif")

Havstrategi_streng_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Havstrategi_streng_Croped_Sea.tif")

Natur_Vildt_Reservater_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Natur_Vildt_Reservater_Croped_Sea.tif")

Fredninger_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fredninger_Croped_Sea.tif")

AllSea <- c(Rast_Natura2000_Croped_Sea, Rast_Habitatomrade_Croped_Sea,
            Rast_Habitatnaturtype_Croped_Sea, Rast_Ramsar_Croped_Sea,
            Havstrategi_standard_Croped_Sea, Havstrategi_streng_Croped_Sea,
            Natur_Vildt_Reservater_Croped_Sea, Fredninger_Croped_Sea)
writeRaster(AllSea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/AllSea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))


## ---- crosstabSea --------

#AllSea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/AllSea.tif")

#dir.create("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/SeaTiles")

#writeRaster(AllSea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/SeaTiles/AllSea.tif", overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))


#AreaSea <- crosstab(AllSea, useNA=T, long=TRUE)

#AreaSea <- readRDS("Area_Total_Sea.rds")
#AreaSea <- AreaSea[!(rowSums(is.na(AreaSea)) == max(rowSums(is.na(AreaSea)))),]

#saveRDS(AreaSea, "Area_Total_Sea.rds")

## ---- SeaTable1 --------

LongSeaTable <- readRDS("LongSeaTable.rds") %>%
  mutate(Area_Sq_Km = (Frequency*100)/1000000, Proportion = ((Frequency*100)/Area_Sea_DK)*100) %>%
  dplyr::filter(Proportion < 100)

Natura2000_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Natura_2000)) %>%
  mutate(Overlaped = case_when(is.na(Havstrategi_standard) & is.na(Havstrategi_streng) & is.na(Natur_Vildt_Reservater) &
                                 is.na(Fredninger) ~ "No", TRUE ~ "Yes")) %>%
  group_by(Natura_2000, Overlaped) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency)

Natura2000_Sea_Table1_Totals <- Natura2000_Sea_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Nautra_2000")

Natura2000_Sea_Table1_Appart <- Natura2000_Sea_Table1a %>%
  mutate(Class = "Nautra_2000") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Natura_2000) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Natura2000_Sea_Table1 <- full_join(Natura2000_Sea_Table1_Totals, Natura2000_Sea_Table1_Appart) %>%
  relocate(Class, .before = everything())


##

Havstrategi_standard_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Havstrategi_standard)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Havstrategi_streng) & is.na(Natur_Vildt_Reservater) &
                                 is.na(Fredninger) ~ "No", TRUE ~ "Yes")) %>%
  group_by(Havstrategi_standard, Overlaped) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency)

Havstrategi_standard_Sea_Table1_Totals <- Havstrategi_standard_Sea_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Havstrategi_standard")

Havstrategi_standard_Sea_Table1_Appart <- Havstrategi_standard_Sea_Table1a %>%
  mutate(Class = "Havstrategi_standard") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Havstrategi_standard) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Havstrategi_standard_Sea_Table1 <- full_join(Havstrategi_standard_Sea_Table1_Totals, Havstrategi_standard_Sea_Table1_Appart) %>%
  relocate(Class, .before = everything())


##

Havstrategi_streng_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Havstrategi_streng)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Havstrategi_standard) & is.na(Natur_Vildt_Reservater) &
                                 is.na(Fredninger) ~ "No", TRUE ~ "Yes")) %>%
  group_by(Havstrategi_streng, Overlaped) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency)

Havstrategi_streng_Sea_Table1_Totals <- Havstrategi_streng_Sea_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Havstrategi_streng")

Havstrategi_streng_Sea_Table1_Appart <- Havstrategi_streng_Sea_Table1a %>%
  mutate(Class = "Havstrategi_streng") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Havstrategi_streng) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
  rename(Area_Overlapped = Yes)

Havstrategi_streng_Sea_Table1 <- full_join(Havstrategi_streng_Sea_Table1_Totals, Havstrategi_streng_Sea_Table1_Appart) %>%
  relocate(Class, .before = everything())

##

Natur_Vildt_Reservater_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Natur_Vildt_Reservater)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Havstrategi_streng) & is.na(Havstrategi_standard) &
                                 is.na(Fredninger) ~ "No", TRUE ~ "Yes")) %>%
  group_by(Natur_Vildt_Reservater, Overlaped) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency)

Natur_Vildt_Reservater_Sea_Table1_Totals <- Natur_Vildt_Reservater_Sea_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Natur_Vildt_Reservater")

Natur_Vildt_Reservater_Sea_Table1_Appart <- Natur_Vildt_Reservater_Sea_Table1a %>%
  mutate(Class = "Natur_Vildt_Reservater") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Natur_Vildt_Reservater) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Natur_Vildt_Reservater_Sea_Table1 <- full_join(Natur_Vildt_Reservater_Sea_Table1_Totals, Natur_Vildt_Reservater_Sea_Table1_Appart) %>%
  relocate(Class, .before = everything())

##

Fredninger_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Fredninger)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000)  & is.na(Havstrategi_streng) & is.na(Havstrategi_standard) &
                                 is.na(Natur_Vildt_Reservater) ~ "No", TRUE ~ "Yes")) %>%
  group_by(Fredninger, Overlaped) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency)

Fredninger_Sea_Table1_Totals <- Fredninger_Sea_Table1a %>%
  ungroup() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Class = "Fredninger")

Fredninger_Sea_Table1_Appart <- Fredninger_Sea_Table1a %>%
  mutate(Class = "Fredninger") %>%
  ungroup() %>%
  dplyr::select(-Proportion, -Fredninger) %>%
  tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Km) %>%
  rename(Area_Overlapped = Yes, Area_Exclusive = No)

Fredninger_Sea_Table1 <- full_join(Fredninger_Sea_Table1_Totals, Fredninger_Sea_Table1_Appart) %>%
  relocate(Class, .before = everything())

##

Total_Sea_Table1a <- LongSeaTable %>%
  dplyr::filter(!is.na(Natura_2000) | !is.na(Havstrategi_standard) | !is.na(Havstrategi_streng) | !is.na(Natur_Vildt_Reservater) |
                                 !is.na(Fredninger)) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::select(-Frequency) %>% mutate(Class = "Total")


TotalOverlap <- list(Total_Sea_Table1a, Natura2000_Sea_Table1, Havstrategi_standard_Sea_Table1, Havstrategi_streng_Sea_Table1, Natur_Vildt_Reservater_Sea_Table1, Fredninger_Sea_Table1) %>%
  purrr::reduce(bind_rows) %>%
  arrange(desc(Area_Sq_Km))

TotalOverlap[is.na(TotalOverlap)] <- 0


openxlsx::write.xlsx(TotalOverlap, "Table1_Marine.xlsx")


## ---- Show-SeaTable1 --------

knitr::kable(TotalOverlap, digits = 2, caption = "Areas in square kms and proportions of the sea of Denmark and their overlaps", format.args	= list(big.mark = ','))

## ---- SeaTable4 --------


Total <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Natura 2000 i alt") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

Habitatomrade <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Habitatomrade == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Kun habitatomraade") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

# Natura 2000 that is not Habitatomrade
fuglebeskyt  <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Fuglebeskyt == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Kun fuglebeskyt") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

Ramsar <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Ramsar == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Ramsar") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

Habitatnaturtype <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Habitatnaturtype == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Habitatnaturtype") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

Havstrategi_standard <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Havstrategi_standard == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Havstrategi standard") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)

Havstrategi_streng <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Havstrategi_streng == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Havstrategi streng") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)


Natur_Vildt_Reservater <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Natur_Vildt_Reservater == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Vildt Reservater") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)


Natur_Habitatomrade_Fugle <- LongSeaTable %>%
  dplyr::filter(Natura_2000 == "Yes" & Habitatomrade == "Yes" & Fuglebeskyt == "Yes") %>%
  dplyr::select(Natura_2000, Area_Sq_Km, Proportion) %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Category = "Habitatatomr. og fuglebesk.") %>%
  relocate(Category, .before = everything()) %>%
  dplyr::select(-Natura_2000)


AllNatura2000 <- list(Total, Habitatomrade, fuglebeskyt, Ramsar, Habitatnaturtype, Havstrategi_standard, Havstrategi_streng, Natur_Vildt_Reservater, Natur_Habitatomrade_Fugle) %>%
  purrr::reduce(bind_rows) %>% arrange(desc(Area_Sq_Km))

openxlsx::write.xlsx(AllNatura2000, "Table4_Marine.xlsx")

## ---- Show-SeaTable4 --------

knitr::kable(AllNatura2000, digits = 2, caption = "Areas in square kms and proportions of Natura 2000 with other groups", format.args	= list(big.mark = ','))
