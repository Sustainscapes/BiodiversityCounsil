library(terra)
library(magrittr)
library(geodata)
library(sf)
## Template

Canopy_Cover <- list.files(path = "O:/Nat_Ecoinformatics/C_Write/_Archive/Assmann_etal_EcoDes-DK15/EcoDes-DK15_v1.1.0/canopy_height/", full.names = T, pattern = ".vrt") %>%
  terra::rast()

DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd()) %>%
  terra::project(terra::crs(Habs))
Area_DK <- terra::expanse(DK)

## join this 2
Habs <- terra::vect("O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp")

Klits <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/MATRIKELKORT/DK_SHAPE_UTM32-EUREF89/MINIMAKS/TEMA/KLIT.shp")

Klits$Natyp_navn <- "Klit"

Klits <- terra::aggregate(Klits, by = "Natyp_navn") %>% terra::project(terra::crs(Habs))

### Check overlap

#HabsRast <- terra::rasterize(Habs, Canopy_Cover, field = "Natyp_navn")

#KlitsRast <- terra::rasterize(Klits, Canopy_Cover, field = "Natyp_navn")

#HabsKlits <- crosstab(c(HabsRast, KlitsRast), useNA=T, long=TRUE)


#Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(HabsRast)[[1]]) - 1), Habitats_P3 = levels(HabsRast)[[1]])

#Klits_DF <- data.frame(Natyp_navn.1 = 0:(length(levels(KlitsRast)[[1]]) - 1), Habitats_Klit = levels(KlitsRast)[[1]])


#HabsKlits2 <- HabsKlits %>% full_join(Paragraph3_DF) %>%
#  full_join(Klits_DF) %>%
#  dplyr::filter(!(is.na(Habitats_P3) & is.na(Habitats_Klit))) %>%
#  dplyr::select(-"Natyp_navn", -"Natyp_navn.1") %>%
#  mutate(Area_Sq_Mt = 100*Freq,
#         Proportion = 100*(Area_Sq_Mt/Area_DK)) %>%
#  dplyr::select(-Freq) %>%
#  ungroup()


## What withing paragraph 3 Klit intersects with
Habs2 <- rbind(Habs, Klits)

Sys.time()
Rast_p3_klit  <- terra::rasterize(Habs2, Canopy_Cover, field = "Natyp_navn")
message(paste("Paragraph 3 ready", Sys.time()))

writeRaster(Rast_p3_klit, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))
Rast_p3_klit_Croped <- terra::mask(Rast_p3_klit, DK)

writeRaster(Rast_p3_klit_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

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

## join this 2

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HABITAT_OMRAADER/HABITAT_OMRAADER.shp")

Natura2000 <- Natura2000[,7]

Natura2000 <- terra::aggregate(Natura2000, by = "Temanavn")

Natura2000b <-terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/FUGLE_BESKYTTELSE_OMRAADER/FUGLE_BESKYTTELSE_OMRAADER.shp")

Natura2000b <- terra::aggregate(Natura2000b, by = "Temanavn")

Natura2000 <- rbind(Natura2000, Natura2000b)

Natura2000$Natura2000 <- "yes"

Sys.time()
Rast_Natura2000  <- terra::rasterize(Natura2000, Canopy_Cover, field = "Natura2000")
Sys.time()

Rast_Natura2000_Croped <- terra::mask(Rast_Natura2000, DK)

writeRaster(Rast_Natura2000, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(Rast_Natura2000_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

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
#####

markblokkort <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Markblokke2021/Markblokke2021.shp")

markblokkort <- markblokkort[(markblokkort$MB_TYPE %in% c("OMD", "PGR")),7]

markblokkort_Aggregated <- terra::aggregate(markblokkort, by='MB_TYPE')

Sys.time()
Rast_markblokkort  <- terra::rasterize(markblokkort_Aggregated, Canopy_Cover, field = "MB_TYPE")
Sys.time()

Rast_markblokkort_Croped <- terra::mask(Rast_markblokkort, DK)

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

### Nature restecet


Wildreserve <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NATUR_VILDT_RESERVATER/NATUR_VILDT_RESERVATER.shp")

NaturaOgVildtreservater <- Wildreserve[!(Wildreserve$Beken_navn %in% c("Agerø og Skibsted Fjord", "Agger Tange", "Anholt",
                                                           "Ertholmene", "Hesselø", "Hirsholmene",
                                                           "Horsens Nørrestrand",  "Vorsø")),]

NaturaOgVildtreservater_Aggregated <- terra::aggregate(NaturaOgVildtreservater, by='Temanavn')

Sys.time()
Rast_NaturaOgVildtreservater  <- terra::rasterize(NaturaOgVildtreservater_Aggregated, Canopy_Cover, field = "Temanavn")
Sys.time()

Rast_NaturaOgVildtreservater_Croped <- terra::mask(Rast_NaturaOgVildtreservater, DK)

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

### IUCN

IUCN <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/IUCN REVISED FREDNINGER/Fredninger_IUCNKat_2018_25832.shp")
IUCN$IUCN <- "Yes"

IUCN_Aggregated <- terra::aggregate(IUCN, by='IUCN')

Sys.time()
Rast_IUCN  <- terra::rasterize(IUCN_Aggregated, Canopy_Cover, field = "IUCN")
Sys.time()

Rast_IUCN_Croped <- terra::mask(Rast_IUCN, DK)

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


## Untouched forest join

Urort_Skov <- list.files(path = "O:/Nat_BDR-data/Arealanalyse/RAW/Uroert skov NST Feb2022/", full.names = T, pattern = "shp") %>%
  purrr::map(vect) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Habs2))

Urort_Skov$Owned <- "State"

private_Urort_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp") %>%
  terra::project(crs(Habs2))
private_Urort_Skov <- private_Urort_Skov[private_Urort_Skov$tilskudsor== "Privat urørt skov", ]

private_Urort_Skov$Owned <- "Private"
Urort_Skov <- rbind(Urort_Skov, private_Urort_Skov)
Urort_Skov <- Urort_Skov[,"Owned"]
Urort_Skov <- Urort_Skov %>% terra::makeValid()


Urort_Skov_Aggregated <- terra::aggregate(Urort_Skov, by='Owned')

Sys.time()
Rast_Urort_Skov  <- terra::rasterize(Urort_Skov_Aggregated, Canopy_Cover, field = "Owned")
Sys.time()

Rast_Urort_Skov_Croped <- terra::mask(Rast_Urort_Skov, DK)

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


#### National parks not to be shown results only

National_Parks <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/GIS filer - de 5/GIS filer - de 5/Naturnationalparker.shp") %>%
  terra::project(crs(Habs2))

National_Parks$ID <- "NationalParks"

National_Parks_Aggregated <- terra::aggregate(National_Parks, by='ID')

Sys.time()
Rast_National_Parks  <- terra::rasterize(National_Parks_Aggregated, Canopy_Cover, field = "ID")
Sys.time()

Rast_National_Parks_Croped <- terra::mask(Rast_National_Parks, DK)

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


### Combine subsidy schemes



stoettte_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoettte_Skov <- stoettte_Skov[stoettte_Skov$tilskudsor== "Privat urørt skov", ]

stoettte_sammenhaengende <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoettte_sammenhaengende <- stoettte_sammenhaengende[stoettte_sammenhaengende$tilskudsor== "Sammenhængende arealer", ]

stoette_egekrat <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/EGEKRAT/egekrat.shp")

stoette_egekrat <- stoette_egekrat[stoette_egekrat$vurdering_ %in% c(1,2),]
stoette_egekrat <- stoette_egekrat[stoette_egekrat$sikret %in% c("ja"),]
#### Total Skov - Uroet skov = Drevet Skov

Total_Forest <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/SKOV_GeoDanmark_April2022/SKOV_GeoDanmark_April2022.shp")

#### TEMPLATE





Test2 <- terra::rasterize(Natura2000, Canopy_Cover, field = "Temanavn")
message(paste("Natura 2000 ready", Sys.time()))
Test3 <- terra::rasterize(markblokkort_Aggregated, Canopy_Cover, field = "MB_TYPE")
message(paste("markblokkort 3 ready", Sys.time()))


All <- c(Test, Test2, Test3)

SeaOfDenmark <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Dansk EEZ/Dansk EEZ/Dansk_EEZ.shp")



ALL <- terra::mask(All, DK)

writeRaster(ALL, "AllIntersections.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

writeRaster(ALL[[1]], "Paragraph3.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES","of=COG"))

sf::gdal_utils("warp",
           source = "Paragraph3.tif",
           destination = "Paragraph3_Optimized.tif",
           options = c(
             "-of", "COG",
             "-co", "RESAMPLING=NEAREST",
             "-co", "TILING_SCHEME=GoogleMapsCompatible",
             "-co", "COMPRESS=DEFLATE",
             "-co", "NUM_THREADS=46"
           ))

message(paste("Start crosstab", Sys.time()))


Area <- crosstab(ALL, useNA=T, long=TRUE)


message(paste("End crosstab", Sys.time()))

Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(Test)[[1]]) - 1), Habitats_P3 = levels(Test)[[1]])

Natura2000_DF <- data.frame(Temanavn = 0, Natura_2000 = "Yes")

markblokkort_DF <- data.frame(MB_TYPE = 0:(length(levels(Test3)[[1]]) - 1), Types_markblokkort = levels(Test3)[[1]])

Area2 <- full_join(Area, Paragraph3_DF) %>%
  full_join(Natura2000_DF) %>%
  full_join(markblokkort_DF) %>%
  dplyr::filter(!(is.na(Natyp_navn) & is.na(Temanavn) & is.na(MB_TYPE))) %>%
  dplyr::select(-"Natyp_navn", -"Temanavn", -"MB_TYPE") %>%
  mutate(Area_Sq_Mt = 100*Freq,
         Proportion = 100*(Area_Sq_Mt/Area_DK)) %>%
  dplyr::select(-Freq) %>%
  ungroup()

saveRDS(Area2, "Area_summary.rds")

SUM <- Area2 %>% group_by(Habitats_P3) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Habitats_P3))

SUM2 <- Area2 %>% group_by(Natura_2000) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Natura_2000))


SUM3 <- Area2 %>% group_by(Types_markblokkort) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Types_markblokkort))

Sys.time()
Area <- expanse(Test)
Sys.time()
