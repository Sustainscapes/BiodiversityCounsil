library(terra)
library(magrittr)
library(geodata)
library(sf)
## Template

Template <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif")

values(Template) <- 1

list.files(path = "O:/Nat_Ecoinformatics/B_Read/LegacyData/Denmark/", full.names = T, pattern = ".vrt")

DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd()) %>%
  terra::project(terra::crs(Habs))
Area_DK <- terra::expanse(DK)

## join this 2
Habs <- terra::vect("O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp")

Klits <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/MATRIKELKORT/DK_SHAPE_UTM32-EUREF89/MINIMAKS/TEMA/KLIT.shp")

Klits$Natyp_navn <- "Klit"

Klits <- terra::aggregate(Klits, by = "Natyp_navn") %>% terra::project(terra::crs(Habs))

### Check overlap

#HabsRast <- terra::rasterize(Habs, Template, field = "Natyp_navn")

#KlitsRast <- terra::rasterize(Klits, Template, field = "Natyp_navn")

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
Rast_p3_klit  <- terra::rasterize(Habs2, Template, field = "Natyp_navn")
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

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Natura2000 MiljøGIS Maj2022/pg-natura_2000_omraader_natura2000.shp")

Natura2000 <- Natura2000[,7]

Natura2000 <- terra::aggregate(Natura2000, by = "Temanavn")

Natura2000b <-terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/FUGLE_BESKYTTELSE_OMRAADER/FUGLE_BESKYTTELSE_OMRAADER.shp")

Natura2000b <- terra::aggregate(Natura2000b, by = "Temanavn")

Natura2000 <- rbind(Natura2000, Natura2000b)

Natura2000$Natura2000 <- "yes"

Sys.time()
Rast_Natura2000  <- terra::rasterize(Natura2000, Template, field = "Natura2000")
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
Rast_markblokkort  <- terra::rasterize(markblokkort_Aggregated, Template, field = "MB_TYPE")
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
Rast_NaturaOgVildtreservater  <- terra::rasterize(NaturaOgVildtreservater_Aggregated, Template, field = "Temanavn")
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
Rast_IUCN  <- terra::rasterize(IUCN_Aggregated, Template, field = "IUCN")
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
Rast_Urort_Skov  <- terra::rasterize(Urort_Skov_Aggregated, Template, field = "Owned")
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
Rast_National_Parks  <- terra::rasterize(National_Parks_Aggregated, Template, field = "ID")
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

stoettte_Skov$Type <- "Skov"
stoettte_sammenhaengende <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoettte_sammenhaengende <- stoettte_sammenhaengende[stoettte_sammenhaengende$tilskudsor== "Sammenhængende arealer", ]
stoettte_sammenhaengende$Type <- "sammenhaengende"


stoette_egekrat <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/EGEKRAT/egekrat.shp")

stoette_egekrat <- stoette_egekrat[stoette_egekrat$vurdering_ %in% c(1,2),]
stoette_egekrat <- stoette_egekrat[stoette_egekrat$sikret %in% c("ja"),]
stoette_egekrat$Type <- "egekrat"

stoette <- list(stoettte_Skov, stoettte_sammenhaengende, stoette_egekrat) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Habs2))


stoette <- stoette[,"Type"]
stoette_Aggregated <- terra::aggregate(stoette, by='Type')

Sys.time()
Rast_stoette  <- terra::rasterize(stoette_Aggregated, Template, field = "Type")
Sys.time()

Rast_stoette_Croped <- terra::mask(Rast_stoette, DK)

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
#### Total Skov - Uroet skov = Drevet Skov

Total_Forest <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/SKOV_GeoDanmark_April2022/SKOV_GeoDanmark_April2022.shp") %>%
  terra::project(crs(Habs2))

Total_Forest$Data <- "Total Forest"

Total_Forest <- Total_Forest[,"Data"]

Total_Forest_Aggregated <- terra::aggregate(Total_Forest, by='Data')

Sys.time()
Total_Forest  <- terra::rasterize(Total_Forest_Aggregated, Template, field = "Data")
Total_Forest_Croped <- terra::mask(Total_Forest, DK)
Sys.time()


Negativo <- Rast_Urort_Skov_Croped
Negativo[is.na(Negativo)] <- 3

m <- c(0, 0.25, 1,
       0.25, 0.5, 2,
       0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(r, rclmat, include.lowest=TRUE)

Negative <-

#### TEMPLATE







All <- c(Rast_Urort_Skov_Croped, Rast_stoette_Croped, Rast_p3_klit_Croped, Rast_markblokkort_Croped, Rast_NaturaOgVildtreservater_Croped, Rast_Natura2000_Croped, Rast_National_Parks_Croped, Rast_IUCN_Croped)

#SeaOfDenmark <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Dansk EEZ/Dansk EEZ/Dansk_EEZ.shp")



message(paste("Start crosstab", Sys.time()))


Area <- crosstab(All, useNA=T, long=TRUE)

saveRDS(Area, "Area_Total.rds")

message(paste("End crosstab", Sys.time()))

Area2 <- Area[!(rowSums(is.na(Area)) == max(rowSums(is.na(Area)))),]


Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(Rast_p3_klit_Croped)[[1]]) - 1), Habitats_P3 = levels(Rast_p3_klit_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(Paragraph3_DF) %>%
  dplyr::select(-Natyp_navn)

Natura2000_DF <- data.frame(Natura2000 = 0, Natura_2000 = "Yes")

Area2 <- Area2 %>%
  full_join(Natura2000_DF) %>%
  dplyr::select(-Natura2000)

markblokkort_DF <- data.frame(MB_TYPE = 0:(length(levels(Rast_markblokkort_Croped)[[1]]) - 1), Types_markblokkort = levels(Rast_markblokkort_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(markblokkort_DF) %>%
  dplyr::select(-"MB_TYPE") %>%
  mutate(Area_Sq_Mt = 100*Freq,
         Proportion = 100*(Area_Sq_Mt/Area_DK)) %>%
  dplyr::select(-Freq) %>%
  ungroup()

IUCN_DF <- data.frame(IUCN = 0, IS_IUCN = "Yes")

Area2 <- Area2 %>%
  full_join(IUCN_DF) %>%
  dplyr::select(-IUCN) %>%
  rename(IUCN= IS_IUCN)

Urort_Skov_DF <- data.frame(Owned = 0:(length(levels(Rast_Urort_Skov_Croped)[[1]]) - 1), Urort_Skov = levels(Rast_Urort_Skov_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(Urort_Skov_DF) %>%
  dplyr::select(-Owned)


stoette_DF <- data.frame(Type  = 0:(length(levels(Rast_stoette_Croped)[[1]]) - 1), Stoette = levels(Rast_stoette_Croped)[[1]])

Area2 <- Area2 %>%
  full_join(stoette_DF) %>%
  dplyr::select(-Type)

NaturaOgVildtreservater_DF <- data.frame(Temanavn  = 0, NaturaOgVildtreservater = "Yes")

Area2 <- Area2 %>%
  full_join(NaturaOgVildtreservater_DF) %>%
  dplyr::select(-Temanavn)

Naturnationalparker_DF <- data.frame(ID  = 0, Naturnationalparker = "Yes")

Area2 <- Area2 %>%
  full_join(Naturnationalparker_DF) %>%
  dplyr::select(-ID) %>%
  dplyr::relocate(Proportion, .after = everything()) %>%
  dplyr::relocate(Area_Sq_Mt, .after = everything())

saveRDS(Area2, "Area_summary.rds")

## For table 1



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

#


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

#

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

##

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

##


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

###

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

##

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


Total_Area__Table1a <- Area2 %>%
  dplyr::filter(!is.na(Naturnationalparker)) %>%
  mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) & is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) & is.na(Stoette) ~ "No",
                               TRUE ~ "Yes")) %>%
  group_by(Naturnationalparker, Overlaped) %>%
  summarise_if(is.numeric, sum)

##

Table1 <- list(Natura2000_Table1, Paragraph3_Table1, NaturaOgVildtreservater_Table1, IUCN_Table1, Urort_Skov_Table1, Stoette_Table1, Naturnationalparker_Table1) %>%
  purrr::reduce(bind_rows)

readr::write_csv(Table1, "Table1.csv")

###


SUM <- Area2 %>% group_by(Habitats_P3) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Habitats_P3))

SUM2 <- Area2 %>% group_by(Natura_2000) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Natura_2000))


SUM3 <- Area2 %>% group_by(Types_markblokkort) %>%
  summarise_if(is.numeric, sum) %>%
  dplyr::filter(!is.na(Types_markblokkort))
