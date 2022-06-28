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


SeaOfDenmark <- mregions::mr_shp("Denmark:eez") %>%
  dplyr::filter(geoname == "Danish Exclusive Economic Zone" ) %>%
  dplyr::select(geoname) %>%
  terra::vect() %>%
  terra::project(terra::crs(DK))

Area_Sea_DK <- expanse(SeaOfDenmark)

TemplateSea <- terra::extend(Template, SeaOfDenmark)



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


Template <- Rast_Habitatnaturtype_Croped_Sea

values(Template) <- as.integer(1)

ForTiles <- aggregate(Template, fact = 6000)

makeTiles(AllSea, ForTiles, filename="tile_.tif", extend=FALSE, na.rm=FALSE)

Tiles <- list.files(pattern = "tile_", full.names = T)
library(magrittr)
temp <- list()
for(i in 1:length(Tiles)){
  Rast <- terra::rast(Tiles[[i]])
  if(sum(is.na(terra::minmax(Rast))) != 16){
    temp[[i]] <- terra::rast(Tiles[[i]]) %>%
      crosstab(useNA=T, long=TRUE)
  }

  message(paste(i, "of", length(Tiles), "ready", Sys.time()))
  }

temp <- temp %>%
  purrr::reduce(dplyr::bind_rows)

names(temp)[1:8] <- c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
                      "Havstrategi_standard", "Havstrategi_streng",
                      "Natur_Vildt_Reservater", "Fredninger")

library(tidyverse)

Long <- temp %>%
  mutate_at(c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
              "Havstrategi_standard", "Havstrategi_streng", "Natur_Vildt_Reservater",
              "Fredninger"), ~ifelse(. == 1, "Yes", NA)) %>%
  group_by_if(is.character) %>%
  summarise(Frequency = sum(Freq)) %>%
  ungroup()

saveRDS(Long, "LongSeaTable.rds")



Habitatnaturtype <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_marine_kortlaeg_2004_2018.shp")

Habitatnaturtype$Habitatnaturtype <- Habitatnaturtype$Naturnavn
Habitatnaturtype <- Habitatnaturtype[,c("Habitatnaturtype")]

Habitatnaturtype <- aggregate(Habitatnaturtype, by='Habitatnaturtype')
Habitatnaturtype_Croped_Sea <- terra::mask(Habitatnaturtype, SeaOfDenmark)


Rast_Habitatnaturtype_Croped_Sea_Detail <- terra::rasterize(Habitatnaturtype_Croped_Sea, TemplateSea, field = "Habitatnaturtype")

AllSea <- c(Rast_Natura2000_Croped_Sea, Rast_Habitatomrade_Croped_Sea,
            Rast_Habitatnaturtype_Croped_Sea, Rast_Ramsar_Croped_Sea,
            Havstrategi_standard_Croped_Sea, Havstrategi_streng_Croped_Sea,
            Natur_Vildt_Reservater_Croped_Sea, Fredninger_Croped_Sea, Rast_Habitatnaturtype_Croped_Sea_Detail)

makeTiles(AllSea, ForTiles, filename="tile_.tif", extend=FALSE, na.rm=FALSE)

Tiles <- list.files(pattern = "tile_", full.names = T)
Tiles <- Tiles[str_detect(Tiles, "aux", negate = T)]
library(magrittr)
temp <- list()
for(i in 1:length(Tiles)){
  Rast <- terra::rast(Tiles[[i]])
  if(sum(is.na(terra::minmax(Rast))) != 16){
    temp[[i]] <- terra::rast(Tiles[[i]]) %>%
      crosstab(useNA=T, long=TRUE)
  }

  message(paste(i, "of", length(Tiles), "ready", Sys.time()))
}

temp <- temp %>%
  purrr::reduce(dplyr::bind_rows)

names(temp)[1:9] <- c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
                      "Havstrategi_standard", "Havstrategi_streng",
                      "Natur_Vildt_Reservater", "Fredninger", "Habitatnaturtype_Detail")

library(tidyverse)

Long <- temp %>%
  mutate_at(c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
              "Havstrategi_standard", "Havstrategi_streng", "Natur_Vildt_Reservater",
              "Fredninger"), ~ifelse(. == 1, "Yes", NA)) %>%
  group_by_if(is.character) %>%
  summarise(Frequency = sum(Freq)) %>%
  ungroup()

saveRDS(Long, "LongSeaTable.rds")
