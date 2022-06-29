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

Fuglebeskyt_Croped_Sea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Fuglebeskyt_Croped_Sea.tif")

AllSea <- c(Rast_Natura2000_Croped_Sea, Rast_Habitatomrade_Croped_Sea,
            Rast_Habitatnaturtype_Croped_Sea, Rast_Ramsar_Croped_Sea,
            Havstrategi_standard_Croped_Sea, Havstrategi_streng_Croped_Sea,
            Natur_Vildt_Reservater_Croped_Sea, Fredninger_Croped_Sea, Fuglebeskyt_Croped_Sea)

temp <- crosstab(AllSea, useNA=T, long=TRUE)


names(temp)[1:9] <- c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
                      "Havstrategi_standard", "Havstrategi_streng",
                      "Natur_Vildt_Reservater", "Fredninger", "Fuglebeskyt")

library(tidyverse)

Long <- temp %>%
  mutate_at(c("Natura_2000", "Habitatomrade", "Habitatnaturtype", "Ramsar",
              "Havstrategi_standard", "Havstrategi_streng", "Natur_Vildt_Reservater",
              "Fredninger", "Fuglebeskyt"), ~ifelse(. == 1, "Yes", NA)) %>%
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
              "Fredninger"), ~ifelse(. == 1, "Yes", NA))


Habitatnaturtype_DF <- data.frame(Habitatnaturtype_Detail = 0:(length(levels(Rast_Habitatnaturtype_Croped_Sea_Detail)[[1]]) - 1), Types_Habitatnaturtype = levels(Rast_Habitatnaturtype_Croped_Sea_Detail)[[1]])

Long <- Long %>%
  full_join(Habitatnaturtype_DF) %>%
  dplyr::select(-"Habitatnaturtype_Detail") %>%
  mutate(Area_Sq_Mt = 100*Freq,
         Proportion = 100*(Area_Sq_Mt/Area_Sea_DK),
         Area_Sq_Km = Area_Sq_Mt/1000000) %>%
  dplyr::select(-Freq) %>%
  ungroup()

Long <- Long %>%
  group_by_if(is.character) %>%
  summarise_if(is.numeric,~sum(.x, na.rm = T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Yes = sum(c_across(Natura_2000:Types_Habitatnaturtype) == "Yes", na.rm = TRUE)) %>%
  dplyr::filter(Yes > 1) %>%
  arrange(desc(Area_Sq_Km))

saveRDS(Long, "LongSeaTable2.rds")

## Table 2

Long <- readRDS("LongSeaTable2.rds")

Table2_Sea <- Long %>%
  dplyr::select(Natura_2000, Havstrategi_standard, Havstrategi_streng,
                Natur_Vildt_Reservater, Fredninger, Area_Sq_Km, Proportion) %>%
  dplyr::group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  pivot_longer(Natura_2000:Fredninger) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value) %>%
  group_by(name) %>%
  summarise_if(is.numeric, sum)

Table2_Sea_b <- Long %>%
  dplyr::select(Natura_2000, Havstrategi_standard, Havstrategi_streng,
                Natur_Vildt_Reservater, Fredninger, Types_Habitatnaturtype, Area_Sq_Km, Proportion) %>%
  dplyr::group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  dplyr::filter(!is.na(Types_Habitatnaturtype)) %>%
  pivot_longer(Natura_2000:Fredninger) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value) %>%
  group_by(name, Types_Habitatnaturtype) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup %>%
  dplyr::select(-Proportion) %>%
  group_split(Types_Habitatnaturtype) %>%
  purrr::map(~pivot_wider(.x, names_from = Types_Habitatnaturtype, values_from = Area_Sq_Km)) %>%
  purrr::reduce(full_join)

Table2_Total <- Long %>%
  ungroup() %>%
  dplyr::select(Natura_2000, Havstrategi_standard, Havstrategi_streng,
                Natur_Vildt_Reservater, Fredninger, Area_Sq_Km, Proportion) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(name = "Total")

Table2_Total_Sub <- Long %>%
  dplyr::select(Types_Habitatnaturtype, Area_Sq_Km, Proportion) %>%
  dplyr::group_by_if(is.character) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  dplyr::filter(!is.na(Types_Habitatnaturtype)) %>%
  ungroup %>%
  dplyr::select(-Proportion) %>%
  pivot_wider(names_from = Types_Habitatnaturtype, values_from = Area_Sq_Km)%>%
  mutate(name = "Total")

Table2 <- full_join(Table2_Sea, Table2_Sea_b)
Table2[is.na(Table2)] <- 0

Table2_Total <- full_join(Table2_Total, Table2_Total_Sub)

Table2_Final <- rbind(Table2_Total, Table2) %>%
  relocate(name, .before = everything()) %>%
  arrange(desc(Area_Sq_Km))

openxlsx::write.xlsx(Table2_Final, "Table2_Marine.xlsx")


