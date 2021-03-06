library(terra)
library(magrittr)
library(geodata)
library(sf)
library(tidyverse)
library(mregions)

Template <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif")

values(Template) <- 1



DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd(), version = "4.0") %>%
  terra::project(terra::crs(Template))

Area_DK <- terra::expanse(DK)
### Natura2000

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Natura2000 MiljøGIS Maj2022/pg-natura_2000_omraader_natura2000.shp")


Natura2000$Natura_2000 <- "yes"
# Get the layer

Natura2000 <- Natura2000[,"Natura_2000"]

# Aggregate to multypolygon

Natura2000 <- terra::aggregate(Natura2000, by = "Natura_2000")

# Select feature

Natura2000 <- Natura2000[,"Natura_2000"]


# Rasterize

Rast_Natura2000  <- terra::rasterize(Natura2000, Template, field = "Natura_2000")


# Crop to Denmark

Rast_Natura2000_Croped <- terra::mask(Rast_Natura2000, DK)

## Habitatnaturtyper

Habitatnaturtype_terrestrial_1 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_habitatsoer_u5ha.shp")
Habitatnaturtype_terrestrial_2 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_lysaaben_natur2016_2019.shp")
Habitatnaturtype_terrestrial_3 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_marine_kortlaeg_2004_2018.shp")
Habitatnaturtype_terrestrial_4 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_skov.shp")
Habitatnaturtype_terrestrial_5 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_soer_over5ha.shp")
Habitatnaturtype_terrestrial_6 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/np3h2021_shp_download/np3h2021_vandlob.shp")

Habitatnaturtype_terrestrial <- list(Habitatnaturtype_terrestrial_1, Habitatnaturtype_terrestrial_2, Habitatnaturtype_terrestrial_3, Habitatnaturtype_terrestrial_4, Habitatnaturtype_terrestrial_5, Habitatnaturtype_terrestrial_6) %>%
  purrr::reduce(rbind)


rm(Habitatnaturtype_terrestrial_1, Habitatnaturtype_terrestrial_2, Habitatnaturtype_terrestrial_3, Habitatnaturtype_terrestrial_4, Habitatnaturtype_terrestrial_5, Habitatnaturtype_terrestrial_6)

gc()

Habitatnaturtype_terrestrial$Habitatnaturtype <- "Yes"

Habitatnaturtype_terrestrial <- Habitatnaturtype_terrestrial[,"Habitatnaturtype"]

# Aggregate to multypolygon

Habitatnaturtype_terrestrial <- terra::aggregate(Habitatnaturtype_terrestrial, by = "Habitatnaturtype")

# Select feature

Habitatnaturtype_terrestrial <- Habitatnaturtype_terrestrial[,"Habitatnaturtype"]


# Rasterize

Habitatnaturtype_terrestrial  <- terra::rasterize(Habitatnaturtype_terrestrial, Template, field = "Habitatnaturtype")


# Crop to Denmark

Rast_Habitatnaturtype_terrestrial_Croped <- terra::mask(Habitatnaturtype_terrestrial, DK)


### P3

Paragraph3 <-  vect("O:/Nat_BDR-data/Arealanalyse/RAW/BES_NATURTYPER_SHAPE")

Paragraph3 <- Paragraph3[,c("Natyp_navn")]

Paragraph3_by_nature <- aggregate(Paragraph3, by='Natyp_navn')


# read klits

Klits <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/MATRIKELKORT/DK_SHAPE_UTM32-EUREF89/MINIMAKS/TEMA/KLIT.shp")

Klits$Natyp_navn <- "Klit"


Klits <- terra::aggregate(Klits, by = "Natyp_navn") %>% terra::project(terra::crs(Template))

# joint both polygons

Habs2 <- rbind(Paragraph3_by_nature, Klits)

# rasterize to take out overlaps

Rast_p3_klit  <- terra::rasterize(Habs2, Template, field = "Natyp_navn")

Rast_p3_klit_Croped <- terra::mask(Rast_p3_klit, DK)

names(Rast_p3_klit_Croped) <- "Paragraph_3_Klit"

### NaturaOgVildtlive


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

names(Rast_NaturaOgVildtreservater_Croped) <- "NaturaOgVildtreservater"

##

IUCN <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/IUCN REVISED FREDNINGER/Fredninger_IUCNKat_2018_25832.shp")
IUCN$IUCN <- "Yes"

IUCN_Aggregated <- terra::aggregate(IUCN, by='IUCN')


Rast_IUCN  <- terra::rasterize(IUCN_Aggregated, Template, field = "IUCN")

Rast_IUCN_Croped <- terra::mask(Rast_IUCN, DK)

## Urort skov

Urort_Skov <- list.files(path = "O:/Nat_BDR-data/Arealanalyse/RAW/Uroert skov NST Feb2022/", full.names = T, pattern = "shp")

Urort_Skov <- Urort_Skov[str_detect(Urort_Skov, "Forslag2021_omfang", negate = T)]

Urort_Skov <- Urort_Skov %>%
  purrr::map(vect) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Template))

Urort_Skov$Owned <- "Urort_Skov"

# Read private owned untouched forest

private_Urort_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp") %>%
  terra::project(crs(Template))
private_Urort_Skov <- private_Urort_Skov[private_Urort_Skov$tilskudsor== "Privat urørt skov", ]

private_Urort_Skov$Owned <- "Urort_Skov"

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

names(Rast_Urort_Skov_Croped) <- "Urort_Skov"

### Naturnationalpark

National_Parks <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NNP from NST/Naturnationalparker.shp") %>%
  terra::project(crs(Template))

National_Parks$Naturnationalparks <- "Yes"

National_Parks_Aggregated <- terra::aggregate(National_Parks, by='Naturnationalparks')

Rast_National_Parks  <- terra::rasterize(National_Parks_Aggregated, Template, field = "Naturnationalparks")

Rast_National_Parks_Croped <- terra::mask(Rast_National_Parks, DK)

## Stoette

stoette_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_Skov <- stoette_Skov[stoette_Skov$tilskudsor== "Privat urørt skov", ]

stoette_Skov$Type <- "Yes"

# Read sammenhaengende

stoette_sammenhaengende <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_sammenhaengende <- stoette_sammenhaengende[stoette_sammenhaengende$tilskudsor== "Sammenhængende arealer", ]
stoette_sammenhaengende$Type <- "Yes"


# read egekrat

stoette_egekrat <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/EGEKRAT/egekrat.shp")

stoette_egekrat <- stoette_egekrat[stoette_egekrat$vurdering_ %in% c(1,2),]
stoette_egekrat <- stoette_egekrat[stoette_egekrat$sikret %in% c("ja"),]
stoette_egekrat$Type <- "Yes"

# Join them together

stoette <- list(stoette_Skov, stoette_sammenhaengende, stoette_egekrat) %>% purrr::reduce(rbind) %>%
  terra::project(crs(Template))

# Transform to multipolygon

stoette <- stoette[,"Type"]
stoette_Aggregated <- terra::aggregate(stoette, by='Type')

# Rasterize

Rast_stoette  <- terra::rasterize(stoette_Aggregated, Template, field = "Type")

Rast_stoette_Croped <- terra::mask(Rast_stoette, DK)

names(Rast_stoette_Croped) <- "Stoette"

## Stoette detail

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

Rast_stoette_detail_Croped <- terra::mask(Rast_stoette, DK)

names(Rast_stoette_detail_Croped) <- "Stoette_detail"

### Fund

Fondsejede <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Archive/ejerskab_20220609.gpkg")
Fondsejede <- Fondsejede[Fondsejede$ejerforhold_dni == "fond",]
Fondsejede$Fond <- "Yes"

Fondsejede_Aggregated <- terra::aggregate(Fondsejede, by = "Fond")

Fondsejede_Ownersip  <- terra::rasterize(Fondsejede_Aggregated, Template, field = "Fond")
Rast_Fondsejede_Croped <- terra::mask(Fondsejede_Ownersip, DK)

### Forests

Forest <-  terra::rast("O:/Nat_BDR-data/Arealanalyse/RAW/Basemap03_tree_cover_NEW_EXPORT/sub_tree_cover_2018_RECLASS1.tif") %>%
  terra::project(terra::crs(Template)) %>%
  terra::resample(Template, method = "near")


Forerst <- Forest %>% mask(DK)

Forerst[Forerst< 1] <- NA
Forerst[Forerst == 4] <- NA
Forerst[Forerst %in% c(1,2,3,5)] <- 1

markblokkort <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Markblokke2021/Markblokke2021.shp")

# Select only permanent grasslands "PGR" and plough area "OMD"

markblokkort <- markblokkort[(markblokkort$MB_TYPE %in% c("OMD", "PGR")),7]

# Transform to multipolygon

markblokkort_Aggregated <- terra::aggregate(markblokkort, by='MB_TYPE')

# Rasterize

Rast_markblokkort  <- terra::rasterize(markblokkort_Aggregated, Template, field = "MB_TYPE")

# Crop to Denmark

Rast_markblokkort_Croped <- terra::mask(Rast_markblokkort, DK)

# ownership

Ownership <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Archive/ejerskab_20220609.gpkg")
Ownership$Ownership <- ifelse(Ownership$ejerforhold_dni %in% c("fond", "andet"), "Privat",
                              ifelse(Ownership$ejerforhold_dni %in% c("naturstyrelsen", "forsvaret","landbrugsstyrelsen", "kystdirektoratet"), "Statslig", "Kommunal"))

Ownersip_Aggregated <- terra::aggregate(Ownership, by = "Ownership")

Rast_Ownersip  <- terra::rasterize(Ownersip_Aggregated, Template, field = "Ownership")
Rast_Ownersip_Croped <- terra::mask(Rast_Ownersip, DK)

# Saerligt
Saerligt_NATURA_2000 <-  terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/SAER_UDPEG_N2000_SHAPE/SAER_UDPEG_N2000.shp")

Saerligt_NATURA_2000$TEMANAVN <- "Yes"

# Transform to multipolygon

Saerligt_NATURA_2000 <- Saerligt_NATURA_2000[,"TEMANAVN"]
Saerligt_NATURA_2000 <- terra::aggregate(Saerligt_NATURA_2000, by='TEMANAVN')

# Rasterize

Rast_Saerligt_NATURA_2000  <- terra::rasterize(Saerligt_NATURA_2000, Template, field = "TEMANAVN")

Rast_Saerligt_NATURA_2000_Croped <- terra::mask(Rast_Saerligt_NATURA_2000, DK)

names(Rast_Saerligt_NATURA_2000_Croped) <- "Saerligt"

## Pleje og graes

Pleje_og_graes <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Tilsagn_til_pleje_af_graes_2015-2020_2021/Tilsagn_til_pleje_af_graes_2015-2020_2021.shp")
Pleje_og_graes$Pleje_og_graes <- "Yes"

# Transform to multipolygon

Pleje_og_graes <- Pleje_og_graes[,"Pleje_og_graes"]
Pleje_og_graes <- terra::aggregate(Pleje_og_graes, by='Pleje_og_graes')

# Rasterize

Rast_Pleje_og_graes  <- terra::rasterize(Pleje_og_graes, Template, field = "Pleje_og_graes")

Rast_Pleje_og_graes_Croped <- terra::mask(Rast_Pleje_og_graes, DK)

names(Rast_Pleje_og_graes_Croped) <- "Pleje_og_graes"
###

All <- c(Rast_Natura2000_Croped, Rast_Habitatnaturtype_terrestrial_Croped, Rast_p3_klit_Croped, Rast_NaturaOgVildtreservater_Croped, Rast_IUCN_Croped, Rast_Urort_Skov_Croped, Rast_National_Parks_Croped, Rast_stoette_Croped, Rast_Fondsejede_Croped, Forerst, Rast_markblokkort_Croped, Rast_Ownersip_Croped, Rast_Saerligt_NATURA_2000_Croped, Rast_Pleje_og_graes_Croped, Rast_stoette_detail_Croped)

Long_Table_All <- terra::crosstab(All, useNA=T, long=TRUE)

saveRDS(Long_Table_All, "ALongerTable.rds")

Long_Table_All2 <- Long_Table_All  %>% mutate_at(c("Natura_2000", "Habitatnaturtype", "NaturaOgVildtreservater",
                                                   "IUCN", "Urort_Skov", "Naturnationalparks", "Stoette", "Fond", "Saerligt",
                                                   "Pleje_og_graes"), ~ifelse(.x == 0, "Yes", NA)) %>%
  mutate(sub_tree_cover_2018_RECLASS1 = ifelse(sub_tree_cover_2018_RECLASS1 == 1, "Yes", NA)) %>%
  rename(Forest = sub_tree_cover_2018_RECLASS1)

markblokkort_DF <- data.frame(MB_TYPE = 0:(length(levels(Rast_markblokkort_Croped)[[1]]) - 1), Types_markblokkort = levels(Rast_markblokkort_Croped)[[1]])

Long_Table_All2 <- Long_Table_All2 %>%
  full_join(markblokkort_DF) %>%
  dplyr::select(-"MB_TYPE") %>%
  mutate(Area_Sq_Mt = 100*Freq,
         Proportion = 100*(Area_Sq_Mt/Area_DK),
         Area_Sq_Km = Area_Sq_Mt/1000000) %>%
  dplyr::select(-Freq, -Area_Sq_Mt) %>%
  ungroup()


Paragraph3_DF <- data.frame(Paragraph_3_Klit = 0:(length(levels(Rast_p3_klit_Croped)[[1]]) - 1), Habitats_P3 = levels(Rast_p3_klit_Croped)[[1]])

Long_Table_All2 <- Long_Table_All2 %>%
  full_join(Paragraph3_DF) %>%
  dplyr::select(-Paragraph_3_Klit)

Ownership_DF <- data.frame(Ownership  = 0:(length(levels(Rast_Ownersip_Croped)[[1]]) - 1), ownership = levels(Rast_Ownersip_Croped)[[1]])

Long_Table_All2 <- Long_Table_All2 %>%
  full_join(Ownership_DF) %>%
  dplyr::select(-Ownership)

Stoette_Detail_DF <- data.frame(Stoette_detail  = 0:(length(levels(Rast_stoette_detail_Croped)[[1]]) - 1), stoette_detail = levels(Rast_stoette_detail_Croped)[[1]])

Long_Table_All2 <- Long_Table_All2 %>%
  full_join(Stoette_Detail_DF) %>%
  dplyr::select(-Stoette_detail)

Long_Table_All2 <- Long_Table_All2[!(rowSums(is.na(Long_Table_All2)) == max(rowSums(is.na(Long_Table_All2)))),]


saveRDS(Long_Table_All2, "ALongerTable.rds")
