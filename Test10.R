library(terra)
library(dplyr)
library(geodata)

Habs <- terra::vect("O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp")

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/HABITAT_OMRAADER/HABITAT_OMRAADER.shp")

Natura2000 <- Natura2000[,7]

Natura2000 <- terra::aggregate(Natura2000, by = "Temanavn")

markblokkort <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Markblokke2021/Markblokke2021.shp")

markblokkort <- markblokkort[(markblokkort$MB_TYPE %in% c("OMD", "PGR")),7]

markblokkort_Aggregated <- terra::aggregate(markblokkort, by='MB_TYPE')


Canopy_Cover <- list.files(path = "O:/Nat_Ecoinformatics/C_Write/_Archive/Assmann_etal_EcoDes-DK15/EcoDes-DK15_v1.1.0/canopy_height/", full.names = T, pattern = ".vrt") %>%
  terra::rast()

Sys.time()
Test <- terra::rasterize(Habs, Canopy_Cover, field = "Natyp_navn")
message(paste("Paragraph 3 ready", Sys.time()))
Test2 <- terra::rasterize(Natura2000, Canopy_Cover, field = "Temanavn")
message(paste("Natura 2000 ready", Sys.time()))
Test3 <- terra::rasterize(markblokkort_Aggregated, Canopy_Cover, field = "MB_TYPE")
message(paste("markblokkort 3 ready", Sys.time()))


All <- c(Test, Test2, Test3)

DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd()) %>%
  terra::project(terra::crs(Habs))
Area_DK <- terra::expanse(DK)

ALL <- terra::mask(All, DK)


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
