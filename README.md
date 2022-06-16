Dataset generation for the Danish Biodiversity council
================
Derek Corcoran
16/06, 2022

-   [1 Objective](#1-objective)
-   [2 Packages needed](#2-packages-needed)
-   [3 Terrestrial ecosystems](#3-terrestrial-ecosystems)
    -   [3.1 Data generation](#31-data-generation)
        -   [3.1.1 Raster template](#311-raster-template)
        -   [3.1.2 Denmark’s Area](#312-denmarks-area)
        -   [3.1.3 Paragraph 3 and dunes](#313-paragraph-3-and-dunes)
        -   [3.1.4 Natura 2000](#314-natura-2000)
        -   [3.1.5 Markblokkort](#315-markblokkort)
        -   [3.1.6 Nature and wildlife
            reserves](#316-nature-and-wildlife-reserves)
        -   [3.1.7 IUCN](#317-iucn)
        -   [3.1.8 Untouched forest](#318-untouched-forest)
        -   [3.1.9 Nature National Parks](#319-nature-national-parks)
        -   [3.1.10 Subsidy schemes](#3110-subsidy-schemes)
    -   [3.2 Results](#32-results)
-   [4 Ocean ecosystems](#4-ocean-ecosystems)
    -   [4.1 Data generation](#41-data-generation)
        -   [4.1.1 Denmark’s Exclusive Economic Zone
            (EEZ)](#411-denmarks-exclusive-economic-zone-eez)
        -   [4.1.2 Natura 2000](#412-natura-2000)
        -   [4.1.3 Nature and wildlife
            reservations](#413-nature-and-wildlife-reservations)
-   [5 Session info](#5-session-info)
-   [6 References](#6-references)

# 1 Objective

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

The goal of this repository is to generate the data sets necessary to
evaluate the current and potential area that is strictly and partially
protected in Denmark, note that all codes will be collapsed so that only
the code that the reader needs to see can be extended.

# 2 Packages needed

All calculations were done using the R software (R Core Team 2021), and
all the geospatial transformations and calculations where performed
using the terra package (Hijmans 2022). There are some datasets
extracted from the geodata package (Hijmans, Ghosh, and Mandel 2022).
The magrittr package was use for piping (Bache and Wickham 2020), and
finally the SF package was used to generate the Cloud Optimized Rasters
(Pebesma 2018).

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Load packages

</summary>

``` r
library(terra)
library(magrittr)
library(geodata)
library(sf)
library(tidyverse)
library(mregions)
```

</details>

# 3 Terrestrial ecosystems

## 3.1 Data generation

In this Section we will try to solve for intersections within the data
and incongruencies within the data, some of the polygons provided have
topological issues so we need to rasterize them in order to resolve this
issues easily.

### 3.1.1 Raster template

In order to align all datasets together a template raster will be
generated using a 10 by 10 meter grid equal area using the following
code.

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Template raster

</summary>

``` r
Template <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif")

values(Template) <- 1
```

</details>

### 3.1.2 Denmark’s Area

To both crop the areas (some include ocean and terrestrial ecosystems)
and to have a value of the total area of Denmark, we need to have a
polygon of the coastline of the country. We used the GADM dataset
version 4.0 using the geodata package (Global Administrative Areas 2022
; Hijmans, Ghosh, and Mandel 2022) using the following code, the
resulting polygon is shown in figure <a href="#fig:PlotDenmark">3.1</a>

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Denmark Area

</summary>

``` r
DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd(), version = "4.0") %>%
    terra::project(terra::crs(Template))
Area_DK <- terra::expanse(DK)
```

</details>

![Figure 3.1: Polygon of Denmark according to GADM
4.0](README_files/figure-gfm/PlotDenmark-1.png)

The total area for Denmark according to that is 43,145 Square kilometers

### 3.1.3 Paragraph 3 and dunes

Within paragraph 3 (miljoportal 2022) and dunes there are several
overlaps, so the next code joins these two data sets and resolves
overlaps.

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Join paragraph 3 with klit and rasterize

</summary>

``` r
# read Paragraph 3

Paragraph3 <- vect("O:/Nat_BDR-data/Arealanalyse/RAW/BES_NATURTYPER_SHAPE")

Paragraph3 <- Paragraph3[, c("Objekt_id", "Natyp_navn")]

Paragraph3_by_nature <- aggregate(Paragraph3, by = "Natyp_navn")

terra::writeVector(Paragraph3_by_nature, "O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp",
    overwrite = T)

Habs <- terra::vect("O:/Nat_BDR-data/Arealanalyse/PROCESSED/Aggregated/Paragraph3_by_nature.shp")

# read klits

Klits <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/MATRIKELKORT/DK_SHAPE_UTM32-EUREF89/MINIMAKS/TEMA/KLIT.shp")

Klits$Natyp_navn <- "Klit"


Klits <- terra::aggregate(Klits, by = "Natyp_navn") %>%
    terra::project(terra::crs(Habs))

# joint both polygons

Habs2 <- rbind(Habs, Klits)

# rasterize to take out overlaps

Rast_p3_klit <- terra::rasterize(Habs2, Template, field = "Natyp_navn")
```

</details>

And then saves it first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write paragraph 3 and klit

</summary>

``` r
# Write raw rasters to disk

writeRaster(Rast_p3_klit, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))
Rast_p3_klit_Croped <- terra::mask(Rast_p3_klit, DK)

# Write croped rasters to disk

writeRaster(Rast_p3_klit_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit.tif",
    destination = "RasterizedCOG/Rast_COG_p3_klit.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_p3_klit_Croped.tif",
    destination = "RasterizedCOG/Rast_COG_p3_klit_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure <a href="#fig:PlotP3klit">3.2</a>

![Figure 3.2: Plot of the areas of Paragraph 3 and klit habitat
types](README_files/figure-gfm/PlotP3klit-1.png)

### 3.1.4 Natura 2000

As it was done with Paragraph 3 before this, we rasterized and cropped
this polygons to the area of Denmark as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize natura 2000

</summary>

``` r
# Read the layer

Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Natura2000 MiljøGIS Maj2022/pg-natura_2000_omraader_natura2000.shp")

# Get the layer

Natura2000 <- Natura2000[, "temanavn"]

# Aggregate to multypolygon

Natura2000 <- terra::aggregate(Natura2000, by = "temanavn")

# Select feature

Natura2000 <- Natura2000[, "temanavn"]

# change feature name

names(Natura2000) <- "Temanavn"

# Add a feature for subsetting

Natura2000$Natura2000 <- "yes"

# Rasterize

Rast_Natura2000 <- terra::rasterize(Natura2000, Template, field = "Natura2000")


# Crop to Denmark

Rast_Natura2000_Croped <- terra::mask(Rast_Natura2000, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write natura 2000

</summary>

``` r
writeRaster(Rast_Natura2000, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_Natura2000_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

# Save as cloud optimized raster

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif",
    destination = "RasterizedCOG/Rast_Natura2000.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped.tif",
    destination = "RasterizedCOG/Rast_Natura2000_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure <a href="#fig:PlotNatura-2000">3.3</a>

![Figure 3.3: Plot of the areas of Natura
2000](README_files/figure-gfm/PlotNatura-2000-1.png)

### 3.1.5 Markblokkort

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize Markblokkort

</summary>

``` r
# read the poltygons

markblokkort <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Markblokke2021/Markblokke2021.shp")

# Select only permanent grasslands 'PGR' and plough area 'OMD'

markblokkort <- markblokkort[(markblokkort$MB_TYPE %in% c("OMD", "PGR")), 7]

# Transform to multipolygon

markblokkort_Aggregated <- terra::aggregate(markblokkort, by = "MB_TYPE")

# Rasterize

Rast_markblokkort <- terra::rasterize(markblokkort_Aggregated, Template, field = "MB_TYPE")

# Crop to Denmark

Rast_markblokkort_Croped <- terra::mask(Rast_markblokkort, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write markblokkort

</summary>

``` r
writeRaster(Rast_markblokkort, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_markblokkort_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort.tif",
    destination = "RasterizedCOG/Rast_markblokkort.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_markblokkort_Croped.tif",
    destination = "RasterizedCOG/Rast_markblokkort_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure
<a href="#fig:Plotmarkblokkort">3.4</a>

![Figure 3.4: Plot of permanent grasslands and plough areas in
markblokkort](README_files/figure-gfm/Plotmarkblokkort-1.png)

### 3.1.6 Nature and wildlife reserves

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize Nature and wildlife reserves

</summary>

``` r
# read the polygons

Wildreserve <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/NATUR_VILDT_RESERVATER/NATUR_VILDT_RESERVATER.shp")

# Eliminate the some of the reserves

NaturaOgVildtreservater <- Wildreserve[!(Wildreserve$Beken_navn %in% c("Agerø og Skibsted Fjord",
    "Agger Tange", "Anholt", "Ertholmene", "Hesselø", "Hirsholmene", "Horsens Nørrestrand",
    "Vorsø")), ]

# Transform to multipolygon

NaturaOgVildtreservater_Aggregated <- terra::aggregate(NaturaOgVildtreservater, by = "Temanavn")

# Rasterize

Rast_NaturaOgVildtreservater <- terra::rasterize(NaturaOgVildtreservater_Aggregated,
    Template, field = "Temanavn")

# And crop

Rast_NaturaOgVildtreservater_Croped <- terra::mask(Rast_NaturaOgVildtreservater,
    DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write Nature and wildlife reserves

</summary>

``` r
writeRaster(Rast_NaturaOgVildtreservater, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_NaturaOgVildtreservater_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater.tif",
    destination = "RasterizedCOG/Rast_NaturaOgVildtreservater.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped.tif",
    destination = "RasterizedCOG/Rast_NaturaOgVildtreservater_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure
<a href="#fig:PlotNaturaOgVildtreservater">3.5</a>

![Figure 3.5: Plot of Nature and wildlife
reserves](README_files/figure-gfm/PlotNaturaOgVildtreservater-1.png)

### 3.1.7 IUCN

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize IUCN

</summary>

``` r
# Read

IUCN <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/IUCN REVISED FREDNINGER/Fredninger_IUCNKat_2018_25832.shp")
IUCN$IUCN <- "Yes"

IUCN_Aggregated <- terra::aggregate(IUCN, by = "IUCN")

Sys.time()
#> [1] "2022-06-16 15:48:49 CEST"
Rast_IUCN <- terra::rasterize(IUCN_Aggregated, Template, field = "IUCN")
Sys.time()
#> [1] "2022-06-16 15:50:01 CEST"

Rast_IUCN_Croped <- terra::mask(Rast_IUCN, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write IUCN reserves

</summary>

``` r
writeRaster(Rast_IUCN, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_IUCN_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN.tif",
    destination = "RasterizedCOG/Rast_IUCN.tif", options = c("-of", "COG", "-co",
        "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible", "-co",
        "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_IUCN_Croped.tif",
    destination = "RasterizedCOG/Rast_IUCN_Croped.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure <a href="#fig:PlotIUCN">3.6</a>

![Figure 3.6: Plot of IUCN
areas](README_files/figure-gfm/PlotIUCN-1.png)

### 3.1.8 Untouched forest

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize Untouched forest

</summary>

``` r
# Read state owned untouched forest

Urort_Skov <- list.files(path = "O:/Nat_BDR-data/Arealanalyse/RAW/Uroert skov NST Feb2022/",
    full.names = T, pattern = "shp") %>%
    purrr::map(vect) %>%
    purrr::reduce(rbind) %>%
    terra::project(crs(Template))

Urort_Skov$Owned <- "State"

# Read private owned untouched forest

private_Urort_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp") %>%
    terra::project(crs(Template))
private_Urort_Skov <- private_Urort_Skov[private_Urort_Skov$tilskudsor == "Privat urørt skov",
    ]

private_Urort_Skov$Owned <- "Private"

# join geometries

Urort_Skov <- rbind(Urort_Skov, private_Urort_Skov)
Urort_Skov <- Urort_Skov[, "Owned"]

# fix validity problems

Urort_Skov <- Urort_Skov %>%
    terra::makeValid()

# Transform to multipolygons

Urort_Skov_Aggregated <- terra::aggregate(Urort_Skov, by = "Owned")

# Rasterize and crop

Rast_Urort_Skov <- terra::rasterize(Urort_Skov_Aggregated, Template, field = "Owned")

Rast_Urort_Skov_Croped <- terra::mask(Rast_Urort_Skov, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write untouched forest

</summary>

``` r
writeRaster(Rast_Urort_Skov, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_Urort_Skov_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov.tif",
    destination = "RasterizedCOG/Rast_Urort_Skov.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Urort_Skov_Croped.tif",
    destination = "RasterizedCOG/Rast_Urort_Skov_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure <a href="#fig:PlotUrortSkov">3.7</a>

![Figure 3.7: Plot of untouched
forests](README_files/figure-gfm/PlotUrortSkov-1.png)

### 3.1.9 Nature National Parks

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize Nature National Parks

</summary>

``` r
# Read state owned untouched forest
National_Parks <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/GIS filer - de 5/GIS filer - de 5/Naturnationalparker.shp") %>%
    terra::project(crs(Template))

National_Parks$ID <- "NationalParks"

# Read New ones

NewNatParks <- list.files(path = "O:/Nat_BDR-data/Arealanalyse/RAW/NewNationalParks/",
    pattern = ".shp", full.names = T)
NewNatParks <- NewNatParks[str_detect(NewNatParks, "lock", negate = T)] %>%
    purrr::map(vect) %>%
    purrr::reduce(rbind) %>%
    terra::project(crs(Template))

NewNatParks$ID <- "NationalParks"

National_Parks_Aggregated <- rbind(NewNatParks, National_Parks) %>%
    terra::makeValid()

National_Parks_Aggregated <- terra::aggregate(National_Parks_Aggregated, by = "ID")

Rast_National_Parks <- terra::rasterize(National_Parks_Aggregated, Template, field = "ID")

Rast_National_Parks_Croped <- terra::mask(Rast_National_Parks, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write Nature National Parks

</summary>

``` r
writeRaster(Rast_National_Parks, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_National_Parks_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks.tif",
    destination = "RasterizedCOG/Rast_National_Parks.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_National_Parks_Croped.tif",
    destination = "RasterizedCOG/Rast_National_Parks_Croped.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

### 3.1.10 Subsidy schemes

For this layer we also rasterized to 10 by 10 meters as seen in the code
bellow

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

rasterize Subsidy

</summary>

``` r
# Read Private owned untouched forest

stoette_Skov <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_Skov <- stoette_Skov[stoette_Skov$tilskudsor == "Privat urørt skov", ]

stoette_Skov$Type <- "Skov"

# Read sammenhaengende

stoette_sammenhaengende <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/PRIVATE_UNTOUCHED_FOREST/aftale_natur_tinglyst.shp")
stoette_sammenhaengende <- stoette_sammenhaengende[stoette_sammenhaengende$tilskudsor ==
    "Sammenhængende arealer", ]
stoette_sammenhaengende$Type <- "sammenhaengende"


# read egekrat

stoette_egekrat <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/EGEKRAT/egekrat.shp")

stoette_egekrat <- stoette_egekrat[stoette_egekrat$vurdering_ %in% c(1, 2), ]
stoette_egekrat <- stoette_egekrat[stoette_egekrat$sikret %in% c("ja"), ]
stoette_egekrat$Type <- "egekrat"

# Join them together

stoette <- list(stoette_Skov, stoette_sammenhaengende, stoette_egekrat) %>%
    purrr::reduce(rbind) %>%
    terra::project(crs(Template))

# Transform to multipolygon

stoette <- stoette[, "Type"]
stoette_Aggregated <- terra::aggregate(stoette, by = "Type")

# Rasterize

Rast_stoette <- terra::rasterize(stoette_Aggregated, Template, field = "Type")

Rast_stoette_Croped <- terra::mask(Rast_stoette, DK)
```

</details>

And then this is saved first as a geotiff and then exports it to a Cloud
Optimized Geotiff with a deflate compression which is a lossless
compression. as seen in the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Write subsidies

</summary>

``` r
writeRaster(Rast_stoette, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

writeRaster(Rast_stoette_Croped, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette_Croped.tif",
    overwrite = TRUE, gdal = c("COMPRESS=NONE", "TFW=YES", "of=COG"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette.tif",
    destination = "RasterizedCOG/Rast_stoette.tif", options = c("-of", "COG", "-co",
        "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible", "-co",
        "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_stoette_Croped.tif",
    destination = "RasterizedCOG/Rast_stoette_Croped.tif", options = c("-of", "COG",
        "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure <a href="#fig:Plotstoette">3.8</a>

![Figure 3.8: Plot of subisdy
areas](README_files/figure-gfm/Plotstoette-1.png)

## 3.2 Results

To get the results we need to make a stack of all the layers

``` r
All <- c(Rast_Urort_Skov_Croped, Rast_stoette_Croped, Rast_p3_klit_Croped, Rast_markblokkort_Croped,
    Rast_NaturaOgVildtreservater_Croped, Rast_Natura2000_Croped, Rast_National_Parks_Croped,
    Rast_IUCN_Croped)
writeRaster(All, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/All.tif", overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TFW=YES", "of=COG"))
```

and then we crosstabulate to solve for all the intersections

``` r
Area <- crosstab(All, useNA = T, long = TRUE)

saveRDS(Area, "Area_Total.rds")
```

The table generated with crosstab will have names that are not
understandable, hence a lot of the columns must be renamed:

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Generate 1st summary table

</summary>

``` r
# First remove all areas that are NA for all categories

Area <- readRDS("Area_Total.rds")
Area2 <- Area[!(rowSums(is.na(Area)) == max(rowSums(is.na(Area)))), ]

# Change the names of the columns of Paragraph 3 from Natyp_navn to Habitats_P3
# and change numbers for actual names

Paragraph3_DF <- data.frame(Natyp_navn = 0:(length(levels(Rast_p3_klit_Croped)[[1]]) -
    1), Habitats_P3 = levels(Rast_p3_klit_Croped)[[1]])

Area2 <- Area2 %>%
    full_join(Paragraph3_DF) %>%
    dplyr::select(-Natyp_navn)

# Change the names of the columns of Natura 2000 from Natura2000 to Natura_2000
# and change numbers for actual names

Natura2000_DF <- data.frame(Natura2000 = 0, Natura_2000 = "Yes")

Area2 <- Area2 %>%
    full_join(Natura2000_DF) %>%
    dplyr::select(-Natura2000)

# Change the names of the columns of markblokkort from MB_TYPE to
# Types_markblokkort and change numbers for actual names

markblokkort_DF <- data.frame(MB_TYPE = 0:(length(levels(Rast_markblokkort_Croped)[[1]]) -
    1), Types_markblokkort = levels(Rast_markblokkort_Croped)[[1]])

Area2 <- Area2 %>%
    full_join(markblokkort_DF) %>%
    dplyr::select(-"MB_TYPE") %>%
    mutate(Area_Sq_Mt = 100 * Freq, Proportion = 100 * (Area_Sq_Mt/Area_DK)) %>%
    dplyr::select(-Freq) %>%
    ungroup()

# Change the names of the columns of IUCN from IUCN to IS_IUCN and change
# numbers for actual names

IUCN_DF <- data.frame(IUCN = 0, IS_IUCN = "Yes")

Area2 <- Area2 %>%
    full_join(IUCN_DF) %>%
    dplyr::select(-IUCN) %>%
    rename(IUCN = IS_IUCN)

# Change the names of the columns of Urort_Skov from Owned to Urort_Skov and
# change numbers for actual names

Urort_Skov_DF <- data.frame(Owned = 0:(length(levels(Rast_Urort_Skov_Croped)[[1]]) -
    1), Urort_Skov = levels(Rast_Urort_Skov_Croped)[[1]])

Area2 <- Area2 %>%
    full_join(Urort_Skov_DF) %>%
    dplyr::select(-Owned)

# Change the names of the columns of stoette from Type to Stoette and change
# numbers for actual names

stoette_DF <- data.frame(Type = 0:(length(levels(Rast_stoette_Croped)[[1]]) - 1),
    Stoette = levels(Rast_stoette_Croped)[[1]])

Area2 <- Area2 %>%
    full_join(stoette_DF) %>%
    dplyr::select(-Type)

# Change the names of the columns of NaturaOgVildtreservater_DF from Temanavn
# to NaturaOgVildtreservater and change numbers for actual names

NaturaOgVildtreservater_DF <- data.frame(Temanavn = 0, NaturaOgVildtreservater = "Yes")

Area2 <- Area2 %>%
    full_join(NaturaOgVildtreservater_DF) %>%
    dplyr::select(-Temanavn)

# Change the names of the columns of Naturnationalparker from ID to
# Naturnationalparker and change numbers for actual names

Naturnationalparker_DF <- data.frame(ID = 0, Naturnationalparker = "Yes")

Area2 <- Area2 %>%
    full_join(Naturnationalparker_DF) %>%
    dplyr::select(-ID) %>%
    dplyr::relocate(Proportion, .after = everything()) %>%
    dplyr::relocate(Area_Sq_Mt, .after = everything())
```

</details>

This leads to the table <a href="#tab:table-overlap">3.1</a> wich can be
downloaded as an excel file
[here](https://github.com/Sustainscapes/BiodiversityCounsil/raw/master/Area_summary.xlsx)
an rds
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Area_summary.rds),
or as a csv
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Area_summary.csv)

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Long table of overlap

</summary>

| Habitats_P3 | Natura_2000 | Types_markblokkort | IUCN | Urort_Skov | Stoette         | NaturaOgVildtreservater | Naturnationalparker | Proportion | Area_Sq_Km |
|:------------|:------------|:-------------------|:-----|:-----------|:----------------|:------------------------|:--------------------|-----------:|-----------:|
| NA          | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |     51.376 | 22,165.914 |
| NA          | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      3.108 |  1,341.141 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      1.489 |    642.515 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      1.256 |    541.958 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      1.236 |    533.315 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      1.065 |    459.565 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.706 |    304.481 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.686 |    296.179 |
| NA          | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.621 |    267.866 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.550 |    237.459 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.463 |    199.692 |
| NA          | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.360 |    155.486 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.351 |    151.470 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.349 |    150.703 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.280 |    120.659 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.270 |    116.587 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.259 |    111.780 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.251 |    108.365 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.226 |     97.660 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.225 |     97.091 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.221 |     95.450 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.203 |     87.507 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.187 |     80.649 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.184 |     79.218 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.183 |     79.068 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.179 |     77.332 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.171 |     73.832 |
| Strandeng   | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.165 |     71.385 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.142 |     61.139 |
| NA          | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.133 |     57.222 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.130 |     55.988 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.125 |     54.060 |
| Strandeng   | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.104 |     45.075 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.102 |     44.210 |
| Klit        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.098 |     42.420 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.093 |     40.260 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.092 |     39.577 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.091 |     39.121 |
| NA          | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.086 |     37.072 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.079 |     34.287 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.079 |     34.066 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.078 |     33.771 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.075 |     32.261 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.074 |     31.948 |
| Strandeng   | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.067 |     29.077 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.065 |     28.159 |
| Strandeng   | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.060 |     25.757 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.057 |     24.666 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.056 |     24.369 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.055 |     23.553 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.053 |     22.844 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.053 |     22.810 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.052 |     22.238 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.046 |     19.954 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.043 |     18.541 |
| Strandeng   | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.042 |     18.172 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.041 |     17.878 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.041 |     17.777 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.041 |     17.520 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.038 |     16.353 |
| Sø          | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.037 |     16.119 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.037 |     15.750 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.036 |     15.740 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.033 |     14.117 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.032 |     13.768 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.029 |     12.439 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.028 |     12.253 |
| NA          | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.028 |     11.984 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.028 |     11.924 |
| Mose        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.028 |     11.916 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.027 |     11.809 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.026 |     11.123 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.026 |     11.067 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.025 |     10.922 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.025 |     10.811 |
| Sø          | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.025 |     10.747 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.023 |      9.776 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.021 |      9.265 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.021 |      9.248 |
| NA          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.021 |      9.106 |
| NA          | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.021 |      9.077 |
| Strandeng   | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.021 |      9.015 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.020 |      8.702 |
| NA          | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.020 |      8.597 |
| Mose        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.019 |      8.301 |
| NA          | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.019 |      8.244 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.019 |      8.045 |
| Overdrev    | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.018 |      7.875 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.018 |      7.612 |
| NA          | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.017 |      7.381 |
| Klit        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.017 |      7.281 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.016 |      7.021 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.015 |      6.586 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.014 |      6.192 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.014 |      6.013 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.014 |      5.992 |
| Overdrev    | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.014 |      5.985 |
| Sø          | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.014 |      5.975 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.013 |      5.601 |
| Hede        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.013 |      5.544 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.012 |      5.345 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.012 |      5.268 |
| Klit        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.012 |      5.121 |
| NA          | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.012 |      5.110 |
| Sø          | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.011 |      4.903 |
| Strandeng   | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.011 |      4.851 |
| NA          | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.011 |      4.699 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.010 |      4.484 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.010 |      4.296 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.010 |      4.241 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.010 |      4.195 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.009 |      3.849 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.009 |      3.699 |
| Overdrev    | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.008 |      3.653 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.008 |      3.579 |
| NA          | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.008 |      3.555 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.008 |      3.525 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.008 |      3.498 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.008 |      3.481 |
| NA          | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.008 |      3.407 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.008 |      3.297 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.008 |      3.289 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.007 |      3.177 |
| Hede        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.007 |      3.139 |
| Hede        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.007 |      3.098 |
| NA          | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.007 |      3.055 |
| NA          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.007 |      3.029 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.006 |      2.773 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.006 |      2.740 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.006 |      2.705 |
| Strandeng   | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.006 |      2.646 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.006 |      2.573 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.006 |      2.506 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.006 |      2.500 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.006 |      2.456 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.006 |      2.419 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.006 |      2.414 |
| NA          | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.005 |      2.331 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.005 |      2.283 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.005 |      2.283 |
| NA          | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.005 |      2.272 |
| NA          | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.005 |      2.266 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.005 |      2.247 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.005 |      2.206 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.005 |      2.183 |
| Hede        | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.005 |      2.166 |
| Sø          | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.005 |      2.152 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.005 |      2.127 |
| Mose        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.005 |      2.123 |
| Klit        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.005 |      2.070 |
| Eng         | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.005 |      2.064 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.005 |      2.061 |
| NA          | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.005 |      2.060 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.005 |      2.031 |
| Mose        | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.005 |      2.012 |
| Klit        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.005 |      2.006 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.005 |      1.982 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.005 |      1.961 |
| Overdrev    | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.005 |      1.943 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |      1.940 |
| Eng         | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |      1.913 |
| NA          | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.004 |      1.890 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.004 |      1.843 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.004 |      1.794 |
| Hede        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |      1.649 |
| Overdrev    | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |      1.619 |
| Overdrev    | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.004 |      1.602 |
| Mose        | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.004 |      1.549 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.004 |      1.522 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |      1.505 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.003 |      1.487 |
| Strandeng   | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |      1.452 |
| NA          | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.003 |      1.420 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |      1.405 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.003 |      1.354 |
| Overdrev    | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.003 |      1.319 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.003 |      1.305 |
| Mose        | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.003 |      1.159 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.003 |      1.114 |
| NA          | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.003 |      1.109 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |      1.104 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.003 |      1.092 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.002 |      1.060 |
| Overdrev    | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.002 |      1.045 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      1.002 |
| Eng         | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |      1.001 |
| NA          | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.002 |      0.971 |
| NA          | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      0.965 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.002 |      0.952 |
| NA          | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |      0.933 |
| Strandeng   | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.002 |      0.912 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      0.887 |
| Eng         | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.002 |      0.880 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      0.873 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      0.865 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.002 |      0.853 |
| NA          | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |      0.837 |
| Hede        | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.002 |      0.835 |
| Mose        | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.002 |      0.829 |
| NA          | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.002 |      0.817 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.002 |      0.789 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      0.776 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.002 |      0.766 |
| Hede        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.002 |      0.732 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.002 |      0.722 |
| Strandeng   | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.002 |      0.713 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      0.703 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.002 |      0.678 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.002 |      0.670 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      0.660 |
| Eng         | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.645 |
| Sø          | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.627 |
| Sø          | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      0.617 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.602 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.600 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.593 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.589 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.572 |
| NA          | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.572 |
| Klit        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.001 |      0.571 |
| Hede        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.561 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      0.551 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.543 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.533 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.532 |
| Klit        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.530 |
| Mose        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.524 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.523 |
| NA          | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      0.522 |
| Strandeng   | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.519 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.509 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.503 |
| Mose        | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.501 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.483 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.481 |
| Hede        | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.001 |      0.481 |
| NA          | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.477 |
| Overdrev    | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.471 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.468 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.466 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.457 |
| Sø          | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.432 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.001 |      0.431 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.428 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.001 |      0.428 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.424 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.414 |
| Hede        | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.001 |      0.405 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.405 |
| NA          | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.001 |      0.395 |
| Strandeng   | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      0.392 |
| Strandeng   | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.387 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      0.384 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.380 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.001 |      0.378 |
| NA          | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.001 |      0.374 |
| Hede        | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.001 |      0.368 |
| Hede        | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      0.366 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.365 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.001 |      0.364 |
| NA          | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      0.353 |
| Strandeng   | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.353 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.351 |
| Overdrev    | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.349 |
| Hede        | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.346 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      0.339 |
| NA          | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.338 |
| Strandeng   | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.338 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.332 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.330 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.326 |
| Overdrev    | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.001 |      0.313 |
| Mose        | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.312 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.311 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      0.308 |
| Klit        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.307 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      0.304 |
| Strandeng   | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.295 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      0.286 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.278 |
| Eng         | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      0.274 |
| Eng         | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.273 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.272 |
| NA          | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.001 |      0.265 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      0.262 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | Yes                     | Yes                 |      0.001 |      0.260 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      0.254 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      0.243 |
| Mose        | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      0.242 |
| NA          | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.001 |      0.233 |
| Mose        | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      0.231 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      0.230 |
| Eng         | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      0.218 |
| Hede        | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.001 |      0.218 |
| Mose        | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.001 |      0.216 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.214 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.213 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.211 |
| Hede        | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.210 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.209 |
| Overdrev    | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.206 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.206 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.205 |
| NA          | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.204 |
| Overdrev    | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.203 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.202 |
| Strandeng   | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.201 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.200 |
| Hede        | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.199 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.199 |
| Overdrev    | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.191 |
| Hede        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.189 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.189 |
| Mose        | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.182 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.174 |
| Strandeng   | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.172 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.172 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.170 |
| Eng         | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.163 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.161 |
| Overdrev    | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.151 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.150 |
| Hede        | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.148 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.148 |
| NA          | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.147 |
| Hede        | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.141 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.140 |
| NA          | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.137 |
| Hede        | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.136 |
| Eng         | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.127 |
| Overdrev    | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.126 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.124 |
| NA          | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.120 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.120 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.118 |
| NA          | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.118 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.117 |
| Hede        | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.116 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.114 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.111 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.111 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.110 |
| Strandeng   | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      0.108 |
| Strandeng   | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.107 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.105 |
| Mose        | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.102 |
| NA          | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.101 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.101 |
| NA          | NA          | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.100 |
| NA          | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.098 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.098 |
| Mose        | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.091 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.090 |
| Mose        | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.090 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.089 |
| Overdrev    | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.088 |
| Mose        | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.088 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.088 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.088 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.087 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.086 |
| NA          | NA          | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.086 |
| NA          | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.084 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.084 |
| Hede        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.084 |
| Klit        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.083 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.082 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.080 |
| NA          | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.078 |
| Eng         | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.076 |
| Sø          | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.075 |
| Eng         | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.075 |
| Mose        | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.074 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.074 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.073 |
| NA          | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.072 |
| Hede        | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.071 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.071 |
| Overdrev    | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.070 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.069 |
| Eng         | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.068 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.068 |
| Klit        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      0.067 |
| Overdrev    | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.066 |
| Hede        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.066 |
| Hede        | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.064 |
| Overdrev    | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.064 |
| Strandeng   | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.062 |
| Sø          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      0.062 |
| NA          | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.061 |
| Eng         | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.061 |
| Sø          | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.059 |
| Sø          | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.059 |
| NA          | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.059 |
| NA          | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.058 |
| Mose        | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.057 |
| Klit        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      0.056 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.056 |
| Mose        | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.055 |
| Eng         | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.055 |
| Hede        | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.054 |
| Strandeng   | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.054 |
| Strandeng   | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.054 |
| Eng         | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.053 |
| Eng         | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.052 |
| Sø          | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.052 |
| Klit        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.052 |
| NA          | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.051 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.051 |
| Mose        | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.049 |
| NA          | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.049 |
| Sø          | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.049 |
| Sø          | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.048 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | Yes                     | Yes                 |      0.000 |      0.047 |
| Hede        | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.047 |
| Strandeng   | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.046 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.045 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.045 |
| Overdrev    | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.043 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.043 |
| Mose        | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.043 |
| Strandeng   | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.042 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.042 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.042 |
| Klit        | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.039 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.038 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.038 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.038 |
| Mose        | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.037 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.036 |
| Sø          | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      0.036 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.035 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.035 |
| Sø          | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.035 |
| Eng         | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.034 |
| Mose        | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.034 |
| Strandeng   | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.033 |
| Mose        | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.033 |
| Overdrev    | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.033 |
| Strandeng   | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.032 |
| Eng         | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.032 |
| Sø          | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.031 |
| NA          | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.031 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.031 |
| Hede        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.030 |
| Mose        | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.030 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.029 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.029 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.028 |
| Strandeng   | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.028 |
| NA          | NA          | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.028 |
| Hede        | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.027 |
| Strandeng   | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.026 |
| NA          | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.026 |
| Strandeng   | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.026 |
| Mose        | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.026 |
| Hede        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.026 |
| Overdrev    | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.025 |
| NA          | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.025 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.024 |
| Hede        | NA          | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.024 |
| NA          | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.024 |
| Hede        | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.024 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.024 |
| Mose        | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.024 |
| NA          | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.023 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.023 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.022 |
| Mose        | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.021 |
| Hede        | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.021 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.021 |
| Hede        | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.021 |
| Strandeng   | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.021 |
| Strandeng   | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.020 |
| NA          | Yes         | NA                 | NA   | State      | NA              | Yes                     | Yes                 |      0.000 |      0.020 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.019 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.019 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.019 |
| Eng         | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.019 |
| NA          | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.019 |
| Eng         | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.018 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.018 |
| Sø          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.018 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.018 |
| Mose        | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.018 |
| Hede        | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.018 |
| Eng         | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.018 |
| Klit        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.018 |
| Overdrev    | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.017 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.017 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.016 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.016 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.016 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.016 |
| NA          | NA          | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.015 |
| Overdrev    | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.015 |
| Hede        | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.015 |
| NA          | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.014 |
| Eng         | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.014 |
| Klit        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.013 |
| Mose        | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.013 |
| Sø          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.013 |
| Hede        | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.013 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.013 |
| Sø          | Yes         | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.012 |
| NA          | Yes         | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.012 |
| Mose        | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.012 |
| Sø          | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.012 |
| Mose        | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.011 |
| Hede        | Yes         | NA                 | NA   | NA         | egekrat         | Yes                     | Yes                 |      0.000 |      0.011 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.010 |
| Klit        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.010 |
| Strandeng   | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.010 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.010 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.010 |
| Hede        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.010 |
| Mose        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.009 |
| NA          | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.009 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.009 |
| Eng         | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.008 |
| Overdrev    | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.008 |
| Eng         | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.008 |
| NA          | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.008 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.008 |
| Overdrev    | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.008 |
| NA          | NA          | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.008 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.007 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.007 |
| Overdrev    | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.007 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.007 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.007 |
| NA          | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.007 |
| Eng         | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.007 |
| Sø          | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.007 |
| Mose        | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.007 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.007 |
| Overdrev    | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.007 |
| NA          | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.006 |
| Overdrev    | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.006 |
| Sø          | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.006 |
| NA          | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.006 |
| Sø          | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.006 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.006 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.006 |
| Mose        | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.006 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.006 |
| Hede        | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.005 |
| Mose        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.005 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.005 |
| Hede        | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.005 |
| NA          | NA          | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.005 |
| Strandeng   | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.005 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.005 |
| Klit        | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.005 |
| NA          | Yes         | OMD                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.004 |
| Overdrev    | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.004 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.004 |
| Mose        | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.004 |
| Overdrev    | NA          | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.004 |
| Mose        | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.004 |
| Strandeng   | Yes         | OMD                | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.004 |
| Klit        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.004 |
| Strandeng   | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.004 |
| Overdrev    | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.004 |
| Mose        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.004 |
| Mose        | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.004 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.004 |
| NA          | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.004 |
| Sø          | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.004 |
| Overdrev    | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.004 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.004 |
| Sø          | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.004 |
| NA          | Yes         | NA                 | NA   | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.004 |
| NA          | NA          | NA                 | NA   | State      | Skov            | NA                      | NA                  |      0.000 |      0.004 |
| Overdrev    | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.004 |
| Sø          | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.004 |
| Mose        | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.003 |
| NA          | NA          | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.003 |
| Strandeng   | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.003 |
| NA          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |      0.003 |
| Eng         | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.003 |
| NA          | NA          | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.003 |
| NA          | Yes         | OMD                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.003 |
| NA          | NA          | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.003 |
| Eng         | Yes         | NA                 | NA   | State      | egekrat         | Yes                     | Yes                 |      0.000 |      0.003 |
| Hede        | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.003 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.003 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.003 |
| Hede        | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.003 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | Yes                     | Yes                 |      0.000 |      0.003 |
| Sø          | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.003 |
| NA          | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | Yes                 |      0.000 |      0.003 |
| Hede        | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.003 |
| NA          | NA          | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.003 |
| Overdrev    | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.003 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.003 |
| NA          | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | Yes                 |      0.000 |      0.002 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.002 |
| Strandeng   | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.002 |
| NA          | Yes         | PGR                | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.002 |
| Hede        | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.002 |
| Overdrev    | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Hede        | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.002 |
| Overdrev    | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.002 |
| Eng         | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Eng         | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.002 |
| Mose        | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.002 |
| NA          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |      0.002 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | Yes                     | Yes                 |      0.000 |      0.002 |
| NA          | NA          | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.002 |
| Overdrev    | NA          | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.002 |
| Sø          | NA          | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.002 |
| Hede        | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Sø          | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.002 |
| Hede        | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.002 |
| Mose        | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.002 |
| Overdrev    | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Mose        | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.002 |
| Eng         | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.002 |
| Overdrev    | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.002 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.002 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.002 |
| Mose        | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Eng         | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | Yes         | OMD                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Eng         | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Mose        | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Mose        | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| NA          | Yes         | OMD                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| NA          | NA          | OMD                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Strandeng   | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Sø          | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Sø          | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Mose        | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| NA          | NA          | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Sø          | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Hede        | Yes         | PGR                | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Hede        | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | Yes         | NA                 | Yes  | State      | NA              | Yes                     | Yes                 |      0.000 |      0.001 |
| Hede        | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.001 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.001 |
| Mose        | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.001 |
| Mose        | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.001 |
| NA          | NA          | PGR                | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Sø          | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | NA          | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Sø          | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.001 |
| NA          | Yes         | OMD                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Hede        | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.001 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Hede        | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Hede        | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Mose        | NA          | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |      0.001 |
| Eng         | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Mose        | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.001 |
| Sø          | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.001 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Sø          | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.001 |
| Overdrev    | NA          | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Sø          | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.000 |
| Sø          | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| NA          | NA          | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.000 |
| Eng         | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Mose        | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.000 |
| Overdrev    | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |      0.000 |
| NA          | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | Yes                 |      0.000 |      0.000 |
| Strandeng   | Yes         | NA                 | NA   | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.000 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.000 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Mose        | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      0.000 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Ukendt      | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.000 |
| Hede        | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | NA          | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |      0.000 |
| Sø          | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.000 |
| NA          | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Eng         | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Klit        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Mose        | Yes         | NA                 | NA   | Private    | Skov            | NA                      | Yes                 |      0.000 |      0.000 |
| Overdrev    | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Overdrev    | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | NA                 | Yes  | State      | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | PGR                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Sø          | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Eng         | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Sø          | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Overdrev    | Yes         | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | PGR                | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.000 |
| NA          | NA          | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Sø          | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Sø          | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | OMD                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |      0.000 |
| Overdrev    | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| Sø          | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | PGR                | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Overdrev    | NA          | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | OMD                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | PGR                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Sø          | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Eng         | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      0.000 |
| NA          | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| NA          | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Mose        | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | Yes                     | Yes                 |      0.000 |      0.000 |
| Eng         | NA          | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |      0.000 |
| Strandeng   | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      0.000 |
| NA          | Yes         | NA                 | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |      0.000 |
| Overdrev    | NA          | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Mose        | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Hede        | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      0.000 |
| Strandeng   | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Sø          | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      0.000 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      0.000 |
| Sø          | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      0.000 |

Table 3.1: Number of cells that are shared by different groups of
rastercells

</details>

From this we can generate a summary table with the general overlap as
follows:

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Code for summary of general overlap

</summary>

``` r
# eliminate cells that are not natura 2000 and separate overlapped and not
# overlapped cells and summarize

Area2 <- readRDS("Area_summary.rds")

Natura2000_Table1a <- Area2 %>%
    dplyr::filter(!is.na(Natura_2000)) %>%
    mutate(Overlaped = case_when(is.na(Habitats_P3) & is.na(Types_markblokkort) &
        is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(NaturaOgVildtreservater) &
        is.na(Naturnationalparker) ~ "No", TRUE ~ "Yes")) %>%
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

# eliminate cells that are not paragraph 3 and separate overlapped and not
# overlapped cells and summarize


Paragraph3_Table1a <- Area2 %>%
    dplyr::filter(!is.na(Habitats_P3)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(NaturaOgVildtreservater) &
        is.na(Naturnationalparker) ~ "No", TRUE ~ "Yes"), Paragrah3 = "yes") %>%
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

# eliminate cells that are not NaturaOgVildtreservater and separate overlapped
# and not overlapped cells and summarize

NaturaOgVildtreservater_Table1a <- Area2 %>%
    dplyr::filter(!is.na(NaturaOgVildtreservater)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(IUCN) & is.na(Urort_Skov) & is.na(Stoette) & is.na(Habitats_P3) & is.na(Naturnationalparker) ~
        "No", TRUE ~ "Yes")) %>%
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

NaturaOgVildtreservater_Table1 <- full_join(NaturaOgVildtreservater_Table1_Totals,
    NaturaOgVildtreservater_Table1_Appart) %>%
    relocate(Class, .before = everything())

# eliminate cells that are not IUCN and separate overlapped and not overlapped
# cells and summarize

IUCN_Table1a <- Area2 %>%
    dplyr::filter(!is.na(IUCN)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(NaturaOgVildtreservater) & is.na(Urort_Skov) & is.na(Stoette) & is.na(Habitats_P3) &
        is.na(Naturnationalparker) ~ "No", TRUE ~ "Yes")) %>%
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

# eliminate cells that are not Untouched forest and separate overlapped and not
# overlapped cells and summarize


Urort_Skov_Table1a <- Area2 %>%
    dplyr::filter(!is.na(Urort_Skov)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Stoette) & is.na(Habitats_P3) &
        is.na(Naturnationalparker) ~ "No", TRUE ~ "Yes")) %>%
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

# eliminate cells that are not subsidized areas and separate overlapped and not
# overlapped cells and summarize

Stoette_Table1a <- Area2 %>%
    dplyr::filter(!is.na(Stoette)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) &
        is.na(Naturnationalparker) ~ "No", TRUE ~ "Yes")) %>%
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

# eliminate cells that are not Naturnationalparker and separate overlapped and
# not overlapped cells and summarize

Naturnationalparker_Table1a <- Area2 %>%
    dplyr::filter(!is.na(Naturnationalparker)) %>%
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) &
        is.na(Stoette) ~ "No", TRUE ~ "Yes")) %>%
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
    mutate(Overlaped = case_when(is.na(Natura_2000) & is.na(Types_markblokkort) &
        is.na(NaturaOgVildtreservater) & is.na(IUCN) & is.na(Urort_Skov) & is.na(Habitats_P3) &
        is.na(Stoette) ~ "No", TRUE ~ "Yes")) %>%
    group_by(Naturnationalparker, Overlaped) %>%
    summarise_if(is.numeric, sum)

# put them all together

Table1 <- list(Natura2000_Table1, Paragraph3_Table1, NaturaOgVildtreservater_Table1,
    IUCN_Table1, Urort_Skov_Table1, Stoette_Table1, Naturnationalparker_Table1) %>%
    purrr::reduce(bind_rows)
```

</details>

This generates a summarized table shown in table
<a href="#tab:table-overlap-summarized">3.2</a>, this can be downloaded
as an rds
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Table1.rds),
or a csv
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Table1.csv)

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

General table of overlap

</summary>

| Class                   | Proportion | Area_Sq_Km | Area_Exclusive | Area_Overlapped |
|:------------------------|-----------:|-----------:|---------------:|----------------:|
| Total                   |     16.090 |  6,941.815 |             NA |              NA |
| Paragraph_3             |     10.516 |  4,537.104 |      1,588.231 |       2,948.872 |
| Nautra_2000             |      8.990 |  3,878.620 |        642.515 |       3,236.105 |
| IUCN                    |      2.531 |  1,091.784 |         97.660 |         994.124 |
| Urort_Skov              |      1.617 |    697.724 |        155.486 |         542.238 |
| NaturaOgVildtreservater |      1.019 |    439.605 |         11.984 |         427.622 |
| Naturnationalparker     |      0.596 |    257.301 |         57.222 |         200.079 |
| Stoette                 |      0.131 |     56.735 |          9.164 |          47.571 |

Table 3.2: Areas that are exclusive or overlapped between different
groups

</details>

To check the overlap between de different protected and potentially
protected layers we generate a new table with the following code

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Code for summary of specific overlaps

</summary>

``` r
# read in data

Area_summary <- readRDS("Area_summary.rds")

# Total

# Total area

All <- Area_summary %>%
    dplyr::filter(!is.na(Natura_2000) | !is.na(Habitats_P3) | !is.na(IUCN) | !is.na(Urort_Skov) |
        !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(Area = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All open habitats area

All_Open_Nature <- Area_summary %>%
    dplyr::filter(!is.na(Natura_2000) & (!is.na(Habitats_P3) | !is.na(IUCN) | !is.na(Urort_Skov) |
        !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker))) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(Open_Nature = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All in untouched forest

All_Urort_Skov <- Area_summary %>%
    dplyr::filter(!is.na(Urort_Skov) & (!is.na(Natura_2000) | !is.na(Habitats_P3) |
        !is.na(IUCN) | !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker))) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(Urort_Skov = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All in Permanent grasslands

All_PGR <- Area_summary %>%
    dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3) | !is.na(IUCN) | !is.na(Urort_Skov) |
        !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) &
        Types_markblokkort == "PGR" & is.na(Habitats_P3)) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(PGR = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All in Arable lands

All_OMD <- Area_summary %>%
    dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3) | !is.na(IUCN) | !is.na(Urort_Skov) |
        !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) &
        Types_markblokkort == "OMD" & is.na(Habitats_P3)) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(OMD = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# All in Lakes

All_SO <- Area_summary %>%
    dplyr::filter((!is.na(Natura_2000) | !is.na(Habitats_P3) | !is.na(IUCN) | !is.na(Urort_Skov) |
        !is.na(Stoette) | !is.na(NaturaOgVildtreservater) | !is.na(Naturnationalparker)) &
        Habitats_P3 == "Sø") %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(SO = Area_Sq_Km) %>%
    mutate(Nature_content = "All") %>%
    relocate(Nature_content, .before = everything())

# Joind toghether

Final_All <- list(All, All_Open_Nature, All_Urort_Skov, All_PGR, All_OMD, All_SO) %>%
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

Final_Natura_2000 <- list(Natura_2000, Natura_2000_Open_Nature, Natura_2000_Urort_Skov,
    Natura_2000_PGR, Natura_2000_OMD, Natura_2000_SO) %>%
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

# Join all

Final_Habitats_P3 <- list(Habitats_P3, Habitats_P3_Open_Nature, Habitats_P3_Urort_Skov,
    Habitats_P3_PGR, Habitats_P3_OMD, Habitats_P3_SO) %>%
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
    dplyr::filter(!is.na(NaturaOgVildtreservater) & Types_markblokkort == "PGR" &
        is.na(Habitats_P3)) %>%
    group_by(NaturaOgVildtreservater) %>%
    summarise(Area_Sq_Km = sum(Area_Sq_Km)) %>%
    rename(PGR = Area_Sq_Km) %>%
    mutate(Nature_content = "NaturaOgVildtreservater") %>%
    dplyr::select(-NaturaOgVildtreservater) %>%
    relocate(Nature_content, .before = everything())

# NaturaOgVildtreservater in Plough areas

NaturaOgVildtreservater_OMD <- Area_summary %>%
    dplyr::filter(!is.na(NaturaOgVildtreservater) & Types_markblokkort == "OMD" &
        is.na(Habitats_P3)) %>%
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

Final_NaturaOgVildtreservater <- list(NaturaOgVildtreservater, NaturaOgVildtreservater_Open_Nature,
    NaturaOgVildtreservater_Urort_Skov, NaturaOgVildtreservater_PGR, NaturaOgVildtreservater_OMD,
    NaturaOgVildtreservater_SO) %>%
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

Final_IUCN <- list(IUCN, IUCN_Open_Nature, IUCN_Urort_Skov, IUCN_PGR, IUCN_OMD, IUCN_SO) %>%
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

Final_Urort_Skov <- list(Urort_Skov, Urort_Skov_Open_Nature, Urort_Skov_Urort_Skov,
    Urort_Skov_PGR, Urort_Skov_OMD, Urort_Skov_SO) %>%
    purrr::reduce(full_join)

# Nationalparks


Naturnationalparker <- Area_summary %>%
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

Final_Naturnationalparker <- list(Naturnationalparker, Naturnationalparker_Open_Nature,
    Naturnationalparker_Urort_Skov, Naturnationalparker_PGR, Naturnationalparker_OMD,
    Naturnationalparker_SO) %>%
    purrr::reduce(full_join)

# Subsidies

Stoette <- Area_summary %>%
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

Final_Stoette <- list(Stoette, Stoette_Open_Nature, Stoette_Urort_Skov, Stoette_PGR,
    Stoette_OMD, Stoette_SO) %>%
    purrr::reduce(full_join)

# Get everything together and arrange by total area


Total <- list(Final_All, Final_Natura_2000, Final_Habitats_P3, Final_NaturaOgVildtreservater,
    Final_IUCN, Final_Urort_Skov, Final_Naturnationalparker, Final_Stoette) %>%
    purrr::reduce(bind_rows) %>%
    arrange(desc(Area))
```

</details>

The results of this code are seen in table
<a href="#tab:show-second-table">3.3</a>, and the table can be
downloaded as an rds
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Total.rds)
or as a csv
[here](https://github.com/Sustainscapes/BiodiversityCounsil/blob/master/Table1.csv)

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Specific table of overlap

</summary>

| Nature_content          |      Area | Open_Nature | Urort_Skov |     PGR |     OMD |      SO |
|:------------------------|----------:|------------:|-----------:|--------:|--------:|--------:|
| All                     | 6,941.815 |   2,586.204 |    524.917 | 188.546 | 674.165 | 698.996 |
| Paragraph 3 and klit    | 4,537.104 |   4,537.104 |    152.469 |   0.000 |   0.000 | 698.996 |
| Natura 2000             | 3,878.620 |   1,948.709 |    473.052 | 159.150 | 587.387 | 355.642 |
| IUCN                    | 1,091.784 |     698.271 |     85.394 |  43.180 | 117.655 | 100.437 |
| Urort_Skov              |   697.724 |     152.469 |    697.724 |  24.440 |  13.107 |  27.081 |
| NaturaOgVildtreservater |   439.605 |     313.375 |     27.536 |   8.019 |  10.167 |  71.268 |
| Naturnationalparker     |   257.301 |     106.569 |     65.392 |   8.737 |   2.232 |   6.244 |
| Stoette                 |    56.735 |      12.691 |     28.809 |   1.606 |   1.813 |   0.306 |

Table 3.3: Total area for protected areas and potential protected areas

</details>

# 4 Ocean ecosystems

## 4.1 Data generation

In this Section we will try to solve for intersections within the data
and incongruencies within the data, some of the polygons provided have
topological issues so we need to rasterize them in order to resolve this
issues easily.

### 4.1.1 Denmark’s Exclusive Economic Zone (EEZ)

To both crop the areas (some include ocean and terrestrial ecosystems)
and to have a value of the total area of the sea Denmark, we need to
have a polygon of the EEZ of the country. We used the marineregions
dataset version 3.0 using the mregions package (Institute 2020 ;
Chamberlain, Schepers, and Fernandez 2022) using the following code, the
resulting polygon is shown in figure
<a href="#fig:PlotSeaDenmark">4.1</a>

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Denmark EEZ

</summary>

``` r
SeaOfDenmark <- mregions::mr_shp("Denmark:eez") %>%
    dplyr::filter(geoname == "Danish Exclusive Economic Zone") %>%
    dplyr::select(geoname) %>%
    terra::vect() %>%
    terra::project(terra::crs(DK))

Area_Sea_DK <- expanse(SeaOfDenmark)
```

</details>

![Figure 4.1: Polygon of de EEZ of
Denmark](README_files/figure-gfm/PlotSeaDenmark-1.png)

The total area for Denmark according to that is 105,021 Square
kilometers

### 4.1.2 Natura 2000

This was already rasterized above, so we only need to mask it to sea and
save it

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Crop and save natura 2000 sea

</summary>

``` r
# read natura2000

Natura2000 <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000.tif")


# Write raw rasters to disk

Natura2000_Croped_Sea <- terra::mask(Natura2000, SeaOfDenmark)

# Write croped rasters to disk

writeRaster(Natura2000_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped_Sea.tif",
    overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TFW=YES", "of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_Natura2000_Croped_Sea.tif",
    destination = "RasterizedCOG/Rast_Natura2000_Croped_Sea.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure
<a href="#fig:PlotNatura2000Sea">4.2</a>

![Figure 4.2: Plot of the areas of Natura 2000 within the sea of
Denmark](README_files/figure-gfm/PlotNatura2000Sea-1.png)

### 4.1.3 Nature and wildlife reservations

This was already rasterized above, so we only need to mask it to sea and
save it

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Nature and wildlife reservations

</summary>

``` r
# read NaturaOgVildtreservater

NaturaOgVildtreservater <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater.tif")


# Write raw rasters to disk

NaturaOgVildtreservater_Croped_Sea <- terra::mask(NaturaOgVildtreservater, SeaOfDenmark)

# Write croped rasters to disk

writeRaster(NaturaOgVildtreservater_Croped_Sea, "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped_Sea.tif",
    overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TFW=YES", "of=COG"))


# save as cloud optimized rasters

sf::gdal_utils("warp", source = "O:/Nat_BDR-data/Arealanalyse/CLEAN/Rasterized/Rast_NaturaOgVildtreservater_Croped_Sea.tif",
    destination = "RasterizedCOG/Rast_NaturaOgVildtreservater_Croped_Sea.tif", options = c("-of",
        "COG", "-co", "RESAMPLING=NEAREST", "-co", "TILING_SCHEME=GoogleMapsCompatible",
        "-co", "COMPRESS=DEFLATE", "-co", "NUM_THREADS=46"))
```

</details>

the results can be seen in figure
<a href="#fig:PlotNaturaOgVildtreservaterSea">4.3</a>

![Figure 4.3: Plot of the areas of Natura 2000 within the sea of
Denmark](README_files/figure-gfm/PlotNaturaOgVildtreservaterSea-1.png)

# 5 Session info

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Session info

</summary>

``` r
sessioninfo::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value
#>  version  R version 4.1.2 (2021-11-01)
#>  os       Windows Server 2012 R2 x64 (build 9600)
#>  system   x86_64, mingw32
#>  ui       RTerm
#>  language en
#>  collate  Danish_Denmark.1252
#>  ctype    Danish_Denmark.1252
#>  tz       Europe/Paris
#>  date     2022-06-16
#>  pandoc   2.14.0.3 @ C:/Program Files/RStudio/bin/pandoc/ (via rmarkdown)
#> 
#> - Packages -------------------------------------------------------------------
#>  package     * version date (UTC) lib source
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.1.2)
#>  backports     1.4.1   2021-12-13 [1] CRAN (R 4.1.2)
#>  bit           4.0.4   2020-08-04 [1] CRAN (R 4.1.2)
#>  bit64         4.0.5   2020-08-30 [1] CRAN (R 4.1.2)
#>  bookdown      0.26    2022-04-15 [1] CRAN (R 4.1.3)
#>  broom         0.7.11  2022-01-03 [1] CRAN (R 4.1.2)
#>  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.1.2)
#>  class         7.3-19  2021-05-03 [2] CRAN (R 4.1.2)
#>  classInt      0.4-3   2020-04-07 [1] CRAN (R 4.1.2)
#>  cli           3.1.0   2021-10-27 [1] CRAN (R 4.1.2)
#>  codetools     0.2-18  2020-11-04 [2] CRAN (R 4.1.2)
#>  colorspace    2.0-2   2021-06-24 [1] CRAN (R 4.1.2)
#>  crayon        1.5.1   2022-03-26 [1] CRAN (R 4.1.3)
#>  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.1.2)
#>  dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.1.2)
#>  digest        0.6.29  2021-12-01 [1] CRAN (R 4.1.2)
#>  dplyr       * 1.0.7   2021-06-18 [1] CRAN (R 4.1.2)
#>  e1071         1.7-9   2021-09-16 [1] CRAN (R 4.1.2)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.2)
#>  evaluate      0.15    2022-02-18 [1] CRAN (R 4.1.3)
#>  fansi         0.5.0   2021-05-25 [1] CRAN (R 4.1.2)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.1.2)
#>  forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.1.2)
#>  formatR       1.12    2022-03-31 [1] CRAN (R 4.1.3)
#>  fs            1.5.2   2021-12-08 [1] CRAN (R 4.1.2)
#>  generics      0.1.1   2021-10-25 [1] CRAN (R 4.1.2)
#>  geodata     * 0.4-6   2022-04-09 [1] CRAN (R 4.1.3)
#>  ggplot2     * 3.3.5   2021-06-25 [1] CRAN (R 4.1.2)
#>  glue          1.5.1   2021-11-30 [1] CRAN (R 4.1.2)
#>  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.1.2)
#>  haven         2.4.3   2021-08-04 [1] CRAN (R 4.1.2)
#>  highr         0.9     2021-04-16 [1] CRAN (R 4.1.2)
#>  hms           1.1.1   2021-09-26 [1] CRAN (R 4.1.2)
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.1.2)
#>  httr          1.4.3   2022-05-04 [1] CRAN (R 4.1.3)
#>  jsonlite      1.8.0   2022-02-22 [1] CRAN (R 4.1.3)
#>  KernSmooth    2.23-20 2021-05-03 [2] CRAN (R 4.1.2)
#>  knitr         1.39    2022-04-26 [1] CRAN (R 4.1.3)
#>  lattice       0.20-45 2021-09-22 [2] CRAN (R 4.1.2)
#>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.2)
#>  lubridate     1.8.0   2021-10-07 [1] CRAN (R 4.1.2)
#>  magrittr    * 2.0.1   2020-11-17 [1] CRAN (R 4.1.2)
#>  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.1.2)
#>  mregions    * 0.1.8   2022-06-15 [1] Github (ropensci/mregions@26f40ba)
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.1.2)
#>  pillar        1.7.0   2022-02-01 [1] CRAN (R 4.1.3)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
#>  proxy         0.4-26  2021-06-07 [1] CRAN (R 4.1.2)
#>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.1.2)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
#>  rappdirs      0.3.3   2021-01-31 [1] CRAN (R 4.1.2)
#>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.1.2)
#>  readr       * 2.1.1   2021-11-30 [1] CRAN (R 4.1.2)
#>  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.1.2)
#>  reprex        2.0.1   2021-08-05 [1] CRAN (R 4.1.3)
#>  rgdal         1.5-28  2021-12-15 [1] CRAN (R 4.1.2)
#>  rlang         1.0.2   2022-03-04 [1] CRAN (R 4.1.3)
#>  rmarkdown     2.14    2022-04-25 [1] CRAN (R 4.1.3)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.2)
#>  rvest         1.0.2   2021-10-16 [1] CRAN (R 4.1.2)
#>  scales        1.1.1   2020-05-11 [1] CRAN (R 4.1.2)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
#>  sf          * 1.0-4   2021-11-14 [1] CRAN (R 4.1.2)
#>  sp            1.5-0   2022-06-05 [1] CRAN (R 4.1.3)
#>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
#>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.1.2)
#>  terra       * 1.5-35  2022-05-18 [1] https://rspatial.r-universe.dev (R 4.1.3)
#>  tibble      * 3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
#>  tidyr       * 1.1.4   2021-09-27 [1] CRAN (R 4.1.2)
#>  tidyselect    1.1.2   2022-02-21 [1] CRAN (R 4.1.3)
#>  tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.1.2)
#>  tzdb          0.2.0   2021-10-27 [1] CRAN (R 4.1.2)
#>  units         0.7-2   2021-06-08 [1] CRAN (R 4.1.2)
#>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
#>  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.2)
#>  vroom         1.5.7   2021-11-30 [1] CRAN (R 4.1.2)
#>  withr         2.5.0   2022-03-03 [1] CRAN (R 4.1.3)
#>  xfun          0.29    2021-12-14 [1] CRAN (R 4.1.2)
#>  xml2          1.3.3   2021-11-30 [1] CRAN (R 4.1.2)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.1.1)
#> 
#>  [1] C:/Users/au687614/Documents/R/win-library/4.1
#>  [2] C:/Program Files/R/R-4.1.2/library
#> 
#> ------------------------------------------------------------------------------
```

</details>

# 6 References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Milton2020" class="csl-entry">

Bache, Stefan Milton, and Hadley Wickham. 2020. *Magrittr: A
Forward-Pipe Operator for r*.
<https://CRAN.R-project.org/package=magrittr>.

</div>

<div id="ref-Chamberlain2022" class="csl-entry">

Chamberlain, Scott, Lennert Schepers, and Salvador Fernandez. 2022.
*Mregions: Marine Regions Data from ’Marineregions.org’*.

</div>

<div id="ref-global2022gadm" class="csl-entry">

Global Administrative Areas, G. 2022. “GADM Database of Global
Administrative Areas, Version 4.0.” *University of Berkeley, Museum of
Vertebrate Zoology and the International Rice Research Institute (CA)*.

</div>

<div id="ref-Hijmans2022" class="csl-entry">

Hijmans, Robert J. 2022. *Terra: Spatial Data Analysis*.
<https://rspatial.org/terra/>.

</div>

<div id="ref-Hijmans2021" class="csl-entry">

Hijmans, Robert J., Aniruddha Ghosh, and Alex Mandel. 2022. *Geodata:
Download Geographic Data*. <https://CRAN.R-project.org/package=geodata>.

</div>

<div id="ref-flanders2014union" class="csl-entry">

Institute, Flanders Marine. 2020. “Union of the ESRI Country Shapefile
and the Exclusive Economic Zones (Version
3).”[ https://www.marineregions.org/]( https://www.marineregions.org/).

</div>

<div id="ref-miljoportalP3" class="csl-entry">

miljoportal. 2022. “Danmarks Miljøportal Paragraph 3 Naturetypes.”
[https://arealdata-api.miljoeportal.dk/download/dai/BES_NATURTYPER_SHAPE.zip
](https://arealdata-api.miljoeportal.dk/download/dai/BES_NATURTYPER_SHAPE.zip
).

</div>

<div id="ref-Pebesma2018" class="csl-entry">

Pebesma, Edzer. 2018. “<span class="nocase">Simple Features for R:
Standardized Support for Spatial Vector Data</span>.” *The R Journal* 10
(1): 439–46. <https://doi.org/10.32614/RJ-2018-009>.

</div>

<div id="ref-Cran2021" class="csl-entry">

R Core Team. 2021. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

</div>
