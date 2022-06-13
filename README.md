Dataset generation for the Danish Biodiversity council
================
Derek Corcoran
14/06, 2022

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

The total area for Denmark according to that is 43,144,848,183 Square
meters

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
#> [1] "2022-06-13 22:34:39 CEST"
Rast_IUCN <- terra::rasterize(IUCN_Aggregated, Template, field = "IUCN")
Sys.time()
#> [1] "2022-06-13 22:35:51 CEST"

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

National_Parks_Aggregated <- terra::aggregate(National_Parks, by = "ID")

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

# Save dataframe as rds

saveRDS(Area2, "Area_summary.rds")
```

</details>

This leads to the table <a href="#tab:table-overlap">3.1</a>

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Long table of overlap

</summary>

| Habitats_P3 | Natura_2000 | Types_markblokkort | IUCN | Urort_Skov | Stoette         | NaturaOgVildtreservater | Naturnationalparker | Proportion |  Area_Sq_Mt |
|:------------|:------------|:-------------------|:-----|:-----------|:----------------|:------------------------|:--------------------|-----------:|------------:|
| NA          | Yes         | OMD                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |        4500 |
| Eng         | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |       17700 |
| Mose        | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |         400 |
| NA          | Yes         | PGR                | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |        6200 |
| Eng         | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |        8400 |
| Mose        | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.001 |      215900 |
| Sø          | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.000 |       59300 |
| NA          | Yes         | NA                 | Yes  | Private    | Skov            | Yes                     | NA                  |      0.001 |      373700 |
| Eng         | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |         300 |
| Mose        | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        1200 |
| NA          | Yes         | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        8700 |
| Eng         | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       54600 |
| Hede        | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        2200 |
| Mose        | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       33800 |
| Overdrev    | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |         400 |
| NA          | Yes         | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        6400 |
| Eng         | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       76400 |
| Hede        | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       71000 |
| Mose        | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.005 |     2012000 |
| Overdrev    | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       66100 |
| Sø          | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       47900 |
| NA          | Yes         | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.007 |     3054900 |
| NA          | NA          | OMD                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        3000 |
| Overdrev    | NA          | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |         500 |
| NA          | NA          | PGR                | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |        3200 |
| Mose        | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |       55400 |
| Overdrev    | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |         900 |
| Sø          | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | NA                 | Yes  | Private    | Skov            | NA                      | NA                  |      0.001 |      264600 |
| NA          | Yes         | OMD                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |         100 |
| Eng         | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |         900 |
| Mose        | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |         800 |
| Sø          | Yes         | PGR                | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |         500 |
| Eng         | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |         300 |
| Mose        | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |       49100 |
| Sø          | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |       34700 |
| NA          | Yes         | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |       51300 |
| Mose        | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        1100 |
| Strandeng   | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        2400 |
| Sø          | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         500 |
| NA          | Yes         | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       23200 |
| Eng         | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        1100 |
| Hede        | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         300 |
| Mose        | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       11800 |
| Overdrev    | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         200 |
| Strandeng   | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        9700 |
| Sø          | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         700 |
| NA          | Yes         | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       14200 |
| Eng         | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       13600 |
| Hede        | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        5100 |
| Mose        | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.003 |     1159500 |
| Overdrev    | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       17100 |
| Strandeng   | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       20400 |
| Sø          | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       48600 |
| NA          | Yes         | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.012 |     5110500 |
| NA          | NA          | NA                 | NA   | Private    | Skov            | Yes                     | NA                  |      0.000 |        3300 |
| Mose        | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         900 |
| Overdrev    | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         100 |
| Strandeng   | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | OMD                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       48800 |
| Eng         | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       60700 |
| Hede        | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         200 |
| Mose        | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       91400 |
| Overdrev    | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |        7200 |
| Strandeng   | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         100 |
| Sø          | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | PGR                | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       18600 |
| Eng         | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       75300 |
| Hede        | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.001 |      368100 |
| Mose        | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.004 |     1548700 |
| Overdrev    | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       69700 |
| Strandeng   | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       25900 |
| Sø          | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.000 |       59100 |
| NA          | NA          | NA                 | NA   | Private    | Skov            | NA                      | NA                  |      0.020 |     8596600 |
| NA          | Yes         | PGR                | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |         100 |
| Eng         | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |         200 |
| Overdrev    | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |         200 |
| NA          | Yes         | NA                 | Yes  | Private    | egekrat         | NA                      | NA                  |      0.000 |      120100 |
| Hede        | Yes         | PGR                | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |        1000 |
| NA          | Yes         | PGR                | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |        2400 |
| Eng         | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |         100 |
| Hede        | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |        5400 |
| NA          | Yes         | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |      146600 |
| NA          | NA          | NA                 | NA   | Private    | egekrat         | NA                      | NA                  |      0.000 |        2600 |
| NA          | Yes         | NA                 | Yes  | State      | Skov            | NA                      | NA                  |      0.000 |         200 |
| NA          | NA          | NA                 | NA   | State      | Skov            | NA                      | NA                  |      0.000 |        3500 |
| NA          | Yes         | OMD                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |         700 |
| Eng         | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        2100 |
| Hede        | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |       35700 |
| Mose        | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |         300 |
| Overdrev    | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        6100 |
| NA          | Yes         | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |      106700 |
| Eng         | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        1300 |
| Hede        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |       56000 |
| Mose        | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        9800 |
| Overdrev    | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        6100 |
| Sø          | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        6700 |
| NA          | Yes         | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.005 |     2177600 |
| Overdrev    | NA          | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |        1200 |
| NA          | NA          | PGR                | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |         500 |
| Overdrev    | NA          | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | NA                 | Yes  | State      | egekrat         | NA                      | NA                  |      0.000 |       28100 |
| Eng         | Yes         | NA                 | NA   | State      | egekrat         | Yes                     | Yes                 |      0.000 |        3000 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | Yes                     | Yes                 |      0.000 |       47200 |
| Hede        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |       83600 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |        5900 |
| Hede        | NA          | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |         300 |
| NA          | NA          | NA                 | NA   | State      | egekrat         | NA                      | Yes                 |      0.000 |        7600 |
| NA          | Yes         | OMD                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        1200 |
| Eng         | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |         200 |
| Hede        | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        1000 |
| Mose        | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        1600 |
| Overdrev    | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        8400 |
| NA          | Yes         | PGR                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        3700 |
| Eng         | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |         500 |
| Hede        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      189400 |
| Mose        | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        4000 |
| Overdrev    | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |       25300 |
| NA          | Yes         | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.003 |     1091600 |
| NA          | NA          | OMD                | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        1200 |
| Hede        | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |      210200 |
| Mose        | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.000 |        4200 |
| NA          | NA          | NA                 | NA   | State      | egekrat         | NA                      | NA                  |      0.001 |      394900 |
| NA          | Yes         | OMD                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| Eng         | Yes         | PGR                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| NA          | Yes         | PGR                | Yes  | State      | sammenhaengende | NA                      | NA                  |      0.000 |         200 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |        2100 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | NA                      | Yes                 |      0.000 |       17800 |
| Eng         | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       34400 |
| Mose        | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       37000 |
| Strandeng   | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       57700 |
| Sø          | Yes         | OMD                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        3600 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      461800 |
| Hede        | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        2400 |
| Mose        | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      101900 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.028 |    12276200 |
| Sø          | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      617100 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      553700 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |      136600 |
| Hede        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        9500 |
| Klit        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       17600 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.001 |      253600 |
| Overdrev    | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        1000 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.006 |     2435400 |
| Sø          | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.005 |     2151800 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.008 |     3528700 |
| Eng         | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      162900 |
| Hede        | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       54100 |
| Mose        | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       25800 |
| Overdrev    | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      350400 |
| Strandeng   | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       20700 |
| Sø          | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |         200 |
| NA          | Yes         | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      341300 |
| Eng         | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.006 |     2714100 |
| Hede        | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.007 |     3000900 |
| Mose        | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      591100 |
| Overdrev    | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.008 |     3262300 |
| Strandeng   | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      541200 |
| Sø          | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       87500 |
| NA          | Yes         | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.010 |     4465900 |
| Eng         | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.003 |     1438500 |
| Hede        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.008 |     3544600 |
| Klit        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      530300 |
| Mose        | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.008 |     3336400 |
| Overdrev    | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.004 |     1915200 |
| Strandeng   | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      525000 |
| Sø          | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.011 |     4933600 |
| NA          | Yes         | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.031 |    13292800 |
| Eng         | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        8400 |
| Mose        | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |         200 |
| Overdrev    | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |         700 |
| Strandeng   | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        1200 |
| Sø          | NA          | PGR                | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |         100 |
| Eng         | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        1600 |
| Mose        | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       11400 |
| Overdrev    | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        1600 |
| Strandeng   | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       62500 |
| Sø          | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |        1200 |
| NA          | NA          | NA                 | Yes  | State      | NA              | Yes                     | NA                  |      0.000 |       24600 |
| Eng         | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       18500 |
| Hede        | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |         700 |
| Mose        | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       12900 |
| Overdrev    | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      151000 |
| Sø          | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |         300 |
| NA          | NA          | OMD                | Yes  | State      | NA              | NA                      | NA                  |      0.002 |      971300 |
| Eng         | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      274500 |
| Hede        | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |        1000 |
| Mose        | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       57000 |
| Overdrev    | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.002 |     1044900 |
| Strandeng   | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |        3300 |
| Sø          | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.000 |        3700 |
| NA          | NA          | PGR                | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      478200 |
| Eng         | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      127100 |
| Hede        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       65500 |
| Klit        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |       82700 |
| Mose        | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      524900 |
| Overdrev    | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.000 |      206300 |
| Strandeng   | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      338000 |
| Sø          | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.001 |      433500 |
| NA          | NA          | NA                 | Yes  | State      | NA              | NA                      | NA                  |      0.008 |     3640700 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | Yes                     | Yes                 |      0.000 |        1900 |
| NA          | Yes         | NA                 | NA   | State      | NA              | Yes                     | Yes                 |      0.000 |       19100 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      113600 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |        1800 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |       79800 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |         600 |
| NA          | Yes         | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      187900 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      196100 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      113600 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |       21500 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |       93400 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |        5500 |
| NA          | Yes         | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      617100 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |      194400 |
| Hede        | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      248600 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.004 |     1772600 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |       35500 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.001 |      406400 |
| NA          | Yes         | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.050 |    21775900 |
| NA          | NA          | OMD                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |         100 |
| Eng         | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |         100 |
| Mose        | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |         100 |
| NA          | NA          | PGR                | NA   | State      | NA              | NA                      | Yes                 |      0.000 |        4800 |
| Hede        | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |        1300 |
| Mose        | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |         200 |
| NA          | NA          | NA                 | NA   | State      | NA              | NA                      | Yes                 |      0.000 |       31200 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      208700 |
| Hede        | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |        3000 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       24500 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       21700 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |        1000 |
| NA          | Yes         | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         100 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       38100 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |        1300 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      139800 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |        6000 |
| Strandeng   | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      392300 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       15500 |
| NA          | Yes         | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.002 |      817200 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.002 |      721600 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      169800 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       41600 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      341500 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |      171700 |
| NA          | Yes         | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.001 |      522800 |
| Eng         | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      364900 |
| Hede        | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       46700 |
| Mose        | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      463600 |
| Overdrev    | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      426200 |
| Strandeng   | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      106700 |
| Sø          | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |        9700 |
| NA          | Yes         | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.006 |     2436600 |
| Eng         | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.013 |     5788400 |
| Hede        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.003 |     1508800 |
| Klit        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       13400 |
| Mose        | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.004 |     1786800 |
| Overdrev    | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.016 |     6751500 |
| Strandeng   | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.002 |      912000 |
| Sø          | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      123400 |
| NA          | Yes         | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.021 |     9029500 |
| Eng         | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.005 |     2053800 |
| Hede        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |     1766000 |
| Klit        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      307000 |
| Mose        | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.057 |    24770200 |
| Overdrev    | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.011 |     4595900 |
| Strandeng   | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      598400 |
| Sø          | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.027 |    11820400 |
| NA          | Yes         | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.656 |   283161400 |
| Eng         | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         200 |
| Hede        | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         700 |
| NA          | NA          | OMD                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         100 |
| Eng         | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         500 |
| Strandeng   | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       42100 |
| NA          | NA          | PGR                | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         300 |
| Eng         | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         300 |
| Mose        | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |        5700 |
| Overdrev    | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       63600 |
| Strandeng   | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       33400 |
| Sø          | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |         200 |
| NA          | NA          | NA                 | NA   | State      | NA              | Yes                     | NA                  |      0.000 |       58700 |
| Eng         | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.001 |      218100 |
| Hede        | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       63700 |
| Mose        | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       23500 |
| Overdrev    | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      190700 |
| Strandeng   | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       31600 |
| Sø          | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.000 |        2700 |
| NA          | NA          | OMD                | NA   | State      | NA              | NA                      | NA                  |      0.021 |     9078200 |
| Eng         | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.005 |     2064100 |
| Hede        | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      138900 |
| Mose        | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.002 |      830500 |
| Overdrev    | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.008 |     3652600 |
| Strandeng   | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |      200700 |
| Sø          | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.000 |       51800 |
| NA          | NA          | PGR                | NA   | State      | NA              | NA                      | NA                  |      0.019 |     8311400 |
| Eng         | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |     1912900 |
| Hede        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.002 |      708000 |
| Klit        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.000 |         300 |
| Mose        | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.019 |     8301600 |
| Overdrev    | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.004 |     1619400 |
| Strandeng   | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.001 |      387000 |
| Sø          | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.014 |     5981000 |
| NA          | NA          | NA                 | NA   | State      | NA              | NA                      | NA                  |      0.371 |   160153600 |
| Eng         | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        1300 |
| Hede        | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       20800 |
| Mose        | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         100 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         500 |
| NA          | Yes         | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       30800 |
| Eng         | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         600 |
| Hede        | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       23800 |
| Mose        | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        6700 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       15900 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        2900 |
| Sø          | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         800 |
| NA          | Yes         | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.001 |      232700 |
| Eng         | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        2100 |
| Hede        | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      198800 |
| Mose        | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       88100 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       35000 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         200 |
| Sø          | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       11700 |
| NA          | Yes         | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.008 |     3409400 |
| Hede        | NA          | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       24300 |
| NA          | NA          | OMD                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        4800 |
| Eng         | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         300 |
| Hede        | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         300 |
| Overdrev    | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         700 |
| NA          | NA          | PGR                | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        6800 |
| Eng         | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |         300 |
| Hede        | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |      140800 |
| Mose        | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |       74500 |
| Overdrev    | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.000 |        2400 |
| NA          | NA          | NA                 | Yes  | NA         | egekrat         | NA                      | NA                  |      0.003 |     1109100 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | Yes                     | Yes                 |      0.000 |         100 |
| Hede        | Yes         | NA                 | NA   | NA         | egekrat         | Yes                     | Yes                 |      0.000 |       11200 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |         400 |
| NA          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |        3300 |
| Eng         | NA          | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |         100 |
| NA          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | Yes                 |      0.000 |        1400 |
| Overdrev    | Yes         | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |         200 |
| NA          | Yes         | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       11900 |
| Eng         | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        3300 |
| Hede        | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       27400 |
| Mose        | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        4000 |
| Overdrev    | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        2600 |
| NA          | Yes         | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       24000 |
| Eng         | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        9600 |
| Hede        | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.001 |      217800 |
| Mose        | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       32900 |
| Overdrev    | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       32900 |
| Strandeng   | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |         100 |
| Sø          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       12700 |
| NA          | Yes         | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.007 |     3028500 |
| Eng         | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |         700 |
| Hede        | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        1700 |
| Mose        | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |         600 |
| NA          | NA          | OMD                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      100600 |
| Eng         | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        6800 |
| Hede        | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       12500 |
| Mose        | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        1300 |
| Overdrev    | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |         900 |
| Sø          | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |        1100 |
| NA          | NA          | PGR                | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       26000 |
| Eng         | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       18900 |
| Hede        | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.001 |      480600 |
| Mose        | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |      181600 |
| Overdrev    | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       14900 |
| Sø          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.000 |       18200 |
| NA          | NA          | NA                 | NA   | NA         | egekrat         | NA                      | NA                  |      0.021 |     9106600 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |        4500 |
| NA          | Yes         | PGR                | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |         200 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |        2500 |
| NA          | Yes         | NA                 | Yes  | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |         100 |
| Mose        | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1400 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1200 |
| NA          | Yes         | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      574800 |
| Eng         | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       51900 |
| Mose        | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       42700 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       41800 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        2900 |
| NA          | Yes         | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       84300 |
| Eng         | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1200 |
| Hede        | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         300 |
| Mose        | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       18100 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       18200 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1200 |
| NA          | Yes         | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       78000 |
| Overdrev    | NA          | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | OMD                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       99800 |
| Overdrev    | NA          | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1800 |
| NA          | NA          | PGR                | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         200 |
| Overdrev    | NA          | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        4400 |
| NA          | NA          | NA                 | Yes  | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       15100 |
| Strandeng   | Yes         | NA                 | NA   | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |         400 |
| NA          | Yes         | NA                 | NA   | NA         | sammenhaengende | Yes                     | NA                  |      0.000 |        3600 |
| Eng         | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      273200 |
| Hede        | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         700 |
| Mose        | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       30300 |
| Overdrev    | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      125600 |
| Strandeng   | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       46500 |
| NA          | Yes         | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |      837000 |
| Eng         | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |     1001000 |
| Hede        | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      115800 |
| Mose        | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      242300 |
| Overdrev    | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      203100 |
| Strandeng   | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      295100 |
| Sø          | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1700 |
| NA          | Yes         | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.002 |      932600 |
| Eng         | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      645400 |
| Hede        | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       17900 |
| Mose        | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.001 |      311900 |
| Overdrev    | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       88400 |
| Strandeng   | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       54000 |
| Sø          | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        1200 |
| NA          | Yes         | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.005 |     2331300 |
| Eng         | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       31600 |
| Mose        | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| Overdrev    | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        7900 |
| Strandeng   | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| NA          | NA          | OMD                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       61400 |
| Eng         | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       53000 |
| Hede        | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        2700 |
| Mose        | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        3400 |
| Overdrev    | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       43200 |
| Strandeng   | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         200 |
| NA          | NA          | PGR                | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |      137300 |
| Eng         | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       68300 |
| Hede        | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         100 |
| Mose        | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        4500 |
| Overdrev    | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |        3700 |
| Sø          | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |         600 |
| NA          | NA          | NA                 | NA   | NA         | sammenhaengende | NA                      | NA                  |      0.000 |       58200 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      245400 |
| Hede        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.002 |      729800 |
| Klit        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |        4100 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |         600 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       83100 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.004 |     1790400 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.005 |     2179100 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       70900 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      168400 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |        1600 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      286600 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      479800 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.007 |     2859200 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      561700 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      106700 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |      112400 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.001 |      263700 |
| Eng         | NA          | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |         700 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |         200 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       72500 |
| Hede        | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |        1400 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |        1800 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |        4600 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       27800 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       60900 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | NA                      | Yes                 |      0.000 |       13200 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      173800 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |       93700 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      483200 |
| Strandeng   | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |     1451900 |
| Sø          | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        6200 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      865200 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      394700 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.005 |     2244000 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |     1505400 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.005 |     2192500 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.003 |     1467800 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.075 |    32260900 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      232800 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      712400 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.002 |      821100 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.044 |    18771000 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.013 |     5700200 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.011 |     4943500 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      600600 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.023 |     9776000 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.030 |    12965200 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.022 |     9532700 |
| Eng         | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.006 |     2722300 |
| Hede        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.007 |     3100800 |
| Klit        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |       56300 |
| Mose        | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.008 |     3276600 |
| Overdrev    | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.005 |     2062200 |
| Strandeng   | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.006 |     2646100 |
| Sø          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |       61800 |
| NA          | Yes         | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.094 |    40643700 |
| Eng         | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.075 |    32284500 |
| Hede        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.103 |    44537300 |
| Klit        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.008 |     3288700 |
| Mose        | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.026 |    11087400 |
| Overdrev    | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.028 |    12013300 |
| Strandeng   | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.037 |    15750500 |
| Sø          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      481000 |
| NA          | Yes         | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.042 |    17920900 |
| Eng         | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.033 |    14161100 |
| Hede        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.240 |   103496800 |
| Klit        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.080 |    34314700 |
| Mose        | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.211 |    90979600 |
| Overdrev    | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.021 |     9255900 |
| Strandeng   | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.032 |    13770600 |
| Sø          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.142 |    61431100 |
| NA          | Yes         | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.199 |    85826100 |
| Mose        | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         500 |
| Strandeng   | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         100 |
| Sø          | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         100 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      160600 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         500 |
| Mose        | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        1700 |
| Overdrev    | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        1300 |
| Strandeng   | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        4700 |
| Sø          | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         200 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | Yes                     | NA                  |      0.001 |      332000 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |         600 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |       43300 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |       84100 |
| Mose        | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        5400 |
| Overdrev    | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        3500 |
| Strandeng   | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |      172200 |
| Sø          | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.000 |        3500 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | Yes                     | NA                  |      0.005 |     2246600 |
| Eng         | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.002 |      880200 |
| Hede        | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      346200 |
| Mose        | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      231300 |
| Overdrev    | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      471000 |
| Strandeng   | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |      107700 |
| Sø          | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |       36100 |
| NA          | NA          | OMD                | Yes  | NA         | NA              | NA                      | NA                  |      0.171 |    73862600 |
| Eng         | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.014 |     6191900 |
| Hede        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.013 |     5544300 |
| Klit        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.000 |       67100 |
| Mose        | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.005 |     2123000 |
| Overdrev    | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.018 |     7881500 |
| Strandeng   | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      518800 |
| Sø          | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.001 |      626900 |
| NA          | NA          | PGR                | Yes  | NA         | NA              | NA                      | NA                  |      0.042 |    17980700 |
| Eng         | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.009 |     3853100 |
| Hede        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.028 |    11878400 |
| Klit        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.008 |     3323300 |
| Mose        | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.028 |    11937600 |
| Overdrev    | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.014 |     5991300 |
| Strandeng   | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.002 |      713100 |
| Sø          | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.037 |    16119200 |
| NA          | NA          | NA                 | Yes  | NA         | NA              | NA                      | NA                  |      0.240 |   103659500 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         200 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |       36300 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         200 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |        1500 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      105200 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |        4100 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      204900 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         200 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         100 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |      124500 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |       68600 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.008 |     3482300 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |       18500 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |       21100 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |        5700 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.001 |      425500 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      192900 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      347900 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |         600 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       19400 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      229600 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      815800 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.003 |     1128400 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      137600 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      508900 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        3900 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      938300 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      527300 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.020 |     8562800 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |         100 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      453300 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       42000 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.005 |     2345400 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.024 |    10185700 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         300 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |         400 |
| NA          | NA          | NA                 | NA   | NA         | NA              | Yes                     | Yes                 |      0.000 |        1900 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      506100 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        5800 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        4400 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       80300 |
| NA          | NA          | OMD                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      564500 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       42900 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.001 |      464900 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        6700 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |      118400 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        1500 |
| NA          | NA          | PGR                | NA   | NA         | NA              | NA                      | Yes                 |      0.004 |     1609000 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       74800 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.004 |     1595100 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.002 |      723300 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |        6900 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.000 |       52400 |
| NA          | NA          | NA                 | NA   | NA         | NA              | NA                      | Yes                 |      0.056 |    24235400 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      600000 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |         800 |
| Klit        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       10100 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      271500 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |        3900 |
| Strandeng   | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.021 |     9015300 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       56000 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.018 |     7611600 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.020 |     8702200 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.004 |     1794200 |
| Klit        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.012 |     5121100 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.005 |     1960600 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      380600 |
| Strandeng   | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.067 |    29077300 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      325600 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.012 |     5268000 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.006 |     2773300 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      259500 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.056 |    24371200 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.038 |    16353000 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      428200 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.079 |    34066000 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.125 |    54059800 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.184 |    79220700 |
| Eng         | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.053 |    22817200 |
| Hede        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.010 |     4198300 |
| Klit        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.001 |      570600 |
| Mose        | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.012 |     5348700 |
| Overdrev    | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.008 |     3524700 |
| Strandeng   | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.042 |    18171900 |
| Sø          | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.002 |      678500 |
| NA          | Yes         | OMD                | NA   | NA         | NA              | NA                      | NA                  |      1.236 |   533389900 |
| Eng         | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.259 |   111837600 |
| Hede        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.084 |    36123400 |
| Klit        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.017 |     7319200 |
| Mose        | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.092 |    39643600 |
| Overdrev    | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.055 |    23577500 |
| Strandeng   | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.165 |    71388700 |
| Sø          | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.006 |     2519800 |
| NA          | Yes         | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.271 |   117002100 |
| Eng         | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.130 |    55992200 |
| Hede        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.370 |   159660000 |
| Klit        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.059 |    25552500 |
| Mose        | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.353 |   152130700 |
| Overdrev    | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.065 |    28162100 |
| Strandeng   | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.187 |    80665200 |
| Sø          | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.463 |   199761000 |
| NA          | Yes         | NA                 | NA   | NA         | NA              | NA                      | NA                  |      1.517 |   654566600 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |         400 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |        1100 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |         200 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       10400 |
| Strandeng   | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       26200 |
| Sø          | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |         600 |
| NA          | NA          | OMD                | NA   | NA         | NA              | Yes                     | NA                  |      0.003 |     1419900 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       87800 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      310900 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      149800 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       73500 |
| Strandeng   | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       28400 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       12500 |
| NA          | NA          | PGR                | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      204500 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       38500 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       35900 |
| Klit        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       51500 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |      198700 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.000 |       28800 |
| Strandeng   | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      352800 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.001 |      543200 |
| NA          | NA          | NA                 | NA   | NA         | NA              | Yes                     | NA                  |      0.028 |    11983600 |
| Eng         | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.179 |    77334400 |
| Hede        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.015 |     6588200 |
| Klit        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.005 |     2005600 |
| Mose        | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.046 |    19978200 |
| Overdrev    | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.036 |    15740300 |
| Strandeng   | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.011 |     4850900 |
| Sø          | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |      0.025 |    10747100 |
| NA          | NA          | OMD                | NA   | NA         | NA              | NA                      | NA                  |     51.377 | 22166314800 |
| Eng         | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      1.065 |   459573400 |
| Hede        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.092 |    39579600 |
| Klit        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.005 |     2070100 |
| Mose        | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.221 |    95466400 |
| Overdrev    | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.251 |   108364700 |
| Strandeng   | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.060 |    25757200 |
| Sø          | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      0.029 |    12444200 |
| NA          | NA          | PGR                | NA   | NA         | NA              | NA                      | NA                  |      3.109 |  1341422000 |
| Eng         | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.550 |   237474700 |
| Hede        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.707 |   305092500 |
| Klit        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.098 |    42425000 |
| Mose        | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      1.257 |   542236200 |
| Overdrev    | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.280 |   120658800 |
| Strandeng   | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.104 |    45074600 |
| Sø          | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.687 |   296212600 |
| Ukendt      | NA          | NA                 | NA   | NA         | NA              | NA                      | NA                  |      0.000 |         400 |

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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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
    tidyr::pivot_wider(names_from = Overlaped, values_from = Area_Sq_Mt) %>%
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

# Save table 1

readr::write_csv(Table1, "Table1.csv")
saveRDS(Table1, "Table1.rds")
```

</details>

This generates a summarized table shown in table
<a href="#tab:table-overlap-summarized">3.2</a>

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>

Long table of overlap

</summary>

| Class                   | Proportion | Area_Sq_Mt | Area_Exclusive | Area_Overlapped | Naturnationalparker | Overlaped |
|:------------------------|-----------:|-----------:|---------------:|----------------:|:--------------------|:----------|
| Nautra_2000             |      8.990 | 3878619900 |      654566600 |      3224053300 | NA                  | NA        |
| Paragraph_3             |     10.516 | 4537103500 |     1589174800 |      2947928700 | NA                  | NA        |
| NaturaOgVildtreservater |      1.019 |  439605300 |       11983600 |       427621700 | NA                  | NA        |
| IUCN                    |      2.531 | 1091783700 |      103659500 |       988124200 | NA                  | NA        |
| Urort_Skov              |      1.617 |  697724000 |      160153600 |       537570400 | NA                  | NA        |
| Stoette                 |      0.131 |   56735000 |        9164800 |        47570200 | NA                  | NA        |
| Naturnationalparker     |      0.225 |   97267500 |       24235400 |        73032100 | NA                  | NA        |
| NA                      |      0.056 |   24235400 |             NA |              NA | Yes                 | No        |
| NA                      |      0.169 |   73032100 |             NA |              NA | Yes                 | Yes       |

Table 3.2: Areas that are exclusive or overlapped between different
groups

</details>

# 4 Ocean ecosystems

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
#>  date     2022-06-14
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
#>  crayon        1.4.2   2021-10-29 [1] CRAN (R 4.1.2)
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
#>  httr          1.4.2   2020-07-20 [1] CRAN (R 4.1.2)
#>  jsonlite      1.8.0   2022-02-22 [1] CRAN (R 4.1.3)
#>  KernSmooth    2.23-20 2021-05-03 [2] CRAN (R 4.1.2)
#>  knitr         1.39    2022-04-26 [1] CRAN (R 4.1.3)
#>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.2)
#>  lubridate     1.8.0   2021-10-07 [1] CRAN (R 4.1.2)
#>  magrittr    * 2.0.1   2020-11-17 [1] CRAN (R 4.1.2)
#>  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.1.2)
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.1.2)
#>  pillar        1.6.4   2021-10-18 [1] CRAN (R 4.1.2)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
#>  proxy         0.4-26  2021-06-07 [1] CRAN (R 4.1.2)
#>  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.1.2)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
#>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.1.2)
#>  readr       * 2.1.1   2021-11-30 [1] CRAN (R 4.1.2)
#>  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.1.2)
#>  reprex        2.0.1   2021-08-05 [1] CRAN (R 4.1.3)
#>  rlang         0.4.12  2021-10-18 [1] CRAN (R 4.1.2)
#>  rmarkdown     2.14    2022-04-25 [1] CRAN (R 4.1.3)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.2)
#>  rvest         1.0.2   2021-10-16 [1] CRAN (R 4.1.2)
#>  scales        1.1.1   2020-05-11 [1] CRAN (R 4.1.2)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
#>  sf          * 1.0-4   2021-11-14 [1] CRAN (R 4.1.2)
#>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
#>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.1.2)
#>  terra       * 1.5-35  2022-05-18 [1] https://rspatial.r-universe.dev (R 4.1.3)
#>  tibble      * 3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
#>  tidyr       * 1.1.4   2021-09-27 [1] CRAN (R 4.1.2)
#>  tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.1.2)
#>  tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.1.2)
#>  tzdb          0.2.0   2021-10-27 [1] CRAN (R 4.1.2)
#>  units         0.7-2   2021-06-08 [1] CRAN (R 4.1.2)
#>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
#>  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.2)
#>  vroom         1.5.7   2021-11-30 [1] CRAN (R 4.1.2)
#>  withr         2.4.3   2021-11-30 [1] CRAN (R 4.1.2)
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
