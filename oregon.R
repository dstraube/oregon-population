######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# This program generates a data visualization of Oregon population by county.

setwd("~/R/GADM")
rm(list=ls())

library(dplyr)
library(foreign)
library(maptools)
library(mapproj)
library(ggplot2)

source("./utils.R", echo = TRUE)

# Shapefiles for United States were downloaded from the Global Administrative Areas
# web site at: http://www.gadm.org. Shapefiles have three levels of detail (0/1/2)
# with level 2 containing county boundaries. 
usa.shp <- readShapePoly("./USA_adm_shp/USA_adm2.shp")
usa.map <- fortify(usa.shp)
usa.dbf <- read.dbf("./USA_adm_shp/USA_adm2.dbf")

# "id" in .shp ranges from 0..N while "ID_2" in .dbf ranges from 1..N+1.
# Convert factor to numeric and increment to correctly correspond to .dbf.
usa.map$id <- as.integer(as.character(usa.map$id)) + 1

# Reduce various data frames to just Oregon counties.
or.ids <- unique(usa.dbf[usa.dbf$NAME_1 == "Oregon", "ID_2"])
or.map <- usa.map[usa.map$id %in% or.ids,]
or.dbf <- usa.dbf[usa.dbf$NAME_1 == "Oregon" & usa.dbf$TYPE_2 == "County",]

# Oregon county population estimates downloaded from the United States Census Bureau
# web site at: http://www.census.gov/popest/data/counties/asrh/2014/index.html.
# AGEGRP == 0 is total population for a county and YEAR == 7 is 2014 estimate.
# Also truncate " County" from all county names.
tmp <- read.csv("./census/CC-EST2014-ALLDATA-41.csv")
or.census <- tmp[tmp$AGEGRP == 0 & tmp$YEAR == 7, 1:10]
or.census$CTYNAME <- sub("* County", "", or.census$CTYNAME)

# Perform a few sanity checks with regards to county names and FIPS codes.
stopifnot(setequal(or.census$CTYNAME, or.dbf$NAME_2))
stopifnot(setequal(or.map$id, or.dbf$ID_2))

# Add column to or.map so we can fill by population - or.dbf and or.census have one row per county.
or.dbf <- or.dbf[order(or.dbf$NAME_2),]
or.census <- or.census[order(or.census$CTYNAME),]
stopifnot(or.dbf$NAME_2 == or.census$CTYNAME)
lookup <- data.frame(id = or.dbf$ID_2,
                     Population = or.census$TOT_POP)
or.map <- left_join(or.map, lookup, by = "id")
or.map$Population <- cut(or.map$Population,
                         breaks = c(0, 10000, 25000, 50000, 100000, 500000, 1000000),
                         labels = c("< 10,000", "10,000 - 25,000", "25,000 - 50,000",
                                    "50,000 - 100,000", "100,000 - 500,000", "> 500,000"))

# US county area data downloaded from the United States Census Bureau
# web site at: http://www.census.gov/geo/maps-data/data/gazetteer2014.html.
# Reduce to Oregon only, strip " County" suffix, and verify.
tmp <- read.table("./census/2014_Gaz_counties_national.txt", header = TRUE, sep = "\t")
or.area <- tmp[tmp$USPS == "OR",]
or.area$NAME <- sub("* County", "", as.character(or.area$NAME))
stopifnot(setequal(or.census$CTYNAME, or.area$NAME))

# Compute people per square mile for each county.
colnames(or.area)[which(colnames(or.area) == "NAME")] <- "CTYNAME"
lookup <- data.frame(CTYNAME = or.census$CTYNAME,
                     TOT_POP = or.census$TOT_POP,
                     stringsAsFactors = FALSE)
or.area <- left_join(or.area, lookup, by = "CTYNAME")
or.area$density <- sapply(or.area$TOT_POP / or.area$ALAND_SQMI, pp, digits = 1)

# Generate plot of Oregon counties color coded by population and annotated with
# county name and population density.
or.plot <- ggplot() +
    geom_polygon(data = or.map,
                 aes(x = long, y = lat, group = group, fill = Population),
                 colour = "black") +
    # Palette colors generated at http://colorbrewer2.org.
    scale_fill_manual(values =c ("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6")) +
    coord_map() +
    theme_bw() +
    geom_text(data = or.area,
              fontface = "bold", size = 2, color = "red",
              aes(x = INTPTLONG, y = INTPTLAT,
                  hjust = 0.5, vjust = 0.5,
                  label = paste(CTYNAME, "\n(", density, ")", sep = ''))) +
    ggtitle(paste("Oregon Counties\nTotal population:",
                  pp(sum(or.census$TOT_POP), digits = 0),
                  "(2014 est.)",
                  "\n(annotated with people / square mile)")) +
    labs(x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(lineheight = .8, face = "bold"))
or.plot

svg("./oregon.svg", width = 9, height = 7)
or.plot
dev.off()

