# Monica Rajendiran (mkr2ff)
# KDE HW - SYS 6018

library("rgdal")
library("tools")
library("dplyr")
library("ggplot2")
library("readr")
library("raster")
library("MASS")
library("RColorBrewer")
library("kedd")

# Kernel Density Estimation for Severe (> 30%) Coral Bleaching
# in the Great Barrier Reef

# Coastline, River, and Coral data obtained from:
# http://www.naturalearthdata.com/downloads/10m-physical-vectors/

# Coral bleaching data obtained from:
# http://www.reefbase.org/gis_maps/datasets.aspx

# Read in coastline data:
path_land <- ("Data/ne_10m_coastline")
name_land <- "ne_10m_coastline.shp"
data_land <- readOGR(dsn = path_land, 
                     layer = file_path_sans_ext(name_land))

# Read in Australian river data:
path_water <- ("Data/ne_10m_rivers_lake_centerlines")
name_water <- "ne_10m_rivers_lake_centerlines.shp"
data_water <- readOGR(dsn = path_water, 
                     layer = file_path_sans_ext(name_water))

# Read in coral mapping data:
path_coral <- ("Data/ne_10m_reefs")
name_coral <- "ne_10m_reefs.shp"
data_coral <- readOGR(dsn = path_coral, 
                     layer = file_path_sans_ext(name_coral))

# Read in coral bleaching data:
data_bleach <- read_csv("Data/CoralBleaching.csv")
data_bleach_gbr <- filter(data_bleach, grepl("Great Barrier Reef", COUNTRY) | grepl("Great Barrier Reef", LOCATION), SEVERITY_CODE==3)
data_bleach_coord <- as.data.frame(data_bleach_gbr[,c("LON","LAT")])

# 2D KDE for Coral Bleaching in GBR
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)

bleach_kde <- kde2d(data_bleach_coord[,1], data_bleach_coord[,2], h=1, n=c(100,100))  # h=1
kde_img <- image(bleach_kde, col = heat.colors, useRaster=TRUE, asp=1)

# Mapping the KDE for the severe bleaching in the Great Barrier Reef (GBR)

# Create coordinate boundaries for GBR coastline, rivers, and corals:
xlims <- c(140, 155)
ylims <- c(-25, -10)

# KDE Maps with Manually Selected Bandwidths (before optimization): 0.1, 0.5, 2.5, 5, 10, 100 

gbr_map1 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 0.1) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 0.1", x = "Longitude", y = "Latitude"))
gbr_map1


gbr_map2 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 0.5) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.03, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 0.5", x = "Longitude", y = "Latitude"))
gbr_map2


gbr_map3 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 2.5) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 2.5", x = "Longitude", y = "Latitude"))
gbr_map3

gbr_map4 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 5) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 5", x = "Longitude", y = "Latitude"))
gbr_map4


gbr_map5 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 10) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 10", x = "Longitude", y = "Latitude"))
gbr_map5


gbr_map6 <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = 100) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 100", x = "Longitude", y = "Latitude"))
gbr_map6


# KDE Bandwidth Selection 
# There are various methods of finding a best bandwidth for multivariate KDEs. I have chosen to 
# compare KDEs and their respective bandwidths through the following means:

X <- data_bleach_coord[,1] # Longitude coordinates for coral bleaching sites
Y <- data_bleach_coord[,2] # Latitude coordinates for coral bleaching sites

# AMISE
h.amise(X) 
# AMISE = 0.001027966;	Bandwidth 'h' = 1.356846
h.amise(Y)
# AMISE = 0.0008120521;	Bandwidth 'h' = 1.634059

plot(h.amise(X))
plot(h.amise(Y))

# BCV
h.bcv(X, deriv.order = 0)
# Min BCV = 0.003274971;	Bandwidth 'h' = 1.358127
h.bcv(Y, deriv.order = 0)
# Min BCV = 0.001998941;	Bandwidth 'h' = 1.321787

plot(h.bcv(X, deriv.order = 0))
plot(h.bcv(Y, deriv.order = 0))

# Best overall bandwidth for KDE taken via average of all 'optimal' bandwidths produced

opt_h <- (1.356846 + 1.634059 + 1.358127 + 1.321787) / 4

# KDE image with Bandwidth 1.4
bleach_kde <- kde2d(data_bleach_coord[,1], data_bleach_coord[,2], h=opt_h, n=c(100,100))  # h=1
kde_img <- image(bleach_kde, col = heat.colors, useRaster=TRUE, asp=1)

# GBR Map with Bandwidth 1.4
gbr_map_h <- ggplot(data_bleach_coord, aes(LON, LAT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE, h = opt_h) +
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Severe coral\nbleaching\ndensity")) +
  geom_path(data = data_land, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.5) + 
  geom_path(data = data_water, aes(x = long, y = lat, group = group), 
            color = "blue", size = 0.08) + 
  geom_point(data = data_coral, aes(x = long, y = lat, group = group), 
             color = "#161616", size = 0.02, shape = 3) +
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "Severe Coral Bleaching (>30%) in the Great Barrier Reef\n Bandwidth 1.4", x = "Longitude", y = "Latitude"))
gbr_map_h

