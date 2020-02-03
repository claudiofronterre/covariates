# LOAD ALL PACKAGES NEEDED FOR THE ANALYSIS ------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("sf", "tmap")
pacman::p_load(pkgs, character.only = T)

# LOAD THE DATASETS ------------------------------------------------------------
malaria <- readr::read_csv("data/TZ_2015.csv")

# convert the data frame to a sf object (spatial object)
# 4326 is the EPSG code for lat long WGS84 
malaria_sp <- malaria %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# load the administrative boundaries of Tanzania
tz <- st_read("data/geodata/gadm36_TZA.gpkg", layer = "gadm36_TZA_0")
africa <- st_read("data/geodata/africa.gpkg")

# OBSERVED PREVALENCE MAP ------------------------------------------------------

# Generate new column with prevalence on % scale
malaria_sp$prev100 <- (malaria_sp$Pf / malaria_sp$Ex) * 100

# Change the order for plotting
malaria_sp <- malaria_sp[order(malaria_sp$prev100), ]

# Define color palette for plotting
pal <- tmaptools::get_brewer_pal("-RdYlBu", n = 8, contrast = c(0, 1), plot = F)

# Define breaks 
brks <- round(c(0, quantile(malaria_sp$prev100[malaria_sp$prev100 > 0],
                      probs = seq(0, 1, l = 8))), 0)
brks[length(brks)] <- 100

# Function to create labels
create_labels <- function(x) {
  n <- length(x)
  # breaks <- round(as.numeric(x), digits = digits)
  labs <- paste(x[1:(n - 1)], x[2:(n)], sep = " - ")
  labs[length(labs)] <- paste(">", x[n - 1])
  return(labs)
}

# Generate labels for legend
labs <- create_labels(brks)

# Generate the maps
tm_shape(tz, is.master = T) +
  tm_polygons(col = "white", border.col = "black") +
tm_shape(malaria_sp) +
  tm_dots(col = "prev100", shape = 21, border.col = "black",
             palette = pal, size = .15, border.lwd = .7,
             style = "fixed", breaks = brks, legend.show = F) +
  tm_add_legend(type = "symbol",
                labels = labs,
                shape = 21, col = pal,
                border.col = "black",
                size = .5, alpha = 1, 
                title = "P.falciparum\nprevalence (%)") +
tm_compass(position = c("right", "top")) +
tm_scale_bar(position = c("left", 0.03), text.size = 0.7) +
tm_layout(design.mode = F, legend.bg.color = "white",  
          legend.position = c("left", "top"), frame = F,
          inner.margins = c(0.01, 0.15, 0.01, 0),
          outer.margins = 0, asp = 0) 

# Save the map
tmap_save(filename = "figs/figure1_prevmap.pdf", width = 7, height = 6.25)  


# Version with Africa in background --------------------------------------------
tm_shape(africa, filter = africa$COUNTRY == "Tanzania", bbox = tz,
         ext = 1.3) +
  tm_polygons(col = "white", colorNULL = "grey90", border.col = "black", ) +
tm_shape(malaria_sp) +
  tm_symbols(col = "prev100", shape = 21, border.col = "black",
             palette = pal, size = .1, border.lwd = .7,
             style = "fixed", breaks = brks, legend.col.show = F) +
  tm_add_legend(type = "symbol",
                labels = labs,
                shape = 21, col = pal,
                border.col = "black",
                size = .6, alpha = 1, 
                title = "P.falciparum\nprevalence (%)") +
tm_compass(position = c("left", "top")) +
tm_scale_bar(position = c("right", "BOTTOM"), text.size = 0.7) +
tm_layout(design.mode = F, legend.bg.color = "white",  
          legend.position = c("left", "bottom"), legend.frame = T,
          bg.color = "lightblue",
          outer.margins = 0, asp = 0) 

tmap_save(filename = "figs/figure1_prevmap_africa.pdf", width = 6)  

# Version with COUNTRY background ----------------------------------------------