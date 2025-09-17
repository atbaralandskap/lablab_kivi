
library(sf)
library(dplyr)
library(mapview)
library(raster)

vastra_skane <- raster("C:/Users/henri/r/skane/data/61_3_2023.tif")
ostra_skane  <- raster("C:/Users/henri/r/skane/data/61_4_2023.tif")
r            <- merge(vastra_skane, ostra_skane)

brks <- c(-Inf, seq(0,10,1), seq(15,50,5), seq(60,200,10), Inf)
cols <- colorRampPalette(c("#08306b","#41b6c4","#a1dab4","#ffffcc",
                           "#fdae61","#f46d43","#d73027"))(length(brks)-1)

top  <- ceiling(raster::cellStats(r, "max", na.rm = TRUE) / 10) * 10
levs <- c(1, 2, seq(10, top, by = 10))
cont <- rasterToContour(r, levels = levs)

# andrees hus SWEREF99 TM (nord, öst)
# 6168972.49, 444025.867

taghusa <- st_sf(
  text = "Här bor jag",
  geometry = st_sfc(st_point(c(444025, 6168972)), crs = 3006)
)





mapview(r, at = brks, col.regions = cols, alpha.regions = 0.7)+
  mapview(cont, zcol = "level", color = "black", lwd = 1)+
  mapview(
    taghusa,
    col.regions = "blue",   # fyllnadsfärg
    color = "blue",         # kantfärg
    pch = 16,               # fylld cirkel
    cex = 12,               # storlek
    alpha = 1,
    popup = taghusa$text    # popup-text
  )
  



