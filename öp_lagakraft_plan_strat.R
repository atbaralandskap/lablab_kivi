library(sf)
library(tidyverse)
library(readxl)
library(mapview)
library(raster)
library(purrr)

xls <- "C:/Users/henri/data/tidsserie-op-laga-kraft-och-planeringsstrategi2.xlsx"

view(xls)

yrs_pat <- "^År\\s?\\d{4}$"

# ---- Boverket: ÖP, år (senaste år med laga kraft) ----


df <- read_excel(xls, sheet = "ÖP, år", skip = 1) %>%
  mutate(Kommun = str_trim(Kommun))

yr_cols <- grep(yrs_pat, names(df), value = TRUE)

view(df)

op_ar <- df %>%
  mutate(
    across(all_of(yr_cols), ~ as.integer(stringr::str_extract(as.character(.), "\\d{4}"))),
    laga_kraft = reduce(as.list(pick(all_of(rev(yr_cols)))), coalesce)
  ) %>%
  summarise(laga_kraft = suppressWarnings(max(laga_kraft, na.rm = TRUE)), .by = Kommun) %>%
  mutate(laga_kraft = ifelse(is.infinite(laga_kraft), NA_integer_, laga_kraft))


# ---- Boverket: Ändringar (summa) ----
andringar <- read_excel(xls, sheet = "Ändringar av ÖP", skip = 1) %>%
  transmute(
    Kommun = str_trim(Kommun),
    andringar_sum = rowSums(across(matches(yrs_pat), ~ suppressWarnings(as.numeric(.))), na.rm = TRUE)
  ) %>%
  summarise(andringar_sum = sum(andringar_sum, na.rm = TRUE), .by = Kommun)

# ---- Boverket: Tillägg (summa) ----
tillagg <- read_excel(xls, sheet = "Tillägg till ÖP", skip = 1) %>%
  transmute(
    Kommun = str_trim(Kommun),
    tillagg_sum = rowSums(across(matches(yrs_pat), ~ suppressWarnings(as.numeric(.))), na.rm = TRUE)
  ) %>%
  summarise(tillagg_sum = sum(tillagg_sum, na.rm = TRUE), .by = Kommun)

# ---- Boverket: Planeringsstrategi (finns + första år) ----
planstrat <- read_excel(xls, sheet = "Planeringsstrategi", skip = 1) %>%
  transmute(Kommun = str_trim(Kommun), across(matches(yrs_pat), ~ as.integer(.))) %>%
  pivot_longer(-Kommun, names_to = "ar", values_to = "v") %>%
  mutate(year = readr::parse_number(ar)) %>%
  summarise(
    ps_finns = as.integer(any(v == 1, na.rm = TRUE)),
    ps_forsta_ar = if (any(v == 1, na.rm = TRUE)) min(year[v == 1], na.rm = TRUE) else NA_integer_,
    .by = Kommun
  )

# ---- Join ----
boverket <- list(op_ar, andringar, tillagg, planstrat) %>% reduce(full_join, by = "Kommun")

# ---- Geometrier ----
deso <- st_read("C:/Users/henri/r/test_sf/DESO_2018_v2.gpkg", quiet = TRUE)

kommun <- deso %>% group_by(kommun, kommunnamn) %>% summarise(.groups = "drop")
lan    <- deso %>% group_by(lan) %>% summarise(.groups = "drop")

kommun2 <- kommun %>%
  mutate(kommunnamn = str_trim(kommunnamn)) %>%
  left_join(boverket, by = c("kommunnamn" = "Kommun")) %>%
  mutate(
    andringar = cut(andringar_sum, c(-Inf, 0, 2, 5, Inf), labels = c("0", "1–2", "3–5", "6+")),
    tillagg   = cut(tillagg_sum,   c(-Inf, 0, 1, 3, Inf), labels = c("0", "1", "2–3", "4+")),
    planeringsstrategi = if_else(ps_finns == 1, "PS:Ja", "PS:Nej", missing = "PS:Okänt")
  )

# ----- Färg för årtal (en färg/år) -----
pal_fun <- colorRampPalette(c("darkred", "#d73027", "#fee08b", "#1a9850"))
yrs <- kommun2$laga_kraft
brks_kom <- seq(min(yrs, na.rm = TRUE) - 1, max(yrs, na.rm = TRUE) + 1, by = 1)

# ----- Höjddata + strandlinjer -----
vastra_skane <- raster("C:/Users/henri/r/skane/data/61_3_2023.tif")
ostra_skane  <- raster("C:/Users/henri/r/skane/data/61_4_2023.tif")
hojddata     <- merge(vastra_skane, ostra_skane)

brks <- c(-Inf, seq(0, 10, 1), seq(15, 50, 5), seq(60, 200, 10), Inf)
cols <- colorRampPalette(c("#08306b", "#41b6c4", "#a1dab4", "#ffffcc", "#fdae61", "#f46d43", "#d73027"))(length(brks) - 1)

#top  <- ceiling(raster::cellStats(hojddata, "max", na.rm = TRUE) / 10) * 10
#levs <- c(1, 2, seq(10, top, by = 10))
#strandlinje <- rasterToContour(hojddata, levels = levs) %>% st_as_sf()

levs <- c(1, 2, 3, 65)
strand_all <- rasterToContour(hojddata, levels = levs) |> sf::st_as_sf()
strand_all$level <- as.numeric(strand_all$level)

strand_1  <- dplyr::filter(strand_all, level == 1)
strand_2  <- dplyr::filter(strand_all, level == 2)
strand_3  <- dplyr::filter(strand_all, level == 3) 
strand_65 <- dplyr::filter(strand_all, level == 65)

mapview(strand_1, zcol = "level", color = "green", lwd = 1, legend = FALSE, hide = TRUE)+
  mapview(strand_2, zcol = "level", color = "darkorange", lwd = 1, legend = FALSE, hide = TRUE)+
  mapview(strand_65, zcol = "level", color = "red", lwd = 1, legend = FALSE, hide = TRUE)+
  mapview(strand_3, zcol = "level", color = "blue", lwd = 1, legend = FALSE, hide = TRUE)
  

# ----- Punkt -----
taghusa <- st_sf(text = "Här bor jag", geometry = st_sfc(st_point(c(444025, 6168972)), crs = 3006))

# ----- Karta -----
mapview(kommun2, zcol = "laga_kraft", at = brks_kom, col.regions = pal_fun, alpha.regions = 0.6,
        popup = TRUE,
        popup.vars = c("Kommun"="kommunnamn","Laga kraft"="laga_kraft",
                       "Ändr (sum)"="andringar_sum","Tillägg (sum)"="tillagg_sum","PS"="planeringsstrategi")) +
  mapview(lan, col.regions = NA, color = "black", lwd = 2, alpha.regions = 0) +
  mapview(hojddata, at = brks, col.regions = cols, alpha.regions = 0.7, hide = TRUE) +
  mapview(strandlinje, zcol = "level", color = "black", lwd = 1, legend = FALSE, hide = TRUE) +
  mapview(taghusa, col.regions = "green", color = "blue", pch = 16, cex = 5, alpha = 1, popup = taghusa$text, hide = TRUE)+
  mapview(strand_1, zcol = "level", color = "green", lwd = 1, legend = FALSE, hide = TRUE)+
  mapview(strand_2, zcol = "level", color = "darkorange", lwd = 1, legend = FALSE, hide = TRUE)+
  mapview(strand_65, zcol = "level", color = "red", lwd = 1, legend = FALSE, hide = TRUE)

# kör öp lagakraft lomma först!

# ---- Behovsindex + paletter/breaks ----
current_year <- 2025
kommun2 <- kommun2 %>%
  mutate(
    ps_finns = coalesce(ps_finns, 0L),
    alder_op = if_else(!is.na(laga_kraft), current_year - laga_kraft, NA_integer_),
    behov_index = coalesce(alder_op, 0) + 2*coalesce(andringar_sum, 0) + 2*coalesce(tillagg_sum, 0) - 3*coalesce(ps_finns, 0)
  )

pal_year   <- colorRampPalette(c("#8c2d04","#d94801","#fdae6b","#fff5eb"))
pal_seq    <- colorRampPalette(c("#67000d","#cb181d","#fcae91","#fee5d9"))
pal_cat    <- c("0"="#f7f7f7","1–2"="#bdd7e7","3–5"="#6baed6","6+"="#2171b5")
pal_till   <- c("0"="#f7f7f7","1"="#d0d1e6","2–3"="#a6bddb","4+"="#3690c0")
pal_ps     <- c("PS:Ja"="#41ab5d","PS:Nej"="#de2d26","PS:Okänt"="#bdbdbd")

# robusta breaks (utan NA)
brks_year <- seq(min(kommun2$laga_kraft, na.rm = TRUE),
                 max(kommun2$laga_kraft, na.rm = TRUE), by = 2)
brks_idx  <- quantile(kommun2$behov_index, probs = seq(0, 1, 0.1), na.rm = TRUE) %>% unique() %>% as.numeric()

# ---- Länsmedel av index ----
lan2 <- lan %>%
  st_make_valid() %>%
  mutate(lan_namn = as.character(lan)) %>%
  dplyr::select(lan, lan_namn) %>%   # <- viktigt: dplyr::select()
  st_make_valid()


kommun2 <- kommun2 %>%
  mutate(
    kommun = as.character(kommun),      # kommunkod som text
    lan_kod = substr(kommun, 1, 2)      # två första tecknen = län
  )

lan_idx <- kommun2 %>%
  st_drop_geometry() %>%
  group_by(lan_kod) %>%
  summarise(behov_index_mean = mean(behov_index, na.rm = TRUE), .groups = "drop")


lan2 <- lan2 %>%
  left_join(lan_idx, by = c("lan" = "lan_kod"))

# ---- Urval: topp 10 kommuner efter index ----
top10 <- kommun2 %>%
  arrange(desc(behov_index)) %>%
  slice_head(n = 10)

# ---- Mapview-lager ----
mv_laga <- mapview(kommun2, zcol = "laga_kraft",
                   at = brks_year, col.regions = pal_year(length(brks_year)-1),
                   layer.name = "ÖP laga kraft (år)", na.color = "transparent")

mv_andr_cat <- mapview(kommun2, zcol = "andringar",
                       col.regions = pal_cat, layer.name = "Ändringar (klasser)",
                       na.color = "transparent")

mv_andr_sum <- mapview(kommun2, zcol = "andringar_sum",
                       at = pretty(range(kommun2$andringar_sum, na.rm = TRUE), 6),
                       col.regions = pal_seq(5),
                       layer.name = "Ändringar (sum)", na.color = "transparent")

mv_till_cat <- mapview(kommun2, zcol = "tillagg",
                       col.regions = pal_till, layer.name = "Tillägg (klasser)",
                       na.color = "transparent")

mv_ps <- mapview(kommun2, zcol = "planeringsstrategi",
                 col.regions = pal_ps, layer.name = "Planeringsstrategi",
                 na.color = "transparent")

mv_idx <- mapview(kommun2, zcol = "behov_index",
                  at = brks_idx, col.regions = pal_seq(length(brks_idx)-1),
                  layer.name = "Behovsindex (kommun)", na.color = "transparent")

mv_lan_idx <- mapview(lan2, zcol = "behov_index_mean",
                      at = quantile(lan2$behov_index_mean, probs = seq(0,1,0.2), na.rm = TRUE) %>% unique(),
                      col.regions = pal_seq(5),
                      layer.name = "Behovsindex (länsmedel)",
                      alpha.regions = 0.5, lwd = 1)

mv_top10 <- mapview(top10, color = "black", col.regions = NA,
                    alpha.regions = 0, lwd = 2,
                    layer.name = "Top 10 kommuner")

# ---- Kombinerad karta ----
mv_laga + mv_andr_cat + mv_andr_sum + mv_till_cat + mv_ps + mv_idx + mv_lan_idx + mv_top10

