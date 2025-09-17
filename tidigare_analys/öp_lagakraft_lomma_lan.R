
# ---------- All-in-one Mapview stack (compact) ----------
library(purrr)

# 1) Indexpåslag + palettes/breaks
current_year <- 2025
kommun2 <- kommun2 %>%
  mutate(
    ps_finns   = coalesce(ps_finns, 0L),
    alder_op   = if_else(!is.na(laga_kraft), current_year - laga_kraft, NA_integer_),
    behov_index = coalesce(alder_op, 0) +
      2*coalesce(andringar_sum, 0) +
      2*coalesce(tillagg_sum, 0) -
      3*coalesce(ps_finns, 0),
    kommun     = as.character(kommun),
    lan_kod    = substr(kommun, 1, 2)
  )

pal_year <- colorRampPalette(c("#8c2d04","#d94801","#fdae6b","#fff5eb"))
# pal_seq  <- colorRampPalette(c("#67000d","#cb181d","#fcae91","#fee5d9"))
pal_cat  <- c("0"="#f7f7f7","1–2"="#bdd7e7","3–5"="#6baed6","6+"="#2171b5")
pal_till <- c("0"="#f7f7f7","1"="#d0d1e6","2–3"="#a6bddb","4+"="#3690c0")
pal_ps   <- c("PS:Ja"="#41ab5d","PS:Nej"="#de2d26","PS:Okänt"="#bdbdbd")

# pal_year <- colorRampPalette(c("#fff5eb","#fdae6b","#d94801","#8c2d04"))
pal_seq  <- colorRampPalette(c("#fee5d9","#fcae91","#cb181d","#67000d"))
# 
# pal_cat  <- c("0"="#f7f7f7","1–2"="#bdd7e7","3–5"="#6baed6","6+"="#08306b")
# pal_till <- c("0"="#f7f7f7","1"="#d0d1e6","2–3"="#6baed6","4+"="#08306b")
# pal_ps   <- c("PS:Ja"="#41ab5d","PS:Nej"="#de2d26","PS:Okänt"="#bdbdbd")


brks_year <- seq(min(kommun2$laga_kraft, na.rm=TRUE),
                 max(kommun2$laga_kraft, na.rm=TRUE), by=2)
brks_idx  <- quantile(kommun2$behov_index, probs = seq(0,1,0.1), na.rm=TRUE) |> unique() |> as.numeric()

# 2) Länsmedel av index (via kommunkod -> två första siffror)
lan_idx <- kommun2 |>
  st_drop_geometry() |>
  group_by(lan_kod) |>
  summarise(behov_index_mean = mean(behov_index, na.rm=TRUE), .groups = "drop")

lan2 <- lan |>
  st_make_valid() |>
  mutate(lan = as.character(lan)) |>
  left_join(lan_idx, by = c("lan" = "lan_kod"))

# 3) Top 10 kommuner
top10 <- kommun2 |> arrange(desc(behov_index)) |> slice_head(n=10)

# 4) Höjdkarta & konturer (du har redan skapat: hojddata, strand_1/2/3/65)
brks_dem <- c(-Inf, seq(0,10,1), seq(15,50,5), seq(60,200,10), Inf)
cols_dem <- colorRampPalette(c("#08306b","#41b6c4","#a1dab4","#ffffcc","#fdae61","#f46d43","#d73027"))(length(brks_dem)-1)

# 5) Bygg alla lager och kombinera
layers <- list(
  mapview(kommun2, zcol = "laga_kraft",
          at = brks_year, col.regions = pal_year(length(brks_year)-1),
          layer.name = "ÖP laga kraft (år)", alpha.regions = 0.6,
          popup = TRUE,
          popup.vars = c("Kommun"="kommunnamn","Laga kraft"="laga_kraft",
                         "Ändr (sum)"="andringar_sum","Tillägg (sum)"="tillagg_sum","PS"="planeringsstrategi")),
  mapview(kommun2, zcol = "andringar", col.regions = pal_cat,
          layer.name = "Ändringar (klasser)", na.color = "transparent", hide = TRUE),
  mapview(kommun2, zcol = "andringar_sum",
          at = pretty(range(kommun2$andringar_sum, na.rm=TRUE), 6),
          col.regions = pal_seq(5), layer.name = "Ändringar (sum)", na.color = "transparent", hide = TRUE),
  mapview(kommun2, zcol = "tillagg", col.regions = pal_till,
          layer.name = "Tillägg (klasser)", na.color = "transparent", hide = TRUE),
  mapview(kommun2, zcol = "planeringsstrategi", col.regions = pal_ps,
          layer.name = "Planeringsstrategi", na.color = "transparent", hide = TRUE),
  mapview(kommun2, zcol = "behov_index",
          at = brks_idx, col.regions = pal_seq(length(brks_idx)-1),
          layer.name = "Behovsindex (kommun)", na.color = "transparent", hide = TRUE),
  mapview(lan2, zcol = "behov_index_mean",
          at = quantile(lan2$behov_index_mean, probs = seq(0,1,0.2), na.rm=TRUE) |> unique(),
          col.regions = pal_seq(5), layer.name = "Behovsindex (länsmedel)",
          alpha.regions = 0.5, lwd = 1, hide = TRUE),
  mapview(lan, col.regions = NA, color = "black", lwd = 2,
          alpha.regions = 0, layer.name = "Länsgräns", legend = FALSE),
  mapview(hojddata, at = brks_dem, col.regions = cols_dem,
          alpha.regions = 0.7, layer.name = "Höjddata", hide = TRUE),
  mapview(strand_1,  color = "green",      lwd = 1, legend = FALSE, hide = TRUE, layer.name = "Strand 1 m"),
  mapview(strand_2,  color = "darkorange", lwd = 1, legend = FALSE, hide = TRUE, layer.name = "Strand 2 m"),
  mapview(strand_3,  color = "blue",       lwd = 1, legend = FALSE, hide = TRUE, layer.name = "Strand 3 m"),
  mapview(strand_65, color = "red",        lwd = 1, legend = FALSE, hide = TRUE, layer.name = "Strand 65 m"),
  mapview(taghusa, col.regions = "green", color = "blue", pch = 16, cex = 5,
          alpha = 1, popup = taghusa$text, layer.name = "Taghusa", hide = TRUE)
)

mv_final <- reduce(layers, `+`)
mv_final
