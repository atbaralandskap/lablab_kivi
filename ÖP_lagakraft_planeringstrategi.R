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
boverket <- list(op_ar, andringar, tillagg, planstrat) %>%
  reduce(full_join, by = "Kommun") %>%
  rename(
    öp_laga_kraft = laga_kraft,
    ändringar_öp = andringar_sum,
    tillägg_öp = tillagg_sum,
    plan_strat = ps_finns,
    plan_strat_år = ps_forsta_ar
  ) %>%
  mutate(
    plan_strat = case_when(
      plan_strat == 1 ~ "Ja",
      plan_strat == 0 ~ "Nej",
      TRUE ~ NA_character_
    ),
    plan_strat = factor(plan_strat, levels = c("Nej", "Ja"))
  )


# ---- Geometrier ----
deso <- st_read("C:/Users/henri/r/test_sf/DESO_2018_v2.gpkg", quiet = TRUE)

kommun <- deso %>% group_by(kommun, kommunnamn) %>% summarise(.groups = "drop")
lan    <- deso %>% group_by(lan, lannamn) %>% summarise(.groups = "drop")

kommun2 <- kommun %>%
  mutate(kommunnamn = str_trim(kommunnamn)) %>%
  left_join(boverket, by = c("kommunnamn" = "Kommun"))


# ----- Färg för årtal (en färg/år) -----
pal_fun <- colorRampPalette(c("darkred", "#d73027", "#fee08b", "#1a9850"))
yrs <- kommun2$öp_laga_kraft
brks_kom <- seq(min(yrs, na.rm = TRUE) - 1, max(yrs, na.rm = TRUE) + 1, by = 1)

# ----- Karta -----

# Kommuner utan planeringsstrategi
kommun_no_plan <- kommun2 %>%
  filter(is.na(plan_strat) | plan_strat == "Nej")

# Bland dessa: de som har öp_laga_kraft 2022–2024
kommun_no_plan_recent <- kommun_no_plan %>%
  filter(öp_laga_kraft %in% c(2022, 2023, 2024))

# Karta
mapview(kommun2, 
        zcol = "öp_laga_kraft", 
        at = brks_kom, 
        col.regions = pal_fun, 
        alpha.regions = 0.6, 
        legend = FALSE, 
        layer.name = "Översiktsplaner") +
  mapview(lan, 
          col.regions = NA, 
          color = "black", 
          lwd = 1, 
          alpha.regions = 0, 
          legend = FALSE, 
          layer.name = "Länsgränser") +
  mapview(kommun_no_plan, 
          col.regions = "blue", 
          alpha.regions = 0.5, 
          legend = FALSE, 
          layer.name = "Saknar planeringsstrategi") +
  mapview(kommun_no_plan_recent, 
          col.regions = "lightblue",  # ljusare blå
          alpha.regions = 0.9,        # mer synlig
          legend = FALSE, 
          layer.name = "Saknar planeringsstrategi + ÖP 2022–2024")











