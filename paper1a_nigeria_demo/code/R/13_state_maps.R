# Script 13: State Variation Choropleth Maps
# Generates two paper figures:
#   fig_map_fintech.pdf   — pre-shock fintech index by state
#   fig_map_ntl_change.pdf — NTL change (peak crunch vs baseline) by state
# Outputs: paper/figures/fig_map_fintech.pdf, fig_map_ntl_change.pdf

library(sf)
library(dplyr)
library(readr)
library(ggplot2)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
shp_dir <- file.path(root, "data/raw/shapefiles/gadm_nigeria")
fig_dir <- file.path(root, "paper/figures")
dir.create(fig_dir, showWarnings = FALSE)

# ── 1. Load Nigeria ADM1 (state) shapefile ─────────────────────────────────────
nga_states <- st_read(file.path(shp_dir, "gadm41_NGA_1.shp"), quiet = TRUE) |>
  select(state_gadm = NAME_1, geometry)

# Standardise state names to match panel data
# GADM uses "Federal Capital Territory" for FCT
nga_states <- nga_states |>
  mutate(state = case_when(
    state_gadm == "Federal Capital Territory" ~ "Abuja",
    TRUE ~ state_gadm
  ))

cat("States in shapefile:", nrow(nga_states), "\n")

# ── 2. Load state-level analysis panel ────────────────────────────────────────
panel <- read_csv(file.path(root, "data/processed/analysis_panel_state.csv"),
                  show_col_types = FALSE)

# Pre-shock fintech index: constant per state, take from first row per state
fintech_state <- panel |>
  distinct(state, .keep_all = TRUE) |>
  select(state, fintech_std = treat,      # 'treat' is the std fintech index
         fintech_raw = pct_fintech_index)  # raw penetration rate

# NTL change: peak crunch (Jan-Feb 2023) vs same months 2022
ntl_change <- panel |>
  mutate(
    is_crunch = year == 2023 & month %in% 1:2,
    is_base   = year == 2022 & month %in% 1:2
  ) |>
  filter(is_crunch | is_base) |>
  group_by(state) |>
  summarise(
    ntl_crunch = mean(ln_ntl_mean[is_crunch], na.rm = TRUE),
    ntl_base   = mean(ln_ntl_mean[is_base],   na.rm = TRUE),
    ntl_change = ntl_crunch - ntl_base,
    .groups = "drop"
  )

cat("States with NTL data:", nrow(ntl_change), "\n")

# ── 3. Join to shapefile ───────────────────────────────────────────────────────
map_data <- nga_states |>
  left_join(fintech_state, by = "state") |>
  left_join(ntl_change,   by = "state")

# ── 4. Paper theme ─────────────────────────────────────────────────────────────
theme_map <- theme_void(base_size = 10) +
  theme(
    legend.position      = "right",
    legend.title         = element_text(size = 8.5),
    legend.text          = element_text(size = 7.5),
    legend.key.height    = unit(0.55, "cm"),
    legend.key.width     = unit(0.35, "cm"),
    plot.title           = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle        = element_text(size = 8,  hjust = 0.5, color = "grey40"),
    plot.margin          = margin(4, 4, 4, 4)
  )

# ── 5. Map A: Pre-shock fintech index ─────────────────────────────────────────
map_fintech <- ggplot(map_data) +
  geom_sf(aes(fill = fintech_raw), color = "white", linewidth = 0.25) +
  scale_fill_distiller(
    palette  = "Blues",
    direction = 1,
    name     = "Fintech\npenetration\n(% hh)",
    labels   = function(x) paste0(round(x, 0), "%"),
    na.value = "grey85"
  ) +
  labs(
    title    = "Pre-Shock Digital Payment Adoption by State",
    subtitle = "NLPS Round 6, October 2022 (11 days pre-announcement)"
  ) +
  theme_map

ggsave(
  file.path(fig_dir, "fig_map_fintech.pdf"),
  map_fintech, width = 5.5, height = 4.5,
  device = cairo_pdf
)
cat("Saved: paper/figures/fig_map_fintech.pdf\n")

# ── 6. Map B: NTL change during peak crunch ───────────────────────────────────
map_ntl <- ggplot(map_data) +
  geom_sf(aes(fill = ntl_change), color = "white", linewidth = 0.25) +
  scale_fill_distiller(
    palette   = "RdBu",
    direction = 1,
    name      = "ΔlnNTL\n(Jan–Feb 2023\nvs. 2022)",
    limits    = c(
      -max(abs(map_data$ntl_change), na.rm = TRUE),
       max(abs(map_data$ntl_change), na.rm = TRUE)
    ),
    na.value  = "grey85"
  ) +
  labs(
    title    = "Change in Nighttime-Light Intensity During Peak Crunch",
    subtitle = "January–February 2023 vs. same months 2022 (log points)"
  ) +
  theme_map

ggsave(
  file.path(fig_dir, "fig_map_ntl_change.pdf"),
  map_ntl, width = 5.5, height = 4.5,
  device = cairo_pdf
)
cat("Saved: paper/figures/fig_map_ntl_change.pdf\n")

# ── 7. Combined panel figure (for appendix or paper) ─────────────────────────
library(patchwork)

fig_maps_panel <- map_fintech + map_ntl +
  plot_layout(ncol = 2) +
  plot_annotation(
    caption = paste0(
      "Notes: Left panel: state-level digital payment penetration from NLPS Round 6 ",
      "(Oct 2022), averaged across mobile money, bank accounts, mobile/USSD banking, ",
      "and mobile banking apps. Right panel: log-point change in mean nighttime-light ",
      "radiance (Jan-Feb 2023 minus Jan-Feb 2022) from NASA VIIRS Black Marble VNP46A3."
    ),
    theme = theme(
      plot.caption = element_text(size = 7, hjust = 0, color = "grey30")
    )
  )

ggsave(
  file.path(fig_dir, "fig_maps_panel.pdf"),
  fig_maps_panel, width = 10, height = 4.5,
  device = cairo_pdf
)
cat("Saved: paper/figures/fig_maps_panel.pdf\n")

cat("\nScript 13 complete.\n")
