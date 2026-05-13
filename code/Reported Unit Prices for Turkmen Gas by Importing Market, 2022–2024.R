# ============================================================
# Table 4.3 – cleaner Word output
# Reported Unit Prices for Turkmen Gas by Importing Market,
# 2022–2024
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(flextable)
library(officer)

all_buyers_tkm <- read_csv(
  "all_importers_tkm_gas_unit_values_preferred_annual_2015_2024.csv",
  show_col_types = FALSE
)

# Build clean table
table_4_3_data <- all_buyers_tkm %>%
  filter(
    year %in% 2022:2024,
    importer_iso %in% c("CHN", "KAZ", "UZB", "GRC", "BGR", "POL", "AZE")
  ) %>%
  mutate(
    Buyer = case_when(
      importer_iso == "CHN" ~ "China",
      importer_iso == "KAZ" ~ "Kazakhstan",
      importer_iso == "UZB" ~ "Uzbekistan",
      importer_iso == "GRC" ~ "Greece",
      importer_iso == "BGR" ~ "Bulgaria",
      importer_iso == "POL" ~ "Poland",
      importer_iso == "AZE" ~ "Azerbaijan",
      TRUE ~ importer_desc
    )
  ) %>%
  select(
    Buyer,
    year,
    unit_value_usd_per_kg
  ) %>%
  mutate(
    unit_value_usd_per_kg = round(unit_value_usd_per_kg, 3)
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = unit_value_usd_per_kg
  ) %>%
  mutate(
    Buyer = factor(
      Buyer,
      levels = c(
        "China",
        "Kazakhstan",
        "Uzbekistan",
        "Greece",
        "Bulgaria",
        "Poland",
        "Azerbaijan"
      )
    )
  ) %>%
  arrange(Buyer) %>%
  mutate(
    Buyer = as.character(Buyer),
    across(c(`2022`, `2023`, `2024`), ~ ifelse(is.na(.x), "—", sprintf("%.3f", .x)))
  )


table_4_3_flex <- flextable(table_4_3_data) %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  fontsize(size = 10, part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = "Buyer", align = "left", part = "body") %>%
  valign(valign = "center", part = "all") %>%
  width(j = "Buyer", width = 1.6) %>%
  width(j = "2022", width = 1.1) %>%
  width(j = "2023", width = 1.1) %>%
  width(j = "2024", width = 1.1) %>%
  set_table_properties(layout = "fixed", width = 0.85) %>%
  height_all(height = 0.32)


doc <- read_docx()

doc <- body_add_par(
  doc,
  "Table 4.3 – Reported Unit Prices for Turkmen Gas by Importing Market, 2022–2024",
  style = "Normal"
)

doc <- body_add_flextable(doc, table_4_3_flex)

doc <- body_add_par(
  doc,
  "Source: Author’s calculations from UN Comtrade reporter-side import records of Turkmen gas, using HS 271121 where available and HS 2711 as fallback. Values are reported in US$/kg. Azerbaijan is retained for transparency but treated separately in the analysis because its reported unit value likely reflects swap-accounting rather than a straightforward commercial import price.",
  style = "Normal"
)


print(
  doc,
  target = "C:/Users/HP/OneDrive/Documents/UvA/The Political Economy of Energy & Critical Minerals/R/table_4_3_clean.docx"
)

