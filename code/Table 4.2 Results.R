# ============================================================
# Table 4.2
# Annual Turkmen→China trade value, reported net weight,
# realised unit price and Henry Hub equivalent, 2015–2024
# ============================================================

library(dplyr)
library(readr)
library(gt)

setwd("C:/Users/HP/OneDrive/Documents/UvA/The Political Economy of Energy & Critical Minerals/R/Turkemn Gas Final Results/Extractions 4.3")

getwd()
list.files()


tkm_china_annual <- read_csv(
  "chn_reported_imports_tkm_gas_preferred_annual_2015_2024.csv",
  show_col_types = FALSE
)


henry_annual <- read_csv(
  "benchmark_henry_hub_annual_2015_2024.csv",
  show_col_types = FALSE
)


henry_kg <- henry_annual %>%
  transmute(
    year,
    Henry_Hub_USD_per_kg = price_usd_per_mmbtu / 20.27
  )


table_4_2_data <- tkm_china_annual %>%
  filter(year >= 2015, year <= 2024) %>%
  transmute(
    Year = year,
    `Trade value (USD bn)` = TradeValue_USD / 1e9,
    `Reported net weight (bn kg)` = NetWeight_kg / 1e9,
    `Turkmen→China unit price (USD/kg)` = TradeValue_USD / NetWeight_kg
  ) %>%
  left_join(
    henry_kg,
    by = c("Year" = "year")
  ) %>%
  rename(
    `Henry Hub equivalent (USD/kg)` = Henry_Hub_USD_per_kg
  ) %>%
  arrange(Year)


print(table_4_2_data)


table_4_2_gt <- table_4_2_data %>%
  gt() %>%
  tab_header(
    title = "Table 4.2 – Annual Turkmen→China Gas Import Values, Net Weight and Unit Prices, 2015–2024"
  ) %>%
  fmt_number(
    columns = c(
      `Trade value (USD bn)`,
      `Reported net weight (bn kg)`,
      `Turkmen→China unit price (USD/kg)`,
      `Henry Hub equivalent (USD/kg)`
    ),
    decimals = 3
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_source_note(
    source_note = "Source: Author's calculations from UN Comtrade reporter-side import records, China reported imports from Turkmenistan, HS 271121, and EIA Henry Hub annual natural gas spot price data."
  )

library(dplyr)
library(flextable)
library(officer)


if (!"Year" %in% names(table_4_2_data)) {
  table_4_2_data <- table_4_2_data %>%
    mutate(Year = 2015:2024, .before = 1)
}


table_4_2_clean <- table_4_2_data %>%
  transmute(
    Year,
    `Trade value\n(US$ bn)` = round(`Trade value (USD bn)`, 2),
    `Net weight\n(bn kg)` = round(`Reported net weight (bn kg)`, 2),
    `Unit price\n(US$/kg)` = round(`Turkmen→China unit price (USD/kg)`, 3),
    `Henry Hub\n(US$/kg)` = round(`Henry Hub equivalent (USD/kg)`, 3)
  )


table_4_2_flex <- flextable(table_4_2_clean) %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  fontsize(size = 8.5, part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  set_table_properties(layout = "fixed", width = 0.95) %>%
  width(j = "Year", width = 0.55) %>%
  width(j = "Trade value\n(US$ bn)", width = 1.15) %>%
  width(j = "Net weight\n(bn kg)", width = 1.15) %>%
  width(j = "Unit price\n(US$/kg)", width = 1.15) %>%
  width(j = "Henry Hub\n(US$/kg)", width = 1.15) %>%
  autofit()

doc <- read_docx()

doc <- doc %>%
  body_add_par(
    "Table 4.2 – Annual Turkmen→China Gas Import Values, Net Weight and Realised Unit Prices, 2015–2024",
    style = "Normal"
  ) %>%
  body_add_flextable(table_4_2_flex) %>%
  body_add_par(
    "Source: Author’s calculations from UN Comtrade reporter-side import records, China reported imports from Turkmenistan, HS 271121, and EIA Henry Hub annual natural gas spot price data. Henry Hub converted to USD/kg equivalent using 1 mmBtu ≈ 20.27 kg.",
    style = "Normal"
  )

print(
  doc,
  target = "C:/Users/HP/OneDrive/Documents/UvA/The Political Economy of Energy & Critical Minerals/R/table_4_2_clean.docx"
)
