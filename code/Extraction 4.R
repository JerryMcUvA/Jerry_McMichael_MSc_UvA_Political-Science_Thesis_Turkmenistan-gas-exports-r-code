# ============================================================
# UN Comtrade Extraction 4
# All-partners mirror view of Turkmen gas exports
#
# Substantive meaning:
# Countries reporting imports of Turkmen gas
#
# Reporter: all countries
# Partner: Turkmenistan / TKM / 795
# Flow: Imports
# HS: 271121 and 2711
# Period: 2015–2024
#
# Purpose:
# Compute unit values by importing partner-year:
#   USD per kg
#   USD per m3, where Qty is reported in cubic metres
#   USD per 1000 m3, where Qty is reported in cubic metres
# ============================================================

library(comtradr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

years <- 2015:2024
gas_codes <- c("271121", "2711")


get_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    return(rep(NA, nrow(df)))
  }
  df[[hit[1]]]
}


safe_ct_get_data <- function(...) {
  tryCatch(
    {
      out <- ct_get_data(...)
      if (is.null(out) || nrow(out) == 0) {
        return(tibble())
      }
      out
    },
    error = function(e) {
      message("Comtrade request failed or returned no usable data: ", e$message)
      tibble()
    }
  )
}


standardise_comtrade <- function(df, freq_label) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  
  ref_year <- as.integer(get_col(df, c("refYear", "ref_year")))
  
  tibble(
    frequency       = freq_label,
    period          = as.character(ref_year),
    year            = ref_year,
    
    importer_code   = get_col(df, c("reporterCode", "reporter_code")),
    importer_iso    = get_col(df, c("reporterISO", "reporter_iso")),
    importer_desc   = get_col(df, c("reporterDesc", "reporter_desc")),
    
    exporter_code   = get_col(df, c("partnerCode", "partner_code")),
    exporter_iso    = get_col(df, c("partnerISO", "partner_iso")),
    exporter_desc   = get_col(df, c("partnerDesc", "partner_desc")),
    
    flow_code       = get_col(df, c("flowCode", "flow_code")),
    flow_desc       = get_col(df, c("flowDesc", "flow_desc")),
    
    cmd_code        = as.character(get_col(df, c("cmdCode", "cmd_code"))),
    cmd_desc        = get_col(df, c("cmdDesc", "cmd_desc")),
    
    TradeValue_USD  = as.numeric(get_col(df, c(
      "primaryValue", "primary_value", "TradeValue", "trade_value"
    ))),
    
    NetWeight_kg    = as.numeric(get_col(df, c(
      "netWgt", "net_wgt", "NetWeight", "net_weight"
    ))),
    
    QtyUnitCode     = as.character(get_col(df, c(
      "qtyUnitCode", "qty_unit_code", "QtyUnitCode"
    ))),
    
    QtyUnitAbbr     = as.character(get_col(df, c(
      "qtyUnitAbbr", "qty_unit_abbr", "QtyUnitAbbr"
    ))),
    
    QtyUnitDesc     = as.character(get_col(df, c(
      "qtyUnitDesc", "qty_unit_desc", "QtyUnitDesc"
    ))),
    
    Qty             = as.numeric(get_col(df, c("qty", "Qty")))
  ) %>%
    filter(cmd_code %in% gas_codes)
}


all_importers_tkm_raw <- map_dfr(years, function(y) {
  message("Pulling all importers of Turkmen gas for ", y)
  
  safe_ct_get_data(
    type = "goods",
    frequency = "A",
    commodity_classification = "HS",
    commodity_code = gas_codes,
    flow_direction = "Import",
    reporter = "all_countries",
    partner = "TKM",
    start_date = as.character(y),
    end_date = as.character(y),
    tidy_cols = FALSE,
    verbose = TRUE,
    requests_per_second = 5 / 60
  )
})

all_importers_tkm_clean <- standardise_comtrade(
  all_importers_tkm_raw,
  "annual"
)


all_importers_tkm_unit_values_raw <- all_importers_tkm_clean %>%
  mutate(
    qty_unit_text = str_to_lower(
      paste(QtyUnitCode, QtyUnitAbbr, QtyUnitDesc, sep = " ")
    ),
    
    qty_is_m3 = str_detect(qty_unit_text, "\\b12\\b|m3|m³|cubic metre|cubic meter"),
    
    unit_value_usd_per_kg = if_else(
      !is.na(TradeValue_USD) & !is.na(NetWeight_kg) & NetWeight_kg > 0,
      TradeValue_USD / NetWeight_kg,
      NA_real_
    ),
    
    unit_value_usd_per_m3 = if_else(
      qty_is_m3 & !is.na(TradeValue_USD) & !is.na(Qty) & Qty > 0,
      TradeValue_USD / Qty,
      NA_real_
    ),
    
    unit_value_usd_per_1000m3 = unit_value_usd_per_m3 * 1000
  )


all_importers_tkm_unit_values_preferred <- all_importers_tkm_unit_values_raw %>%
  mutate(
    code_priority = case_when(
      cmd_code == "271121" ~ 1,
      cmd_code == "2711"   ~ 2,
      TRUE                 ~ 99
    ),
    has_trade_value = !is.na(TradeValue_USD) & TradeValue_USD > 0,
    has_kg_unit_value = !is.na(unit_value_usd_per_kg),
    has_m3_unit_value = !is.na(unit_value_usd_per_m3)
  ) %>%
  arrange(
    year,
    importer_iso,
    code_priority,
    desc(has_trade_value),
    desc(has_m3_unit_value),
    desc(has_kg_unit_value)
  ) %>%
  group_by(year, importer_iso, importer_desc) %>%
  slice(1) %>%
  ungroup() %>%
  rename(
    source_cmd_code = cmd_code,
    source_cmd_desc = cmd_desc
  ) %>%
  select(
    frequency,
    period,
    year,
    importer_code,
    importer_iso,
    importer_desc,
    exporter_code,
    exporter_iso,
    exporter_desc,
    flow_code,
    flow_desc,
    source_cmd_code,
    source_cmd_desc,
    TradeValue_USD,
    NetWeight_kg,
    QtyUnitCode,
    QtyUnitAbbr,
    QtyUnitDesc,
    Qty,
    qty_is_m3,
    unit_value_usd_per_kg,
    unit_value_usd_per_m3,
    unit_value_usd_per_1000m3
  )

turkmen_gas_price_comparators <- all_importers_tkm_unit_values_preferred %>%
  filter(importer_iso %in% c("CHN", "TUR", "IRN", "RUS")) %>%
  arrange(year, importer_iso)


turkmen_gas_usd_per_1000m3_wide <- turkmen_gas_price_comparators %>%
  select(year, importer_iso, importer_desc, unit_value_usd_per_1000m3) %>%
  mutate(importer_label = paste0(importer_iso, "_usd_per_1000m3")) %>%
  select(year, importer_label, unit_value_usd_per_1000m3) %>%
  pivot_wider(
    names_from = importer_label,
    values_from = unit_value_usd_per_1000m3
  ) %>%
  arrange(year)


all_importers_check <- all_importers_tkm_clean %>%
  count(year, importer_iso, importer_desc, cmd_code) %>%
  arrange(year, importer_iso, cmd_code)

unit_value_availability_check <- all_importers_tkm_unit_values_preferred %>%
  summarise(
    rows = n(),
    rows_with_usd_per_kg = sum(!is.na(unit_value_usd_per_kg)),
    rows_with_usd_per_m3 = sum(!is.na(unit_value_usd_per_m3)),
    rows_with_usd_per_1000m3 = sum(!is.na(unit_value_usd_per_1000m3))
  )

print(all_importers_check)
print(unit_value_availability_check)
print(turkmen_gas_price_comparators)
print(turkmen_gas_usd_per_1000m3_wide)


write_csv(
  all_importers_tkm_clean,
  "all_importers_tkm_gas_raw_annual_2015_2024.csv"
)

write_csv(
  all_importers_tkm_unit_values_raw,
  "all_importers_tkm_gas_unit_values_raw_annual_2015_2024.csv"
)

write_csv(
  all_importers_tkm_unit_values_preferred,
  "all_importers_tkm_gas_unit_values_preferred_annual_2015_2024.csv"
)

write_csv(
  turkmen_gas_price_comparators,
  "turkmen_gas_price_comparators_china_turkiye_iran_russia_annual_2015_2024.csv"
)

write_csv(
  turkmen_gas_usd_per_1000m3_wide,
  "turkmen_gas_usd_per_1000m3_comparator_wide_annual_2015_2024.csv"
)

# Confirm export location
getwd()
list.files(pattern = "tkm_gas|turkmen_gas")
