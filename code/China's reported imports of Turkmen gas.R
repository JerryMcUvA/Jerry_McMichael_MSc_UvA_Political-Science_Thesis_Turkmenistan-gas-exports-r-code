# ============================================================
# UN Comtrade Extraction 1
# China's reported imports of Turkmen gas
# Reporter: China / CHN / 156
# Partner: Turkmenistan / TKM / 795
# Flow: Imports
# HS: 271121 and 2711
# Period: 2015–2024
# ============================================================

library(comtradr)

Sys.setenv(COMTRADE_PRIMARY = "f3845d2a81db4c43bc4c01eed3c4c7d8")

comtradr::get_primary_comtrade_key()

library(comtradr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

years <- 2015:2024
gas_codes <- c("271121", "2711")

get_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    return(rep(NA, nrow(df)))
  }
  df[[hit[1]]]
}

standardise_comtrade <- function(df, freq_label) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble())
  }
  
  tibble(
    frequency       = freq_label,
    period          = ifelse(
      get_col(df, c("freqCode", "freq_code")) == "M",
      sprintf(
        "%04d-%02d",
        as.integer(get_col(df, c("refYear", "ref_year"))),
        as.integer(get_col(df, c("refMonth", "ref_month")))
      ),
      as.character(get_col(df, c("refYear", "ref_year")))
    ),
    year            = as.integer(get_col(df, c("refYear", "ref_year"))),
    month           = suppressWarnings(as.integer(get_col(df, c("refMonth", "ref_month")))),
    
    reporter_code   = get_col(df, c("reporterCode", "reporter_code")),
    reporter_iso    = get_col(df, c("reporterISO", "reporter_iso")),
    reporter_desc   = get_col(df, c("reporterDesc", "reporter_desc")),
    
    partner_code    = get_col(df, c("partnerCode", "partner_code")),
    partner_iso     = get_col(df, c("partnerISO", "partner_iso")),
    partner_desc    = get_col(df, c("partnerDesc", "partner_desc")),
    
    flow_code       = get_col(df, c("flowCode", "flow_code")),
    flow_desc       = get_col(df, c("flowDesc", "flow_desc")),
    
    cmd_code        = as.character(get_col(df, c("cmdCode", "cmd_code"))),
    cmd_desc        = get_col(df, c("cmdDesc", "cmd_desc")),
    
    TradeValue_USD  = as.numeric(get_col(df, c("primaryValue", "primary_value", "TradeValue", "trade_value"))),
    NetWeight_kg    = as.numeric(get_col(df, c("netWgt", "net_wgt", "NetWeight", "net_weight"))),
    QtyUnitCode     = get_col(df, c("qtyUnitCode", "qty_unit_code", "QtyUnitCode")),
    Qty             = as.numeric(get_col(df, c("qty", "Qty")))
  ) %>%
    filter(cmd_code %in% gas_codes)
}

annual_raw <- ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = gas_codes,
  flow_direction = "Import",
  reporter = "CHN",
  partner = "TKM",
  start_date = "2015",
  end_date = "2024",
  tidy_cols = FALSE,
  verbose = TRUE
)

annual_clean <- standardise_comtrade(annual_raw, "annual")

monthly_raw <- map_dfr(years, function(y) {
  message("Pulling monthly data for ", y)
  
  ct_get_data(
    type = "goods",
    frequency = "M",
    commodity_classification = "HS",
    commodity_code = gas_codes,
    flow_direction = "Import",
    reporter = "CHN",
    partner = "TKM",
    start_date = paste0(y, "-01"),
    end_date = paste0(y, "-12"),
    tidy_cols = FALSE,
    verbose = TRUE,
    requests_per_second = 5 / 60
  )
})

monthly_clean <- standardise_comtrade(monthly_raw, "monthly")


make_preferred <- function(df) {
  df %>%
    mutate(
      code_priority = case_when(
        cmd_code == "271121" ~ 1,
        cmd_code == "2711"   ~ 2,
        TRUE                 ~ 99
      )
    ) %>%
    arrange(period, code_priority) %>%
    group_by(frequency, period) %>%
    slice(1) %>%
    ungroup() %>%
    rename(source_cmd_code = cmd_code,
           source_cmd_desc = cmd_desc) %>%
    select(
      frequency,
      period,
      year,
      month,
      reporter_code,
      reporter_iso,
      partner_code,
      partner_iso,
      flow_code,
      flow_desc,
      source_cmd_code,
      source_cmd_desc,
      TradeValue_USD,
      NetWeight_kg,
      QtyUnitCode,
      Qty
    )
}

annual_preferred <- make_preferred(annual_clean)
monthly_preferred <- make_preferred(monthly_clean)


annual_check <- annual_clean %>%
  count(year, cmd_code) %>%
  arrange(year, cmd_code)

monthly_check <- monthly_clean %>%
  count(year, month, cmd_code) %>%
  arrange(year, month, cmd_code)

print(annual_check)
print(monthly_check)


write_csv(annual_clean, "chn_reported_imports_tkm_gas_raw_annual_2015_2024.csv")
write_csv(monthly_clean, "chn_reported_imports_tkm_gas_raw_monthly_2015_2024.csv")

write_csv(annual_preferred, "chn_reported_imports_tkm_gas_preferred_annual_2015_2024.csv")
write_csv(monthly_preferred, "chn_reported_imports_tkm_gas_preferred_monthly_2015_2024.csv")

rlang::last_trace()

getwd()
