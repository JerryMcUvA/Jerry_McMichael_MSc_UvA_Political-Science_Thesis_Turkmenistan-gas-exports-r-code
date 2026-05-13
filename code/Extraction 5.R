# ============================================================
# Extraction 5 — Reference benchmark prices
#
# Benchmarks:
#   1. Henry Hub — EIA, USD/mmBtu
#   2. TTF — optional CSV input, EUR/MWh
#   3. JKM — optional CSV input, USD/mmBtu
#
# Period: 2015–2024
#
# Conversions:
#   1 m3 = 0.0353 mmBtu
#   USD/m3 = USD/mmBtu * 0.0353
#   USD/1000m3 = USD/mmBtu * 35.3
#
#   TTF:
#   1 MWh = 3.412 mmBtu
#   USD/mmBtu = (EUR/MWh / 3.412) * EURUSD
# ============================================================

library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)


Sys.setenv(EIA_API_KEY = "EIA_PRIMARY_KEY")

eia_key <- Sys.getenv("EIA_API_KEY")

if (eia_key == "") {
  stop("No EIA key found.")
}

print("EIA API key loaded successfully.")

start_year <- 2015
end_year   <- 2024


get_eia_seriesid <- function(series_id) {
  
  url <- paste0(
    "https://api.eia.gov/v2/seriesid/",
    series_id,
    "?api_key=",
    eia_key
  )
  
  response <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  
  if (!"response" %in% names(response)) {
    stop("No response object returned by EIA. Check API key or series ID.")
  }
  
  dat <- as_tibble(response$response$data)
  
  if (nrow(dat) == 0) {
    stop(paste("No data returned for EIA series:", series_id))
  }
  
  value_col <- intersect(c("value", "price"), names(dat))[1]
  
  if (is.na(value_col)) {
    print(names(dat))
    stop("No value column found in EIA response.")
  }
  
  dat %>%
    mutate(
      value = as.numeric(.data[[value_col]])
    )
}


henry_monthly_raw <- get_eia_seriesid("NG.RNGWHHD.M")
henry_annual_raw  <- get_eia_seriesid("NG.RNGWHHD.A")


henry_monthly <- henry_monthly_raw %>%
  transmute(
    benchmark = "Henry Hub",
    frequency = "monthly",
    period = as.character(period),
    year = as.integer(str_sub(period, 1, 4)),
    month = as.integer(str_sub(period, 6, 7)),
    price_usd_per_mmbtu = value,
    price_usd_per_m3 = price_usd_per_mmbtu * 0.0353,
    price_usd_per_1000m3 = price_usd_per_mmbtu * 35.3,
    source = "EIA Henry Hub Natural Gas Spot Price"
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  arrange(period)


henry_annual <- henry_annual_raw %>%
  transmute(
    benchmark = "Henry Hub",
    frequency = "annual",
    period = as.character(period),
    year = as.integer(period),
    month = NA_integer_,
    price_usd_per_mmbtu = value,
    price_usd_per_m3 = price_usd_per_mmbtu * 0.0353,
    price_usd_per_1000m3 = price_usd_per_mmbtu * 35.3,
    source = "EIA Henry Hub Natural Gas Spot Price"
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  arrange(year)


ecb_url <- paste0(
  "https://data-api.ecb.europa.eu/service/data/EXR/",
  "M.USD.EUR.SP00.A",
  "?startPeriod=", start_year, "-01",
  "&endPeriod=", end_year, "-12",
  "&format=csvdata"
)

eurusd_monthly_raw <- read_csv(ecb_url, show_col_types = FALSE)

eurusd_monthly <- eurusd_monthly_raw %>%
  transmute(
    period = as.character(TIME_PERIOD),
    eurusd = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(eurusd))

eurusd_annual <- eurusd_monthly %>%
  mutate(year = as.integer(str_sub(period, 1, 4))) %>%
  group_by(year) %>%
  summarise(
    eurusd = mean(eurusd, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
#  Optional TTF input
#
# Save a CSV in your working directory called:
#   ttf_monthly_2015_2024.csv
#
# Required columns:
#   period
#   price_eur_per_mwh
#
# Example:
#   period,price_eur_per_mwh
#   2015-01,20.12
#   2015-02,19.84
# ------------------------------------------------------------

if (file.exists("ttf_monthly_2015_2024.csv")) {
  
  ttf_monthly_raw <- read_csv(
    "ttf_monthly_2015_2024.csv",
    show_col_types = FALSE
  )
  
  ttf_monthly <- ttf_monthly_raw %>%
    mutate(
      period = as.character(period),
      year = as.integer(str_sub(period, 1, 4)),
      month = as.integer(str_sub(period, 6, 7)),
      price_eur_per_mwh = as.numeric(price_eur_per_mwh)
    ) %>%
    left_join(eurusd_monthly, by = "period") %>%
    transmute(
      benchmark = "TTF",
      frequency = "monthly",
      period = period,
      year = year,
      month = month,
      price_eur_per_mwh = price_eur_per_mwh,
      eurusd = eurusd,
      price_usd_per_mmbtu = (price_eur_per_mwh / 3.412) * eurusd,
      price_usd_per_m3 = price_usd_per_mmbtu * 0.0353,
      price_usd_per_1000m3 = price_usd_per_mmbtu * 35.3,
      source = "TTF monthly CSV; ECB EUR/USD conversion"
    ) %>%
    filter(year >= start_year, year <= end_year) %>%
    arrange(period)
  
  ttf_annual <- ttf_monthly %>%
    group_by(year) %>%
    summarise(
      benchmark = "TTF",
      frequency = "annual",
      period = as.character(first(year)),
      month = NA_integer_,
      price_eur_per_mwh = mean(price_eur_per_mwh, na.rm = TRUE),
      eurusd = mean(eurusd, na.rm = TRUE),
      price_usd_per_mmbtu = mean(price_usd_per_mmbtu, na.rm = TRUE),
      price_usd_per_m3 = mean(price_usd_per_m3, na.rm = TRUE),
      price_usd_per_1000m3 = mean(price_usd_per_1000m3, na.rm = TRUE),
      source = "TTF monthly CSV; annual average from monthly values",
      .groups = "drop"
    )
  
} else {
  
  message("No TTF CSV found. Skipping TTF for now.")
  ttf_monthly <- tibble()
  ttf_annual <- tibble()
}

# ------------------------------------------------------------
#  Optional JKM input
#
# Save a CSV in your working directory called:
#   jkm_monthly_2015_2024.csv
#
# Required columns:
#   period
#   price_usd_per_mmbtu
#
# Example:
#   period,price_usd_per_mmbtu
#   2015-01,9.85
#   2015-02,8.94
# ------------------------------------------------------------

if (file.exists("jkm_monthly_2015_2024.csv")) {
  
  jkm_monthly_raw <- read_csv(
    "jkm_monthly_2015_2024.csv",
    show_col_types = FALSE
  )
  
  jkm_monthly <- jkm_monthly_raw %>%
    transmute(
      benchmark = "JKM",
      frequency = "monthly",
      period = as.character(period),
      year = as.integer(str_sub(period, 1, 4)),
      month = as.integer(str_sub(period, 6, 7)),
      price_usd_per_mmbtu = as.numeric(price_usd_per_mmbtu),
      price_usd_per_m3 = price_usd_per_mmbtu * 0.0353,
      price_usd_per_1000m3 = price_usd_per_mmbtu * 35.3,
      source = "JKM monthly CSV"
    ) %>%
    filter(year >= start_year, year <= end_year) %>%
    arrange(period)
  
  jkm_annual <- jkm_monthly %>%
    group_by(year) %>%
    summarise(
      benchmark = "JKM",
      frequency = "annual",
      period = as.character(first(year)),
      month = NA_integer_,
      price_usd_per_mmbtu = mean(price_usd_per_mmbtu, na.rm = TRUE),
      price_usd_per_m3 = mean(price_usd_per_m3, na.rm = TRUE),
      price_usd_per_1000m3 = mean(price_usd_per_1000m3, na.rm = TRUE),
      source = "JKM monthly CSV; annual average from monthly values",
      .groups = "drop"
    )
  
} else {
  
  message("No JKM CSV found. Skipping JKM for now.")
  jkm_monthly <- tibble()
  jkm_annual <- tibble()
}


benchmark_monthly <- bind_rows(
  henry_monthly,
  ttf_monthly,
  jkm_monthly
) %>%
  arrange(benchmark, period)

benchmark_annual <- bind_rows(
  henry_annual,
  ttf_annual,
  jkm_annual
) %>%
  arrange(benchmark, year)


print(henry_monthly)
print(henry_annual)

print(benchmark_monthly)
print(benchmark_annual)


write_csv(
  henry_monthly,
  "benchmark_henry_hub_monthly_2015_2024.csv"
)

write_csv(
  henry_annual,
  "benchmark_henry_hub_annual_2015_2024.csv"
)

write_csv(
  benchmark_monthly,
  "benchmark_prices_monthly_2015_2024.csv"
)

write_csv(
  benchmark_annual,
  "benchmark_prices_annual_2015_2024.csv"
)



getwd()
list.files(pattern = "benchmark")

