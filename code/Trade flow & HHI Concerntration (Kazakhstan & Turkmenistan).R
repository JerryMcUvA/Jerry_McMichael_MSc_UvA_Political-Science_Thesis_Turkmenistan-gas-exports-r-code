names(comtradr::country_codes)

comtradr::country_codes[
  comtradr::country_codes$country %in% c("Turkmenistan","Kazakhstan","China"),
  c("country","iso_3")
]
library(dplyr)

comtradr::country_codes %>%
  filter(country %in% c("Turkmenistan","Kazakhstan","China")) %>%
  select(country, iso_3)
comtradr::country_codes %>%
  filter(country %in% c("Turkmenistan","Kazakhstan","China")) %>%
  select(country, iso_3)

library(comtradr)

tkm_gas_all <- ct_get_data(
  reporter = "TKM",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = "271121"
)
Sys.setenv(COMTRADE_PRIMARY = "COMTRADE_PRIMARY_KEY")

kaz_gas_all <- ct_get_data(
  reporter = "KAZ",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = "271121"
)
library(comtradr)

tkm_gas_all <- ct_get_data(
  reporter = "TKM",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = "2711"
)

names(tkm_gas_all)
head(tkm_gas_all)

library(comtradr)

tkm_gas_all <- ct_get_data(
  reporter = "TKM",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = "2711"
)

names(tkm_gas_all)
head(tkm_gas_all)

chn_imports_from_tkm_gas <- ct_get_data(
  reporter = "CHN",
  partner  = "TKM",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Import",
  commodity_classification = "HS",
  commodity_code = "2711"
)

names(chn_imports_from_tkm_gas)
head(chn_imports_from_tkm_gas)

library(dplyr)


value_col <- intersect(c("trade_value_usd","trade_value","primary_value"), names(tkm_gas_all))[1]
partner_col <- intersect(c("partner_iso","partner_iso3","partner"), names(tkm_gas_all))[1]
year_col <- intersect(c("ref_year","year"), names(tkm_gas_all))[1]


tkm_china_share <- tkm_gas_all %>%
  mutate(value = .data[[value_col]]) %>%
  group_by(.data[[year_col]]) %>%
  summarise(
    total_exports = sum(value, na.rm = TRUE),
    exports_to_china = sum(value[.data[[partner_col]] == "CHN"], na.rm = TRUE),
    china_share = exports_to_china / total_exports,
    .groups = "drop"
  ) %>%
  rename(ref_year = 1) %>%
  arrange(ref_year)

tkm_china_share

nrow(tkm_gas_all)
names(tkm_gas_all)
head(tkm_gas_all)


pick_col <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) > 0) return(hits[1])
  NA_character_
}

get_cols <- function(df){
  value_col  <- pick_col(df, c("trade_value_usd","trade_value","primary_value","fob_value","cif_value"))
  year_col   <- pick_col(df, c("ref_year","year","ref_period","period"))
  partner_col<- pick_col(df, c("partner_iso3","partner_iso","partner","partner_code","partner_id"))
  
  cat("Using columns:\n",
      " value_col =", value_col, "\n",
      " year_col  =", year_col,  "\n",
      " partner_col =", partner_col, "\n")
  
  if (is.na(value_col) || is.na(year_col) || is.na(partner_col)) {
    stop("Could not detect required columns. Columns present are:\n", paste(names(df), collapse=", "))
  }
  list(value_col=value_col, year_col=year_col, partner_col=partner_col)
}
library(dplyr)

cols <- get_cols(tkm_gas_all)

tkm_china_share <- tkm_gas_all %>%
  mutate(value = .data[[cols$value_col]]) %>%
  group_by(.data[[cols$year_col]]) %>%
  summarise(
    total_exports = sum(value, na.rm = TRUE),
    exports_to_china = sum(value[.data[[cols$partner_col]] == "CHN"], na.rm = TRUE),
    china_share = exports_to_china / total_exports,
    .groups = "drop"
  ) %>%
  rename(ref_year = 1) %>%
  arrange(ref_year)

tkm_china_share

library(comtradr)

tkm_gas_all <- ct_get_data(
  reporter = "TKM",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = "2711"
)

nrow(tkm_gas_all)
names(tkm_gas_all)
head(tkm_gas_all)

chn_imports_from_tkm_gas <- ct_get_data(
  reporter = "CHN",
  partner  = "TKM",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Import",
  commodity_classification = "HS",
  commodity_code = "2711"
)

nrow(chn_imports_from_tkm_gas)
names(chn_imports_from_tkm_gas)
head(chn_imports_from_tkm_gas)

kaz_ree_magnets_all <- ct_get_data(
  reporter = "KAZ",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = c("2846","8505")
)

nrow(kaz_ree_magnets_all)
names(kaz_ree_magnets_all)
head(kaz_ree_magnets_all)

library(dplyr)
library(ggplot2)

tkm_gas_series <- chn_imports_from_tkm_gas %>%
  transmute(
    year = ref_year,
    value_usd = primary_value,
    qty_kg = qty
  ) %>%
  arrange(year)

tkm_gas_series

ggplot(tkm_gas_series, aes(x = year, y = value_usd/1e9)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2026, linetype = "dashed") +
  labs(x = NULL, y = "China imports from Turkmenistan (USD bn)",
       title = "Turkmen gas trade to China (HS 2711), mirror data")
ggplot(tkm_gas_series, aes(x = year, y = value_usd/1e9)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2026, linetype = "dashed") +
  labs(x = NULL, y = "China imports from Turkmenistan (USD bn)",
       title = "Turkmen gas trade to China (HS 2711), mirror data")

ls()
library(comtradr)

kaz_ree_magnets_all <- ct_get_data(
  reporter = "KAZ",
  partner  = "all_countries",
  start_date = "1991",
  end_date   = "2026",
  flow_direction = "Export",
  commodity_classification = "HS",
  commodity_code = c("2846","8505")
)

nrow(kaz_ree_magnets_all)
names(kaz_ree_magnets_all)

library(dplyr)

kaz_clean <- kaz_ree_magnets_all %>%
  transmute(
    year = ref_year,
    hs = cmd_code,
    partner = partner_iso,
    value_usd = primary_value,
    qty_kg = qty
  )

saveRDS(kaz_ree_magnets_all, "kaz_ree_magnets_all_2015_2024.rds")

library(dplyr)

kaz_clean <- kaz_ree_magnets_all %>%
  transmute(
    year = ref_year,
    hs = cmd_code,
    partner = partner_iso,
    value_usd = primary_value,
    qty_kg = qty

    kaz_china_share <- kaz_clean %>%
      group_by(year, hs) %>%
      summarise(
        total = sum(value_usd, na.rm = TRUE),
        to_china = sum(value_usd[partner == "CHN"], na.rm = TRUE),
        china_share = to_china / total,
        .groups = "drop"
      ) %>%
      arrange(hs, year)
    
    kaz_china_share
    
    kaz_hhi <- kaz_clean %>%
      group_by(year, hs, partner) %>%
      summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
      group_by(year, hs) %>%
      mutate(share = value_usd / sum(value_usd, na.rm = TRUE)) %>%
      summarise(HHI = sum(share^2, na.rm = TRUE), .groups = "drop") %>%
      arrange(hs, year)
    
    kaz_hhi
    library(ggplot2)
    
    ggplot(kaz_china_share, aes(x = year, y = china_share)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = 2026, linetype = "dashed") +
      facet_wrap(~hs, scales = "free_y") +
      labs(x = NULL, y = "China share", title = "Kazakhstan export dependence on China (HS 2846, 8505)")
    
    ggplot(kaz_hhi, aes(x = year, y = HHI)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = 2026, linetype = "dashed") +
      facet_wrap(~hs, scales = "free_y") +
      labs(x = NULL, y = "HHI", title = "Kazakhstan export concentration (outside options proxy)")   
    
    nrow(kaz_gas_all)
    names(kaz_gas_all)
    
    kaz_clean %>%
      filter(hs == "2846", year == 2026) %>%
      group_by(partner) %>%
      summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value_usd)) %>%
      slice_head(n = 10)
    
    kaz_gas_clean <- kaz_gas_all %>%
      transmute(
        year = ref_year,
        hs = cmd_code,
        partner = partner_iso,
        value_usd = primary_value
      )
    
    kaz_gas_china_share <- kaz_gas_clean %>%
      group_by(year) %>%
      summarise(
        total = sum(value_usd, na.rm = TRUE),
        to_china = sum(value_usd[partner == "CHN"], na.rm = TRUE),
        china_share = to_china / total,
        .groups = "drop"
      ) %>%
      arrange(year)
    
    kaz_gas_hhi <- kaz_gas_clean %>%
      group_by(year, partner) %>%
      summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
      group_by(year) %>%
      mutate(share = value_usd / sum(value_usd, na.rm = TRUE)) %>%
      summarise(HHI = sum(share^2, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
    
    kaz_gas_china_share
    kaz_gas_hhi
    
    library(ggplot2)
    
    ggplot(kaz_gas_china_share, aes(x = year, y = china_share)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = 2022, linetype = "dashed") +
      labs(x=NULL, y="China share", title="Kazakhstan gas export dependence on China (HS 2711)")
    
    ggplot(kaz_gas_hhi, aes(x = year, y = HHI)) +
      geom_line() + geom_point() +
      geom_vline(xintercept = 2022, linetype = "dashed") +
      labs(x=NULL, y="HHI", title="Kazakhstan gas export partner concentration (HS 2711)")
    
    kaz_ree_to_china_mirror <- ct_get_data(
      reporter = "CHN", partner = "KAZ",
      start_date="2015", end_date="2024",
      flow_direction="Import",
      commodity_classification="HS",
      commodity_code="2846"
    )
    
    kaz_ree_to_china_mirror %>%
      transmute(year=ref_year, value_usd=primary_value) %>%
      arrange(year)
    
    kaz_ree_to_rus_mirror <- ct_get_data(
      reporter = "RUS", partner = "KAZ",
      start_date="2015", end_date="2024",
      flow_direction="Import",
      commodity_classification="HS",
      commodity_code="2846"
    )
    
    kaz_ree_to_rus_mirror %>%
      transmute(year=ref_year, value_usd=primary_value) %>%
      arrange(year)
    
    library(dplyr)
    
    # Minerals indicators (Kazakhstan)
    kaz_minerals_indicators <- kaz_china_share %>%
      left_join(kaz_hhi, by = c("year","hs")) %>%
      mutate(country = "Kazakhstan",
             sector = case_when(hs == "2846" ~ "REE compounds (HS 2846)",
                                hs == "8505" ~ "Magnets proxy (HS 8505)",
                                TRUE ~ hs)) %>%
      select(country, sector, hs, year, china_share, HHI)
    
    # Gas indicators (Kazakhstan)
    kaz_gas_indicators <- kaz_gas_china_share %>%
      left_join(kaz_gas_hhi, by = "year") %>%
      mutate(country = "Kazakhstan",
             sector = "Natural gas (HS 2711)",
             hs = "2711") %>%
      select(country, sector, hs, year, china_share, HHI)
    
    indicators <- bind_rows(kaz_minerals_indicators, kaz_gas_indicators) %>%
      arrange(country, sector, year)
    
    write.csv(indicators, "CA_bargaining_indicators_KAZ.csv", row.names = FALSE)
    
    ggsave("KAZ_china_share.png", width = 10, height = 5, dpi = 300)
    ggsave("KAZ_hhi.png", width = 10, height = 5, dpi = 300)
    
    indicators %>%
      mutate(post2022 = year >= 2022) %>%
      group_by(sector, post2022) %>%
      summarise(
        avg_china_share = mean(china_share, na.rm = TRUE),
        avg_HHI = mean(HHI, na.rm = TRUE),
        .groups = "drop"
      )
    
    indicators %>%
      filter(year <= 2023) %>%
      mutate(post2022 = year >= 2022) %>%
      group_by(sector, post2022) %>%
      summarise(
        avg_china_share = mean(china_share, na.rm = TRUE),
        avg_HHI = mean(HHI, na.rm = TRUE),
        .groups = "drop"
      )
    
    library(dplyr)
    
    summary_tbl <- indicators %>%
      filter(year <= 2023) %>%
      mutate(post2022 = year >= 2022) %>%
      group_by(sector, post2022) %>%
      summarise(
        avg_china_share = mean(china_share, na.rm = TRUE),
        avg_HHI = mean(HHI, na.rm = TRUE),
        .groups = "drop"
      )
    
    summary_pretty <- summary_tbl %>%
      mutate(
        period = ifelse(post2022, "2022–2023", "2015–2021"),
        avg_china_share_pct = round(avg_china_share * 100, 1),
        avg_HHI = round(avg_HHI, 3)
      ) %>%
      select(sector, period, avg_china_share_pct, avg_HHI) %>%
      arrange(sector, period)
    
    summary_pretty
    
    install.packages(c("flextable", "officer"))
    library(flextable)
    library(officer)
    ft <- flextable(summary_pretty) %>%
      set_header_labels(
        sector = "Sector",
        period = "Period",
        avg_china_share_pct = "Average China share (%)",
        avg_HHI = "Average partner concentration (HHI)"
      ) %>%
      autofit()
    
    doc <- read_docx() %>%
      body_add_par("Table X. Pre- and post-2022 dependence indicators (Kazakhstan)", style = "heading 2") %>%
      body_add_flextable(ft)
    
    print(doc, target = "Table_PrePost2022_Dependency.docx")
    
    library(dplyr)      # gives you %>%
    library(flextable)
    library(officer)
    
    summary_tbl <- indicators %>%
      filter(year <= 2023) %>%
      mutate(post2022 = year >= 2022) %>%
      group_by(sector, post2022) %>%
      summarise(
        avg_china_share = mean(china_share, na.rm = TRUE),
        avg_HHI = mean(HHI, na.rm = TRUE),
        .groups = "drop"
      )
    
    summary_pretty <- summary_tbl %>%
      mutate(
        period = ifelse(post2022, "2022–2023", "2015–2021"),
        avg_china_share_pct = round(avg_china_share * 100, 1),
        avg_HHI = round(avg_HHI, 3)
      ) %>%
      select(sector, period, avg_china_share_pct, avg_HHI) %>%
      arrange(sector, period) 
    
    ft <- flextable(summary_pretty) %>%
      set_header_labels(
        sector = "Sector",
        period = "Period",
        avg_china_share_pct = "Average China share (%)",
        avg_HHI = "Average partner concentration (HHI)"
      ) %>%
      autofit()
    
    doc <- read_docx() %>%
      body_add_par("Table X. Pre- and post-2022 dependence indicators (Kazakhstan)", style = "heading 2") %>%
      body_add_flextable(ft)
    
    print(doc, target = "Table_PrePost2022_Dependency.docx")
    getwd()
    list.files(pattern = "Table_PrePost2022_Dependency.docx")    
    
    
    library(dplyr)
    
    tkm_gas_indicators_year <- chn_imports_from_tkm_gas %>%
      transmute(
        country = "Turkmenistan",
        sector  = "Natural gas (HS 2711)",
        hs      = "2711",
        year    = ref_year,
        value_usd = primary_value
      ) %>%
      group_by(country, sector, hs, year) %>%
      summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        china_share = 1,   # because this series is China-as-importer mirror data
        HHI = 1            # with only one observed importer, concentration is mechanically 1
      ) %>%
      select(country, sector, hs, year, china_share, HHI)
    
    tkm_gas_indicators_year
    
    library(dplyr)

tkm_gas_indicators_year <- chn_imports_from_tkm_gas %>%
  transmute(
    country = "Turkmenistan",
    sector  = "Natural gas (HS 2711)",
    hs      = "2711",
    year    = ref_year,
    value_usd = primary_value
  ) %>%
  group_by(country, sector, hs, year) %>%
  summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    china_share = 1,   # because this series is China-as-importer mirror data
    HHI = 1            # with only one observed importer, concentration is mechanically 1
  ) %>%
  select(country, sector, hs, year, china_share, HHI)

tkm_gas_indicators_year
indicators_kaz <- read.csv("CA_bargaining_indicators_KAZ.csv")
indicators_all <- bind_rows(indicators_kaz, tkm_gas_indicators_year) %>%
  arrange(country, sector, year)

write.csv(indicators_all, "CA_bargaining_indicators_KAZ_TKM.csv", row.names = FALSE)
library(flextable)
library(officer)

summary_all <- indicators_all %>%
  filter(year <= 2023) %>%
  mutate(period = ifelse(year >= 2022, "2022–2023", "2015–2021")) %>%
  group_by(country, sector, period) %>%
  summarise(
    avg_china_share_pct = round(mean(china_share, na.rm = TRUE) * 100, 1),
    avg_HHI = round(mean(HHI, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(country, sector, period)

ft <- flextable(summary_all) %>%
  set_header_labels(
    country = "Country",
    sector = "Sector",
    period = "Period",
    avg_china_share_pct = "Average China share (%)",
    avg_HHI = "Average partner concentration (HHI)"
  ) %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table X. Pre- and post-2022 dependence indicators (Kazakhstan and Turkmenistan)", style = "heading 2") %>%
  body_add_flextable(ft)

print(doc, target = "Table_PrePost2022_Dependency_KAZ_TKM.docx")

library(dplyr)

indicators_kaz <- read.csv("CA_bargaining_indicators_KAZ.csv") %>%
  mutate(hs = as.character(hs))

tkm_gas_indicators_year <- tkm_gas_indicators_year %>%
  mutate(hs = as.character(hs))
indicators_all <- bind_rows(indicators_kaz, tkm_gas_indicators_year) %>%
  arrange(country, sector, year)

write.csv(indicators_all, "CA_bargaining_indicators_KAZ_TKM.csv", row.names = FALSE)
library(flextable)
library(officer)

summary_all <- indicators_all %>%
  filter(year <= 2023) %>%
  mutate(period = ifelse(year >= 2022, "2022–2023", "2015–2021")) %>%
  group_by(country, sector, period) %>%
  summarise(
    avg_china_share_pct = round(mean(china_share, na.rm = TRUE) * 100, 1),
    avg_HHI = round(mean(HHI, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(country, sector, period)

ft <- flextable(summary_all) %>%
  set_header_labels(
    country = "Country",
    sector = "Sector",
    period = "Period",
    avg_china_share_pct = "Average China share (%)",
    avg_HHI = "Average partner concentration (HHI)"
  ) %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table X. Pre- and post-2022 dependence indicators (Kazakhstan and Turkmenistan)", style = "heading 2") %>%
  body_add_flextable(ft)

print(doc, target = "Table_PrePost2022_Dependency_KAZ_TKM.docx")


tkm_gas_mirror_all <- all_mirror_imports_from_tkm_gas %>%
  transmute(
    country = "Turkmenistan",
    sector = "Natural gas (HS 2711)",
    hs = "2711",
    year = ref_year,
    importer = reporter_desc,
    value_usd = primary_value
  ) %>%
  group_by(year, importer) %>%
  summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total_observed_imports = sum(value_usd, na.rm = TRUE),
    share = value_usd / total_observed_imports
  ) %>%
  ungroup()

tkm_gas_indicators_year <- tkm_gas_mirror_all %>%
  group_by(year) %>%
  summarise(
    total_observed_imports = sum(value_usd, na.rm = TRUE),
    china_imports = sum(value_usd[importer == "China"], na.rm = TRUE),
    china_share = china_imports / total_observed_imports,
    HHI = sum(share^2, na.rm = TRUE),
    observed_importers = n_distinct(importer[value_usd > 0]),
    .groups = "drop"
  )

