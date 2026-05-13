library(comtradr)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

library(flextable)
library(officer)

set_primary_comtrade_key("f3845d2a81db4c43bc4c01eed3c4c7d8")

tkm_gas_271121_raw <- ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = "271121",
  flow_direction = "Import",
  reporter = "all_countries",
  partner = "TKM",
  start_date = "2015",
  end_date = "2024",
  tidy_cols = TRUE,
  verbose = TRUE


tkm_gas_2711_raw <- ct_get_data(
    type = "goods",
    frequency = "A",
    commodity_classification = "HS",
    commodity_code = "2711",
    flow_direction = "Import",
    reporter = "all_countries",
    partner = "TKM",
    start_date = "2015",
    end_date = "2024",
    tidy_cols = TRUE,
    verbose = TRUE
  )

glimpse(tkm_gas_271121_raw)
glimpse(tkm_gas_2711_raw)

tkm_gas_271121_raw %>%
  count(ref_year, reporter_desc) %>%
  arrange(ref_year, reporter_desc)

tkm_gas_2711_raw %>%
  count(ref_year, reporter_desc) %>%
  arrange(ref_year, reporter_desc)

tkm_gas_mirror_271121 <- tkm_gas_271121_raw %>%
  transmute(
    country = "Turkmenistan",
    sector = "Natural gas",
    hs = "271121",
    year = ref_year,
    importer = reporter_desc,
    importer_iso = reporter_iso,
    partner = partner_desc,
    value_usd = primary_value
  ) %>%
  filter(!is.na(value_usd), value_usd > 0)

tkm_gas_mirror_2711 <- tkm_gas_2711_raw %>%
  transmute(
    country = "Turkmenistan",
    sector = "Natural gas / petroleum gases",
    hs = "2711",
    year = ref_year,
    importer = reporter_desc,
    importer_iso = reporter_iso,
    partner = partner_desc,
    value_usd = primary_value
  ) %>%
  filter(!is.na(value_usd), value_usd > 0)

tkm_gas_mirror_final <- tkm_gas_mirror_271121


tkm_gas_indicators_year <- tkm_gas_mirror_final %>%
  group_by(year, importer) %>%
  summarise(
    value_usd = sum(value_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    total_observed_imports = sum(value_usd, na.rm = TRUE),
    share = value_usd / total_observed_imports
  ) %>%
  summarise(
    country = "Turkmenistan",
    sector = first(tkm_gas_mirror_final$sector),
    hs = first(tkm_gas_mirror_final$hs),
    total_observed_imports = sum(value_usd, na.rm = TRUE),
    china_imports = sum(value_usd[importer == "China"], na.rm = TRUE),
    china_share = china_imports / total_observed_imports,
    HHI = sum(share^2, na.rm = TRUE),
    observed_importers = n_distinct(importer[value_usd > 0]),
    .groups = "drop"
  ) %>%
  arrange(year)

tkm_gas_indicators_year


tkm_gas_prepost <- tkm_gas_indicators_year %>%
  mutate(
    period = case_when(
      year <= 2021 ~ "2015–2021",
      year >= 2022 ~ "2022–2024"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    avg_total_observed_imports = mean(total_observed_imports, na.rm = TRUE),
    avg_china_imports = mean(china_imports, na.rm = TRUE),
    avg_china_share_pct = mean(china_share, na.rm = TRUE) * 100,
    avg_HHI = mean(HHI, na.rm = TRUE),
    avg_observed_importers = mean(observed_importers, na.rm = TRUE),
    .groups = "drop"
  )

tkm_gas_prepost


tkm_gas_importer_breakdown <- tkm_gas_mirror_final %>%
  group_by(year, importer) %>%
  summarise(value_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total_observed_imports = sum(value_usd, na.rm = TRUE),
    share_pct = 100 * value_usd / total_observed_imports
  ) %>%
  arrange(year, desc(value_usd))

tkm_gas_importer_breakdown


write_csv(tkm_gas_mirror_final, "TKM_gas_mirror_imports_final.csv")
write_csv(tkm_gas_indicators_year, "TKM_gas_indicators_year.csv")
write_csv(tkm_gas_prepost, "TKM_gas_prepost_summary.csv")
write_csv(tkm_gas_importer_breakdown, "TKM_gas_importer_breakdown.csv")

setwd("C:/Users/HP/OneDrive/Documents/UvA/The Political Economy of Energy & Critical Minerals/R/Turkemn Gas Final Results")
getwd()
list.files(pattern = "TKM_gas")

tkm_year <- readr::read_csv("TKM_gas_indicators_year.csv")
tkm_prepost <- readr::read_csv("TKM_gas_prepost_summary.csv")

names(tkm_year)
names(tkm_prepost)

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(flextable)
library(officer)

tkm_year <- read_csv("TKM_gas_indicators_year.csv")
tkm_prepost <- read_csv("TKM_gas_prepost_summary.csv")

# Figure 4.1
tkm_plot <- tkm_year %>%
  select(year, china_share, HHI) %>%
  pivot_longer(
    cols = c(china_share, HHI),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      china_share = "China share",
      HHI = "HHI partner concentration"
    )
  )

fig_4_1 <- ggplot(tkm_plot, aes(x = year, y = value, linetype = indicator, shape = indicator)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  scale_y_continuous(
    limits = c(0.75, 1.00),
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Figure 4.1 – China Share and HHI Concentration in Observable Turkmen Gas Exports, 2015–2024",
    x = "Year",
    y = "Share / concentration score",
    linetype = NULL,
    shape = NULL,
    caption = "Source: Author's calculation from UN Comtrade mirror-import data, HS 271121."
  ) +
  theme_minimal()

fig_4_1

ggsave(
  filename = "Figure_4_1_Turkmenistan_ChinaShare_HHI.png",
  plot = fig_4_1,
  width = 8,
  height = 5,
  dpi = 300
)

# Table 4.1
tkm_prepost_table <- tkm_prepost %>%
  mutate(
    avg_total_observed_imports = round(avg_total_observed_imports / 1e9, 2),
    avg_china_imports = round(avg_china_imports / 1e9, 2),
    avg_china_share_pct = round(avg_china_share_pct, 1),
    avg_HHI = round(avg_HHI, 3),
    avg_observed_importers = round(avg_observed_importers, 1)
  )

ft_4_1 <- flextable(tkm_prepost_table) %>%
  set_header_labels(
    period = "Period",
    avg_total_observed_imports = "Average observed mirror imports (USD bn)",
    avg_china_imports = "Average China imports (USD bn)",
    avg_china_share_pct = "Average China share (%)",
    avg_HHI = "Average HHI",
    avg_observed_importers = "Average observed importers"
  ) %>%
  autofit()

ft_4_1

doc <- read_docx() %>%
  body_add_par(
    "Table 4.1 – Pre- and Post-2022 Dependence Indicators for Turkmen Gas Exports",
    style = "heading 2"
  ) %>%
  body_add_flextable(ft_4_1) %>%
  body_add_par(
    "Source: Author's calculation from UN Comtrade mirror-import data, HS 271121.",
    style = "Normal"
  )

print(doc, target = "Table_4_1_Turkmenistan_PrePost_Dependency.docx")

list.files(pattern = "Figure_4_1|Table_4_1")

fig_4_1 <- ggplot(tkm_plot, aes(x = year, y = value, linetype = indicator, shape = indicator)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  scale_y_continuous(
    limits = c(0.75, 1.00),
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    x = "Year",
    y = "Share / concentration score",
    linetype = NULL,
    shape = NULL,
    caption = "Source: Author's calculation from UN Comtrade mirror-import data, HS 271121."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.caption = element_text(size = 10),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

fig_4_1

ggsave(
  filename = "Figure_4_1_Turkmenistan_ChinaShare_HHI_HIGH_RES.png",
  plot = fig_4_1,
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)

library(dplyr)
library(readr)
library(flextable)
library(officer)

tkm_prepost <- read_csv("TKM_gas_prepost_summary.csv")

tkm_prepost_table <- tkm_prepost %>%
  mutate(
    avg_total_observed_imports = round(avg_total_observed_imports / 1e9, 2),
    avg_china_imports = round(avg_china_imports / 1e9, 2),
    avg_china_share_pct = round(avg_china_share_pct, 1),
    avg_HHI = round(avg_HHI, 3),
    avg_observed_importers = round(avg_observed_importers, 1)
  )

ft_4_1 <- flextable(tkm_prepost_table) %>%
  set_header_labels(
    period = "Period",
    avg_total_observed_imports = "Observed imports\nUSD bn",
    avg_china_imports = "China imports\nUSD bn",
    avg_china_share_pct = "China share\n%",
    avg_HHI = "HHI",
    avg_observed_importers = "Observed\nimporters"
  ) %>%
  fontsize(size = 9, part = "all") %>%
  fontsize(size = 8.5, part = "header") %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  set_table_properties(layout = "fixed", width = 1) %>%
  width(j = "period", width = 1.0) %>%
  width(j = "avg_total_observed_imports", width = 1.1) %>%
  width(j = "avg_china_imports", width = 1.1) %>%
  width(j = "avg_china_share_pct", width = 0.9) %>%
  width(j = "avg_HHI", width = 0.7) %>%
  width(j = "avg_observed_importers", width = 0.9) %>%
  autofit()

doc <- read_docx() %>%
  body_add_par(
    "Table 4.1 – Pre- and Post-2022 Dependence Indicators for Turkmen Gas Exports",
    style = "heading 2"
  ) %>%
  body_add_flextable(ft_4_1) %>%
  body_add_par(
    "Source: Author's calculation from UN Comtrade mirror-import data, HS 271121.",
    style = "Normal"
  )

print(doc, target = "Table_4_1_Turkmenistan_PrePost_Dependency_FIXED.docx")

list.files(pattern = "Table_4_1")

setwd("C:/Users/HP/OneDrive/Documents/UvA/The Political Economy of Energy & Critical Minerals/R/Turkemn Gas Final Results")
getwd()
list.files(pattern = "TKM_gas_prepost_summary.csv")


library(dplyr)
library(readr)
library(flextable)
library(officer)

tkm_prepost <- read_csv("TKM_gas_prepost_summary.csv")

tkm_prepost_table <- tkm_prepost %>%
  select(
    period,
    avg_total_observed_imports,
    avg_china_imports,
    avg_china_share_pct,
    avg_HHI,
    avg_observed_importers
  ) %>%
  mutate(
    avg_total_observed_imports = round(avg_total_observed_imports / 1e9, 2),
    avg_china_imports = round(avg_china_imports / 1e9, 2),
    avg_china_share_pct = round(avg_china_share_pct, 1),
    avg_HHI = round(avg_HHI, 3),
    avg_observed_importers = round(avg_observed_importers, 1)
  )

ft_4_1 <- flextable(tkm_prepost_table) %>%
  set_header_labels(
    period = "Period",
    avg_total_observed_imports = "Avg. observed imports\n(USD bn/year)",
    avg_china_imports = "Avg. China imports\n(USD bn/year)",
    avg_china_share_pct = "Avg. China share\n(%)",
    avg_HHI = "Avg. HHI",
    avg_observed_importers = "Avg. observed\nimporters"
  ) %>%
  fontsize(size = 9, part = "all") %>%
  fontsize(size = 8.5, part = "header") %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  set_table_properties(layout = "fixed", width = 1) %>%
  width(j = "period", width = 1.0) %>%
  width(j = "avg_total_observed_imports", width = 1.25) %>%
  width(j = "avg_china_imports", width = 1.25) %>%
  width(j = "avg_china_share_pct", width = 1.0) %>%
  width(j = "avg_HHI", width = 0.75) %>%
  width(j = "avg_observed_importers", width = 1.0)


ft_4_1


doc <- read_docx() %>%
  body_add_par(
    "Table 4.1 – Pre- and Post-2022 Dependence Indicators for Turkmen Gas Exports",
    style = "heading 2"
  ) %>%
  body_add_flextable(ft_4_1) %>%
  body_add_par(
    "Note: Import values are annual averages within each period, not cumulative totals.",
    style = "Normal"
  ) %>%
  body_add_par(
    "Source: Author's calculation from UN Comtrade mirror-import data, HS 271121.",
    style = "Normal"
  )

print(doc, target = "Table_4_1_Turkmenistan_PrePost_Dependency_CORRECTED.docx")

list.files(pattern = "Table_4_1")

