library(tidyverse)
library(comtradr)
library(scales)

Sys.setenv(COMTRADE_PRIMARY = "COMTRADE_PRIMARY_KEY")


Sys.getenv("COMTRADE_PRIMARY")


start_year <- 2008
end_year   <- 2024

# HS 2711 = petroleum gases and other gaseous hydrocarbons.
# This is the broader gas category and is usually more reliable
# for long historical coverage.
gas_code <- "2711"

# If you later want to test natural gas in gaseous state only,
# replace the line above with:
# gas_code <- "271121"

destinations <- tibble(
  destination = c("Russia", "Iran", "China"),
  reporter_code = c("RUS", "IRN", "CHN")
)

# Comtrade/comtradr cannot pull more than 12 annual years at once.
# So split 2008–2024 into two legal requests.
year_chunks <- list(
  c(2008, 2019),
  c(2020, 2024)
)


fetch_gas_imports <- function(reporter_code, destination_name) {
  
  message("Fetching ", destination_name, " imports from Turkmenistan...")
  
  result <- map_dfr(year_chunks, function(chunk) {
    
    chunk_start <- chunk[1]
    chunk_end   <- chunk[2]
    
    message("  Years: ", chunk_start, "–", chunk_end)
    
    tryCatch(
      ct_get_data(
        type = "goods",
        frequency = "A",
        commodity_classification = "HS",
        commodity_code = gas_code,
        flow_direction = "import",
        reporter = reporter_code,
        partner = "TKM",
        start_date = as.character(chunk_start),
        end_date = as.character(chunk_end),
        tidy_cols = TRUE,
        verbose = TRUE
      ),
      error = function(e) {
        message(
          "  Error for ", destination_name,
          " in ", chunk_start, "–", chunk_end, ": ",
          e$message
        )
        return(tibble())
      }
    )
  })
  
  if (nrow(result) == 0) {
    return(tibble())
  }
  
  result %>%
    mutate(destination = destination_name)
}


comtrade_raw <- map2_dfr(
  destinations$reporter_code,
  destinations$destination,
  fetch_gas_imports
)

if (nrow(comtrade_raw) == 0) {
  stop("No Comtrade data returned. Check API key, gas_code, country codes, and flow_direction.")
}


names(comtrade_raw)


glimpse(comtrade_raw)


# Query returned Coloumns:
#   period
#   destination
#   qty_unit_abbr
#   qty
#   net_wgt
#
# Approximate conversion:
#   1 bcm natural gas ≈ 0.717 million tonnes
#   bcm = kg / (0.717 * 1e9)

turkmen_gas_exports <- comtrade_raw %>%
  mutate(
    year = as.integer(period),
    
    quantity_value = as.numeric(qty),
    unit_value = as.character(qty_unit_abbr),
    weight_kg = as.numeric(net_wgt),
    
    bcm = case_when(
      str_detect(unit_value, regex("m3|cubic", ignore_case = TRUE)) &
        !is.na(quantity_value) ~
        quantity_value / 1e9,
      
      !is.na(weight_kg) & weight_kg > 0 ~
        weight_kg / (0.717 * 1e9),
      
      str_detect(unit_value, regex("kg|kilogram|KGM", ignore_case = TRUE)) &
        !is.na(quantity_value) ~
        quantity_value / (0.717 * 1e9),
      
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    year >= start_year,
    year <= end_year,
    destination %in% c("Russia", "Iran", "China")
  ) %>%
  group_by(year, destination) %>%
  summarise(
    bcm = sum(bcm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    year = start_year:end_year,
    destination = c("Russia", "Iran", "China"),
    fill = list(bcm = 0)
  ) %>%
  mutate(
    destination = factor(destination, levels = c("Russia", "Iran", "China"))
  ) %>%
  arrange(year, destination)

print(turkmen_gas_exports)



turkmen_gas_exports %>%
  pivot_wider(
    names_from = destination,
    values_from = bcm
  ) %>%
  print(n = Inf)




fig_3_1 <- ggplot(
  turkmen_gas_exports,
  aes(x = year, y = bcm, fill = destination)
) +
  geom_area(
    alpha = 0.85,
    colour = "white",
    linewidth = 0.15
  ) +
  
  geom_vline(xintercept = 2009, linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = 2016, linetype = "dashed", linewidth = 0.3) +
  geom_vline(xintercept = 2022, linetype = "dashed", linewidth = 0.3) +
  
  annotate(
    "text",
    x = 2009,
    y = Inf,
    label = "2009 rupture",
    angle = 90,
    vjust = 1.3,
    hjust = 1.05,
    size = 3
  ) +
  annotate(
    "text",
    x = 2016,
    y = Inf,
    label = "Gazprom halt",
    angle = 90,
    vjust = 1.3,
    hjust = 1.05,
    size = 3
  ) +
  annotate(
    "text",
    x = 2022,
    y = Inf,
    label = "post-2022 shock",
    angle = 90,
    vjust = 1.3,
    hjust = 1.05,
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = seq(2008, 2024, 2),
    limits = c(2008, 2024.7),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = " bcm"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    title = "Figure 3.1 — Turkmenistan's gas exports by destination, 2008–2024",
    subtitle = "UN Comtrade mirror import data from China, Russia and Iran",
    x = NULL,
    y = "Gas exports, bcm",
    fill = NULL,
    caption = stringr::str_wrap(
      "Source: UN Comtrade mirror import data. Reporter countries are China, Russia and Iran; partner is Turkmenistan; flow is imports. HS 2711. Values are converted to bcm where reported by weight.",
      width = 120
    )
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.1),
    
    plot.margin = margin(
      t = 12,
      r = 35,
      b = 18,
      l = 12
    )
  )

fig_3_1



ggsave(
  filename = "figure_3_1_turkmenistan_gas_exports_by_destination_2008_2024.png",
  plot = fig_3_1,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figure_3_1_turkmenistan_gas_exports_by_destination_2008_2024.pdf",
  plot = fig_3_1,
  width = 8,
  height = 5
)


write_csv(
  turkmen_gas_exports,
  "figure_3_1_turkmenistan_gas_exports_by_destination_2008_2024_data.csv"
)

