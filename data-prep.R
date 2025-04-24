# Load necessary packages
library(tidyverse)
library(tidyquant)
library(lubridate)
library(janitor)
library(sf)

# 1. Prepare price data
price_data <- read.csv("price_data.csv") %>%
  mutate(date = as.Date(date))

company_names <- c(
  "MG.TO" = "Magna International",
  "LNR.TO" = "Linamar Corporation",
  "MRE.TO" = "Martinrea Intl.",
  "XTC.TO" = "Exco Technologies",
  "ACQ.TO" = "AutoCanada",
  "XIC.TO" = "TSX Index"
)

combined_data <- price_data %>%
  filter(adjusted > 0) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    cumulative_return = adjusted / first(adjusted) - 1,
    log_price = log10(adjusted)
  ) %>%
  mutate(company = company_names[symbol]) %>%
  ungroup()

monthly_returns <- combined_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  summarise(month_end_price = last(adjusted), .groups = "drop") %>%
  group_by(symbol) %>%
  arrange(month) %>%
  mutate(monthly_return = month_end_price / lag(month_end_price) - 1) %>%
  filter(!is.na(monthly_return)) %>%
  mutate(month_label = format(month, "%b %Y"),
         monthly_return = round(monthly_return * 100, 1))

returns_wide <- monthly_returns %>%
  select(symbol, month_label, monthly_return) %>%
  pivot_wider(names_from = month_label, values_from = monthly_return)
returns_wide$company <- company_names[returns_wide$symbol]
returns_wide <- returns_wide[, c("company", setdiff(names(returns_wide), "company"))]

saveRDS(combined_data, "data/combined_data.rds")
saveRDS(returns_wide, "data/returns_wide.rds")


# 2. Prepare auto exports data
auto_exports_all <- list.files(pattern = "*_auto_exp.csv") %>%
  map_df(read_csv) %>%
  clean_names() %>%
  mutate(value = ifelse(value == "..", 0, value),
         value = as.numeric(value)) %>%
  group_by(ref_date, geo, principal_trading_partners) %>%
  summarise(total_exports = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(ref_date = as.Date(paste0(ref_date, "-01"))) %>%
  group_by(geo, principal_trading_partners) %>%
  arrange(ref_date) %>%
  mutate(
    prev_exports = lag(total_exports),
    pct_change = (total_exports - prev_exports) / prev_exports * 100
  ) %>%
  ungroup()

saveRDS(auto_exports_all, "data/auto_exports_all.rds")


# 3. Cleaned spatial data
canada_provinces <- readRDS("canada_provinces.rds") %>%
  mutate(name = case_when(
    name == "Québec" ~ "Quebec",
    TRUE ~ name
  ))

saveRDS(canada_provinces, "data/canada_provinces_cleaned.rds")

