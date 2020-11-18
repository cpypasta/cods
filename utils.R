find_date_bounds <- function(data, years) {
  valid_date_range <- data[year(data) <= max(years) & year(data) >= min(years)]
  min_date <- min(valid_date_range)
  max_date <- max(valid_date_range)
  list(min_date = min_date, max_date = max_date)
}

get_pearson <- function(cod_measures, cods, years) {
  data <- cod_measures %>% select(all_of(cods))
  date_bounds <- find_date_bounds(cod_measures$week_end, years)
  min_year <- min(years)
  max_year <- max(years)
  cod_xts <- xts(x = data, order.by = cod_measures$week_end)
  corr_xts <- na.fill(diff.xts(cod_xts[paste(min_year, max_year, sep = "/")]),0)
  round(cor(corr_xts), 1)
}

get_pearson_names <- function(cod_measures, cods, years) {
  covid_fields <- c("covid", "covid_multiple")
  corr <- get_pearson(cod_measures, cods, years)
  sig_pearson <- as.data.frame(corr) %>%
    rownames_to_column("from") %>%
    pivot_longer(-from, names_to = "to", values_to = "corr") %>%
    filter(from %in% covid_fields | to %in% covid_fields) %>%
    filter(corr >= 0.5 | corr <= -0.5) %>%
    filter(corr != 1) %>%
    select(from, to)
  unique(c(sig_pearson$from, sig_pearson$to))
}

xts_to_tibble <- function(data) {
  as.data.frame(data) %>% 
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    as_tibble()
}