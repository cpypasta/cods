library(lmtest)
library(xts)
library(forecast)
library(vars)
library(tidyverse)
library(readr)
library(lubridate)
library(gtools)
library(ggthemes)


select <- dplyr::select

#Get most current data from CDC
deaths2018 <- read_csv("https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD")
deaths2020 <- read_csv("https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD")


dim(deaths2018)
dim(deaths2020)
#clean up column names for ease of use and readability, remove unneeded columns & rows
deaths_tidy2018 <- deaths2018 %>%
  rename(
    state = "Jurisdiction of Occurrence",
    year = "MMWR Year",
    week = "MMWR Week",
    week_end = "Week Ending Date",
    all_causes = "All  Cause",
    natural_causes = "Natural Cause",
    septicemia = "Septicemia (A40-A41)",
    diabetes = "Diabetes mellitus (E10-E14)",
    influenza_pneumonia = "Influenza and pneumonia (J10-J18)",
    other_respiratory = "Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)",
    unknown_cause = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)",
    cerebrovascular = "Cerebrovascular diseases (I60-I69)",
    cancer = "Malignant neoplasms (C00-C97)",
    alzheimer = "Alzheimer disease (G30)",
    lower_respiratory = "Chronic lower respiratory diseases (J40-J47)",
    kidney_disease = "Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)",
    heart_disease = "Diseases of heart (I00-I09,I11,I13,I20-I51)"
  ) %>%
  mutate(week_end = mdy(week_end)) %>%
  select(-starts_with("flag_")) %>%
  filter(state != "United States")

deaths_tidy2020 <- deaths2020 %>%
  rename(
    state = "Jurisdiction of Occurrence",
    year = "MMWR Year",
    week = "MMWR Week",
    week_end = "Week Ending Date",
    all_causes = "All Cause",
    natural_causes = "Natural Cause",
    septicemia = "Septicemia (A40-A41)",
    diabetes = "Diabetes mellitus (E10-E14)",
    influenza_pneumonia = "Influenza and pneumonia (J09-J18)",
    other_respiratory = "Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)",
    unknown_cause = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)",
    cerebrovascular = "Cerebrovascular diseases (I60-I69)",
    cancer = "Malignant neoplasms (C00-C97)",
    alzheimer = "Alzheimer disease (G30)",
    lower_respiratory = "Chronic lower respiratory diseases (J40-J47)",
    kidney_disease = "Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)",
    heart_disease = "Diseases of heart (I00-I09,I11,I13,I20-I51)",
    covid  = "COVID-19 (U071, Underlying Cause of Death)",
    covid_multiple = "COVID-19 (U071, Multiple Cause of Death)"
  ) %>%
  select(-starts_with("flag_")) %>%
  filter(state != "United States")

#add datasets together
deaths_tidy <- deaths_tidy2018 %>%
  bind_rows(deaths_tidy2020)

#create a df that is long form
deaths_tidy_long <- deaths_tidy %>%
  pivot_longer(all_causes:covid, names_to = "measure", values_to = "value")
#Note: we will this set later as the base for another dataset 

#For the most accurate data, we remove the last 6 weeks. This allows for places that report monthly
#and the additional 7+ days it takes to properly report COVID-19 deaths and an extra week for buffer
max_reliable_date <- max(deaths_tidy$week_end) - weeks(6)


cod_measures <- deaths_tidy %>% 
  filter(week_end <= max_reliable_date) %>%
  group_by(week_end) %>%
  summarize(across(septicemia:covid, ~ sum(.x, na.rm = TRUE)))

cod_measures_long <- deaths_tidy_long %>%
  filter(!(measure %in% c("all_causes", "natural_causes"))) %>%
  filter(week_end <= max_reliable_date) %>%
  group_by(week_end, measure) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(measure = as.factor(measure)) %>%
  arrange(week_end, measure)

write_rds(cod_measures, "cod_measures.rds", compress = "gz")
write_rds(cod_measures_long, "cod_measures_long.rds", compress = "gz")

#### Granger Causality

covid_period_measures <- cod_measures %>% filter(covid > 0)
cod_xts <- xts(x = covid_period_measures %>% select(-week_end), order.by = covid_period_measures$week_end)

granger_test <- function(formula, data) {
  p_values <- sapply(0:9, function(lag) {
    g_test <- tryCatch({
      g_test <- grangertest(formula, order = 10 - lag, data = data)
      g_test_p <- g_test["Pr(>F)"][2,]
      if (is.nan(g_test_p)) stop("is NaN")
      g_test_p
    }, error = function(err) {
      Inf
    })    
  })
  round(min(p_values), 4)
}

get_granger <- function(data) {
  # separate time series
  series <- lapply(1:ncol(data), function (i) data[,i])
  
  # make each series stationary
  stationary_series <- lapply(series, function(a_series) {
    diffs <- ndiffs(a_series)
    if (diffs > 0) {
      na.fill(diff(a_series, lag = diffs), 0)
    } else {
      a_series
    }
  })
  
  # combine all combinations
  series <- do.call(merge.xts, stationary_series)
  fields <- names(series)
  field_comb <- permutations(length(fields), 2, fields)
  compare_inputs <- as.data.frame(field_comb) %>% rename(result = V1, predictor = V2)
  
  # calculate granger p-value
  pairs <- by(compare_inputs, 1:nrow(compare_inputs), function(pair) {
    a <- series[,pair$result]
    b <- series[,pair$predictor]
    g_formula <- paste(pair$result, pair$predictor, sep = " ~ ")
    g_test_p_value <- granger_test(as.formula(g_formula), series)
    cbind(pair, granger = g_test_p_value, granger_formula = g_formula)
  })
  do.call(rbind, pairs)
}

cod_granger <- get_granger(cod_xts)

write_rds(cod_granger, "cod_granger.rds", compress = "gz")

#### Predictions

causes_model_params <- list(
  "septicemia" = list(p = 3, d = 0, q = 1, P = 1, D = 0, Q = 1, gamma = 1),
  "cancer" = list(p = 2, d = 0, q = 2, P = 0, D = 0, Q = 0, gamma = 1),
  "diabetes" = list(p = 2, d = 0, q = 2, P = 0, D = 0, Q = 0, gamma = 1),
  "alzheimer" = list(p = 2, d = 0, q = 2, P = 0, D = 0, Q = 0, gamma = 0),
  "influenza_pneumonia" = list(p = 2, d = 0, q = 3, P = 0, D = 0, Q = 0, gamma = 0),
  "lower_respiratory" = list(p = 3, d = 0, q = 1, P = 0, D = 0, Q = 0, gamma = 1),
  "other_respiratory" = list(p = 4, d = 0, q = 3, P = 0, D = 0, Q = 0, gamma = 0),
  "kidney_disease" = list(p = 3, d = 0, q = 2, P = 0, D = 0, Q = 0, gamma = 1),
  "unknown_cause" = list(p = 3, d = 0, q = 2, P = 1, D = 0, Q = 0, gamma = 0),
  "heart_disease" = list(p = 2, d = 0, q = 3, P = 0, D = 0, Q = 0, gamma = 1),
  "cerebrovascular" = list(p = 2, d = 0, q = 2, P = 1, D = 0, Q = 0, gamma = 1)
)

xts_to_tibble <- function(data) {
  as.data.frame(data) %>% 
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    as_tibble()
}

get_prediction <- function(data, disease) {
  train_disease_xts <- data[,disease]["2015/2019"]
  test_disease_xts <- data[,disease]["2020"]
  disease_xts_index = index(test_disease_xts)
  
  fit_params <- causes_model_params[[disease]]
  fit_order <- c(fit_params$p, fit_params$d, fit_params$q)
  fit_seasonal <- list(order = c(0,0,0)) # ignored seasonal
  disease_adjusted <- if (fit_params$gamma == 0) log(train_disease_xts) else train_disease_xts
  disease_fit <- arima(disease_adjusted, order = fit_order, seasonal = fit_seasonal)
  disease_est <- forecast(disease_fit, h = length(test_disease_xts))  
  
  if (fit_params$gamma == 0) {
    disease_est$mean <- exp(disease_est$mean)
    disease_est$upper <- exp(disease_est$upper)
    disease_est$lower <- exp(disease_est$lower)
  }
  
  mean_est_xts <- xts(disease_est$mean, order.by = disease_xts_index)
  upper_est_xts <- xts(disease_est$upper, order.by = disease_xts_index)
  lower_est_xts <- xts(disease_est$lower, order.by = disease_xts_index)
  
  disease_est_xts <- cbind.xts(test_disease_xts, mean_est_xts, upper_est_xts[,"95%"], lower_est_xts[,"95%"])
  names(disease_est_xts) <- c("actual", "mean", "upper", "lower")  
  xts_to_tibble(disease_est_xts) %>% mutate(disease = disease)
}

cod_xts <- xts(x = cod_measures %>% select(-week_end), order.by = cod_measures$week_end)
cod_names <- names(cod_xts)
cod_names <- cod_names[!(cod_names %in% c("covid", "covid_multiple"))]
cod_predictions <- lapply(cod_names, function(name) get_prediction(cod_xts, name))
cod_all_predictions <- do.call(rbind, cod_predictions)

write_rds(cod_all_predictions, "cod_predictions.rds", compress = "gz")


#### USA State Polygon Data

USA <- map_data("state")
write_rds(USA, "usa_states.rds", compress = "gz")

####

#Create a subset to merge later with pop density dataset
StateCausesFull  <- deaths_tidy_long %>%  filter((!(measure %in% c("all_causes", "natural_causes"))))

#New York and New York City is a special case and is separated out. We need to combine them 
updateNYFull <- StateCausesFull %>% filter(state %in% c('New York','New York City')) %>% 
  group_by(week_end, measure, year, week) %>% 
  summarize(
    value = sum(value)) %>% 
  mutate(state = 'New York') 

updateNYFull <- updateNYFull[, c(6,3,4,1,2,5)]

StateCausesFull <- StateCausesFull %>% filter(!(state %in% c('New York','New York City')))
StateCausesFull <- StateCausesFull %>% bind_rows(updateNYFull) 

#We need to have DC renamed so it will match with other data sets
StateCausesFull$state <- gsub("District of Columbia", "Washington DC", StateCausesFull$state, fixed=TRUE)


#Add state info and give short names for measures
StatesDF <- data.frame(state = state.name, abb = state.abb, region = state.region, div = state.division) 

#Need to add rows for places we have COD data, but aren't listed in states datasest
StatesDF <- StatesDF %>% add_row(state="Washington DC", abb="DC", region="Northeast", div="Middle Atlantic")
StatesDF <- StatesDF  %>% add_row(state="Puerto Rico", abb="PR")

#combine the datasets so we have regional data with our main dataset
StateCausesFull <- merge(StateCausesFull, StatesDF, by="state", fixed=TRUE)

#Need to create format to match backwards compatibility of code that is used in other files
StateCausesFull <- StateCausesFull %>% 
  mutate(LongCause = case_when(
    measure == "alzheimer" ~ "AlzheimerDisease",
    measure == "cancer" ~"Cancer",
    measure == "cerebrovascular" ~ "Cerebrovascular",
    measure == "covid" ~ "Covid",
    measure == "covid_multiple" ~ "CovidMultiple",
    measure == "diabetes" ~ "Diabetes",
    measure == "heart_disease" ~ "HeartDisease",
    measure == "influenza_pneumonia" ~ "InfluenzaAndPneumonia",
    measure == "kidney_disease" ~ "KidneyDisease",
    measure == "lower_respiratory" ~ "LowerRespiratory",
    measure == "other_respiratory" ~ "OtherRespiratory",
    measure == "septicemia" ~ "Septicemia",
    measure == "unknown_cause" ~ "UnknownCauses"
  )) 

#some of our graphs will do better with names of shorter length
StateCausesFull <- StateCausesFull %>% 
  mutate(Cause = case_when(
    LongCause == "AlzheimerDisease" ~ "Alzheim",
    LongCause == "Cancer" ~"Cancer",
    LongCause == "Cerebrovascular" ~ "Cerebro",
    LongCause == "Covid" ~ "Covid",
    LongCause == "CovidMultiple" ~ "Covid Multi",
    LongCause == "Diabetes" ~ "Diabetes",
    LongCause == "HeartDisease" ~ "Heart",
    LongCause == "InfluenzaAndPneumonia" ~ "Flu",
    LongCause == "KidneyDisease" ~ "Kidney",
    LongCause == "LowerRespiratory" ~ "Lower Resp",
    LongCause == "OtherRespiratory" ~ "Other Resp",
    LongCause == "Septicemia" ~ "Septicemia",
    LongCause == "UnknownCauses" ~ "Unknown"
  )) %>%
  arrange(state,week_end,measure)

#Add population data - this originally was downloaded from: https://worldpopulationreview.com/state-rankings/state-densities#dataTable
USPopDensity <- read_csv("https://raw.githubusercontent.com/rollerb/cods/master/data/USPopDensity.csv")

#All years
StateCausesFullPop <- merge(StateCausesFull, USPopDensity, by.x="state", by.y="State",all=TRUE)
#create a field so we know how many cases by density
StateCausesFullPop <- StateCausesFullPop %>% mutate(ValuePop = value/Pop*100000)

#we don't need this as we previously had format with different names. 
StateCausesFullPop <- StateCausesFullPop %>% select(-starts_with("measure"))

#rename for backwards compatibility downstream
StateCausesFullPop <- StateCausesFullPop %>%
  rename(
    Year = "year",
    Week = "week",
    WeekEndDate = "week_end",
    Measure = "LongCause",
    Value = "value"
  )

#We need to create the column upstate separately so we don't break things downstream for code already submitted
StateCausesFullPop <- StateCausesFullPop %>% mutate(upstate = state)

#reordering JIC for downstream code. Don't think there is dependency, but putting here for safety
StateCausesFullPop <- StateCausesFullPop[, c(1,2,3,4,9,5,6,7,8,10,15,11,12,13,14)]

write_rds(StateCausesFullPop, "statecausesfullpop.rds", compress = "gz")

