##################################
# Call Packages 
#################################
library(dynamac)
library(data.table)
library(dplyr)
library(plm)
library(AER)
library(dynlm)
library(ARDL)
library(plm)
library(tseries)
library(urca)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plm)
library(statar) 
library(zoo)
library(rio)
library(tseries)
library(lubridate)
library(psych)
library(seasonal)
library(knitr)
library(broom)
library(janitor)
library(flextable)
library(officer)
library(car)
library(estimatr)


######################################
# HICP / Price index data cleaning
#####################################

price_index_wide = read_excel("Book1.xlsx", sheet = 1, col_names = TRUE)

price_index_wide <- price_index_wide %>% rename(country = 1) %>% 
  filter(!is.na(country), country != "country") %>% 
  mutate(across(-country, as.character))

# transform wide to long
price_index_long <- price_index_wide %>% 
  pivot_longer(cols = -country,
               values_to = "Value",
               names_to = "Date") %>% 
  mutate(
    # Normalise column-name dates
    Date  = str_replace(Date, "(\\d{4})[^0-9]?(\\d{2}]).*", "\\1-\\2"),
    Date  = ym(Date),                                 # year-month
    Value = parse_number(Value)                       # numeric; turns "…" into NA safely
  ) %>%
  arrange(country, Date)
  
price_index_quarter <- price_index_long %>% 
  mutate(quarter_date = floor_date(Date, "quarter")) %>% 
  group_by(country, quarter_date) %>% 
  summarise(Value = mean(Value, na.rm =TRUE), .groups = "drop") %>% 
  mutate(year = year(quarter_date),
         quarter = paste0('Q', quarter(quarter_date))
  )

hicp_quarter <- price_index_quarter %>% 
  select(country, year, quarter, hicp = Value)




##############################################
# Cal. Inflation from HICP 
##############################################

inflation_quarter <- price_index_quarter %>% 
  arrange(country, quarter_date) %>% 
  group_by(country) %>% 
  mutate(
    # QoQ annual inflation
    pi_qa = 400 * (log(Value) - log(dplyr::lag(Value))),
    # YoY inflation
    pi_yoy = 100 * (log(Value) - log(dplyr::lag(Value, 4)))
  ) %>% 
  ungroup() %>% 
  select(country, year, quarter, pi_qa, pi_yoy)



##################################################################################
# Read Consumption, financial worth and other data for cleaning and transformation
##################################################################################


# Function to clean country column 
clean_country <- function(x) sub(" \\(.*\\)", "", x)

# Household consumption
consumption <- read.csv("consumption_adjusted.csv") %>% 
  separate(year_quarter, into = c("year", "quarter"), sep = "-") %>% 
  mutate(
    year = as.integer(year),
    country = clean_country(country)
  )


# Financial worth

financial_worth = read_excel("financial_worth.xlsx", sheet = "financial_worth") %>% 
  separate(year_quarter, into = c("quarter", "year"), sep = " ") %>% 
  mutate(
    year = as.integer(year),
    country = clean_country(country)
  )



# GDP (quarterly growth, etc.)
gdp_p <- read.csv("gdp_p.csv") %>%
  separate(year_quarter, into = c("year", "quarter"), sep = "-") %>%
  mutate(
    year    = as.integer(year),
    country = clean_country(country)
  )


# House price
house_price <- read.csv("house_price.csv") %>%
  separate(year_quarter, into = c("quarter", "year"), sep = " ") %>%
  mutate(
    year    = as.integer(year),
    country = clean_country(country)
  )

# Credit standards
credit_stand <- read.csv("credit_standard.csv") %>%
  separate(year_quarter, into = c("quarter", "year"), sep = " ") %>%
  mutate(
    year    = as.integer(year),
    country = clean_country(country)
  )

# Consumer Confidence Index (CCI) → quarterly
cci <- read.csv("CCI.csv")

cci_quarterly <- cci %>%
  mutate(
    time_mon     = as.yearmon(TIME_PERIOD),
    quarter_date = as.Date(as.yearqtr(time_mon))
  ) %>%
  group_by(country, quarter_date) %>%
  summarise(cci_q = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year    = year(quarter_date),
    quarter = paste0("Q", quarter(quarter_date)),
    country = clean_country(country)
  ) %>%
  select(country, year, quarter, cci_q)

# Short-term interest rates
short_term_interest <- read_excel("short_term_interest_rate.xlsx") %>%
  separate(year_quarter, into = c("year", "quarter"), sep = "-") %>%
  mutate(
    year    = as.integer(year),
    # If quarter is like "Q1", keep; if "1", change to "Q1"
    quarter = ifelse(str_detect(quarter, "Q"),
                     quarter,
                     paste0("Q", quarter)),
    country = clean_country(country)
  )

# Household credit / debt
credit <- read.csv("debt.csv") %>%
  separate(year_quarter, into = c("quarter", "year"), sep = " ") %>%
  mutate(
    year    = as.integer(year),
    country = clean_country(country)
  )

# Annual GDP (level)
gdp <- read.csv("gdp_1.csv") %>%
  separate(year_quarter, into = c("quarter", "year"), sep = " ") %>%
  mutate(
    year    = as.integer(year),
    country = clean_country(country)
  )

##################################################
# Merge all data ito one panel data
#################################################

data <- consumption %>% 
  # add financial worth
  left_join(financial_worth, by = c("country", "year", "quarter")) %>% 
  # add GDP
  left_join(gdp, by = c("country", "year", "quarter")) %>% 
  # add house prices
  left_join(house_price, by = c("country", "year", "quarter")) %>% 
  # add credit standard
  left_join(credit_stand, by = c("country", "year", "quarter")) %>% 
  # add CCI
  left_join(cci_quarterly, by = c("country", "year", "quarter")) %>% 
  # add HICP 
  left_join(hicp_quarter, by = c("country", "year", "quarter"))
  

###################################################
# Transform variables to real terms using HICP
##################################################


short_term_interest <- short_term_interest %>% 
  left_join(inflation_quarter, by = c("country", "year", "quarter")) %>% 
  mutate(
    real_rate_q = interest_rate - pi_qa,
    real_rate_y = interest_rate - pi_yoy,
    
    # using fisher equation
    real_rate_q_exact = 100 * ((1 + interest_rate/100) / (1 + pi_qa/100) - 1),
    real_rate_y_exact = 100 * ((1 + interest_rate/100) / (1 + pi_yoy/100) - 1)
  )


data <- data %>% 
  left_join(short_term_interest, by = c("country", "year", "quarter"))

# Private credit to gdp ratio

data <- data %>% 
  left_join(credit, by = c("country", "year", "quarter")) %>% 
  mutate(private_credit_gdp = (debt_loan/gdp)*100
         )

median_credit <- median(data$private_credit_gdp, na.rm = TRUE)
 # classify countries to high or low credit
country_classification <- data %>% group_by(country) %>% 
  summarise(country_mean = mean(private_credit_gdp, na.rm = TRUE),
         credit_group = ifelse(country_mean >= median_credit, 1, 0))

 data <- data %>% 
   left_join(country_classification, by = "country")


#make sure the relevant variables are numeric
 data <- data %>% 
   mutate(
     consumption = as.numeric(consumption),
     financial_worth = as.numeric(financial_worth),
     houseprice = as.numeric(houseprice),
     cci_q = as.numeric(cci_q),
     real_rate_q = as.numeric(real_rate_q),
     hicp = as.numeric(hicp)
    )

 
 # transform variable to real terms
 
data <- data %>% 
  mutate( 
    cons_real = consumption / (hicp/100),
    worth_real = financial_worth / (hicp/100),
    houseprice_real = houseprice/(hicp/100)
  )


data$country_id <- as.numeric(as.factor(data$country))

# Final thesis data
thesis_df <- data %>% 
  select(-c(DATE.x, DATE.y, country_code.x, country_code.y, country_code.x.x, country_code.y.y, country_new)
         )




