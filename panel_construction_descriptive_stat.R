#########################################################
# Panel Construction and Descriptive Statistics
#########################################################


# libraries
library(plm)
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
library(ggplot2)
library(gt)
library(stargazer)


# Log transformation
thesis_df <- thesis_df %>% 
  mutate(
    logcon = log(consumption),
    logworth = log(financial_worth),
    loghouseprice = log(houseprice),
    logrealcons = log(cons_real),
    logrealworth = log(worth_real),
    logrealhouseprice = log(houseprice_real)
  )


# panel data construction
thesis_df <-  thesis_df %>% 
  mutate(year_quarter = paste0(year, " ",quarter)
         )

panel_df <- thesis_df %>% pdata.frame(index = c("country_id", "year_quarter"))


#########################################################
# Descriptive Statistics
########################################################
# general statistics

desc_stat <- thesis_df %>% 
  select(c(logrealcons, logrealworth, logrealhouseprice, 
           private_credit_gdp, real_rate_q, cci_q)) %>% 
  sum_up()

# Country descriptive statistics
country_desc_stat <- thesis_df %>% group_by(country) %>% 
  select(c(logrealcons, logrealworth, logrealhouseprice, 
           private_credit_gdp, real_rate_q, cci_q)) %>% 
  sum_up()



