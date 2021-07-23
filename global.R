library(shiny)
library(shinydashboard)

library(ggplot2)
library(ggthemes)

library(plyr)
library(dplyr)
library(readr)

library(jsonlite)
library(lubridate)
library(extrafont)
library(zoo)
library(plotly)
#library(Cairo) #for anti-aliasing

#options(shiny.usecairo=T)



#Date
today_date = "2021/07/22"

#reading
datasets = c("case","death","blueprint","main","hospicu","pcr")
for(i in 1:length(datasets)){
  detail <- datasets[i]
  assign(paste(detail,"_list", sep = ""),fromJSON(paste("json_data/covid_dataoccovid_",detail,"_csv.json",sep = "")))
}

#cleaning
blueprint_df <- blueprint_list$features$attributes
case_df <- case_list$features$attributes
death_df <- death_list$features$attributes
main_df <- main_list$features$attributes
hospicu_df <- hospicu_list$features$attributes
pcr_df <- pcr_list$features$attributes
names(case_df)[1] <- 'date'

#adding dates
#UPDATE DATE BY 1
date_seq = seq(as.Date("2020/01/22"),as.Date(today_date), "days")

#merging
#UPDATE NAS TO REMOVE
big_df = left_join(case_df,blueprint_df,by = c("date")) %>% left_join(y=death_df,by = c("date")) %>% left_join(y=main_df, by = c("date")) %>% left_join(y=hospicu_df, by = c("date"))%>% left_join(y = pcr_df, by = c("date"))
big_df <- big_df[-((length(date_seq)+1):nrow(case_df)),]

#select  only relevant features
names(big_df)
relevant <- c('date','total_cases_repo','daily_cases_repo','daily_cases_spec','daily_7day_avg.x','daily_dth.x','dth_date','total_dth_date','daily_neg_spec','daily_pos_spec','daily_spec','tot_pcr_pos','cdph_tpp','daily_test_repo','daily_7day_avg.y','daily_case_rate','positive_rate','new_var','hospital','icu')
df <- big_df %>% select(relevant)
df$date <- date_seq


#working with postivity and case rate

df %>% select('date','positive_rate')
non_missing_ind <- which(!is.na(df$positive_rate))
pos_week_ind <- seq(min(non_missing_ind)+1,length(df$positive_rate),7)
pos_week_ind
relevant_pos <- df$positive_rate[pos_week_ind]
relevant_case_rate <- df$daily_case_rate[pos_week_ind]
relevant_equity_index <- df$new_var[pos_week_ind]
relevant_date <- df$date[pos_week_ind]
pos_mini_df <- data.frame(start_week =format(relevant_date, format = "%m/%d"),end_week = format(relevant_date +6, format = "%m/%d"), week = 1:length(relevant_pos),positivity_for_week = relevant_pos, case_rate_for_week = relevant_case_rate, equity_index_for_week = relevant_equity_index)
pos_mini_df$equity_index_for_week[which(pos_mini_df$equity_index_for_week == 0)] <- NA

#exploring number of tests done


#WORK WITH COVID CAST
# df <- covidcast_signal(data_source = "usa-facts", 
#                        signal = "confirmed_cumulative_prop",
#                        start_day = "2020-12-31", end_day = "2020-12-31")
# plot(df)



