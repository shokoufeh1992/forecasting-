# forecasting-
sales forecasting
---
title: "final-forcast"
output: html_document
date: "2023-06-27"
---
# install required packages
```{r}
library("readxl")
library("writexl")
library("tidyverse")
library("forecast")
library("tseries")
library("greybox")
library("smooth")
library(ggplot2)
library(jalcal) # it is for converting jalali date to gregorian (jalali:iranian calendar)
library(fpp3) # it is for tsibble (time series table)
library(lubridate)
library(zoo)
library(slider)
library(openxlsx)

  totalsale <-read_excel(file.choose())  #insert the data 
  (totalsale)
  gredate <- as_tibble(totalsale$date)  
#converting the jalali date if your date is already in fregorian scape this step
#------------------------------------------------------------------------------------------------------------
  gredate %>% 
    separate(col = value, into = c("year", "month", "day")) -> gredat1
  gredat1$year <- as.integer(gredat1$year)
  gredat1$month <- as.integer(gredat1$month)
  gredat1$day <- as.integer(gredat1$day)
  
  gre_date <- data.frame("miladi" = rep(jal2greg(gredat1[1,1], gredat1[1,2], gredat1[1,3]) ,length(gredat1$year)))
  
  for(i in 1:length(gredat1$year)){
  gre_date$miladi[i] <- jal2greg(gredat1[i,1], gredat1[i,2], gredat1[i,3], asDate = T)
  }
  totalsale$date <- gre_date$miladi
#-------------------------------------------------------------------------------------------
#converting the table to a time series table (tsibble) 
  sale_tsib <- totalsale %>% 
    tsibble( index = date, key = c(code,name))

#Linear interpolation estimates the missing values 
  sale_tsib_fill<- sale_tsib %>% 
    fill_gaps() 
  na_fill<-na.approx(sale_tsib_fill$number)
  sale_tsib_fill$number<-as.integer(na_fill)
 # change the name
    totalsale_tidy<- sale_tsib_fill

#log transform to address heteroscedasticity
  log_totalsale_tidy <- totalsale_tidy %>% 
    mutate(log_number = log(number)) 
#fitting an arima model
  fit<- log_totalsale_tidy %>% 
    model(arima=ARIMA(log_number,greedy=FALSE,approximation=TRUE,stepwise=FALSE) )


#generating forecasts based on the model fit stored in the fit object
  forecast<- fit %>% 
    forecast(h=95,bootstrap=TRUE) %>% 
    hilo(50)  # create prediction intervals (e.g., 50% prediction intervals) around the point forecasts

#transform forecasted values back to their original scale
  forecast$plausible<- exp(forecast$.mean)
  forecast$pessimistic<- exp(forecast$'50%'$lower)
  forecast$optimistic<- exp(forecast$'50%'$upper)

# transform gregorian date back to the jalali date if your date is gregorian you can skip this step
#---------------------------------------------------------------------------------------------------------------------------------------------------------
  greg2jal2 <- function (year, month, day) 
  {
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    if (month < 1 | month > 12) 
      stop("month is outside the range 1-12")
    if (day < 1 | day > 31) 
      stop("day is outside the range 1-31")
    gdm <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 
      334)
    year2 <- ifelse(month > 2, year + 1, year)
    days <- 355666 + 365 * year + (3 + year2)%/%4 - (year2 + 
      99)%/%100 + (year2 + 399)%/%400 + day + gdm[month]
    jyear <- -1595 + 33 * (days%/%12053)
    days <- days%%12053
    jyear <- jyear + 4 * (days%/%1461)
    days <- days%%1461
    if (days > 365) {
      jyear <- jyear + (days - 1)%/%365
      days <- (days - 1)%%365
    }
    if (days < 186) {
      jmonth <- 1 + (days%/%31)
      jday <- 1 + days%%31
    }
    else {
      jmonth <- 7 + (days - 186)%/%30
      jday <- 1 + (days - 186)%%30
    }
    
    
    return(as.Date(paste(jyear, jmonth, jday ,sep = "-"), format = "%Y-%m-%d"))
  }
  jaldate <- as_tibble(forecast$date)

 jaldate1 <- jaldate %>% 
   separate(value,into = c("year","month", "day"))

jaldate1$year <- as.integer(jaldate1$year) 
jaldate1$month<- as.integer(jaldate1$month) 
jaldate1$day <- as.integer(jaldate1$day) 
view(jaldate1)

jal_date <- data.frame("jalali" = rep(greg2jal2(jaldate1[1,1], jaldate1[1,2], jaldate1[1,3]) ,length(jaldate1$year)))

for(i in 1:length( jaldate1$year)){
jal_date$jalali[i] <- greg2jal2(jaldate1[i,1], jaldate1[i,2], jaldate1[i,3])
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------
jaldate$value <- jal_date$jalali  
forecast$date2 <- jal_date$jalali
#montly plausible sales
forecast<-forecast %>%
  group_by(monthly = yearmonth(date2)) %>% 
  mutate(monthly_plausible = sum(plausible)) %>% 
  mutate(monthly_pessimistic = sum(pessimistic)) %>% 
  mutate(monthly_optimistic = sum(optimistic))
(forecast)
forecast$date2<-format(as.Date(forecast$date2,format="%Y/%m/%d"),"%Y-%m-%d")
# write data frame to Excel file using openxlsx
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", forecast)
saveWorkbook(wb, "dailyforecast.xlsx", overwrite = TRUE)

 fit %>% 
  forecast(h=95,bootstrap=TRUE) %>% 
  autoplot(filter(log_totalsale_tidy,year(date)>= 2023),c(50,60))


```


```{r}
augment(fit) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = log_number, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
    title = "نمودار انطباق مدل بر مقدار واقعی"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))
ggsave(filename = "plastonicmodel.png")
```

