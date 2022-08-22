###################################
# Energy Consumption 
###################################
#install.packages("xts")
#install.packages("zoo")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("tsibble")
library(xts)
library(zoo)
library(dplyr)
library(lubridate)
library(MASS)
library(tidyverse)

########################### 
# TASK 1: DATA CLEANING
########################### 
### Reading in time series files
aep <- read.csv("AEP_hourly.csv", header = T)
comed <- read.csv("COMED_hourly.csv", header = T)
dayton <- read.csv("DAYTON_hourly.csv", header = T)
deok <- read.csv("DEOK_hourly.csv", header = T)
dom <- read.csv("DOM_hourly.csv", header = T)
duq <- read.csv("DUQ_hourly.csv", header = T)
ekpc <- read.csv("EKPC_hourly.csv", header = T)
fe <- read.csv("FE_hourly.csv", header = T)
ni <- read.csv("NI_hourly.csv", header = T)
pjm_load <- read.csv("PJM_Load_hourly.csv", header = T)
pjme <- read.csv("PJME_hourly.csv", header = T)
pjmw <- read.csv("PJMW_hourly.csv", header = T)

### Creating backups of original variables
aep_original <- aep
comed_original <- comed
dayton_original <- dayton
deok_original <- deok
dom_original <- dom
duq_original <- duq
ekpc_original <- ekpc
fe_original <- fe
ni_original <- ni
pjm_load_original <- pjm_load
pjme_original <- pjme
pjmw_original <- pjmw

### Sorting by Datetime and sorting row names
aep <- aep[order(aep$Datetime), ]
comed <- comed[order(comed$Datetime), ]
dayton <- dayton[order(dayton$Datetime), ]
deok <- deok[order(deok$Datetime), ]
dom <- dom[order(dom$Datetime), ]
duq <- duq[order(duq$Datetime), ]
ekpc <- ekpc[order(ekpc$Datetime), ]
fe <- fe[order(fe$Datetime), ]
ni <- ni[order(ni$Datetime), ]
pjm_load <- pjm_load[order(pjm_load$Datetime), ]
pjme <- pjme[order(pjme$Datetime), ]
pjmw <- pjmw[order(pjmw$Datetime), ]

rownames(aep) <- seq_len(nrow(aep))
rownames(comed) <- seq_len(nrow(comed))
rownames(dayton) <- seq_len(nrow(dayton))
rownames(deok) <- seq_len(nrow(deok))
rownames(dom) <- seq_len(nrow(dom))
rownames(duq) <- seq_len(nrow(duq))
rownames(ekpc) <- seq_len(nrow(ekpc))
rownames(fe) <- seq_len(nrow(fe))
rownames(ni) <- seq_len(nrow(ni))
rownames(pjm_load) <- seq_len(nrow(pjm_load))
rownames(pjme) <- seq_len(nrow(pjme))
rownames(pjmw) <- seq_len(nrow(pjmw))

### Checking for NA values (none found)
sum(is.na(c(aep$AEP_MW, comed$COMED_MW, dayton$DAYTON_MW, deok$DEOK_MW,
            dom$DOM_MW, duq$DUQ_MW, ekpc$EKPC_MW, fe$FE_MW, ni$NI_MW,
            pjm_load$PJM_Load_MW, pjme$PJME_MW, pjmw$PJMW_MW)))

### Checking start and end dates
time_series <- c("aep", "comed", "dayton",
                 "deok", "dom", "duq",
                 "ekpc", "fe", "ni",
                 "pjm_load", "pjme", "pjmw")
start_date <- c(aep$Datetime[1], comed$Datetime[1], dayton$Datetime[1],
                deok$Datetime[1], dom$Datetime[1], duq$Datetime[1],
                ekpc$Datetime[1], fe$Datetime[1], ni$Datetime[1],
                pjm_load$Datetime[1], pjme$Datetime[1], pjmw$Datetime[1])
end_date <- c(aep$Datetime[nrow(aep)], comed$Datetime[nrow(comed)],
              dayton$Datetime[nrow(dayton)], deok$Datetime[nrow(deok)],
              dom$Datetime[nrow(dom)], duq$Datetime[nrow(duq)],
              ekpc$Datetime[nrow(ekpc)], fe$Datetime[nrow(fe)],
              ni$Datetime[nrow(ni)], pjm_load$Datetime[nrow(pjm_load)],
              pjme$Datetime[nrow(pjme)], pjmw$Datetime[nrow(pjmw)])
time_series_dates <- cbind(time_series, start_date, end_date)
time_series_dates

### Data Cleaning: AEP 
# Data
head(aep)
dim(aep)

# Checking for duplicates entries 
dup_entries = aep[duplicated(aep),]
dup_entries

# Checking for duplicates timestamps 
dup_timestamp = aep[duplicated(aep$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
aep = aep %>% distinct(Datetime, .keep_all = TRUE)    
dim(aep)

# Number of missing timestamps
date = as.POSIXct(aep$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(aep)), aep$AEP_MW, type = "l")

### Data Cleaning: COMED 
# Data
head(comed)
dim(comed)

# Checking for duplicates entries 
dup_entries = comed[duplicated(comed),]
dup_entries

# Checking for duplicates timestamps 
dup_timestamp = comed[duplicated(comed$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
comed = comed %>% distinct(Datetime, .keep_all = TRUE)    
dim(comed)

# Number of missing timestamps
date = as.POSIXct(comed$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(comed)), comed$COMED_MW, type = "l")

### Data Cleaning: DAYTON 
# Data
head(dayton)
dim(dayton)

# Checking for duplicates entries 
dup_entries = comed[duplicated(dayton),]
dup_entries

# Checking for duplicates timestamps 
dup_timestamp = dayton[duplicated(dayton$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
dayton = dayton %>% distinct(Datetime, .keep_all = TRUE)     
dim(dayton)

# Number of missing timestamps
date = as.POSIXct(dayton$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - 2 outliers 
plot(seq_len(nrow(dayton)), dayton$DAYTON_MW, type = "l")
outliers_index = which(dayton$DAYTON_MW <= 1000)
outliers_index
dayton[outliers_index,]


### Data Cleaning: DEOK 
# Data
head(deok)
dim(deok)

# Checking for duplicates entries
dup_entries = deok[duplicated(deok),]
dup_entries

# Checking for duplicates timestamps
dup_timestamp = deok[duplicated(deok$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
deok = deok %>% distinct(Datetime, .keep_all = TRUE)     
dim(deok)

# Number of missing timestamps
date = as.POSIXct(deok$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers 
plot(seq_len(nrow(deok)), deok$DEOK_MW, type = "l")
outliers_index = which(deok$DEOK_MW <= 1500)
deok[outliers_index,]

### Data Cleaning: DOM 
# Data
head(dom)
dim(dom)

# Checking for duplicates entries 
dup_entries = dom[duplicated(dom),]
dup_entries

# Checking for duplicates timestamps 
dup_timestamp = dom[duplicated(dom$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
dom = dom %>% distinct(Datetime, .keep_all = TRUE)     
dim(dom)

# Number of missing timestamps
date = as.POSIXct(dom$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - 1 outlier
plot(seq_len(nrow(dom)), dom$DOM_MW, type = "l")
outliers_index = which(dom$DOM_MW <= 5000)
outliers_index
dom[outliers_index,]


### Data Cleaning: DUQ 
# Data
head(duq)
dim(duq)

# Checking for duplicates entries 
dup_entries = duq[duplicated(duq),]
dup_entries

# Checking for duplicates timestamps 
dup_timestamp = duq[duplicated(duq$Datetime),]
dup_timestamp

# Removing duplicates timestamps      
duq = duq %>% distinct(Datetime, .keep_all = TRUE)     
dim(duq)

# Number of missing timestamps
date = as.POSIXct(duq$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(duq)), duq$DUQ_MW, type = "l")

### Data Cleaning: EKPC 
# Data
head(ekpc)
dim(ekpc)

# Checking for duplicates entries 
identical(ekpc, unique(ekpc))

# Checking for duplicates timestamps 
identical(ekpc$Datetime, unique(ekpc$Datetime)) #FALSE

# Removing duplicates timestamps      
dup_name_ekpc = names(table(ekpc$Datetime)[table(ekpc$Datetime) > 1])
ekpc_duplicates = ekpc[ekpc$Datetime %in% dup_name_ekpc,]
e = as.numeric(row.names(ekpc_duplicates)[c(2,4,6,8)])
ekpc = ekpc[-e,]
dim(ekpc)

# Number of missing timestamps
date = as.POSIXct(ekpc$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(ekpc)), ekpc$EKPC_MW, type = "l")

### Data Cleaning: FE 
# Data
head(ekpc)
dim(ekpc)

# Checking for duplicates entries 
identical(fe, unique(fe))

# Checking for duplicates timestamps 
identical(fe$Datetime, unique(fe$Datetime)) #FALSE

# Removing duplicates timestamps      
dup_name_fe = names(table(fe$Datetime)[table(fe$Datetime) > 1])
fe_duplicates = fe[fe$Datetime %in% dup_name_fe,]
f = as.numeric(row.names(fe_duplicates)[c(2,4,6,8)])
fe = fe[-f,]
dim(fe)

# Number of missing timestamps
date = as.POSIXct(fe$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - 1 outlier
plot(seq_len(nrow(fe)), fe$FE_MW, type = "l")
outliers_index = which(fe$FE_MW <= 500)
outliers_index
fe[outliers_index,]

### Data Cleaning: NI 
# Data
head(ni)
dim(ni)

# Checking for duplicates entries 
identical(ni, unique(ni))

# Checking for duplicates timestamps 
identical(ni$Datetime, unique(ni$Datetime)) #TRUE

# Removing duplicates timestamps - unnecessary

# Number of missing timestamps
date = as.POSIXct(ni$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(ni)), ni$NI_MW, type = "l")

### Data Cleaning: PJM_LOAD 
# Data
head(pjm_load)
dim(pjm_load)

# Checking for duplicates entries 
identical(pjm_load, unique(pjm_load))

# Checking for duplicates timestamps 
identical(pjm_load$Datetime, unique(pjm_load$Datetime)) #TRUE

# Removing duplicates timestamps - unnecessary 

# Number of missing timestamps
date = as.POSIXct(pjm_load$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(pjm_load)), pjm_load$PJM_Load_MW, type = "l")
head(sort(pjm_load$PJM_Load_MW))
tail(sort(pjm_load$PJM_Load_MW))

### Data Cleaning: PJME 
# Data
head(pjme)
dim(pjme)

# Checking for duplicates entries 
identical(pjme, unique(pjme))

# Checking for duplicates timestamps 
identical(pjme$Datetime, unique(pjme$Datetime)) #FALSE

# Removing duplicates timestamps  
pjme_duplicate_dates <- table(pjme$Datetime)[table(pjme$Datetime) > 1]
pjme_duplicate_dates
# Two entries at 2014-11-02 02:00:00
# Two entries at 2015-11-01 02:00:00
# Two entries at 2016-11-06 02:00:00
# Two entries at 2017-11-05 02:00:00

pjme_duplicates <- pjme[pjme$Datetime %in% names(pjme_duplicate_dates), ]
pjme_removals <- rownames(pjme_duplicates)[seq(2, nrow(pjme_duplicates),
                                               by = 2)]
pjme <- pjme[-as.numeric(pjme_removals), ]
identical(pjme$Datetime, unique(pjme$Datetime))

# Number of missing timestamps
date = as.POSIXct(pjme$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - none
plot(seq_len(nrow(pjme)), pjme$PJME_MW, type = "l")
head(sort(pjme$PJME_MW))
tail(sort(pjme$PJME_MW))
which.min(pjme$PJME_MW)
pjme[94890:94910, ]
plot(pjme$PJME_MW[94800:95000], type = "l")

### Data Cleaning: PJMW 
# Data
head(pjmw)
dim(pjmw)

# Checking for duplicates entries 
identical(pjmw, unique(pjmw))

# Checking for duplicates timestamps 
identical(pjmw$Datetime, unique(pjmw$Datetime)) #FALSE

# Removing duplicates timestamps  
pjmw_duplicate_dates <- table(pjmw$Datetime)[table(pjmw$Datetime) > 1]
pjmw_duplicate_dates
# Two entries at 2014-11-02 02:00:00
# Two entries at 2015-11-01 02:00:00
# Two entries at 2016-11-06 02:00:00
# Two entries at 2017-11-05 02:00:00

pjmw_duplicates <- pjmw[pjmw$Datetime %in% names(pjmw_duplicate_dates), ]
pjmw_removals <- rownames(pjmw_duplicates)[seq(2, nrow(pjmw_duplicates),
                                               by = 2)]
pjmw <- pjmw[-as.numeric(pjmw_removals), ]
identical(pjmw$Datetime, unique(pjmw$Datetime))

# Number of missing timestamps
date = as.POSIXct(pjmw$Datetime, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(date))

# Date of missing timestamps - due to daylight savings
index = which(is.na(date))
mat = vapply(index, function(x){(x-1):(x+1)}, numeric(3))

for (i in seq(ncol(mat))) {
  print(date[mat[,i]])
}

# Checking for outliers - 1 outlier
plot(seq_len(nrow(pjmw)), pjmw$PJMW_MW, type = "l")
head(sort(pjmw$PJMW_MW))
tail(sort(pjmw$PJMW_MW))
which.min(pjmw$PJMW_MW)
pjmw[10140:10160, ]
plot(pjmw$PJMW_MW[10100:10200], type = "l")
pjmw[87640:87680, ]
plot(pjmw$PJMW_MW[87600:87700], type = "l")

### Creating backups of cleaned variables
aep_cleaned <- aep
comed_cleaned <- comed
dayton_cleaned <- dayton
deok_cleaned <- deok
dom_cleaned <- dom
duq_cleaned <- duq
ekpc_cleaned <- ekpc
fe_cleaned <- fe
ni_cleaned <- ni
pjm_load_cleaned <- pjm_load
pjme_cleaned <- pjme
pjmw_cleaned <- pjmw

########################### 
# TASK 1: CREATE TSIBBLE
########################### 
library(tsibble)
aep_tsibble = aep %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime) 

comed_tsibble = comed %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

dayton_tsibble = dayton %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

deok_tsibble <- deok %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

dom_tsibble <- dom %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

duq_tsibble <- duq %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

ekpc_tsibble = ekpc %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

fe_tsibble = fe %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

ni_tsibble = ni %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

pjm_load_tsibble <- pjm_load %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

pjme_tsibble <- pjme %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)

pjmw_tsibble <- pjmw %>%
  mutate(Datetime = ymd_hms(Datetime)) %>%
  as_tsibble(index = Datetime)


########################### 
# TASK 2: PLOTS ANALYSIS 
########################### 
### Plots Analysis: AEP
# Create xts object (sorted in chronological order)
aep_mw = aep$AEP_MW
head(aep$Datetime)
aep_date = as_datetime(aep$Datetime)
aep.xts = xts(aep_mw, 
              order.by = aep_date, 
              frequency="hours")
head(aep.xts)
tail(aep.xts)
class(aep.xts)
str(aep.xts)

# Time Plot of AEP
library(dygraphs)
dygraph(aep.xts,main="AEP Energy Consumption with multiple seasonalities",
        ylab = "Energy Consumption (MW)") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(aep_date); head(hour); length(hour)
year = year(aep_date); head(year); length(year)
week = week(aep_date); head(week); length(week)
day = day(aep_date); head(day); length(day)
weekday = wday(aep_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(aep_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
aep_data = data.frame(year, month, day, week, weekday, hour, aep_mw)
aep_newdata = aep_data[order(year, month, day, week, weekday, hour),]
head(aep_newdata)
tail(aep_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday.
boxplot(aep_mw~month,lwd=1.5,cex=1.5, 
        main="(a) AEP: Higher consumption in Winter and Summer months\nthan other months",
        ylab="Energy Consumption (MW)")
boxplot(aep_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) AEP: Lower consumption during the night\nthan during the day",
        ylab="Energy Consumption (MW)")
boxplot(aep_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) AEP: Lower consumption on Saturday and Sunday\nthan other days of the week",
        ylab="Energy Consumption (MW)")
boxplot(aep_mw~week, lwd=1.5,cex=1.5, 
        main="(d) AEP: Lower consumption on weeks 16-20 and \n weeks 40-44 than other days of the week",
        ylab="Energy Consumption (MW)")
boxplot(aep_mw~day, lwd=1.5,cex=1.5, 
        main="(e) AEP: Not much seasonality when comparing between\ndays of the month",
        ylab="Energy Consumption (MW)")

# ACFs with different seasonalities
aep_core = coredata(aep.xts)
dt_start = ymd_hms(start(aep.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 275 of the year at hour 1
aep_day = ts(aep_core, start = c(275, 1), frequency = 24)
acf(aep_day,
    main = "(a) AEP: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 40 of the year at hour 25
aep_week = ts(aep_core, start = c(40, 25), frequency = 168)
acf(aep_week, lag = 400,
    main = "(b) AEP: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 10 of the year at hour 1
aep_month = ts(aep_core, start = c(10, 1), frequency = 730.5)
acf(aep_month, lag = 1500,
    main = "(c) AEP: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2004 at hour 6577
aep_year = ts(aep_core, start = c(2004, 6577), frequency = 8766)
acf(aep_year, lag = 18000,
    main = "(d) AEP: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: COMED
# Create xts object (sorted in chronological order)
comed_mw = comed$COMED_MW
comed_date = as_datetime(comed$Datetime)
comed.xts = xts(comed_mw, 
                order.by = comed_date, 
                frequency="hours")
head(comed.xts)
tail(comed.xts)
class(comed.xts)
str(comed.xts)

# Time Plot of COMED
library(dygraphs)
dygraph(comed.xts,main="comed_energy Consumption with multiple seasonalities",
        ylab = "comed_energy Consumption (comed_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(comed_date); head(hour); length(hour)
year = year(comed_date); head(year); length(year)
week = week(comed_date); head(week); length(week)
day = day(comed_date); head(day); length(day)
weekday = wday(comed_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(comed_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
comed_data = data.frame(year, month, day, week, weekday, hour, comed_mw)
comed_newdata = comed_data[order(year, month, day, week, weekday, hour),]
head(comed_newdata,60)
tail(comed_newdata,60)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(comed_mw~month,lwd=1.5,cex=1.5, 
        main="(a) Higher consumption in Winter and Summer months \nthan other months",
        ylab="comed_energy consumption (comed_mw)")
boxplot(comed_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) Lower consumption during the night\nthan during the day",
        ylab="comed_energy consumption (comed_mw)" )
boxplot(comed_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="comed_energy consumption (comed_mw)")
boxplot(comed_mw~week, lwd=1.5,cex=1.5, 
        main="(d) Lower consumption on weeks 16-20  and \n weeks 40-44 than other weeks of the year",
        ylab="comed_energy consumption (comed_mw)")
boxplot(comed_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (comed_mw)")

# ACFs with different seasonalities
comed_core = coredata(comed.xts)
dt_start = ymd_hms(start(comed.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 1 of the year at hour 1
comed_day = ts(comed_core, start = c(1, 1), frequency = 24)
acf(comed_day,
    main = "(a) comed: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 1 of the year at hour 1
comed_week = ts(comed_core, start = c(1, 1), frequency = 168)
acf(comed_week, lag = 400,
    main = "(b) comed: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 1 of the year at hour 1
comed_month = ts(comed_core, start = c(1, 1), frequency = 730.5)
acf(comed_month, lag = 1500,
    main = "(c) comed: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2011 at hour 1
comed_year = ts(comed_core, start = c(2011, 1), frequency = 8766)
acf(comed_year, lag = 18000,
    main = "(d) comed: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: DAYTON
# Create xts object (sorted in chronological order)
dayton_mw = dayton$DAYTON_MW
dayton_date = as_datetime(dayton$Datetime)
dayton.xts = xts(dayton_mw, 
                 order.by = dayton_date, 
                 frequency="hours")
head(dayton.xts)
tail(dayton.xts)
class(dayton.xts)
str(dayton.xts)

# Time Plot of DAYTON
library(dygraphs)
dygraph(dayton.xts,main="dayton_energy Consumption with multiple seasonalities",
        ylab = "dayton_energy Consumption (dayton_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(dayton_date); head(hour); length(hour)
year = year(dayton_date); head(year); length(year)
week = week(dayton_date); head(week); length(week)
day = day(dayton_date); head(day); length(day)
weekday = wday(dayton_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dayton_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
dayton_data = data.frame(year, month, day, week, weekday, hour, dayton_mw)
dayton_newdata = dayton_data[order(year, month, day, week, weekday, hour),]
head(dayton_newdata)
tail(dayton_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(dayton_mw~month,lwd=1.5,cex=1.5, 
        main="(a) DAYTON: Higher consumption in Winter and Summer months \nthan other months",
        ylab="dayton_energy consumption (dayton_mw)")
boxplot(dayton_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) DAYTON: Lower consumption during the night\nthan during the day",
        ylab="dayton_energy consumption (dayton_mw)" )
boxplot(dayton_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) DAYTON: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="dayton_energy consumption (dayton_mw)")
boxplot(dayton_mw~week, lwd=1.5,cex=1.5, 
        main="(d) DAYTON: Lower consumption on weeks 14-21 and \nweeks 40-44 than other weeks of the year",
        ylab="dayton_energy consumption (dayton_mw)")
boxplot(dayton_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) DAYTON: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (dayton_mw)")

# ACFs with different seasonalities
dayton_core = coredata(dayton.xts)
dt_start = ymd_hms(start(dayton.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 275 of the year at hour 1
dayton_day = ts(dayton_core, start = c(275, 1), frequency = 24)
acf(dayton_day,
    main = "(a) dayton: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 40 of the year at hour 25
dayton_week = ts(dayton_core, start = c(40, 25), frequency = 168)
acf(dayton_week, lag = 400,
    main = "(b) dayton: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 10 of the year at hour 1
dayton_month = ts(dayton_core, start = c(10, 1), frequency = 730.5)
acf(dayton_month, lag = 1500,
    main = "(c) dayton: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2004 at hour 6577
dayton_year = ts(dayton_core, start = c(2004, 6577), frequency = 8766)
acf(dayton_year, lag = 18000,
    main = "(d) dayton: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: DEOK
# Create xts object (sorted in chronological order)
deok_mw = deok$DEOK_MW
deok_date = as_datetime(deok$Datetime)
deok.xts = xts(deok_mw,
               order.by = deok_date, 
               frequency = "hours")

acf(deok_tsibble$DEOK_MW, lag = 50)

# Time Plot of DEOK
library(dygraphs)
dygraph(deok.xts,main="deok_energy Consumption with multiple seasonalities",
        ylab = "deok_energy Consumption (deok_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(deok_date); head(hour); length(hour)
year = year(deok_date); head(year); length(year)
week = week(deok_date); head(week); length(week)
day = day(deok_date); head(day); length(day)
weekday = wday(deok_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(deok_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
deok_data = data.frame(year, month, day, week, weekday, hour, deok_mw)
deok_newdata = deok_data[order(year, month, day, week, weekday, hour),]
head(deok_newdata)
tail(deok_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(deok_mw~month,lwd=1.5,cex=1.5, 
        main="(a) DEOK: Higher consumption in Winter and Summer months \nthan other months",
        ylab="deok_energy consumption (deok_mw)")
boxplot(deok_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) DEOK: Lower consumption during the night\nthan during the day",
        ylab="deok_energy consumption (deok_mw)" )
boxplot(deok_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) DEOK: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="deok_energy consumption (deok_mw)")
boxplot(deok_mw~week, lwd=1.5,cex=1.5, 
        main="(d) DEOK: Lower consumption on weeks 14-21 and \nweeks 37-44 than other weeks of the year",
        ylab="deok_energy consumption (deok_mw)")
boxplot(deok_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) DEOK: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (deok_mw)")

# ACFs with different seasonalities
deok_core = coredata(deok.xts)
dt_start = ymd_hms(start(deok.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 1 of the year at hour 1
deok_day = ts(deok_core, start = c(1, 1), frequency = 24)
acf(deok_day,
    main = "(a) deok: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 1 of the year at hour 1
deok_week = ts(deok_core, start = c(1, 1), frequency = 168)
acf(deok_week, lag = 400,
    main = "(b) deok: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 1 of the year at hour 1
deok_month = ts(deok_core, start = c(1, 1), frequency = 730.5)
acf(deok_month, lag = 1500,
    main = "(c) deok: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2012 at hour 1
deok_year = ts(deok_core, start = c(2012, 1), frequency = 8766)
acf(deok_year, lag = 18000,
    main = "(d) deok: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: DOM
# Create xts object (sorted in chronological order)
dom_mw = dom$DOM_MW
dom_date = as_datetime(dom$Datetime)
dom.xts = xts(dom_mw,
              order.by = dom_date, 
              frequency = "hours")

acf(dom_tsibble$DOM_MW, lag = 50)

# Time Plot of DOM
library(dygraphs)
dygraph(dom.xts,main="dom_energy Consumption with multiple seasonalities",
        ylab = "dom_energy Consumption (dom_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(dom_date); head(hour); length(hour)
year = year(dom_date); head(year); length(year)
week = week(dom_date); head(week); length(week)
day = day(dom_date); head(day); length(day)
weekday = wday(dom_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dom_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
dom_data = data.frame(year, month, day, week, weekday, hour, dom_mw)
dom_newdata = dom_data[order(year, month, day, week, weekday, hour),]
head(dom_newdata)
tail(dom_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(dom_mw~month,lwd=1.5,cex=1.5, 
        main="(a) DOM: Higher consumption in Winter and Summer months \nthan other months",
        ylab="dom_energy consumption (dom_mw)")
boxplot(dom_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) DOM: Lower consumption during the night\nthan during the day",
        ylab="dom_energy consumption (dom_mw)" )
boxplot(dom_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) DOM: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="dom_energy consumption (dom_mw)")
boxplot(dom_mw~week, lwd=1.5,cex=1.5, 
        main="(d) DOM: Lower consumption on weeks 13-21 and \nweeks 37-44 than other weeks of the year",
        ylab="dom_energy consumption (dom_mw)")
boxplot(dom_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) DOM: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (dom_mw)")

# ACFs with different seasonalities
dom_core = coredata(dom.xts)
dt_start = ymd_hms(start(dom.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 121 of the year at hour 1
dom_day = ts(dom_core, start = c(121, 1), frequency = 24)
acf(dom_day,
    main = "(a) dom: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 18 of the year at hour 25
dom_week = ts(dom_core, start = c(18, 25), frequency = 168)
acf(dom_week, lag = 400,
    main = "(b) dom: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 5 of the year at hour 1
dom_month = ts(dom_core, start = c(5, 1), frequency = 730.5)
acf(dom_month, lag = 1500,
    main = "(c) dom: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2005 at hour 2881
dom_year = ts(dom_core, start = c(2005, 2881), frequency = 8766)
acf(dom_year, lag = 18000,
    main = "(d) dom: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: DUQ
# Create xts object (sorted in chronological order)
duq_mw = duq$DUQ_MW
duq_date = as_datetime(duq$Datetime)
duq.xts = xts(duq_mw,
              order.by = duq_date, 
              frequency = "hours")

acf(duq_tsibble$DUQ_MW, lag = 50)

# Time Plot of DUQ
library(dygraphs)
dygraph(duq.xts,main="duq_energy Consumption with multiple seasonalities",
        ylab = "duq_energy Consumption (duq_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(duq_date); head(hour); length(hour)
year = year(duq_date); head(year); length(year)
week = week(duq_date); head(week); length(week)
day = day(duq_date); head(day); length(day)
weekday = wday(duq_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(duq_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
duq_data = data.frame(year, month, day, week, weekday, hour, duq_mw)
duq_newdata = duq_data[order(year, month, day, week, weekday, hour),]
head(duq_newdata)
tail(duq_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(duq_mw~month,lwd=1.5,cex=1.5, 
        main="(a) DUQ: Higher consumption in Winter and Summer months \nthan other months",
        ylab="duq_energy consumption (duq_mw)")
boxplot(duq_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) DUQ: Lower consumption during the night\nthan during the day",
        ylab="duq_energy consumption (duq_mw)" )
boxplot(duq_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) DUQ: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="duq_energy consumption (duq_mw)")
boxplot(duq_mw~week, lwd=1.5,cex=1.5, 
        main="(d) DUQ: Lower consumption on weeks 14-21 and \nweeks 37-44 than other weeks of the year",
        ylab="duq_energy consumption (duq_mw)")
boxplot(duq_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) DUQ: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (duq_mw)")

# ACFs with different seasonalities
duq_core = coredata(duq.xts)
dt_start = ymd_hms(start(duq.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 1 of the year at hour 1
duq_day = ts(duq_core, start = c(1, 1), frequency = 24)
acf(duq_day,
    main = "(a) duq: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 1 of the year at hour 1
duq_week = ts(duq_core, start = c(1, 1), frequency = 168)
acf(duq_week, lag = 400,
    main = "(b) duq: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 1 of the year at hour 1
duq_month = ts(duq_core, start = c(1, 1), frequency = 730.5)
acf(duq_month, lag = 1500,
    main = "(c) duq: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2005 at hour 1
duq_year = ts(duq_core, start = c(2005, 1), frequency = 8766)
acf(duq_year, lag = 18000,
    main = "(d) duq: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)

### Plots Analysis: EKPC
# Create xts object (sorted in chronological order)
ekpc_mw = ekpc$EKPC_MW
ekpc_date = as_datetime(ekpc$Datetime)
ekpc.xts = xts(ekpc_mw,
               order.by = ekpc_date, 
               frequency = "hours")

head(ekpc.xts, 40)
tail(ekpc.xts)
class(ekpc.xts)
str(ekpc.xts)

# Time Plot of EKPC
dygraph(ekpc.xts,main="EKPC_energy Consumption with multiple seasonalities",
        ylab = "EKPC_energy Consumption (ekpc_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(ekpc_date); head(hour); length(hour)
year = year(ekpc_date); head(year); length(year)
week = week(ekpc_date); head(week); length(week)
day = day(ekpc_date)
length(hour)
weekday = wday(ekpc_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ekpc_date)],levels=month.abb); length(month)

# Create data frame with extracted elmts
ekpc_data = data.frame(year, month, day, week, weekday, hour, ekpc_mw)
ekpc_newdata <- ekpc_data[order(year,month,day,week,weekday, hour),]
head(ekpc_newdata)
tail(ekpc_newdata)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(ekpc_mw~month,lwd=1.5,cex=1.5, 
        main="(a) EKPC: Higher consumption in Winter and Summer months than other months",
        ylab="Energy consumption (EKPW_MW)")
boxplot(ekpc_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) EKPC: Lower consumption during the night\nthan during the day",
        ylab="Energy consumption (EKPW_MW)" )
boxplot(ekpc_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) EKPC: Lower consumption on Saturday and Sunday\nthan other days of the week",
        ylab="Energy consumption (EKPW_MW)")
boxplot(ekpc_mw~week, lwd=1.5,cex=1.5, 
        main="(d) EKPC: Lower consumption on weeks 16-20 and \n week 40-44 than other days of the week \n Higher consumption on weeks 21-39",
        ylab="Energy consumption (EKPW_MW)")
boxplot(ekpc_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) EKPC: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (ekpc_mw)")

# ACFs with different seasonalities
ekpc_core = coredata(ekpc.xts)
dt_start = ymd_hms(start(ekpc.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 152 of the year at hour 1
ekpc_day = ts(ekpc_core, start = c(152, 1), frequency = 24)
acf(ekpc_day,
    main = "(a) ekpc: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 22 of the year at hour 97
ekpc_week = ts(ekpc_core, start = c(22, 97), frequency = 168)
acf(ekpc_week, lag = 400,
    main = "(b) ekpc: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 6 of the year at hour 1
ekpc_month = ts(ekpc_core, start = c(6, 1), frequency = 730.5)
acf(ekpc_month, lag = 1500,
    main = "(c) ekpc: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2013 at hour 3625
ekpc_year = ts(ekpc_core, start = c(2013, 3625), frequency = 8766)
acf(ekpc_year, lag = 18000,
    main = "(d) ekpc: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


### Plots Analysis: FE
# Create xts object (sorted in chronological order)
fe_mw = fe$FE_MW
fe_date = as_datetime(fe$Datetime)
fe.xts = xts(fe_mw, 
             order.by=fe_date, 
             frequency="hours")

head(fe.xts,40)
tail(fe.xts)
class(fe.xts)
str(fe.xts)

# Time Plot of FE
dygraph(fe.xts,main="fe_energy Consumption with multiple seasonalities",
        ylab = "fe_energy Consumption (fe_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(fe_date); head(hour); length(hour)
year = year(fe_date); head(year); length(year)
week = week(fe_date); head(week); length(week)
day = day(fe_date); head(day); length(day)
weekday = wday(fe_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(fe_date)], levels=month.abb); length(month)

# Create data frame with extracted elmts
fe_data = data.frame(year, month, day, week, weekday, hour, fe_mw)
fe_newdata = fe_data[order(year,month,day,week,weekday, hour),]
head(fe_newdata,60)
tail(fe_newdata,60)

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(fe_mw~month,lwd=1.5,cex=1.5, 
        main="(a) FE: Higher consumption in Winter and Summer months than other months",
        ylab="Energy consumption (FE_MW)")
boxplot(fe_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) FE: Lower consumption during the night\nthan during the day",
        ylab="Energy consumption (FE_MW)" )
boxplot(fe_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) FE: Lower consumption on Saturday and Sunday\nthan other days of the week",
        ylab="Energy consumption (FE_MW)")
boxplot(fe_mw~week, lwd=1.5,cex=1.5, 
        main="(d) FE: Lower consumption on weeks 14-20 and \n week 40-44 than other days of the week \n Higher consumption on weeks 21-39",
        ylab="Energy consumption (FE_MW)")
boxplot(fe_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) FE: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (fe_mw)")

# ACFs with different seasonalities
fe_core = coredata(fe.xts)
dt_start = ymd_hms(start(fe.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 152 of the year at hour 1
fe_day = ts(fe_core, start = c(152, 1), frequency = 24)
acf(fe_day,
    main = "(a) fe: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 22 of the year at hour 97
fe_week = ts(fe_core, start = c(22, 97), frequency = 168)
acf(fe_week, lag = 400,
    main = "(b) fe: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 6 of the year at hour 1
fe_month = ts(fe_core, start = c(6, 1), frequency = 730.5)
acf(fe_month, lag = 1500,
    main = "(c) fe: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2011 at hour 3625
fe_year = ts(fe_core, start = c(2011, 3625), frequency = 8766)
acf(fe_year, lag = 18000,
    main = "(d) fe: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


### Plots Analysis: NI
# Create xts object (sorted in chronological order)
ni_mw = ni$NI_MW
ni_date = as_datetime(ni$Datetime)
ni.xts = xts(ni_mw,
             order.by = ni_date, 
             frequency = "hours")

head(ni.xts,40)
tail(ni.xts)
class(ni.xts)
str(ni.xts)

# Time Plot of NI
dygraph(ni.xts,main="ni_energy Consumption with multiple seasonalities",
        ylab = "ni_energy Consumption (ni_mw") %>% dyRangeSelector

# Further study of the seasonality at different levels of granurality. 
# Extract the hour, week and month 
hour = hour(ni_date); head(hour); length(hour)
year = year(ni_date); head(year); length(year)
week = week(ni_date); head(week); length(week)
day = day(ni_date); head(day); length(day)
weekday = wday(ni_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ni_date)],levels=month.abb); length(month)

# Create data frame with extracted elmts
ni_data = data.frame(year, month, day, week, weekday, hour, ni_mw)
ni_newdata <- ni_data[order(year,month,day,week,weekday, hour),]

# Box Plot of month, hour and weekday. 
# Inspecting seasonality by month, hour and weekday. 
boxplot(ni_mw~month,lwd=1.5,cex=1.5, 
        main="(a) NI: Higher consumption in Winter and Summer months \nthan other months",
        ylab="Energy consumption (NI_MW)")
boxplot(ni_mw~hour,lwd=1.5,cex=1.5, 
        main="(b) NI: Lower consumption during the night\nthan during the day",
        ylab="Energy consumption (NI_MW)" )
boxplot(ni_mw~weekday, lwd=1.5,cex=1.5, 
        main="(c) NI: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab="Energy consumption (NI_MW)")
boxplot(ni_mw~week, lwd=1.5,cex=1.5, 
        main="(d) NI: Lower consumption on weeks 14-20 and \n week 40-44 than other days of the week \n Higher consumption on weeks 21-39",
        ylab="Energy consumption (NI_MW)")
boxplot(ni_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) NI: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (ni_mw)")

# ACFs with different seasonalities
ni_core = coredata(ni.xts)
dt_start = ymd_hms(start(ni.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 122 of the year at hour 1
ni_day = ts(ni_core, start = c(122, 1), frequency = 24)
acf(ni_day,
    main = "(a) ni: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 18 of the year at hour 49
ni_week = ts(ni_core, start = c(18, 49), frequency = 168)
acf(ni_week, lag = 400,
    main = "(b) ni: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 5 of the year at hour 1
ni_month = ts(ni_core, start = c(5, 1), frequency = 730.5)
acf(ni_month, lag = 1500,
    main = "(c) ni: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2004 at hour 2905
ni_year = ts(ni_core, start = c(2004, 2905), frequency = 8766)
acf(ni_year, lag = 18000,
    main = "(d) ni: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


### Plots Analysis: PJM_Load
# Create xts object (sorted in chronological order)
pjm_load_mw = pjm_load$PJM_Load_MW
head(pjm_load$Datetime)
pjm_load_date = as_datetime(pjm_load$Datetime)
pjm_load.xts = xts(pjm_load_mw, order.by = pjm_load_date, frequency = "hours")
head(pjm_load.xts)
tail(pjm_load.xts)
class(pjm_load.xts)
str(pjm_load.xts)

# Time Plot of PJM_Load
dygraph(pjm_load.xts,
        main = "pjm_load Energy Consumption with multiple seasonalities",
        ylab = "Energy Consumption (pjm_load_mw)") %>% dyRangeSelector

# Further study of the seasonality at different levels of granularity. 
# Extract the hour, week and month 
hour = hour(pjm_load_date); head(hour); length(hour)
year = year(pjm_load_date); head(year); length(year)
week = week(pjm_load_date); head(week); length(week)
day = day(pjm_load_date); head(day); length(day)
weekday = wday(pjm_load_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjm_load_date)],
               levels = month.abb); length(month)

# Create data frame with extracted elmts
pjm_load_data = data.frame(year, month, day, week, weekday, hour, pjm_load_mw)
pjm_load_newdata = pjm_load_data[order(year, month, day, week, weekday, hour), ]
head(pjm_load_newdata)
tail(pjm_load_newdata)

# Box plots with different seasonalities
boxplot(pjm_load_mw ~ month, lwd = 1.5, cex = 1.5,
        main = "(a) PJM Load: Higher consumption in Winter and Summer months \nthan other months",
        ylab = "Energy consumption (pjm_load_mw)")
boxplot(pjm_load_mw ~ hour, lwd = 1.5, cex = 1.5,
        main = "(b) PJM Load: Lower consumption during the night\nthan during the day",
        ylab = "Energy consumption (pjm_load_mw)")
boxplot(pjm_load_mw ~ weekday, lwd = 1.5, cex = 1.5,
        main = "(c) PJM Load: Lower consumption on Saturday and Sunday \nthan other days of the week",
        ylab = "Energy consumption (pjm_load_mw)")
boxplot(pjm_load_mw ~ week, lwd = 1.5, cex = 1.5,
        main = "(d) PJM Load: Lower consumption in weeks 14-21 and \n weeks 40-48 than other weeks",
        ylab = "Energy consumption (pjm_load_mw)")
boxplot(pjm_load_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) PJM Load: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (pjm_load_mw)")

# ACFs with different seasonalities
pjm_load_core = coredata(pjm_load.xts)
dt_start <- ymd_hms(start(pjm_load.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 91 of the year at hour 1
pjm_load_day = ts(pjm_load_core, start = c(91, 1), frequency = 24)
acf(pjm_load_day,
    main = "(a) PJM_Load: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 13 of the year at hour 145
pjm_load_week = ts(pjm_load_core, start = c(13, 145), frequency = 168)
acf(pjm_load_week, lag = 400,
    main = "(b) PJM_Load: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 4 of the year at hour 1
pjm_load_month = ts(pjm_load_core, start = c(4, 1), frequency = 730.5)
acf(pjm_load_month, lag = 1500,
    main = "(c) PJM_Load: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 1998 at hour 2161
pjm_load_year = ts(pjm_load_core, start = c(1998, 2161), frequency = 8766)
acf(pjm_load_year, lag = 18000,
    main = "(d) PJM_Load: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


### Plots Analysis: PJME
# Create xts object (sorted in chronological order)
pjme_mw = pjme$PJME_MW
head(pjme$Datetime)
pjme_date = as_datetime(pjme$Datetime)
pjme.xts = xts(pjme_mw, order.by = pjme_date, frequency = "hours")
head(pjme.xts)
tail(pjme.xts)
class(pjme.xts)
str(pjme.xts)

# Time Plot of PJME
dygraph(pjme.xts,
        main = "PJME Energy Consumption with multiple seasonalities",
        ylab = "Energy Consumption (MW)") %>% dyRangeSelector

# Further study of the seasonality at different levels of granularity. 
# Extract the hour, week and month 
hour = hour(pjme_date); head(hour); length(hour)
year = year(pjme_date); head(year); length(year)
week = week(pjme_date); head(week); length(week)
day = day(pjme_date); head(day); length(day)
weekday = wday(pjme_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjme_date)],
               levels = month.abb); length(month)

# Create data frame with extracted elmts
pjme_data = data.frame(year, month, day, week, weekday, hour, pjme_mw)
pjme_newdata = pjme_data[order(year, month, day, week, weekday, hour), ]
head(pjme_newdata)
tail(pjme_newdata)

# Box plots with different seasonalities
boxplot(pjme_mw ~ month, lwd = 1.5, cex = 1.5,
        main = "(a) PJME: Higher consumption in Winter and Summer months\nthan other months",
        ylab = "Energy Consumption (MW)")
boxplot(pjme_mw ~ hour, lwd = 1.5, cex = 1.5,
        main = "(b) PJME: Lower consumption during the night\nthan during the day",
        ylab = "Energy Consumption (MW)")
boxplot(pjme_mw ~ weekday, lwd = 1.5, cex = 1.5,
        main = "(c) PJME: Lower consumption on Saturday and Sunday\nthan other days of the week",
        ylab = "Energy Consumption (MW)")
boxplot(pjme_mw ~ week, lwd = 1.5, cex = 1.5,
        main = "(d) PJME: Lower consumption in weeks 15-20 and \n weeks 40-44 than other weeks",
        ylab = "Energy Consumption (MW)")
boxplot(pjme_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) PJME: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy Consumption (MW)")

# ACFs with different seasonalities
pjme_core = coredata(pjme.xts)
dt_start <- ymd_hms(start(pjme.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 1 of the year at hour 1
pjme_day = ts(pjme_core, start = c(1, 1), frequency = 24)
acf(pjme_day,
    main = "(a) PJME: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 1 of the year at hour 1
pjme_week = ts(pjme_core, start = c(1, 1), frequency = 168)
acf(pjme_week, lag = 400,
    main = "(b) PJME: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 1 of the year at hour 1
pjme_month = ts(pjme_core, start = c(1, 1), frequency = 730.5)
acf(pjme_month, lag = 1500,
    main = "(c) PJME: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2002 at hour 1
pjme_year = ts(pjme_core, start = c(2002, 1), frequency = 8766)
acf(pjme_year, lag = 18000,
    main = "(d) PJME: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


### Plots Analysis: PJMW
# Create xts object (sorted in chronological order)
pjmw_mw = pjmw$PJMW_MW
head(pjmw$Datetime)
pjmw_date = as_datetime(pjmw$Datetime)
pjmw.xts = xts(pjmw_mw, order.by = pjmw_date, frequency = "hours")
head(pjmw.xts)
tail(pjmw.xts)
class(pjmw.xts)
str(pjmw.xts)

# Time Plot of PJMW
dygraph(pjmw.xts,
        main = "pjmw Energy Consumption with multiple seasonalities",
        ylab = "Energy Consumption (pjmw_mw)") %>% dyRangeSelector

# Further study of the seasonality at different levels of granularity. 
# Extract the hour, week and month 
hour = hour(pjmw_date); head(hour); length(hour)
year = year(pjmw_date); head(year); length(year)
week = week(pjmw_date); head(week); length(week)
day = day(pjmw_date); head(day); length(day)
weekday = wday(pjmw_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjmw_date)],
               levels = month.abb); length(month)

# Create data frame with extracted elmts
pjmw_data = data.frame(year, month, day, week, weekday, hour, pjmw_mw)
pjmw_newdata = pjmw_data[order(year, month, day, week, weekday, hour), ]
head(pjmw_newdata)
tail(pjmw_newdata)

# Box plots with different seasonalities
boxplot(pjmw_mw ~ month, lwd = 1.5, cex = 1.5,
        main = "(a) PJMW: Higher consumption in Winter and Summer months than other months",
        ylab = "Energy consumption (pjmw_mw)")
boxplot(pjmw_mw ~ hour, lwd = 1.5, cex = 1.5,
        main = "(b) PJMW: Lower consumption during the night\nthan during the day",
        ylab = "Energy consumption (pjmw_mw)")
boxplot(pjmw_mw ~ weekday, lwd = 1.5, cex = 1.5,
        main = "(c) PJMW: Lower consumption on Saturday and Sunday\nthan other days of the week",
        ylab = "Energy consumption (pjmw_mw)")
boxplot(pjmw_mw ~ week, lwd = 1.5, cex = 1.5,
        main = "(d) PJMW: Lower consumption in weeks 14-21 and \n weeks 39-43 than other weeks",
        ylab = "Energy consumption (pjmw_mw)")
boxplot(pjmw_mw ~ day, lwd = 1.5, cex = 1.5,
        main = "(e) PJMW: Not much seasonality when comparing between\ndays of the month",
        ylab = "Energy consumption (pjmw_mw)")

# ACFs with different seasonalities
pjmw_core = coredata(pjmw.xts)
dt_start <- ymd_hms(start(pjmw.xts))

# Time interval hour, cycle daily, frequency 24
yday(dt_start)
hour(dt_start)
# Start at day 91 of the year at hour 1
pjmw_day = ts(pjmw_core, start = c(91, 1), frequency = 24)
acf(pjmw_day,
    main = "(a) PJMW: The 1 lag means 1 cycle, \n in this picture 24 hours",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle weekly, frequency 168 (24 * 7)
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)
# Start at week 13 of the year at hour 145
pjmw_week = ts(pjmw_core, start = c(13, 145), frequency = 168)
acf(pjmw_week, lag = 400,
    main = "(b) PJMW: The 1 lag means 1 cycle, \n in this picture 168 hours (a week)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle monthly, frequency 730.5 (24 * (365.25 / 12))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)
# Start at month 4 of the year at hour 1
pjmw_month = ts(pjmw_core, start = c(4, 1), frequency = 730.5)
acf(pjmw_month, lag = 1500,
    main = "(c) PJMW: The 1 lag means 1 cycle, \n in this picture 730.5 hours (a month)",
    lwd = 1.5, cex = 1.5)

# Time interval hour, cycle yearly, frequency 8766 (24 * 365.25)
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)
# Start at year 2002 at hour 2161
pjmw_year = ts(pjmw_core, start = c(2002, 2161), frequency = 8766)
acf(pjmw_year, lag = 18000,
    main = "(d) PJMW: The 1 lag means 1 cycle, \n in this picture 8766 hours (a year)",
    lwd = 1.5, cex = 1.5)


########################### 
# TASK 2: FEATURES
########################### 
### Ingrid: ACF, PACF, Trend
### Feature 1: Autocorrelations and Partial Autocorrelations of Regular and Sesonal lags
#################################################
# NOTE: ACF plot analysis completed the code below won't be needed
# Time Series for daily cycle
# Find start date
dt_start = ymd_hms(start(pjme.xts))
yday(dt_start)
hour(dt_start)

aep_day = ts(coredata(aep.xts), start = c(275, 1), frequency = 24)
comed_day = ts(coredata(comed.xts), start = c(1, 1), frequency = 24)
dayton_day = ts(coredata(dayton.xts), start = c(275, 1), frequency = 24)
deok_day = ts(coredata(deok.xts), start = c(1, 1), frequency = 24)
dom_day = ts(coredata(dom.xts), start = c(121, 1), frequency = 24)
duq_day = ts(coredata(duq.xts), start = c(1, 1), frequency = 24)
ekpc_day = ts(coredata(ekpc.xts), start = c(152, 1), frequency = 24)
fe_day = ts(coredata(fe.xts), start = c(152, 1), frequency = 24)
ni_day = ts(coredata(ni.xts), start = c(122, 1), frequency = 24)
pjm_load_day = ts(coredata(pjm_load.xts), start = c(91, 1), frequency = 24)
pjme_day = ts(coredata(pjme.xts), start = c(1, 1), frequency = 24)
pjmw_day = ts(coredata(pjmw.xts), start = c(91, 1), frequency = 24)

# Time Series for weekly cycle
# Find start date
dt_start = ymd_hms(start(pjmw.xts))
week(dt_start)
wday_jan1 <- wday(ymd(paste(year(dt_start), "01", "01", sep = "-")))
wday_start <- wday(dt_start)
day_diff <- ifelse(wday_start < wday_jan1, wday_start + 7 - wday_jan1,
                   wday_start - wday_jan1)
day_diff * 24 + hour(dt_start)

aep_week = ts(coredata(aep.xts), start = c(40, 25), frequency = 168)
comed_week = ts(coredata(comed.xts), start = c(1, 1), frequency = 168) 
dayton_week = ts(coredata(dayton.xts), start = c(40, 25), frequency = 168)
deok_week = ts(coredata(deok.xts), start = c(1, 1), frequency = 168) 
dom_week = ts(coredata(dom.xts), start = c(18, 25), frequency = 168)
duq_week = ts(coredata(duq.xts), start = c(1, 1), frequency = 168)
ekpc_week = ts(coredata(ekpc.xts), start = c(22, 97), frequency = 168)
fe_week = ts(coredata(fe.xts), start = c(22, 97), frequency = 168)
ni_week = ts(coredata(ni.xts), start = c(18, 29), frequency = 168)
pjm_load_week = ts(coredata(pjm_load.xts), start = c(13, 145), frequency = 168)
pjme_week = ts(coredata(pjme.xts), start = c(1, 1), frequency = 168)
pjmw_week = ts(coredata(pjmw.xts), start = c(13, 145), frequency = 168)

# Time Series for monthly cycle
# Find start date
dt_start = ymd_hms(start(pjmw.xts))
month(dt_start)
(mday(dt_start) - 1) * 24 + hour(dt_start)

aep_month = ts(coredata(aep.xts), start = c(10, 1), frequency = 730.5)
comed_month = ts(coredata(comed.xts), start = c(1, 1), frequency = 730.5) 
dayton_month = ts(coredata(dayton.xts), start = c(10, 1), frequency = 730.5)
deok_month = ts(coredata(deok.xts), start = c(1, 1), frequency = 730.5) 
dom_month = ts(coredata(dom.xts), start = c(5, 1), frequency = 730.5)
duq_month = ts(coredata(duq.xts), start = c(1, 1), frequency = 730.5)
ekpc_month = ts(coredata(ekpc.xts), start = c(6, 1), frequency = 730.5)
fe_month = ts(coredata(fe.xts), start = c(6, 1), frequency = 730.5)
ni_month = ts(coredata(ni.xts), start = c(5, 1), frequency = 730.5)
pjm_load_month = ts(coredata(pjm_load.xts), start = c(4, 1), frequency = 730.5)
pjme_month = ts(coredata(pjme.xts), start = c(1, 1), frequency = 730.5)
pjmw_month = ts(coredata(pjmw.xts), start = c(4, 1), frequency = 730.5)

# Time Series for yearly cycle
# Find start date
dt_start = ymd_hms(start(pjmw.xts))
year(dt_start)
(yday(dt_start) - 1) * 24 + hour(dt_start)

aep_year = ts(coredata(aep.xts), start = c(2004, 6577), frequency = 8766)
comed_year = ts(coredata(comed.xts), start = c(2011, 1), frequency = 8766) #
dayton_year = ts(coredata(dayton.xts), start = c(2004, 6577), frequency = 8766)
deok_year = ts(coredata(deok.xts), start = c(2012, 1), frequency = 8766) #
dom_year = ts(coredata(dom.xts), start = c(2005, 2881), frequency = 8766)
duq_year = ts(coredata(duq.xts), start = c(2005, 1), frequency = 8766)
ekpc_year = ts(coredata(ekpc.xts), start = c(2013, 3625), frequency = 8766)
fe_year = ts(coredata(fe.xts), start = c(2011, 3625), frequency = 8766)
ni_year = ts(coredata(ni.xts), start = c(2004, 2905), frequency = 8766)
pjm_load_year = ts(coredata(pjm_load.xts), start = c(1998, 2161), frequency = 8766)
pjme_year = ts(coredata(pjme.xts), start = c(2002, 1), frequency = 8766)
pjmw_year = ts(coredata(pjmw.xts), start = c(2002, 2161), frequency = 8766)
#################################################

### Autocorrelations (ACF)
# ACF for daily cycle
acf_aep_day = acf(aep_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_comed_day = acf(comed_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_dayton_day = acf(dayton_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_deok_day = acf(deok_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_dom_day = acf(dom_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_duq_day = acf(duq_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_ekpc_day = acf(ekpc_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_fe_day = acf(fe_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_ni_day = acf(ni_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_pjm_load_day = acf(pjm_load_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_pjme_day = acf(pjme_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]
acf_pjmw_day = acf(pjmw_day, plot = F)$acf[c(2, 3, 4, 24, 25, 26)]

# ACF for weekly cycle
acf_aep_week = acf(aep_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_comed_week = acf(comed_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_dayton_week = acf(dayton_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_deok_week = acf(deok_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_dom_week = acf(dom_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_duq_week = acf(duq_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_ekpc_week = acf(ekpc_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_fe_week = acf(fe_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_ni_week = acf(ni_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_pjm_load_week = acf(pjm_load_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_pjme_week = acf(pjme_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]
acf_pjmw_week = acf(pjmw_week, lag.max = 200, plot = F)$acf[c(2, 3, 4, 168, 169, 170)]

# ACF for monthly cycle
acf_aep_month = acf(aep_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_comed_month = acf(comed_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_dayton_month = acf(dayton_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_deok_month = acf(deok_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_dom_month = acf(dom_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_duq_month = acf(duq_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_ekpc_month = acf(ekpc_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_fe_month = acf(fe_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_ni_month = acf(ni_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_pjm_load_month = acf(pjm_load_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_pjme_month = acf(pjme_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]
acf_pjmw_month = acf(pjmw_month, lag.max = 750, plot = F)$acf[c(2, 3, 4, 730, 731, 732)]

# ACF for yearly cycle
acf_aep_year = acf(aep_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_comed_year = acf(comed_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_dayton_year = acf(dayton_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_deok_year = acf(deok_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_dom_year = acf(dom_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_duq_year = acf(duq_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_ekpc_year = acf(ekpc_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_fe_year = acf(fe_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_ni_year = acf(ni_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_pjm_load_year = acf(pjm_load_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_pjme_year = acf(pjme_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]
acf_pjmw_year = acf(pjmw_year, lag.max = 8800, plot = F)$acf[c(2, 3, 4, 8766, 8767, 8768)]

### Partial Autocorrelations (PACF)
# PACF for daily cycle
pacf_aep_day = pacf(aep_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_comed_day = pacf(comed_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_dayton_day = pacf(dayton_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_deok_day = pacf(deok_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_dom_day = pacf(dom_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_duq_day = pacf(duq_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_ekpc_day = pacf(ekpc_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_fe_day = pacf(fe_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_ni_day = pacf(ni_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_pjm_load_day = pacf(pjm_load_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_pjme_day = pacf(pjme_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]
pacf_pjmw_day = pacf(pjmw_day, plot = F)$acf[c(1, 2, 3, 23, 24, 25)]

# PACF for weekly cycle
pacf_aep_week = pacf(aep_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_comed_week = pacf(comed_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_dayton_week = pacf(dayton_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_deok_week = pacf(deok_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_dom_week = pacf(dom_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_duq_week = pacf(duq_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_ekpc_week = pacf(ekpc_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_fe_week = pacf(fe_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_ni_week = pacf(ni_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_pjm_load_week = pacf(pjm_load_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_pjme_week = pacf(pjme_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]
pacf_pjmw_week = pacf(pjmw_week, lag.max = 200, plot = F)$acf[c(1, 2, 3, 167, 168, 169)]

# PACF for monthly cycle
pacf_aep_month = pacf(aep_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_comed_month = pacf(comed_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_dayton_month = pacf(dayton_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_deok_month = pacf(deok_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_dom_month = pacf(dom_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_duq_month = pacf(duq_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_ekpc_month = pacf(ekpc_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_fe_month = pacf(fe_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_ni_month = pacf(ni_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_pjm_load_month = pacf(pjm_load_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_pjme_month = pacf(pjme_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]
pacf_pjmw_month = pacf(pjmw_month, lag.max = 750, plot = F)$acf[c(1, 2, 3, 729, 730, 731)]

# PACF for yearly cycle
pacf_aep_year = pacf(aep_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_comed_year = pacf(comed_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_dayton_year = pacf(dayton_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_deok_year = pacf(deok_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_dom_year = pacf(dom_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_duq_year = pacf(duq_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_ekpc_year = pacf(ekpc_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_fe_year = pacf(fe_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_ni_year = pacf(ni_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_pjm_load_year = pacf(pjm_load_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_pjme_year = pacf(pjme_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]
pacf_pjmw_year = pacf(pjmw_year, lag.max = 8800, plot = F)$acf[c(1, 2, 3, 8765, 8766, 8767)]

# Create data frame of ACF and PACF 
# Row - each TS; 
# Col - autocorr of daily, week, monthly, yearly cycle
acf_features = rbind(c(acf_aep_day, acf_aep_week, acf_aep_month, acf_aep_year),
                     c(acf_comed_day, acf_comed_week, acf_comed_month, acf_comed_year),
                     c(acf_dayton_day, acf_dayton_week, acf_dayton_month, acf_dayton_year),
                     c(acf_deok_day, acf_deok_week, acf_deok_month, acf_deok_year),
                     c(acf_dom_day, acf_dom_week, acf_dom_month, acf_dom_year),
                     c(acf_duq_day, acf_duq_week, acf_duq_month, acf_duq_year),
                     c(acf_ekpc_day, acf_ekpc_week, acf_ekpc_month, acf_ekpc_year),
                     c(acf_fe_day, acf_fe_week, acf_fe_month, acf_fe_year),
                     c(acf_ni_day, acf_ni_week, acf_ni_month, acf_ni_year),
                     c(acf_pjm_load_day, acf_pjm_load_week, acf_pjm_load_month, acf_pjm_load_year),
                     c(acf_pjme_day, acf_pjme_week, acf_pjme_month, acf_pjme_year),
                     c(acf_pjmw_day, acf_pjmw_week, acf_pjmw_month, acf_pjmw_year))

pacf_features = rbind(c(pacf_aep_day, pacf_aep_week, pacf_aep_month, pacf_aep_year),
                      c(pacf_comed_day, pacf_comed_week, pacf_comed_month, pacf_comed_year),
                      c(pacf_dayton_day, pacf_dayton_week, pacf_dayton_month, pacf_dayton_year),
                      c(pacf_deok_day, pacf_deok_week, pacf_deok_month, pacf_deok_year),
                      c(pacf_dom_day, pacf_dom_week, pacf_dom_month, pacf_dom_year),
                      c(pacf_duq_day, pacf_duq_week, pacf_duq_month, pacf_duq_year),
                      c(pacf_ekpc_day, pacf_ekpc_week, pacf_ekpc_month, pacf_ekpc_year),
                      c(pacf_fe_day, pacf_fe_week, pacf_fe_month, pacf_fe_year),
                      c(pacf_ni_day, pacf_ni_week, pacf_ni_month, pacf_ni_year),
                      c(pacf_pjm_load_day, pacf_pjm_load_week, pacf_pjm_load_month, pacf_pjm_load_year),
                      c(pacf_pjme_day, pacf_pjme_week, pacf_pjme_month, pacf_pjme_year),
                      c(pacf_pjmw_day, pacf_pjmw_week, pacf_pjmw_month, pacf_pjmw_year))

colnames(acf_features) = c('acf daily reg 1', 'acf daily reg 2', 'acf daily reg 3',
                           'acf daily seas 1', 'acf daily seas 2', 'acf daily seas 3',
                           'acf week reg 1', 'acf week reg 2', 'acf week reg 3',
                           'acf week seas 1', 'acf week seas 2', 'acf week seas 3',
                           'acf month reg 1', 'acf month reg 2', 'acf month reg 3',
                           'acf month seas 1', 'acf month seas 2', 'acf month seas 3',
                           'acf year reg 1', 'acf year reg 2', 'acf year reg 3',
                           'acf year seas 1', 'acf year seas 2', 'acf year seas 3')

colnames(pacf_features) = c('pacf daily reg 1', 'pacf daily reg 2', 'pacf daily reg 3',
                            'pacf daily seas 1', 'pacf daily seas 2', 'pacf daily seas 3',
                            'pacf week reg 1', 'pacf week reg 2', 'pacf week reg 3',
                            'pacf week seas 1', 'pacf week seas 2', 'pacf week seas 3',
                            'pacf month reg 1', 'pacf month reg 2', 'pacf month reg 3',
                            'pacf month seas 1', 'pacf month seas 2', 'pacf month seas 3',
                            'pacf year reg 1', 'pacf year reg 2', 'pacf year reg 3',
                            'pacf year seas 1', 'pacf year seas 2', 'pacf year seas 3')

head(acf_features); head(pacf_features)
dim(acf_features); dim(pacf_features)

### Put the features into a df: features 1, 2, 3
acf_pacf_features_df <- cbind(acf_features, pacf_features)

### Daniel: Missing obs, 
### Feature 2: Number of missing observations
# With our specific dataframes, shows us the amount of hour gaps inside of the data
gaps_function <- function (dataframe) {
  as.numeric(dataframe[nrow(dataframe), 1] - dataframe[1, 1])*24 - nrow(dataframe)
}

missing_obs <- vector(length = 12)
missing_obs[1] <- gaps_function(aep_tsibble)
missing_obs[2] <- gaps_function(comed_tsibble)
missing_obs[3] <- gaps_function(dayton_tsibble)
missing_obs[4] <- gaps_function(deok_tsibble)
missing_obs[5] <- gaps_function(dom_tsibble)
missing_obs[6] <- gaps_function(duq_tsibble)
missing_obs[7] <- gaps_function(ekpc_tsibble)
missing_obs[8] <- gaps_function(fe_tsibble)
missing_obs[9] <- gaps_function(ni_tsibble)
missing_obs[10] <- gaps_function(pjm_load_tsibble)
missing_obs[11] <- gaps_function(pjme_tsibble)
missing_obs[12] <- gaps_function(pjmw_tsibble)

### Feature 3: Slope of a simple lm
# With our specific dataframes, shows us the the slope of a simple lm
simplelm_slope_function <- function (dataframe) {
  lm(unlist(dataframe[,2]) ~ c(1:nrow(dataframe)))$coefficients[2]
}

lm_slope <- vector(length = 12)
lm_slope[1] <- simplelm_slope_function(aep_tsibble)
lm_slope[2] <- simplelm_slope_function(comed_tsibble)
lm_slope[3] <- simplelm_slope_function(dayton_tsibble)
lm_slope[4] <- simplelm_slope_function(deok_tsibble)
lm_slope[5] <- simplelm_slope_function(dom_tsibble)
lm_slope[6] <- simplelm_slope_function(duq_tsibble)
lm_slope[7] <- simplelm_slope_function(ekpc_tsibble)
lm_slope[8] <- simplelm_slope_function(fe_tsibble)
lm_slope[9] <- simplelm_slope_function(ni_tsibble)
lm_slope[10] <- simplelm_slope_function(pjm_load_tsibble)
lm_slope[11] <- simplelm_slope_function(pjme_tsibble)
lm_slope[12] <- simplelm_slope_function(pjmw_tsibble)

### Feature 4: Optimal boxcox coefficent
# With our specific dataframes, shows us the optimal boxcox coefficent
# library(MASS)
order_boxcox <- function (dataframe) {
  bc <- boxcox(unlist(dataframe[,2]) ~ unlist(dataframe[,1]))
  bc$x[which.max(bc$y)]
}

boxcox_order <- vector(length = 12)
boxcox_order[1] <- order_boxcox(aep_tsibble)
boxcox_order[2] <- order_boxcox(comed_tsibble)
boxcox_order[3] <- order_boxcox(dayton_tsibble)
boxcox_order[4] <- order_boxcox(deok_tsibble)
boxcox_order[5] <- order_boxcox(dom_tsibble)
boxcox_order[6] <- order_boxcox(duq_tsibble)
boxcox_order[7] <- order_boxcox(ekpc_tsibble)
boxcox_order[8] <- order_boxcox(fe_tsibble[-1,])
boxcox_order[9] <- order_boxcox(ni_tsibble)
boxcox_order[10] <- order_boxcox(pjm_load_tsibble)
boxcox_order[11] <- order_boxcox(pjme_tsibble)
boxcox_order[12] <- order_boxcox(pjmw_tsibble)

### Feature 5: Positive trend and Negative trend
# With our specific dataframes, shows us the number of positive trends
sum_positive_trends <- function(dataframe){
  sum((ts(dataframe[,2])- stats::lag(ts(dataframe[,2]), 1)) < 0)/(nrow(dataframe)-1)
}

# With our specific dataframes, shows us the number of negative trends
sum_negative_trends <- function(dataframe){
  sum((ts(dataframe[,2]) - stats::lag(ts(dataframe[,2]), 1)) > 0)/(nrow(dataframe)-1)
}

positive_trends <- vector(length = 12)
positive_trends[1] <- sum_positive_trends(aep_tsibble)
positive_trends[2] <- sum_positive_trends(comed_tsibble)
positive_trends[3] <- sum_positive_trends(dayton_tsibble)
positive_trends[4] <- sum_positive_trends(deok_tsibble)
positive_trends[5] <- sum_positive_trends(dom_tsibble)
positive_trends[6] <- sum_positive_trends(duq_tsibble)
positive_trends[7] <- sum_positive_trends(ekpc_tsibble)
positive_trends[8] <- sum_positive_trends(fe_tsibble)
positive_trends[9] <- sum_positive_trends(ni_tsibble)
positive_trends[10] <- sum_positive_trends(pjm_load_tsibble)
positive_trends[11] <- sum_positive_trends(pjme_tsibble)
positive_trends[12] <- sum_positive_trends(pjmw_tsibble)

negative_trends <- vector(length = 12)
negative_trends[1] <- sum_negative_trends(aep_tsibble)
negative_trends[2] <- sum_negative_trends(comed_tsibble)
negative_trends[3] <- sum_negative_trends(dayton_tsibble)
negative_trends[4] <- sum_negative_trends(deok_tsibble)
negative_trends[5] <- sum_negative_trends(dom_tsibble)
negative_trends[6] <- sum_negative_trends(duq_tsibble)
negative_trends[7] <- sum_negative_trends(ekpc_tsibble)
negative_trends[8] <- sum_negative_trends(fe_tsibble)
negative_trends[9] <- sum_negative_trends(ni_tsibble)
negative_trends[10] <- sum_negative_trends(pjm_load_tsibble)
negative_trends[11] <- sum_negative_trends(pjme_tsibble)
negative_trends[12] <- sum_negative_trends(pjmw_tsibble)


### Put the features into a df: features 2, 3, 4, 5
daniel_features_df <- data.frame(lm_slope, missing_obs, boxcox_order, 
                                 positive_trends, negative_trends)

### Feature 6: Summary statistics for each time series
### Mean, median, IQR, min, max, and SD for the whole series

aep_summary_stats <- c(mean(aep_tsibble$AEP_MW),
                       median(aep_tsibble$AEP_MW),
                       IQR(aep_tsibble$AEP_MW),
                       min(aep_tsibble$AEP_MW),
                       max(aep_tsibble$AEP_MW),
                       sd(aep_tsibble$AEP_MW))
comed_summary_stats <- c(mean(comed_tsibble$COMED_MW),
                         median(comed_tsibble$COMED_MW),
                         IQR(comed_tsibble$COMED_MW),
                         min(comed_tsibble$COMED_MW),
                         max(comed_tsibble$COMED_MW),
                         sd(comed_tsibble$COMED_MW))
dayton_summary_stats <- c(mean(dayton_tsibble$DAYTON_MW),
                          median(dayton_tsibble$DAYTON_MW),
                          IQR(dayton_tsibble$DAYTON_MW),
                          min(dayton_tsibble$DAYTON_MW),
                          max(dayton_tsibble$DAYTON_MW),
                          sd(dayton_tsibble$DAYTON_MW))
deok_summary_stats <- c(mean(deok_tsibble$DEOK_MW),
                        median(deok_tsibble$DEOK_MW),
                        IQR(deok_tsibble$DEOK_MW),
                        min(deok_tsibble$DEOK_MW),
                        max(deok_tsibble$DEOK_MW),
                        sd(deok_tsibble$DEOK_MW))
dom_summary_stats <- c(mean(dom_tsibble$DOM_MW),
                       median(dom_tsibble$DOM_MW),
                       IQR(dom_tsibble$DOM_MW),
                       min(dom_tsibble$DOM_MW),
                       max(dom_tsibble$DOM_MW),
                       sd(dom_tsibble$DOM_MW))
duq_summary_stats <- c(mean(duq_tsibble$DUQ_MW),
                       median(duq_tsibble$DUQ_MW),
                       IQR(duq_tsibble$DUQ_MW),
                       min(duq_tsibble$DUQ_MW),
                       max(duq_tsibble$DUQ_MW),
                       sd(duq_tsibble$DUQ_MW))
ekpc_summary_stats <- c(mean(ekpc_tsibble$EKPC_MW),
                        median(ekpc_tsibble$EKPC_MW),
                        IQR(ekpc_tsibble$EKPC_MW),
                        min(ekpc_tsibble$EKPC_MW),
                        max(ekpc_tsibble$EKPC_MW),
                        sd(ekpc_tsibble$EKPC_MW))
fe_summary_stats <- c(mean(fe_tsibble$FE_MW),
                      median(fe_tsibble$FE_MW),
                      IQR(fe_tsibble$FE_MW),
                      min(fe_tsibble$FE_MW),
                      max(fe_tsibble$FE_MW),
                      sd(fe_tsibble$FE_MW))
ni_summary_stats <- c(mean(ni_tsibble$NI_MW),
                      median(ni_tsibble$NI_MW),
                      IQR(ni_tsibble$NI_MW),
                      min(ni_tsibble$NI_MW),
                      max(ni_tsibble$NI_MW),
                      sd(ni_tsibble$NI_MW))
pjm_load_summary_stats <- c(mean(pjm_load_tsibble$PJM_Load_MW),
                            median(pjm_load_tsibble$PJM_Load_MW),
                            IQR(pjm_load_tsibble$PJM_Load_MW),
                            min(pjm_load_tsibble$PJM_Load_MW),
                            max(pjm_load_tsibble$PJM_Load_MW),
                            sd(pjm_load_tsibble$PJM_Load_MW))
pjme_summary_stats <- c(mean(pjme_tsibble$PJME_MW),
                        median(pjme_tsibble$PJME_MW),
                        IQR(pjme_tsibble$PJME_MW),
                        min(pjme_tsibble$PJME_MW),
                        max(pjme_tsibble$PJME_MW),
                        sd(pjme_tsibble$PJME_MW))
pjmw_summary_stats <- c(mean(pjmw_tsibble$PJMW_MW),
                        median(pjmw_tsibble$PJMW_MW),
                        IQR(pjmw_tsibble$PJMW_MW),
                        min(pjmw_tsibble$PJMW_MW),
                        max(pjmw_tsibble$PJMW_MW),
                        sd(pjmw_tsibble$PJMW_MW))


### Summary statistics for different periods' averages
### Mean, median, IQR, min, max, and SD for each period
### Proportion of electricity usage from each level within a period

aep_periods <- aep_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
aep_by_year <- aep_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(AEP_MW),
            year_sum = sum(AEP_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
aep_by_month <- aep_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(AEP_MW),
            month_sum = sum(AEP_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
aep_by_week <- aep_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(AEP_MW),
            week_sum = sum(AEP_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
aep_by_wday <- aep_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(AEP_MW),
            wday_sum = sum(AEP_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
aep_by_day <- aep_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(AEP_MW),
            day_sum = sum(AEP_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
aep_by_hour <- aep_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(AEP_MW),
            hour_sum = sum(AEP_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
aep_year_summary_stats <- c(mean(aep_by_year$year_mean),
                            median(aep_by_year$year_mean),
                            IQR(aep_by_year$year_mean),
                            min(aep_by_year$year_mean),
                            max(aep_by_year$year_mean),
                            sd(aep_by_year$year_mean))
aep_month_summary_stats <- c(mean(aep_by_month$month_mean),
                             median(aep_by_month$month_mean),
                             IQR(aep_by_month$month_mean),
                             min(aep_by_month$month_mean),
                             max(aep_by_month$month_mean),
                             sd(aep_by_month$month_mean))
aep_week_summary_stats <- c(mean(aep_by_week$week_mean),
                            median(aep_by_week$week_mean),
                            IQR(aep_by_week$week_mean),
                            min(aep_by_week$week_mean),
                            max(aep_by_week$week_mean),
                            sd(aep_by_week$week_mean))
aep_wday_summary_stats <- c(mean(aep_by_wday$wday_mean),
                            median(aep_by_wday$wday_mean),
                            IQR(aep_by_wday$wday_mean),
                            min(aep_by_wday$wday_mean),
                            max(aep_by_wday$wday_mean),
                            sd(aep_by_wday$wday_mean))
aep_day_summary_stats <- c(mean(aep_by_day$day_mean),
                           median(aep_by_day$day_mean),
                           IQR(aep_by_day$day_mean),
                           min(aep_by_day$day_mean),
                           max(aep_by_day$day_mean),
                           sd(aep_by_day$day_mean))
aep_hour_summary_stats <- c(mean(aep_by_hour$hour_mean),
                            median(aep_by_hour$hour_mean),
                            IQR(aep_by_hour$hour_mean),
                            min(aep_by_hour$hour_mean),
                            max(aep_by_hour$hour_mean),
                            sd(aep_by_hour$hour_mean))

comed_periods <- comed_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
comed_by_year <- comed_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(COMED_MW),
            year_sum = sum(COMED_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
comed_by_month <- comed_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(COMED_MW),
            month_sum = sum(COMED_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
comed_by_week <- comed_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(COMED_MW),
            week_sum = sum(COMED_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
comed_by_wday <- comed_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(COMED_MW),
            wday_sum = sum(COMED_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
comed_by_day <- comed_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(COMED_MW),
            day_sum = sum(COMED_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
comed_by_hour <- comed_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(COMED_MW),
            hour_sum = sum(COMED_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
comed_year_summary_stats <- c(mean(comed_by_year$year_mean),
                              median(comed_by_year$year_mean),
                              IQR(comed_by_year$year_mean),
                              min(comed_by_year$year_mean),
                              max(comed_by_year$year_mean),
                              sd(comed_by_year$year_mean))
comed_month_summary_stats <- c(mean(comed_by_month$month_mean),
                               median(comed_by_month$month_mean),
                               IQR(comed_by_month$month_mean),
                               min(comed_by_month$month_mean),
                               max(comed_by_month$month_mean),
                               sd(comed_by_month$month_mean))
comed_week_summary_stats <- c(mean(comed_by_week$week_mean),
                              median(comed_by_week$week_mean),
                              IQR(comed_by_week$week_mean),
                              min(comed_by_week$week_mean),
                              max(comed_by_week$week_mean),
                              sd(comed_by_week$week_mean))
comed_wday_summary_stats <- c(mean(comed_by_wday$wday_mean),
                              median(comed_by_wday$wday_mean),
                              IQR(comed_by_wday$wday_mean),
                              min(comed_by_wday$wday_mean),
                              max(comed_by_wday$wday_mean),
                              sd(comed_by_wday$wday_mean))
comed_day_summary_stats <- c(mean(comed_by_day$day_mean),
                             median(comed_by_day$day_mean),
                             IQR(comed_by_day$day_mean),
                             min(comed_by_day$day_mean),
                             max(comed_by_day$day_mean),
                             sd(comed_by_day$day_mean))
comed_hour_summary_stats <- c(mean(comed_by_hour$hour_mean),
                              median(comed_by_hour$hour_mean),
                              IQR(comed_by_hour$hour_mean),
                              min(comed_by_hour$hour_mean),
                              max(comed_by_hour$hour_mean),
                              sd(comed_by_hour$hour_mean))

dayton_periods <- dayton_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
dayton_by_year <- dayton_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(DAYTON_MW),
            year_sum = sum(DAYTON_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
dayton_by_month <- dayton_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(DAYTON_MW),
            month_sum = sum(DAYTON_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
dayton_by_week <- dayton_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(DAYTON_MW),
            week_sum = sum(DAYTON_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
dayton_by_wday <- dayton_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(DAYTON_MW),
            wday_sum = sum(DAYTON_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
dayton_by_day <- dayton_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(DAYTON_MW),
            day_sum = sum(DAYTON_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
dayton_by_hour <- dayton_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(DAYTON_MW),
            hour_sum = sum(DAYTON_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
dayton_year_summary_stats <- c(mean(dayton_by_year$year_mean),
                               median(dayton_by_year$year_mean),
                               IQR(dayton_by_year$year_mean),
                               min(dayton_by_year$year_mean),
                               max(dayton_by_year$year_mean),
                               sd(dayton_by_year$year_mean))
dayton_month_summary_stats <- c(mean(dayton_by_month$month_mean),
                                median(dayton_by_month$month_mean),
                                IQR(dayton_by_month$month_mean),
                                min(dayton_by_month$month_mean),
                                max(dayton_by_month$month_mean),
                                sd(dayton_by_month$month_mean))
dayton_week_summary_stats <- c(mean(dayton_by_week$week_mean),
                               median(dayton_by_week$week_mean),
                               IQR(dayton_by_week$week_mean),
                               min(dayton_by_week$week_mean),
                               max(dayton_by_week$week_mean),
                               sd(dayton_by_week$week_mean))
dayton_wday_summary_stats <- c(mean(dayton_by_wday$wday_mean),
                               median(dayton_by_wday$wday_mean),
                               IQR(dayton_by_wday$wday_mean),
                               min(dayton_by_wday$wday_mean),
                               max(dayton_by_wday$wday_mean),
                               sd(dayton_by_wday$wday_mean))
dayton_day_summary_stats <- c(mean(dayton_by_day$day_mean),
                              median(dayton_by_day$day_mean),
                              IQR(dayton_by_day$day_mean),
                              min(dayton_by_day$day_mean),
                              max(dayton_by_day$day_mean),
                              sd(dayton_by_day$day_mean))
dayton_hour_summary_stats <- c(mean(dayton_by_hour$hour_mean),
                               median(dayton_by_hour$hour_mean),
                               IQR(dayton_by_hour$hour_mean),
                               min(dayton_by_hour$hour_mean),
                               max(dayton_by_hour$hour_mean),
                               sd(dayton_by_hour$hour_mean))

deok_periods <- deok_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
deok_by_year <- deok_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(DEOK_MW),
            year_sum = sum(DEOK_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
deok_by_month <- deok_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(DEOK_MW),
            month_sum = sum(DEOK_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
deok_by_week <- deok_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(DEOK_MW),
            week_sum = sum(DEOK_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
deok_by_wday <- deok_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(DEOK_MW),
            wday_sum = sum(DEOK_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
deok_by_day <- deok_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(DEOK_MW),
            day_sum = sum(DEOK_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
deok_by_hour <- deok_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(DEOK_MW),
            hour_sum = sum(DEOK_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
deok_year_summary_stats <- c(mean(deok_by_year$year_mean),
                             median(deok_by_year$year_mean),
                             IQR(deok_by_year$year_mean),
                             min(deok_by_year$year_mean),
                             max(deok_by_year$year_mean),
                             sd(deok_by_year$year_mean))
deok_month_summary_stats <- c(mean(deok_by_month$month_mean),
                              median(deok_by_month$month_mean),
                              IQR(deok_by_month$month_mean),
                              min(deok_by_month$month_mean),
                              max(deok_by_month$month_mean),
                              sd(deok_by_month$month_mean))
deok_week_summary_stats <- c(mean(deok_by_week$week_mean),
                             median(deok_by_week$week_mean),
                             IQR(deok_by_week$week_mean),
                             min(deok_by_week$week_mean),
                             max(deok_by_week$week_mean),
                             sd(deok_by_week$week_mean))
deok_wday_summary_stats <- c(mean(deok_by_wday$wday_mean),
                             median(deok_by_wday$wday_mean),
                             IQR(deok_by_wday$wday_mean),
                             min(deok_by_wday$wday_mean),
                             max(deok_by_wday$wday_mean),
                             sd(deok_by_wday$wday_mean))
deok_day_summary_stats <- c(mean(deok_by_day$day_mean),
                            median(deok_by_day$day_mean),
                            IQR(deok_by_day$day_mean),
                            min(deok_by_day$day_mean),
                            max(deok_by_day$day_mean),
                            sd(deok_by_day$day_mean))
deok_hour_summary_stats <- c(mean(deok_by_hour$hour_mean),
                             median(deok_by_hour$hour_mean),
                             IQR(deok_by_hour$hour_mean),
                             min(deok_by_hour$hour_mean),
                             max(deok_by_hour$hour_mean),
                             sd(deok_by_hour$hour_mean))

dom_periods <- dom_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
dom_by_year <- dom_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(DOM_MW),
            year_sum = sum(DOM_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
dom_by_month <- dom_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(DOM_MW),
            month_sum = sum(DOM_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
dom_by_week <- dom_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(DOM_MW),
            week_sum = sum(DOM_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
dom_by_wday <- dom_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(DOM_MW),
            wday_sum = sum(DOM_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
dom_by_day <- dom_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(DOM_MW),
            day_sum = sum(DOM_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
dom_by_hour <- dom_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(DOM_MW),
            hour_sum = sum(DOM_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
dom_year_summary_stats <- c(mean(dom_by_year$year_mean),
                            median(dom_by_year$year_mean),
                            IQR(dom_by_year$year_mean),
                            min(dom_by_year$year_mean),
                            max(dom_by_year$year_mean),
                            sd(dom_by_year$year_mean))
dom_month_summary_stats <- c(mean(dom_by_month$month_mean),
                             median(dom_by_month$month_mean),
                             IQR(dom_by_month$month_mean),
                             min(dom_by_month$month_mean),
                             max(dom_by_month$month_mean),
                             sd(dom_by_month$month_mean))
dom_week_summary_stats <- c(mean(dom_by_week$week_mean),
                            median(dom_by_week$week_mean),
                            IQR(dom_by_week$week_mean),
                            min(dom_by_week$week_mean),
                            max(dom_by_week$week_mean),
                            sd(dom_by_week$week_mean))
dom_wday_summary_stats <- c(mean(dom_by_wday$wday_mean),
                            median(dom_by_wday$wday_mean),
                            IQR(dom_by_wday$wday_mean),
                            min(dom_by_wday$wday_mean),
                            max(dom_by_wday$wday_mean),
                            sd(dom_by_wday$wday_mean))
dom_day_summary_stats <- c(mean(dom_by_day$day_mean),
                           median(dom_by_day$day_mean),
                           IQR(dom_by_day$day_mean),
                           min(dom_by_day$day_mean),
                           max(dom_by_day$day_mean),
                           sd(dom_by_day$day_mean))
dom_hour_summary_stats <- c(mean(dom_by_hour$hour_mean),
                            median(dom_by_hour$hour_mean),
                            IQR(dom_by_hour$hour_mean),
                            min(dom_by_hour$hour_mean),
                            max(dom_by_hour$hour_mean),
                            sd(dom_by_hour$hour_mean))

duq_periods <- duq_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
duq_by_year <- duq_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(DUQ_MW),
            year_sum = sum(DUQ_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
duq_by_month <- duq_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(DUQ_MW),
            month_sum = sum(DUQ_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
duq_by_week <- duq_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(DUQ_MW),
            week_sum = sum(DUQ_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
duq_by_wday <- duq_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(DUQ_MW),
            wday_sum = sum(DUQ_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
duq_by_day <- duq_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(DUQ_MW),
            day_sum = sum(DUQ_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
duq_by_hour <- duq_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(DUQ_MW),
            hour_sum = sum(DUQ_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
duq_year_summary_stats <- c(mean(duq_by_year$year_mean),
                            median(duq_by_year$year_mean),
                            IQR(duq_by_year$year_mean),
                            min(duq_by_year$year_mean),
                            max(duq_by_year$year_mean),
                            sd(duq_by_year$year_mean))
duq_month_summary_stats <- c(mean(duq_by_month$month_mean),
                             median(duq_by_month$month_mean),
                             IQR(duq_by_month$month_mean),
                             min(duq_by_month$month_mean),
                             max(duq_by_month$month_mean),
                             sd(duq_by_month$month_mean))
duq_week_summary_stats <- c(mean(duq_by_week$week_mean),
                            median(duq_by_week$week_mean),
                            IQR(duq_by_week$week_mean),
                            min(duq_by_week$week_mean),
                            max(duq_by_week$week_mean),
                            sd(duq_by_week$week_mean))
duq_wday_summary_stats <- c(mean(duq_by_wday$wday_mean),
                            median(duq_by_wday$wday_mean),
                            IQR(duq_by_wday$wday_mean),
                            min(duq_by_wday$wday_mean),
                            max(duq_by_wday$wday_mean),
                            sd(duq_by_wday$wday_mean))
duq_day_summary_stats <- c(mean(duq_by_day$day_mean),
                           median(duq_by_day$day_mean),
                           IQR(duq_by_day$day_mean),
                           min(duq_by_day$day_mean),
                           max(duq_by_day$day_mean),
                           sd(duq_by_day$day_mean))
duq_hour_summary_stats <- c(mean(duq_by_hour$hour_mean),
                            median(duq_by_hour$hour_mean),
                            IQR(duq_by_hour$hour_mean),
                            min(duq_by_hour$hour_mean),
                            max(duq_by_hour$hour_mean),
                            sd(duq_by_hour$hour_mean))

ekpc_periods <- ekpc_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
ekpc_by_year <- ekpc_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(EKPC_MW),
            year_sum = sum(EKPC_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
ekpc_by_month <- ekpc_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(EKPC_MW),
            month_sum = sum(EKPC_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
ekpc_by_week <- ekpc_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(EKPC_MW),
            week_sum = sum(EKPC_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
ekpc_by_wday <- ekpc_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(EKPC_MW),
            wday_sum = sum(EKPC_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
ekpc_by_day <- ekpc_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(EKPC_MW),
            day_sum = sum(EKPC_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
ekpc_by_hour <- ekpc_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(EKPC_MW),
            hour_sum = sum(EKPC_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
ekpc_year_summary_stats <- c(mean(ekpc_by_year$year_mean),
                             median(ekpc_by_year$year_mean),
                             IQR(ekpc_by_year$year_mean),
                             min(ekpc_by_year$year_mean),
                             max(ekpc_by_year$year_mean),
                             sd(ekpc_by_year$year_mean))
ekpc_month_summary_stats <- c(mean(ekpc_by_month$month_mean),
                              median(ekpc_by_month$month_mean),
                              IQR(ekpc_by_month$month_mean),
                              min(ekpc_by_month$month_mean),
                              max(ekpc_by_month$month_mean),
                              sd(ekpc_by_month$month_mean))
ekpc_week_summary_stats <- c(mean(ekpc_by_week$week_mean),
                             median(ekpc_by_week$week_mean),
                             IQR(ekpc_by_week$week_mean),
                             min(ekpc_by_week$week_mean),
                             max(ekpc_by_week$week_mean),
                             sd(ekpc_by_week$week_mean))
ekpc_wday_summary_stats <- c(mean(ekpc_by_wday$wday_mean),
                             median(ekpc_by_wday$wday_mean),
                             IQR(ekpc_by_wday$wday_mean),
                             min(ekpc_by_wday$wday_mean),
                             max(ekpc_by_wday$wday_mean),
                             sd(ekpc_by_wday$wday_mean))
ekpc_day_summary_stats <- c(mean(ekpc_by_day$day_mean),
                            median(ekpc_by_day$day_mean),
                            IQR(ekpc_by_day$day_mean),
                            min(ekpc_by_day$day_mean),
                            max(ekpc_by_day$day_mean),
                            sd(ekpc_by_day$day_mean))
ekpc_hour_summary_stats <- c(mean(ekpc_by_hour$hour_mean),
                             median(ekpc_by_hour$hour_mean),
                             IQR(ekpc_by_hour$hour_mean),
                             min(ekpc_by_hour$hour_mean),
                             max(ekpc_by_hour$hour_mean),
                             sd(ekpc_by_hour$hour_mean))

fe_periods <- fe_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
fe_by_year <- fe_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(FE_MW),
            year_sum = sum(FE_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
fe_by_month <- fe_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(FE_MW),
            month_sum = sum(FE_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
fe_by_week <- fe_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(FE_MW),
            week_sum = sum(FE_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
fe_by_wday <- fe_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(FE_MW),
            wday_sum = sum(FE_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
fe_by_day <- fe_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(FE_MW),
            day_sum = sum(FE_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
fe_by_hour <- fe_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(FE_MW),
            hour_sum = sum(FE_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
fe_year_summary_stats <- c(mean(fe_by_year$year_mean),
                           median(fe_by_year$year_mean),
                           IQR(fe_by_year$year_mean),
                           min(fe_by_year$year_mean),
                           max(fe_by_year$year_mean),
                           sd(fe_by_year$year_mean))
fe_month_summary_stats <- c(mean(fe_by_month$month_mean),
                            median(fe_by_month$month_mean),
                            IQR(fe_by_month$month_mean),
                            min(fe_by_month$month_mean),
                            max(fe_by_month$month_mean),
                            sd(fe_by_month$month_mean))
fe_week_summary_stats <- c(mean(fe_by_week$week_mean),
                           median(fe_by_week$week_mean),
                           IQR(fe_by_week$week_mean),
                           min(fe_by_week$week_mean),
                           max(fe_by_week$week_mean),
                           sd(fe_by_week$week_mean))
fe_wday_summary_stats <- c(mean(fe_by_wday$wday_mean),
                           median(fe_by_wday$wday_mean),
                           IQR(fe_by_wday$wday_mean),
                           min(fe_by_wday$wday_mean),
                           max(fe_by_wday$wday_mean),
                           sd(fe_by_wday$wday_mean))
fe_day_summary_stats <- c(mean(fe_by_day$day_mean),
                          median(fe_by_day$day_mean),
                          IQR(fe_by_day$day_mean),
                          min(fe_by_day$day_mean),
                          max(fe_by_day$day_mean),
                          sd(fe_by_day$day_mean))
fe_hour_summary_stats <- c(mean(fe_by_hour$hour_mean),
                           median(fe_by_hour$hour_mean),
                           IQR(fe_by_hour$hour_mean),
                           min(fe_by_hour$hour_mean),
                           max(fe_by_hour$hour_mean),
                           sd(fe_by_hour$hour_mean))

ni_periods <- ni_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
ni_by_year <- ni_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(NI_MW),
            year_sum = sum(NI_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
ni_by_month <- ni_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(NI_MW),
            month_sum = sum(NI_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
ni_by_week <- ni_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(NI_MW),
            week_sum = sum(NI_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
ni_by_wday <- ni_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(NI_MW),
            wday_sum = sum(NI_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
ni_by_day <- ni_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(NI_MW),
            day_sum = sum(NI_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
ni_by_hour <- ni_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(NI_MW),
            hour_sum = sum(NI_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
ni_year_summary_stats <- c(mean(ni_by_year$year_mean),
                           median(ni_by_year$year_mean),
                           IQR(ni_by_year$year_mean),
                           min(ni_by_year$year_mean),
                           max(ni_by_year$year_mean),
                           sd(ni_by_year$year_mean))
ni_month_summary_stats <- c(mean(ni_by_month$month_mean),
                            median(ni_by_month$month_mean),
                            IQR(ni_by_month$month_mean),
                            min(ni_by_month$month_mean),
                            max(ni_by_month$month_mean),
                            sd(ni_by_month$month_mean))
ni_week_summary_stats <- c(mean(ni_by_week$week_mean),
                           median(ni_by_week$week_mean),
                           IQR(ni_by_week$week_mean),
                           min(ni_by_week$week_mean),
                           max(ni_by_week$week_mean),
                           sd(ni_by_week$week_mean))
ni_wday_summary_stats <- c(mean(ni_by_wday$wday_mean),
                           median(ni_by_wday$wday_mean),
                           IQR(ni_by_wday$wday_mean),
                           min(ni_by_wday$wday_mean),
                           max(ni_by_wday$wday_mean),
                           sd(ni_by_wday$wday_mean))
ni_day_summary_stats <- c(mean(ni_by_day$day_mean),
                          median(ni_by_day$day_mean),
                          IQR(ni_by_day$day_mean),
                          min(ni_by_day$day_mean),
                          max(ni_by_day$day_mean),
                          sd(ni_by_day$day_mean))
ni_hour_summary_stats <- c(mean(ni_by_hour$hour_mean),
                           median(ni_by_hour$hour_mean),
                           IQR(ni_by_hour$hour_mean),
                           min(ni_by_hour$hour_mean),
                           max(ni_by_hour$hour_mean),
                           sd(ni_by_hour$hour_mean))

pjm_load_periods <- pjm_load_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
pjm_load_by_year <- pjm_load_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(PJM_Load_MW),
            year_sum = sum(PJM_Load_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
pjm_load_by_month <- pjm_load_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(PJM_Load_MW),
            month_sum = sum(PJM_Load_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
pjm_load_by_week <- pjm_load_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(PJM_Load_MW),
            week_sum = sum(PJM_Load_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
pjm_load_by_wday <- pjm_load_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(PJM_Load_MW),
            wday_sum = sum(PJM_Load_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
pjm_load_by_day <- pjm_load_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(PJM_Load_MW),
            day_sum = sum(PJM_Load_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
pjm_load_by_hour <- pjm_load_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(PJM_Load_MW),
            hour_sum = sum(PJM_Load_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
pjm_load_year_summary_stats <- c(mean(pjm_load_by_year$year_mean),
                                 median(pjm_load_by_year$year_mean),
                                 IQR(pjm_load_by_year$year_mean),
                                 min(pjm_load_by_year$year_mean),
                                 max(pjm_load_by_year$year_mean),
                                 sd(pjm_load_by_year$year_mean))
pjm_load_month_summary_stats <- c(mean(pjm_load_by_month$month_mean),
                                  median(pjm_load_by_month$month_mean),
                                  IQR(pjm_load_by_month$month_mean),
                                  min(pjm_load_by_month$month_mean),
                                  max(pjm_load_by_month$month_mean),
                                  sd(pjm_load_by_month$month_mean))
pjm_load_week_summary_stats <- c(mean(pjm_load_by_week$week_mean),
                                 median(pjm_load_by_week$week_mean),
                                 IQR(pjm_load_by_week$week_mean),
                                 min(pjm_load_by_week$week_mean),
                                 max(pjm_load_by_week$week_mean),
                                 sd(pjm_load_by_week$week_mean))
pjm_load_wday_summary_stats <- c(mean(pjm_load_by_wday$wday_mean),
                                 median(pjm_load_by_wday$wday_mean),
                                 IQR(pjm_load_by_wday$wday_mean),
                                 min(pjm_load_by_wday$wday_mean),
                                 max(pjm_load_by_wday$wday_mean),
                                 sd(pjm_load_by_wday$wday_mean))
pjm_load_day_summary_stats <- c(mean(pjm_load_by_day$day_mean),
                                median(pjm_load_by_day$day_mean),
                                IQR(pjm_load_by_day$day_mean),
                                min(pjm_load_by_day$day_mean),
                                max(pjm_load_by_day$day_mean),
                                sd(pjm_load_by_day$day_mean))
pjm_load_hour_summary_stats <- c(mean(pjm_load_by_hour$hour_mean),
                                 median(pjm_load_by_hour$hour_mean),
                                 IQR(pjm_load_by_hour$hour_mean),
                                 min(pjm_load_by_hour$hour_mean),
                                 max(pjm_load_by_hour$hour_mean),
                                 sd(pjm_load_by_hour$hour_mean))

pjme_periods <- pjme_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
pjme_by_year <- pjme_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(PJME_MW),
            year_sum = sum(PJME_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
pjme_by_month <- pjme_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(PJME_MW),
            month_sum = sum(PJME_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
pjme_by_week <- pjme_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(PJME_MW),
            week_sum = sum(PJME_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
pjme_by_wday <- pjme_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(PJME_MW),
            wday_sum = sum(PJME_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
pjme_by_day <- pjme_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(PJME_MW),
            day_sum = sum(PJME_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
pjme_by_hour <- pjme_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(PJME_MW),
            hour_sum = sum(PJME_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
pjme_year_summary_stats <- c(mean(pjme_by_year$year_mean),
                             median(pjme_by_year$year_mean),
                             IQR(pjme_by_year$year_mean),
                             min(pjme_by_year$year_mean),
                             max(pjme_by_year$year_mean),
                             sd(pjme_by_year$year_mean))
pjme_month_summary_stats <- c(mean(pjme_by_month$month_mean),
                              median(pjme_by_month$month_mean),
                              IQR(pjme_by_month$month_mean),
                              min(pjme_by_month$month_mean),
                              max(pjme_by_month$month_mean),
                              sd(pjme_by_month$month_mean))
pjme_week_summary_stats <- c(mean(pjme_by_week$week_mean),
                             median(pjme_by_week$week_mean),
                             IQR(pjme_by_week$week_mean),
                             min(pjme_by_week$week_mean),
                             max(pjme_by_week$week_mean),
                             sd(pjme_by_week$week_mean))
pjme_wday_summary_stats <- c(mean(pjme_by_wday$wday_mean),
                             median(pjme_by_wday$wday_mean),
                             IQR(pjme_by_wday$wday_mean),
                             min(pjme_by_wday$wday_mean),
                             max(pjme_by_wday$wday_mean),
                             sd(pjme_by_wday$wday_mean))
pjme_day_summary_stats <- c(mean(pjme_by_day$day_mean),
                            median(pjme_by_day$day_mean),
                            IQR(pjme_by_day$day_mean),
                            min(pjme_by_day$day_mean),
                            max(pjme_by_day$day_mean),
                            sd(pjme_by_day$day_mean))
pjme_hour_summary_stats <- c(mean(pjme_by_hour$hour_mean),
                             median(pjme_by_hour$hour_mean),
                             IQR(pjme_by_hour$hour_mean),
                             min(pjme_by_hour$hour_mean),
                             max(pjme_by_hour$hour_mean),
                             sd(pjme_by_hour$hour_mean))

pjmw_periods <- pjmw_tsibble %>%
  mutate(dt_year = year(Datetime),
         dt_month = month(Datetime),
         dt_week = week(Datetime),
         dt_wday = wday(Datetime),
         dt_day = day(Datetime),
         dt_hour = hour(Datetime))
pjmw_by_year <- pjmw_periods %>%
  index_by(dt_year) %>%
  summarise(year_mean = mean(PJMW_MW),
            year_sum = sum(PJMW_MW),
            year_count = n()) %>%
  mutate(year_share = year_sum / sum(year_sum)) %>%
  filter(year_count > 30)
pjmw_by_month <- pjmw_periods %>%
  index_by(dt_month) %>%
  summarise(month_mean = mean(PJMW_MW),
            month_sum = sum(PJMW_MW),
            month_count = n()) %>%
  mutate(month_share = month_sum / sum(month_sum)) %>%
  filter(month_count > 30)
pjmw_by_week <- pjmw_periods %>%
  index_by(dt_week) %>%
  summarise(week_mean = mean(PJMW_MW),
            week_sum = sum(PJMW_MW),
            week_count = n()) %>%
  mutate(week_share = week_sum / sum(week_sum)) %>%
  filter(week_count > 30)
pjmw_by_wday <- pjmw_periods %>%
  index_by(dt_wday) %>%
  summarise(wday_mean = mean(PJMW_MW),
            wday_sum = sum(PJMW_MW),
            wday_count = n()) %>%
  mutate(wday_share = wday_sum / sum(wday_sum)) %>%
  filter(wday_count > 30)
pjmw_by_day <- pjmw_periods %>%
  index_by(dt_day) %>%
  summarise(day_mean = mean(PJMW_MW),
            day_sum = sum(PJMW_MW),
            day_count = n()) %>%
  mutate(day_share = day_sum / sum(day_sum)) %>%
  filter(day_count > 30)
pjmw_by_hour <- pjmw_periods %>%
  index_by(dt_hour) %>%
  summarise(hour_mean = mean(PJMW_MW),
            hour_sum = sum(PJMW_MW),
            hour_count = n()) %>%
  mutate(hour_share = hour_sum / sum(hour_sum)) %>%
  filter(hour_count > 30)
pjmw_year_summary_stats <- c(mean(pjmw_by_year$year_mean),
                             median(pjmw_by_year$year_mean),
                             IQR(pjmw_by_year$year_mean),
                             min(pjmw_by_year$year_mean),
                             max(pjmw_by_year$year_mean),
                             sd(pjmw_by_year$year_mean))
pjmw_month_summary_stats <- c(mean(pjmw_by_month$month_mean),
                              median(pjmw_by_month$month_mean),
                              IQR(pjmw_by_month$month_mean),
                              min(pjmw_by_month$month_mean),
                              max(pjmw_by_month$month_mean),
                              sd(pjmw_by_month$month_mean))
pjmw_week_summary_stats <- c(mean(pjmw_by_week$week_mean),
                             median(pjmw_by_week$week_mean),
                             IQR(pjmw_by_week$week_mean),
                             min(pjmw_by_week$week_mean),
                             max(pjmw_by_week$week_mean),
                             sd(pjmw_by_week$week_mean))
pjmw_wday_summary_stats <- c(mean(pjmw_by_wday$wday_mean),
                             median(pjmw_by_wday$wday_mean),
                             IQR(pjmw_by_wday$wday_mean),
                             min(pjmw_by_wday$wday_mean),
                             max(pjmw_by_wday$wday_mean),
                             sd(pjmw_by_wday$wday_mean))
pjmw_day_summary_stats <- c(mean(pjmw_by_day$day_mean),
                            median(pjmw_by_day$day_mean),
                            IQR(pjmw_by_day$day_mean),
                            min(pjmw_by_day$day_mean),
                            max(pjmw_by_day$day_mean),
                            sd(pjmw_by_day$day_mean))
pjmw_hour_summary_stats <- c(mean(pjmw_by_hour$hour_mean),
                             median(pjmw_by_hour$hour_mean),
                             IQR(pjmw_by_hour$hour_mean),
                             min(pjmw_by_hour$hour_mean),
                             max(pjmw_by_hour$hour_mean),
                             sd(pjmw_by_hour$hour_mean))


### Combining the features into one row per time series

aep_features <- c(aep_summary_stats,
                  aep_year_summary_stats,
                  aep_month_summary_stats,
                  aep_week_summary_stats,
                  aep_wday_summary_stats,
                  aep_day_summary_stats,
                  aep_hour_summary_stats)
comed_features <- c(comed_summary_stats,
                    comed_year_summary_stats,
                    comed_month_summary_stats,
                    comed_week_summary_stats,
                    comed_wday_summary_stats,
                    comed_day_summary_stats,
                    comed_hour_summary_stats)
dayton_features <- c(dayton_summary_stats,
                     dayton_year_summary_stats,
                     dayton_month_summary_stats,
                     dayton_week_summary_stats,
                     dayton_wday_summary_stats,
                     dayton_day_summary_stats,
                     dayton_hour_summary_stats)
deok_features <- c(deok_summary_stats,
                   deok_year_summary_stats,
                   deok_month_summary_stats,
                   deok_week_summary_stats,
                   deok_wday_summary_stats,
                   deok_day_summary_stats,
                   deok_hour_summary_stats)
dom_features <- c(dom_summary_stats,
                  dom_year_summary_stats,
                  dom_month_summary_stats,
                  dom_week_summary_stats,
                  dom_wday_summary_stats,
                  dom_day_summary_stats,
                  dom_hour_summary_stats)
duq_features <- c(duq_summary_stats,
                  duq_year_summary_stats,
                  duq_month_summary_stats,
                  duq_week_summary_stats,
                  duq_wday_summary_stats,
                  duq_day_summary_stats,
                  duq_hour_summary_stats)
ekpc_features <- c(ekpc_summary_stats,
                   ekpc_year_summary_stats,
                   ekpc_month_summary_stats,
                   ekpc_week_summary_stats,
                   ekpc_wday_summary_stats,
                   ekpc_day_summary_stats,
                   ekpc_hour_summary_stats)
fe_features <- c(fe_summary_stats,
                 fe_year_summary_stats,
                 fe_month_summary_stats,
                 fe_week_summary_stats,
                 fe_wday_summary_stats,
                 fe_day_summary_stats,
                 fe_hour_summary_stats)
ni_features <- c(ni_summary_stats,
                 ni_year_summary_stats,
                 ni_month_summary_stats,
                 ni_week_summary_stats,
                 ni_wday_summary_stats,
                 ni_day_summary_stats,
                 ni_hour_summary_stats)
pjm_load_features <- c(pjm_load_summary_stats,
                       pjm_load_year_summary_stats,
                       pjm_load_month_summary_stats,
                       pjm_load_week_summary_stats,
                       pjm_load_wday_summary_stats,
                       pjm_load_day_summary_stats,
                       pjm_load_hour_summary_stats)
pjme_features <- c(pjme_summary_stats,
                   pjme_year_summary_stats,
                   pjme_month_summary_stats,
                   pjme_week_summary_stats,
                   pjme_wday_summary_stats,
                   pjme_day_summary_stats,
                   pjme_hour_summary_stats)
pjmw_features <- c(pjmw_summary_stats,
                   pjmw_year_summary_stats,
                   pjmw_month_summary_stats,
                   pjmw_week_summary_stats,
                   pjmw_wday_summary_stats,
                   pjmw_day_summary_stats,
                   pjmw_hour_summary_stats)

names(aep_features) <- c("Mean", "Median", "IQR", "Min", "Max", "SD",
                         "Mean Avg by Year", "Median Avg by Year",
                         "IQR of Avg by Year", "Min Avg by Year",
                         "Max Avg by Year", "SD of Avg by Year",
                         "Mean Avg by Month", "Median Avg by Month",
                         "IQR of Avg by Month", "Min Avg by Month",
                         "Max Avg by Month", "SD of Avg by Month",
                         "Mean Avg by Week", "Median Avg by Week",
                         "IQR of Avg by Week", "Min Avg by Week",
                         "Max Avg by Week", "SD of Avg by Week",
                         "Mean Avg by Day of Week",
                         "Median Avg by Day of Week",
                         "IQR of Avg by Day of Week",
                         "Min Avg by Day of Week",
                         "Max Avg by Day of Week",
                         "SD of Avg by Day of Week",
                         "Mean Avg by Day of Month",
                         "Median Avg by Day of Month",
                         "IQR of Avg by Day of Month",
                         "Min Avg by Day of Month",
                         "Max Avg by Day of Month",
                         "SD of Avg by Day of Month",
                         "Mean Avg by Hour", "Median Avg by Hour",
                         "IQR of Avg by Hour", "Min Avg by Hour",
                         "Max Avg by Hour", "SD of Avg by Hour")
names(comed_features) <- names(aep_features)
names(dayton_features) <- names(aep_features)
names(deok_features) <- names(aep_features)
names(dom_features) <- names(aep_features)
names(duq_features) <- names(aep_features)
names(ekpc_features) <- names(aep_features)
names(fe_features) <- names(aep_features)
names(ni_features) <- names(aep_features)
names(pjm_load_features) <- names(aep_features)
names(pjme_features) <- names(aep_features)
names(pjmw_features) <- names(aep_features)

summary_stats_features <- rbind(aep_features, comed_features, dayton_features,
                                deok_features, dom_features, duq_features,
                                ekpc_features, fe_features, ni_features,
                                pjm_load_features, pjme_features,
                                pjmw_features)
rownames(summary_stats_features) <- c("AEP", "COMED", "DAYTON",
                                      "DEOK", "DOM", "DUQ",
                                      "EKPC", "FE", "NI",
                                      "PJM_Load", "PJME", "PJMW")

# Remove features by year and by day of month - not relevant seasonalities
summary_stats_features <- summary_stats_features[, -c(7:12, 31:36)]

# ### Checking changes in summary stats features when removing outliers
# 
# # First create tsibbles and summary stats features with original data
# # Then back up the summary stats features' original values
# summary_stats_features_backup <- summary_stats_features
# 
# # Run right before creating new tsibbles
# 
# dayton_outlier <- 61571
# dom_outlier <- 40455
# fe_outlier <- 1
# pjmw_outlier <- 10149
# 
# dayton <- dayton[-dayton_outlier, ]
# dom <- dom[-dom_outlier, ]
# fe <- fe[-fe_outlier, ]
# pjmw <- pjmw[-pjmw_outlier, ]
# 
# # Run right after calculating new summary stats features
# 
# dayton_changes <- (summary_stats_features[3, ] -
#                      summary_stats_features_backup[3, ]) /
#   summary_stats_features_backup[3, ]
# dom_changes <- (summary_stats_features[5, ] -
#                   summary_stats_features_backup[5, ]) /
#   summary_stats_features_backup[5, ]
# fe_changes <- (summary_stats_features[8, ] -
#                  summary_stats_features_backup[8, ]) /
#   summary_stats_features_backup[8, ]
# pjmw_changes <- (summary_stats_features[12, ] -
#                    summary_stats_features_backup[12, ]) /
#   summary_stats_features_backup[12, ]
# 
# format(dayton_changes[dayton_changes > 0] * 100, scientific = FALSE)
# format(dom_changes[dom_changes > 0] * 100, scientific = FALSE)
# format(fe_changes[fe_changes > 0] * 100, scientific = FALSE)
# format(pjmw_changes[pjmw_changes > 0] * 100, scientific = FALSE)
# # Changes were all under 5%, except for minimums
# 
# # Do not need to remove outliers except for changing the minimums
# # Revert to original summary_stats_features
# summary_stats_features <- summary_stats_features_backup

# ### Checking changes in summary stats features when imputing outliers
# 
# # First create tsibbles and summary stats features with original data
# # Then back up the summary stats features' original values
# summary_stats_features_backup <- summary_stats_features
# 
# # Run right before creating new tsibbles
# 
# dayton_outlier <- 61571
# dom_outlier <- 40455
# fe_outlier <- 1
# pjmw_outlier <- 10149
# 
# dayton_outlier_dt <- ymd_hms(dayton[dayton_outlier, 1])
# dayton_outlier_year <- year(dayton_outlier_dt)
# dayton_outlier_month <- month(dayton_outlier_dt)
# dayton_outlier_wday <- wday(dayton_outlier_dt)
# dayton_outlier_impute <- mean(dayton[year(ymd_hms(dayton[, 1])) ==
#                                        dayton_outlier_year &
#               month(ymd_hms(dayton[, 1])) == dayton_outlier_month &
#               wday(ymd_hms(dayton[, 1])) == dayton_outlier_wday, 2])
# # 1842.781
# 
# dom_outlier_dt <- ymd_hms(dom[dom_outlier, 1])
# dom_outlier_year <- year(dom_outlier_dt)
# dom_outlier_month <- month(dom_outlier_dt)
# dom_outlier_wday <- wday(dom_outlier_dt)
# dom_outlier_impute <- mean(dom[year(ymd_hms(dom[, 1])) == dom_outlier_year &
#            month(ymd_hms(dom[, 1])) == dom_outlier_month &
#            wday(ymd_hms(dom[, 1])) == dom_outlier_wday, 2])
# # 11456.98
# 
# fe_outlier_dt <- ymd_hms(fe[fe_outlier, 1])
# fe_outlier_year <- year(fe_outlier_dt)
# fe_outlier_month <- month(fe_outlier_dt)
# fe_outlier_wday <- wday(fe_outlier_dt)
# fe_outlier_impute <- mean(fe[year(ymd_hms(fe[, 1])) == fe_outlier_year &
#            month(ymd_hms(fe[, 1])) == fe_outlier_month &
#            wday(ymd_hms(fe[, 1])) == fe_outlier_wday, 2])
# # 8799.479
# 
# pjmw_outlier_dt <- ymd_hms(pjmw[pjmw_outlier, 1])
# pjmw_outlier_year <- year(pjmw_outlier_dt)
# pjmw_outlier_month <- month(pjmw_outlier_dt)
# pjmw_outlier_wday <- wday(pjmw_outlier_dt)
# pjmw_outlier_impute <- mean(pjmw[year(ymd_hms(pjmw[, 1])) == pjmw_outlier_year &
#           month(ymd_hms(pjmw[, 1])) == pjmw_outlier_month &
#           wday(ymd_hms(pjmw[, 1])) == pjmw_outlier_wday, 2])
# # 5249
# 
# dayton[dayton_outlier, 2] <- dayton_outlier_impute
# dom[dom_outlier, 2] <- dom_outlier_impute
# fe[fe_outlier, 2] <- fe_outlier_impute
# pjmw[pjmw_outlier, 2] <- pjmw_outlier_impute
# 
# # Run right after calculating new summary stats features
# 
# dayton_changes <- (summary_stats_features[3, ] -
#                      summary_stats_features_backup[3, ]) /
#   summary_stats_features_backup[3, ]
# dom_changes <- (summary_stats_features[5, ] -
#                   summary_stats_features_backup[5, ]) /
#   summary_stats_features_backup[5, ]
# fe_changes <- (summary_stats_features[8, ] -
#                  summary_stats_features_backup[8, ]) /
#   summary_stats_features_backup[8, ]
# pjmw_changes <- (summary_stats_features[12, ] -
#                    summary_stats_features_backup[12, ]) /
#   summary_stats_features_backup[12, ]
# 
# format(dayton_changes[dayton_changes > 0] * 100, scientific = FALSE)
# format(dom_changes[dom_changes > 0] * 100, scientific = FALSE)
# format(fe_changes[fe_changes > 0] * 100, scientific = FALSE)
# format(pjmw_changes[pjmw_changes > 0] * 100, scientific = FALSE)
# # Changes were all under 5%, except for minimums
# 
# # Do not need to remove outliers except for changing the minimums
# # Revert to original summary_stats_features
# summary_stats_features <- summary_stats_features_backup

# Using original dom, fe, and pjmw time series without any removing or imputing:
head(sort(dom[, 2]), 2)
# dom second smallest value: 5518
head(sort(fe[, 2]), 2)
# fe second smallest value: 4909
head(sort(pjmw[, 2]), 2)
# pjmw second smallest value: 2553

summary_stats_features[5, 4] <- sort(dom[, 2])[2]
summary_stats_features[8, 4] <- sort(fe[, 2])[2]
summary_stats_features[12, 4] <- sort(pjmw[, 2])[2]

### Put the features into a df: feature 6
head(summary_stats_features)

### Young In Kang
### Features 7: Differencing, d and D
####### aep ######
aep_energy=coredata(aep.xts)
head(aep_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(aep_energy)
plot.ts(log(aep_energy))
plot.ts(sqrt(aep_energy))

# Sqrt transformation makes variance more constant.
sqrt_aep_energy = sqrt(aep_energy)

# First check only regular differencing 
# d=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff1=diff(sqrt_aep_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1, lag = 10000, main="d = 1")   

diff1=diff(sqrt_aep_energy,lag=1,differences=2)  
acf(diff1, lag = 10000, main="d = 2")   

diff1=diff(sqrt_aep_energy,lag=1,differences=3)  
acf(diff1, lag = 10000, main="d = 3")   

diff1=diff(sqrt_aep_energy,lag=1,differences=4)  
acf(diff1, lag = 10000, main="d = 4")   

diff1=diff(sqrt_aep_energy,lag=1,differences=5)  
acf(diff1, lag = 10000, main="d = 5")   

diff1=diff(sqrt_aep_energy,lag=1,differences=6) 
acf(diff1, lag = 10000, main="d = 6")  

# Second check only seasonal differencing
# D=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff24=diff(sqrt_aep_energy,lag=24,differences=1)
acf(diff24, lag = 100000, main="D = 1")

diff24=diff(sqrt_aep_energy,lag=24,differences=2)
acf(diff24, lag = 10000, main="D = 2")   

diff24=diff(sqrt_aep_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")   

diff24=diff(sqrt_aep_energy,lag=24,differences=4)
acf(diff24, lag = 10000, main="D = 4")

diff24=diff(sqrt_aep_energy,lag=24,differences=5)
acf(diff24, lag = 10000, main="D = 5")

diff24=diff(sqrt_aep_energy,lag=24,differences=6)
acf(diff24, lag = 10000, main="D = 6")

#Now do seasonal differencing of regular differencing
# (d=1 and D=1) or (d=1 and D=2) is sufficiently stationary
diff1=diff(sqrt_aep_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000,main="d=1, D=1")      

diff1=diff(sqrt_aep_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000,main="d=1, D=2")      

diff1=diff(sqrt_aep_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=3)
acf(diff1diff24, lag = 10000,main="d=1, D=3")     

# Best from each differencing 
par(mfrow=c(2,1))

# d=3
diff1=diff(sqrt_aep_energy,lag=1,differences=3)
acf(diff1, lag = 10000, main="d = 3")

# D=3
diff24=diff(sqrt_aep_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")

# d=1, D=1
diff1=diff(sqrt_aep_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000, main="d = 1, D = 1")

# d=1, D=2
diff1=diff(sqrt_aep_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000, main="d = 1, D = 2")

#Therefore, d=1, D=1 for aep data
dev.off()

####### comed ####### 
comed_energy=coredata( comed.xts)
head(comed_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(comed_energy)
plot.ts(log(comed_energy))
plot.ts(sqrt(comed_energy))
# No transformation needed to make variance more constant.

# First check only regular differencing 
# d=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff1=diff(comed_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1, lag = 10000, main="d = 1")   

diff1=diff(comed_energy,lag=1,differences=2)  
acf(diff1, lag = 10000, main="d = 2")   

diff1=diff(comed_energy,lag=1,differences=3)  
acf(diff1, lag = 10000, main="d = 3")   

diff1=diff(comed_energy,lag=1,differences=4)  
acf(diff1, lag = 10000, main="d = 4")   

diff1=diff(comed_energy,lag=1,differences=5)  
acf(diff1, lag = 10000, main="d = 5")   

diff1=diff(comed_energy,lag=1,differences=6) 
acf(diff1, lag = 10000, main="d = 6")  

# Second check only seasonal differencing
# D=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff24=diff(comed_energy,lag=24,differences=1)
acf(diff24, lag = 100000, main="D = 1")

diff24=diff(comed_energy,lag=24,differences=2)
acf(diff24, lag = 10000, main="D = 2")   

diff24=diff(comed_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")   

diff24=diff(comed_energy,lag=24,differences=4)
acf(diff24, lag = 10000, main="D = 4")

diff24=diff(comed_energy,lag=24,differences=5)
acf(diff24, lag = 10000, main="D = 5")

diff24=diff(comed_energy,lag=24,differences=6)
acf(diff24, lag = 10000, main="D = 6")

# Now do seasonal differencing of regular differencing
# d=1 and D=2 is sufficiently stationary
diff1=diff(comed_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000,main="d=1, D=1")      

diff1=diff(comed_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000,main="d=1, D=2")      

diff1=diff(comed_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=3)
acf(diff1diff24, lag = 10000,main="d=1, D=3")     

# Best from each differencing 
par(mfrow=c(3,1))

# d=3
diff1=diff(comed_energy,lag=1,differences=3)
acf(diff1, lag = 10000, main="d = 3")

# D=3
diff24=diff(comed_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")

# d=1, D=2
diff1=diff(comed_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000, main="d = 1, D = 2")

#Therefore, d=1, D=2 for comed data
dev.off()

####### dayton ####### 
dayton_energy=coredata( dayton.xts)
head(dayton_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(dayton_energy)
plot.ts(log(dayton_energy))
plot.ts(sqrt(dayton_energy))
# No transformation needed to make variance more constant.

# First check only regular differencing 
# d=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff1=diff(dayton_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1, lag = 10000, main="d = 1")   

diff1=diff(dayton_energy,lag=1,differences=2)  
acf(diff1, lag = 10000, main="d = 2")   

diff1=diff(dayton_energy,lag=1,differences=3)  
acf(diff1, lag = 10000, main="d = 3")   

diff1=diff(dayton_energy,lag=1,differences=4)  
acf(diff1, lag = 10000, main="d = 4")   

diff1=diff(dayton_energy,lag=1,differences=5)  
acf(diff1, lag = 10000, main="d = 5")   

diff1=diff(dayton_energy,lag=1,differences=6) 
acf(diff1, lag = 10000, main="d = 6")  

# Second check only seasonal differencing
# D=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff24=diff(dayton_energy,lag=24,differences=1)
acf(diff24, lag = 10000, main="D = 1")

diff24=diff(dayton_energy,lag=24,differences=2)
acf(diff24, lag = 10000, main="D = 2")   

diff24=diff(dayton_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")   

diff24=diff(dayton_energy,lag=24,differences=4)
acf(diff24, lag = 10000, main="D = 4")

diff24=diff(dayton_energy,lag=24,differences=5)
acf(diff24, lag = 10000, main="D = 5")

diff24=diff(dayton_energy,lag=24,differences=6)
acf(diff24, lag = 10000, main="D = 6")

#Now do seasonal differencing of regular differencing
# (d=1 and D=1) or (d=1 and D=2) is sufficiently stationary
diff1=diff(dayton_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000,main="d=1, D=1")      

diff1=diff(dayton_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000,main="d=1, D=2")      

diff1=diff(dayton_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=3)
acf(diff1diff24, lag = 10000,main="d=1, D=3")     

# Best from each differencing 
par(mfrow=c(2,1))

# d=3
diff1=diff(dayton_energy,lag=1,differences=3)
acf(diff1, lag = 10000, main="d = 3")

# D=3
diff24=diff(dayton_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")

# d=1, D=1
diff1=diff(dayton_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000, main="d = 1, D = 1")

# d=1, D=2
diff1=diff(dayton_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=3)
acf(diff1diff24, lag = 10000, main="d = 1, D = 2")

#Therefore, d=1, D=1 for dayton data
dev.off()

####### deok ####### 
deok_energy=coredata( deok.xts)
head(deok_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(deok_energy)
plot.ts(log(deok_energy))
plot.ts(sqrt(deok_energy))

# Log transformation variance more constant.
log_deok_energy = log(deok_energy)

# First check only regular differencing 
# d=3 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff1=diff(log_deok_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1, lag = 10000, main="d = 1")   

diff1=diff(log_deok_energy,lag=1,differences=2)  
acf(diff1, lag = 10000, main="d = 2")   

diff1=diff(log_deok_energy,lag=1,differences=3)  
acf(diff1, lag = 10000, main="d = 3")   

diff1=diff(log_deok_energy,lag=1,differences=4)  
acf(diff1, lag = 10000, main="d = 4")   

diff1=diff(log_deok_energy,lag=1,differences=5)  
acf(diff1, lag = 10000, main="d = 5")   

diff1=diff(log_deok_energy,lag=1,differences=6) 
acf(diff1, lag = 10000, main="d = 6")  

# Second check only seasonal differencing
# D=2 looks sufficiently stationary. 
par(mfrow=c(3,2))
diff24=diff(log_deok_energy,lag=24,differences=1)
acf(diff24, lag = 100000, main="D = 1")

diff24=diff(log_deok_energy,lag=24,differences=2)
acf(diff24, lag = 10000, main="D = 2")   

diff24=diff(log_deok_energy,lag=24,differences=3)
acf(diff24, lag = 10000, main="D = 3")   

diff24=diff(log_deok_energy,lag=24,differences=4)
acf(diff24, lag = 10000, main="D = 4")

diff24=diff(log_deok_energy,lag=24,differences=5)
acf(diff24, lag = 10000, main="D = 5")

diff24=diff(log_deok_energy,lag=24,differences=6)
acf(diff24, lag = 10000, main="D = 6")


#Now do seasonal differencing of regular differencing
# (d=1 and D=1) or (d=1 and D=2) is sufficiently stationary
diff1=diff(log_deok_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000,main="d=1, D=1")      

diff1=diff(log_deok_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000,main="d=1, D=2")      

diff1=diff(log_deok_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=3)
acf(diff1diff24, lag = 10000,main="d=1, D=3")     

# Best from each differencing 
par(mfrow=c(2,1))

# d=3
diff1=diff(log_deok_energy,lag=1,differences=3)
acf(diff1, lag = 10000, main="d = 3")

# D=2
diff24=diff(log_deok_energy,lag=24,differences=2)
acf(diff24, lag = 10000, main="D = 3")

# d=1, D=1
diff1=diff(log_deok_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24, lag = 10000, main="d = 1, D = 1")

# d=1, D=2
diff1=diff(log_deok_energy,lag=1,differences=1)
diff1diff24=diff(diff1,lag=24,differences=2)
acf(diff1diff24, lag = 10000, main="d = 1, D = 2")

#Therefore, d=1, D=1 for deok data
dev.off()

####### dom ####### 
dom_energy=coredata(dom.xts)
head(dom_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(dom_energy)
plot.ts(log(dom_energy))
plot.ts(sqrt(dom_energy))
#No improvement.

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(dom_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(dom_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(dom_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(dom_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(dom_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(dom_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=4 looks most stationary. 

diff1=diff(dom_energy,lag=1,differences=4) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

# Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(dom_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(dom_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(dom_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(dom_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(dom_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(dom_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary.

diff24=diff(dom_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,main="d=4, D=1")      
Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.


par(mfrow=c(3,1))
acf(diff1,main="d = 4")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 4, D = 1")
#Therefore, d=4, D=1 for dom data
dev.off()

####### duq ####### 
duq_energy=coredata(duq.xts)
head(duq_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(duq_energy)
plot.ts(log(duq_energy))
plot.ts(sqrt(duq_energy))
#No improvement

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(duq_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(duq_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(duq_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(duq_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(duq_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(duq_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=5 looks most stationary. 

diff1=diff(duq_energy,lag=1,differences=5) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

# Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(duq_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(duq_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(duq_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(duq_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(duq_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(duq_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(duq_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,lag.max=50,main="d=3, D=1")      

Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 5")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 5, D = 1")
#Therefore, d=5, D=0 for duq data
dev.off()

####### ekpc ####### 
ekpc_energy=coredata(ekpc.xts)
head(ekpc_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(ekpc_energy)
plot.ts(log(ekpc_energy))
plot.ts(sqrt(ekpc_energy))
#Log transformation makes variance more constant.
ekpc_energy<-log(ekpc_energy)

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(ekpc_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(ekpc_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(ekpc_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(ekpc_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(ekpc_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(ekpc_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=3 looks most stationary. 

diff1=diff(ekpc_energy,lag=1,differences=3) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#  Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(ekpc_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(ekpc_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(ekpc_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(ekpc_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(ekpc_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(ekpc_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary.

diff24=diff(ekpc_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,main="d=3, D=1")      
Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.


par(mfrow=c(3,1))
acf(diff1,main="d = 3")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 3, D = 1")
#Therefore, d=3, D=0 for ekpc data
dev.off()

####### fe ####### 
fe_energy=coredata(fe.xts)
head(fe_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(fe_energy)
plot.ts(log(fe_energy))
plot.ts(sqrt(fe_energy))
#Transformations don't make variance more constant.

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(fe_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(fe_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(fe_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(fe_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(fe_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(fe_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=5 looks most stationary. 

diff1=diff(fe_energy,lag=1,differences=5) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

# Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(fe_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(fe_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(fe_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(fe_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(fe_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(fe_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(fe_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,lag.max=50,main="d=5, D=1")      

Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 5")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 5, D = 1")
#Therefore, d=5, D=1 for fe data
dev.off()

####### ni ####### 
ni_energy=coredata(ni.xts)
head(ni_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(ni_energy)
plot.ts(log(ni_energy))
plot.ts(sqrt(ni_energy))
#Log transformation makes variance more constant.
ni_energy<-log(ni_energy)

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(ni_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(ni_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(ni_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(ni_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(ni_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(ni_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=5 looks most stationary. 

diff1=diff(ni_energy,lag=1,differences=5) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

# Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(ni_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(ni_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(ni_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(ni_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(ni_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(ni_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(ni_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,lag.max=50,main="d=5, D=1")      

Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 5")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 5, D = 1")
#Therefore, d=5, D=0 for ni data
dev.off()

####### pjm_load ####### 
pjm_load_energy=coredata(pjm_load.xts)
head(pjm_load_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(pjm_load_energy)
plot.ts(log(pjm_load_energy))
plot.ts(sqrt(pjm_load_energy))
#No improvement

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(pjm_load_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(pjm_load_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(pjm_load_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(pjm_load_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(pjm_load_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(pjm_load_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")   
#d=6 looks most stationary. 

diff1=diff(pjm_load_energy,lag=1,differences=6) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#  Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(pjm_load_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(pjm_load_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(pjm_load_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(pjm_load_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(pjm_load_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(pjm_load_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(pjm_load_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,main="d=6, D=1")      

Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 6")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 6, D = 1")
#Therefore, d=6, D=1 for pjm_load data
dev.off()

####### pjme ####### 
pjme_energy=coredata(pjme.xts)
head(pjme_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(pjme_energy)
plot.ts(log(pjme_energy))
plot.ts(sqrt(pjme_energy))
#Log transformation makes var more constant.
pjme_energy<-log(pjme_energy)

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(pjme_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(pjme_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(pjme_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(pjme_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(pjme_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(pjme_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")
#d=5 looks most stationary. 

diff1=diff(pjme_energy,lag=1,differences=5) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#  Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(pjme_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(pjme_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(pjme_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(pjme_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(pjme_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(pjme_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(pjme_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,main="d=5, D=1")      


Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 5")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 5, D = 1")
#Therefore, d=5, D=1 for pjme data
dev.off()

####### pjmw #######
pjmw_energy=coredata(pjmw.xts)
head(pjmw_energy)

# Check how much differencing should be done
par(mfrow=c(3,1))
plot.ts(pjmw_energy)
plot.ts(log(pjmw_energy))
plot.ts(sqrt(pjmw_energy))
#Log transformation makes var more constant.
pjmw_energy<-log(pjmw_energy)

# First check only regular differencing 
par(mfrow=c(3,2))
diff1=diff(pjmw_energy,lag=1,differences=1)  #This does regular differencing for trend over time
acf(diff1,main="d = 1")   
diff1=diff(pjmw_energy,lag=1,differences=2)  
acf(diff1,main="d = 2")   
diff1=diff(pjmw_energy,lag=1,differences=3)  
acf(diff1,main="d = 3")   
diff1=diff(pjmw_energy,lag=1,differences=4)  
acf(diff1,main="d = 4")   
diff1=diff(pjmw_energy,lag=1,differences=5)  
acf(diff1,main="d = 5")   
diff1=diff(pjmw_energy,lag=1,differences=6) 
acf(diff1,main="d = 6")
#d=5 looks most stationary. 


diff1=diff(pjmw_energy,lag=1,differences=5) 
Box.test(diff1, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

# Second check only seasonal differencing
par(mfrow=c(3,2))
diff24=diff(pjmw_energy,lag=24,differences=1)
acf(diff24,main="D = 1")
diff24=diff(pjmw_energy,lag=24,differences=2)
acf(diff24,main="D = 2")    
diff24=diff(pjmw_energy,lag=24,differences=3)
acf(diff24,main="D = 3")    
diff24=diff(pjmw_energy,lag=24,differences=4)
acf(diff24,main="D = 4")
diff24=diff(pjmw_energy,lag=24,differences=5)
acf(diff24,main="D = 5")
diff24=diff(pjmw_energy,lag=24,differences=6)
acf(diff24,main="D = 6")
#D=1 looks most stationary (Not much improvement after D=1)

diff24=diff(pjmw_energy,lag=24,differences=1)
Box.test(diff24, lag=20,type = "Ljung-Box")
#Results of all differencing are not white noise.

#Now do seasonal differencing of regular differencing
diff1diff24=diff(diff1,lag=24,differences=1)
acf(diff1diff24,main="d=5, D=1")      


Box.test(diff1diff24, lag=20,type = "Ljung-Box")
#Not WN.

par(mfrow=c(3,1))
acf(diff1,main="d = 5")
acf(diff24,main="D = 1")
acf(diff1diff24,main="d = 5, D = 1")
#Therefore, d=5, D=0 for pjmw data
dev.off()

# Create data drame features for differencing 
d <- c(1,1,1,1,4,5,3,5,5,6,5,5)
D <- c(1,2,1,1,1,0,0,1,0,1,1,0)
differencing <- data.frame(d, D)
print(differencing)

### Features 8: Sample Size of each TS
name_chr <- list(c("aep","comed","dayton","deok","dom","duq","ekpc","fe","ni","pjm_load","pjme","pjmw"))
name_list <- list(c(aep,comed,dayton,deok,dom,duq,ekpc,fe,ni,pjm_load,pjme,pjmw))
sample_size <- c()
for (i in seq(1,24,2)) {
  sample_size[(i+1)/2] <- length(name_list[[1]][[i]])
}
print(sample_size)

df1 <- data.frame(name_chr,sample_size)
colnames(df1) <- c('name','sample_size')
print(df1)

### Features 9: Number of times the time series goes above the mean
#All datasets 
mean_vec <- c()
for (i in seq(2,24,2)) {
  mean_vec[i/2] <- round(mean(name_list[[1]][[i]]))
}
print(mean_vec)

num_vec <- c()
for (i in seq(2,24,2)) {
  num_vec[i/2] <- sum(name_list[[1]][[i]]>mean_vec[i/2])
}
print(num_vec)

df2 <- data.frame(name_chr,mean_vec,num_vec)
colnames(df2) <- c('name','mean','num_above_mean')

print(df2)

### Features 10: How many times is the time series more 
###             than two standard deviations from the mean
# All datasets 
sd_vec <- c()
for (i in seq(2,24,2)) {
  sd_vec[i/2] <- round(sd(name_list[[1]][[i]]))
}
above <- mean_vec + 2*sd_vec
below <- mean_vec - 2*sd_vec

over_sd_vec <- c()
for (i in seq(2,24,2)) {
  over_sd_vec[i/2] <- sum(name_list[[1]][[i]] > above[i/2] | name_list[[1]][[i]] < below[i/2])
}
print(over_sd_vec)

df3 <- data.frame(name_chr,sd_vec,over_sd_vec)
colnames(df3) <- c('name','sd','num_of_over_2*sd')
print(df3)

### Put the features into a df: feature 7, 8, 9, 10
df4 <- data.frame(name_chr,d,D,sample_size,num_vec,round(num_vec/sample_size,2),over_sd_vec,round(over_sd_vec/sample_size,2))
colnames(df4) <- c('name','d','D','sample_size','num_above_mean','%_above_mean','num_of_over_2*sd','%_over_2*sd')

print(df4)

### Combine all features category into a df: feature 1 - 10
# Feature 1 
acf_pacf_features_df
# Feature 2 - 5
daniel_features_df
# Feature 6
summary_stats_features
# Feature 7 - 10 
df4

features = cbind(acf_pacf_features_df, daniel_features_df, 
                 summary_stats_features, df4[,-1])

# View the first few lines of this new data set. 
head(round(features,3)) 
dim(features)

### Write features to csv file
# write.csv(features, "Task2_features.csv")

########################### 
# TASK 2: K MEANS CLUSTERING 
########################### 
# Create labels (ID) for the time series created, i.e., for each row of the new data matrix. 
models = c("AEP", "COMED", "DAYTON",
           "DEOK", "DOM", "DUQ",
           "EKPC", "FE", "NI",
           "PJM_Load", "PJME", "PJMW")

### K-means clustering - 2 clusters
### Set seed to make the results reproducible 
set.seed(1)
kmeans = kmeans(features,2)

# Cluster means
centers = kmeans$centers
centers

# Clusters from k-means
cluster=kmeans$cluster
cluster

# Find which cluster means are most different using percentage
percentage_inc = apply(centers, 2, function(x) {
  round((x[1]-x[2])/x[1] * 100, 3)
})
percentage_inc

# percentage greater than 50 used as cutoff
index = which(abs(percentage_inc) > 50)
index
centers[,index]
percentage_inc[index]

# View the data frame with the IDs for each time series 
clusterdata = data.frame(models, features, cluster)

# Classification table tells us whether we have some misclassification.
table(clusterdata$models, clusterdata$cluster)

# Plots showing the cluster. 
par(mfrow=c(3,3))
### median average by month vs PACF year seasonal lag 3
plot(clusterdata[,61], clusterdata[,48],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-0.008, 0.003),
     main="Fig 26.1\n Average Median by Month and \n Pacf of yearly cycle of seasonal lag 3", 
     xlab="Median Avg by Month", ylab="pacf year seas 3")
text(clusterdata[,61], clusterdata[,48], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs Max
plot(clusterdata[,22], clusterdata[,58],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-1000, 20000),
     main='Fig 26.2\n Acf of yearly cycle of seasonal lag 1 and\n maximum energy consumption in time series',
     xlab="acf year seas 1", ylab="Max")
text(clusterdata[,22], clusterdata[,58], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs PACF month seasonal lag 1
plot(clusterdata[,22], clusterdata[,40],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-0.1, 0.5),
     main="Fig 26.3\n Acf of yearly cycle and \n partial Acf of monthly cycle of seasonal lag 1", 
     xlab="acf year seas 1", ylab="pacf month seas 1")
text(clusterdata[,22], clusterdata[,40], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs Box Cox 51
plot(clusterdata[,22], clusterdata[,51],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(0, 30),
     main="Fig 26.4\n Acf of yearly cycle of seasonal lag 1 and\n Order of Box-Cox transformation", 
     xlab="acf year seas 1", ylab="order of box cox")
text(clusterdata[,22], clusterdata[,51], labels=clusterdata$cluster,pos=1)

### By month vs By week 
# mean by month vs mean by week
plot(clusterdata[,60], clusterdata[,66],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-200, 3500),
     main="Fig 26.5\n Average Mean by Month and by Week", 
     xlab="mean average by month", ylab="mean average by week")
text(clusterdata[,60], clusterdata[,66], labels=clusterdata$cluster,pos=1)

# IQR by month vs IQR by week
plot(clusterdata[,62], clusterdata[,68],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-1000, 35000),
     main="Fig 26.6\n IQR by Month and by Week", 
     xlab="IQR of average by month", ylab="IQR of average by week")
text(clusterdata[,62], clusterdata[,68], labels=clusterdata$cluster,pos=1)

### By month vs By hour 
# std dev by month vs std dev by hour
plot(clusterdata[,65], clusterdata[,83],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-1000, 40000),
     main="Fig 26.7\n Standard deviation by Month and by Hour", 
     xlab="standard deviation of average by month", ylab="standard deviation of average by hour")
text(clusterdata[,65], clusterdata[,83], labels=clusterdata$cluster,pos=1)

# min av by month vs median by hour
plot(clusterdata[,63], clusterdata[,81],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim=c(-1000, 6500),
     main="Fig 26.8\n Average Minimum by Month and by Hour", 
     xlab="minimum average by month", ylab="minimum average by hour")
text(clusterdata[,63], clusterdata[,81], labels=clusterdata$cluster,pos=1)

### By av by day of the week vs By hour 
# min av by day of week vs min av by hour
plot(clusterdata[,76], clusterdata[,82],
     col=c("red","blue")[unclass(clusterdata$cluster)],
     pch=c(16,17)[unclass(clusterdata$cluster)], ylim = c(-2000, 27000),
     main="Fig 26.9\n Average Maximum by Day of Week and by Hour", 
     xlab="Max average by day of week", ylab="max average by hour")
text(clusterdata[,76], clusterdata[,82], labels=clusterdata$cluster,pos=1)

dev.off()

### K-means clustering - 3 clusters
set.seed(1)
kmeans = kmeans(features,3)

# Cluster means
centers = kmeans$centers
centers

# Clusters from k-means
cluster=kmeans$cluster
cluster

# View the data frame with the IDs for each time series 
clusterdata = data.frame(models, features, cluster)

# Classification table tells us whether we have some misclassification.
table(clusterdata$models, clusterdata$cluster)

# Plots showing the cluster. 
par(mfrow=c(3,3))
### median average by month vs PACF year seasonal lag 3
plot(clusterdata[,61], clusterdata[,48],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17, 18)[unclass(clusterdata$cluster)], ylim=c(-0.008, 0.003),
     main="Fig 27.1\n Average Median by Month and \n Pacf of yearly cycle of seasonal lag 3", 
     xlab="Median Avg by Month", ylab="pacf year seas 3")
text(clusterdata[,61], clusterdata[,48], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs Max
plot(clusterdata[,22], clusterdata[,58],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-1000, 20000),
     main='Fig 27.2\n Acf of yearly cycle of seasonal lag 1 and\n maximum energy consumption in time series',
     xlab="acf year seas 1", ylab="Max")
text(clusterdata[,22], clusterdata[,58], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs PACF month seasonal lag 1
plot(clusterdata[,22], clusterdata[,40],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-0.1, 0.5),
     main="Fig 27.3\n Acf of yearly cycle and \n partial Acf of monthly cycle of seasonal lag 1", 
     xlab="acf year seas 1", ylab="pacf month seas 1")
text(clusterdata[,22], clusterdata[,40], labels=clusterdata$cluster,pos=1)

### ACF year seasonal lag 1 vs Box Cox 51
plot(clusterdata[,22], clusterdata[,51],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(0, 30),
     main="Fig 27.4\n Acf of yearly cycle of seasonal lag 1 and\n Order of Box-Cox transformation", 
     xlab="acf year seas 1", ylab="order of box cox")
text(clusterdata[,22], clusterdata[,51], labels=clusterdata$cluster,pos=1)

### By month vs By week 
# mean by month vs mean by week
plot(clusterdata[,60], clusterdata[,66],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-200, 3500),
     main="Fig 27.5\n Average Mean by Month and by Week", 
     xlab="mean average by month", ylab="mean average by week")
text(clusterdata[,60], clusterdata[,66], labels=clusterdata$cluster,pos=1)

# IQR by month vs IQR by week
plot(clusterdata[,62], clusterdata[,68],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-1000, 35000),
     main="Fig 27.6\n IQR by Month and by Week", 
     xlab="IQR of average by month", ylab="IQR of average by week")
text(clusterdata[,62], clusterdata[,68], labels=clusterdata$cluster,pos=1)

### By month vs By hour 
# std dev by month vs std dev by hour
plot(clusterdata[,65], clusterdata[,83],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-1000, 40000),
     main="Fig 27.7\n Standard deviation by Month and by Hour", 
     xlab="standard deviation of average by month", ylab="standard deviation of average by hour")
text(clusterdata[,65], clusterdata[,83], labels=clusterdata$cluster,pos=1)

# min av by month vs median by hour
plot(clusterdata[,63], clusterdata[,81],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim=c(-1000, 6500),
     main="Fig 27.8\n Average Minimum by Month and by Hour", 
     xlab="minimum average by month", ylab="minimum average by hour")
text(clusterdata[,63], clusterdata[,81], labels=clusterdata$cluster,pos=1)

### By av by day of the week vs By hour 
# min av by day of week vs min av by hour
plot(clusterdata[,76], clusterdata[,82],
     col=c("red","blue", "black")[unclass(clusterdata$cluster)],
     pch=c(16,17,18)[unclass(clusterdata$cluster)], ylim = c(-2000, 27000),
     main="Fig 27.9\n Average Maximum by Day of Week and by Hour", 
     xlab="Max average by day of week", ylab="max average by hour")
text(clusterdata[,76], clusterdata[,82], labels=clusterdata$cluster,pos=1)

dev.off()


########################### 
# TASK 3:PROPHET
########################### 

library(lubridate)
library(prophet)
library(tidyverse)

##### TASK 3 PROPHET MODELS #####



#### AEP data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(aep_date); head(hour); length(hour)
year = year(aep_date); head(year); length(year)
week = week(aep_date); head(week); length(week)
day = day(aep_date); head(day); length(day)
weekday = wday(aep_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(aep_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
aep_data = data.frame(year, month, day, week, weekday, hour, aep_mw)
head(aep_data)
aep_newdata = aep_data[order(year, month, day, week, weekday, hour),]
head(aep_newdata)

aep_pro <- data.frame(aep_date,aep_mw, year, month, day, week, weekday, hour)
head(aep_pro)


## Creating training testing data
aep_train <- aep_pro[aep_pro$year >=2014&aep_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
aep_test <- aep_pro[aep_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


aep_train$ds = aep_train$aep_date
aep_train$y = aep_train$aep_mw
aep_training_prophet=aep_train[,3:length(aep_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, aep_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
aep_rmse <- sqrt(sum((myforecast[,'yhat'] - aep_test["aep_mw"])^2) / 5880)
aep_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of aep", main="Prophet fit and forecast(aep)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of AEP")


## Prediction Plots
head(aep_test)
head(myforecast)
aep_energy = as_tibble(aep_test["aep_mw"])
mergedata = tibble(myforecast,aep_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=aep_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### COMED data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(comed_date); head(hour); length(hour)
year = year(comed_date); head(year); length(year)
week = week(comed_date); head(week); length(week)
day = day(comed_date); head(day); length(day)
weekday = wday(comed_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(comed_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
comed_data = data.frame(year, month, day, week, weekday, hour, comed_mw)
head(comed_data)
comed_newdata = comed_data[order(year, month, day, week, weekday, hour),]
head(comed_newdata)

comed_pro <- data.frame(comed_date,comed_mw, year, month, day, week, weekday, hour)
head(comed_pro)


## Creating training testing data
comed_train <- comed_pro[comed_pro$year >=2014&comed_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
comed_test <- comed_pro[comed_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


comed_train$ds = comed_train$comed_date
comed_train$y = comed_train$comed_mw
comed_training_prophet=comed_train[,3:length(comed_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, comed_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
comed_rmse <- sqrt(sum((myforecast[,'yhat'] - comed_test["comed_mw"])^2) / 5880)
comed_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of comed", main="Prophet fit and forecast(comed)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of COMED")


## Prediction Plots
head(comed_test)
head(myforecast)
comed_energy = as_tibble(comed_test["comed_mw"])
mergedata = tibble(myforecast,comed_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=comed_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DAYTON data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(dayton_date); head(hour); length(hour)
year = year(dayton_date); head(year); length(year)
week = week(dayton_date); head(week); length(week)
day = day(dayton_date); head(day); length(day)
weekday = wday(dayton_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dayton_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
dayton_data = data.frame(year, month, day, week, weekday, hour, dayton_mw)
head(dayton_data)
dayton_newdata = dayton_data[order(year, month, day, week, weekday, hour),]
head(dayton_newdata)

dayton_pro <- data.frame(dayton_date,dayton_mw, year, month, day, week, weekday, hour)
head(dayton_pro)


## Creating training testing data
dayton_train <- dayton_pro[dayton_pro$year >=2014&dayton_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
dayton_test <- dayton_pro[dayton_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


dayton_train$ds = dayton_train$dayton_date
dayton_train$y = dayton_train$dayton_mw
dayton_training_prophet=dayton_train[,3:length(dayton_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, dayton_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
dayton_rmse <- sqrt(sum((myforecast[,'yhat'] - dayton_test["dayton_mw"])^2) / 5880)
dayton_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of dayton", main="Prophet fit and forecast(dayton)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DAYTON")


## Prediction Plots
head(dayton_test)
head(myforecast)
dayton_energy = as_tibble(dayton_test["dayton_mw"])
mergedata = tibble(myforecast,dayton_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=dayton_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DEOK data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(deok_date); head(hour); length(hour)
year = year(deok_date); head(year); length(year)
week = week(deok_date); head(week); length(week)
day = day(deok_date); head(day); length(day)
weekday = wday(deok_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(deok_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
deok_data = data.frame(year, month, day, week, weekday, hour, deok_mw)
head(deok_data)
deok_newdata = deok_data[order(year, month, day, week, weekday, hour),]
head(deok_newdata)

deok_pro <- data.frame(deok_date,deok_mw, year, month, day, week, weekday, hour)
head(deok_pro)


## Creating training testing data
deok_train <- deok_pro[deok_pro$year >=2014&deok_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
deok_test <- deok_pro[deok_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


deok_train$ds = deok_train$deok_date
deok_train$y = deok_train$deok_mw
deok_training_prophet=deok_train[,3:length(deok_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, deok_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
deok_rmse <- sqrt(sum((myforecast[,'yhat'] - deok_test["deok_mw"])^2) / 5880)
deok_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of deok", main="Prophet fit and forecast(deok)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DEOK")


## Prediction Plots
head(deok_test)
head(myforecast)
deok_energy = as_tibble(deok_test["deok_mw"])
mergedata = tibble(myforecast,deok_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=deok_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DOM data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(dom_date); head(hour); length(hour)
year = year(dom_date); head(year); length(year)
week = week(dom_date); head(week); length(week)
day = day(dom_date); head(day); length(day)
weekday = wday(dom_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dom_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
dom_data = data.frame(year, month, day, week, weekday, hour, dom_mw)
head(dom_data)
dom_newdata = dom_data[order(year, month, day, week, weekday, hour),]
head(dom_newdata)

dom_pro <- data.frame(dom_date,dom_mw, year, month, day, week, weekday, hour)
head(dom_pro)


## Creating training testing data
dom_train <- dom_pro[dom_pro$year >=2014&dom_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
dom_test <- dom_pro[dom_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


dom_train$ds = dom_train$dom_date
dom_train$y = dom_train$dom_mw
dom_training_prophet=dom_train[,3:length(dom_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, dom_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
dom_rmse <- sqrt(sum((myforecast[,'yhat'] - dom_test["dom_mw"])^2) / 5880)
dom_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of dom", main="Prophet fit and forecast(dom)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DOM")


## Prediction Plots
head(dom_test)
head(myforecast)
dom_energy = as_tibble(dom_test["dom_mw"])
mergedata = tibble(myforecast,dom_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=dom_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DUQ data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(duq_date); head(hour); length(hour)
year = year(duq_date); head(year); length(year)
week = week(duq_date); head(week); length(week)
day = day(duq_date); head(day); length(day)
weekday = wday(duq_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(duq_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
duq_data = data.frame(year, month, day, week, weekday, hour, duq_mw)
head(duq_data)
duq_newdata = duq_data[order(year, month, day, week, weekday, hour),]
head(duq_newdata)

duq_pro <- data.frame(duq_date,duq_mw, year, month, day, week, weekday, hour)
head(duq_pro)

## Write duq_pro to csv
# write.csv(duq_pro, "Task3_DUQ_features_energy.csv")

## Creating training testing data
duq_train <- duq_pro[duq_pro$year >=2014&duq_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
duq_test <- duq_pro[duq_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


duq_train$ds = duq_train$duq_date
duq_train$y = duq_train$duq_mw
duq_training_prophet=duq_train[,3:length(duq_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, duq_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
duq_rmse <- sqrt(sum((myforecast[,'yhat'] - duq_test["duq_mw"])^2) / 5880)
duq_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of duq", main="Prophet fit and forecast(duq)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DUQ")


## Prediction Plots
head(duq_test)
head(myforecast)
duq_energy = as_tibble(duq_test["duq_mw"])
mergedata = tibble(myforecast,duq_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=duq_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### EKPC data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(ekpc_date); head(hour); length(hour)
year = year(ekpc_date); head(year); length(year)
week = week(ekpc_date); head(week); length(week)
day = day(ekpc_date); head(day); length(day)
weekday = wday(ekpc_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ekpc_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
ekpc_data = data.frame(year, month, day, week, weekday, hour, ekpc_mw)
head(ekpc_data)
ekpc_newdata = ekpc_data[order(year, month, day, week, weekday, hour),]
head(ekpc_newdata)

ekpc_pro <- data.frame(ekpc_date,ekpc_mw, year, month, day, week, weekday, hour)
head(ekpc_pro)


## Creating training testing data
ekpc_train <- ekpc_pro[ekpc_pro$year >=2014&ekpc_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
ekpc_test <- ekpc_pro[ekpc_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


ekpc_train$ds = ekpc_train$ekpc_date
ekpc_train$y = ekpc_train$ekpc_mw
ekpc_training_prophet=ekpc_train[,3:length(ekpc_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, ekpc_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
ekpc_rmse <- sqrt(sum((myforecast[,'yhat'] - ekpc_test["ekpc_mw"])^2) / 5880)
ekpc_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of ekpc", main="Prophet fit and forecast(ekpc)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of EKPC")


## Prediction Plots
head(ekpc_test)
head(myforecast)
ekpc_energy = as_tibble(ekpc_test["ekpc_mw"])
mergedata = tibble(myforecast,ekpc_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=ekpc_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### FE data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(fe_date); head(hour); length(hour)
year = year(fe_date); head(year); length(year)
week = week(fe_date); head(week); length(week)
day = day(fe_date); head(day); length(day)
weekday = wday(fe_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(fe_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
fe_data = data.frame(year, month, day, week, weekday, hour, fe_mw)
head(fe_data)
fe_newdata = fe_data[order(year, month, day, week, weekday, hour),]
head(fe_newdata)

fe_pro <- data.frame(fe_date,fe_mw, year, month, day, week, weekday, hour)
head(fe_pro)


## Creating training testing data
fe_train <- fe_pro[fe_pro$year >=2014&fe_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
fe_test <- fe_pro[fe_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


fe_train$ds = fe_train$fe_date
fe_train$y = fe_train$fe_mw
fe_training_prophet=fe_train[,3:length(fe_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, fe_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
fe_rmse <- sqrt(sum((myforecast[,'yhat'] - fe_test["fe_mw"])^2) / 5880)
fe_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of fe", main="Prophet fit and forecast(fe)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of FE")


## Prediction Plots
head(fe_test)
head(myforecast)
fe_energy = as_tibble(fe_test["fe_mw"])
mergedata = tibble(myforecast,fe_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=fe_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### NI data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(ni_date); head(hour); length(hour)
year = year(ni_date); head(year); length(year)
week = week(ni_date); head(week); length(week)
day = day(ni_date); head(day); length(day)
weekday = wday(ni_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ni_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
ni_data = data.frame(year, month, day, week, weekday, hour, ni_mw)
head(ni_data)
ni_newdata = ni_data[order(year, month, day, week, weekday, hour),]
head(ni_newdata)

ni_pro <- data.frame(ni_date,ni_mw, year, month, day, week, weekday, hour)
head(ni_pro)


## Creating training testing data
ni_train <- ni_pro[ni_pro$year >=2005&ni_pro$year<=2007,] ## select 3 years (2005.1.1 ~ 2007.12.31)
ni_test <- ni_pro[ni_pro$year==2008,][1:5880,] ## select 35 weeks (2008.1.1~2009.9.2)


ni_train$ds = ni_train$ni_date
ni_train$y = ni_train$ni_mw
ni_training_prophet=ni_train[,3:length(ni_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, ni_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
ni_rmse <- sqrt(sum((myforecast[,'yhat'] - ni_test["ni_mw"])^2) / 5880)
ni_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of ni", main="Prophet fit and forecast(ni)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of NI")


## Prediction Plots
head(ni_test)
head(myforecast)
ni_energy = as_tibble(ni_test["ni_mw"])
mergedata = tibble(myforecast,ni_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=ni_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJM_LOAD data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjm_load_date); head(hour); length(hour)
year = year(pjm_load_date); head(year); length(year)
week = week(pjm_load_date); head(week); length(week)
day = day(pjm_load_date); head(day); length(day)
weekday = wday(pjm_load_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjm_load_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjm_load_data = data.frame(year, month, day, week, weekday, hour, pjm_load_mw)
head(pjm_load_data)
pjm_load_newdata = pjm_load_data[order(year, month, day, week, weekday, hour),]
head(pjm_load_newdata)

pjm_load_pro <- data.frame(pjm_load_date,pjm_load_mw, year, month, day, week, weekday, hour)
head(pjm_load_pro)


## Creating training testing data
pjm_load_train <- pjm_load_pro[pjm_load_pro$year >=1998&pjm_load_pro$year<=2000,] ## select 2 years and 9 months (1998.4.1 ~ 2000.12.31)
pjm_load_test <- pjm_load_pro[pjm_load_pro$year==2001,][1:5880,] ## select 35 weeks (2001.1.1~2001.9.3)

pjm_load_train$ds = pjm_load_train$pjm_load_date
pjm_load_train$y = pjm_load_train$pjm_load_mw
pjm_load_training_prophet=pjm_load_train[,3:length(pjm_load_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, pjm_load_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjm_load_rmse <- sqrt(sum((myforecast[,'yhat'] - pjm_load_test["pjm_load_mw"])^2) / 5880)
pjm_load_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjm_load", main="Prophet fit and forecast(pjm_load)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJM_LOAD")


## Prediction Plots
head(pjm_load_test)
head(myforecast)
pjm_load_energy = as_tibble(pjm_load_test["pjm_load_mw"])
mergedata = tibble(myforecast,pjm_load_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjm_load_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJME data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjme_date); head(hour); length(hour)
year = year(pjme_date); head(year); length(year)
week = week(pjme_date); head(week); length(week)
day = day(pjme_date); head(day); length(day)
weekday = wday(pjme_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjme_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjme_data = data.frame(year, month, day, week, weekday, hour, pjme_mw)
head(pjme_data)
pjme_newdata = pjme_data[order(year, month, day, week, weekday, hour),]
head(pjme_newdata)

pjme_pro <- data.frame(pjme_date,pjme_mw, year, month, day, week, weekday, hour)
head(pjme_pro)


## Creating training testing data
pjme_train <- pjme_pro[pjme_pro$year >=2014&pjme_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
pjme_test <- pjme_pro[pjme_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


pjme_train$ds = pjme_train$pjme_date
pjme_train$y = pjme_train$pjme_mw
pjme_training_prophet=pjme_train[,3:length(pjme_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, pjme_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjme_rmse <- sqrt(sum((myforecast[,'yhat'] - pjme_test["pjme_mw"])^2) / 5880)
pjme_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjme", main="Prophet fit and forecast(pjme)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJME")


## Prediction Plots
head(pjme_test)
head(myforecast)
pjme_energy = as_tibble(pjme_test["pjme_mw"])
mergedata = tibble(myforecast,pjme_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjme_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJMW data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjmw_date); head(hour); length(hour)
year = year(pjmw_date); head(year); length(year)
week = week(pjmw_date); head(week); length(week)
day = day(pjmw_date); head(day); length(day)
weekday = wday(pjmw_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjmw_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjmw_data = data.frame(year, month, day, week, weekday, hour, pjmw_mw)
head(pjmw_data)
pjmw_newdata = pjmw_data[order(year, month, day, week, weekday, hour),]
head(pjmw_newdata)

pjmw_pro <- data.frame(pjmw_date,pjmw_mw, year, month, day, week, weekday, hour)
head(pjmw_pro)


## Creating training testing data
pjmw_train <- pjmw_pro[pjmw_pro$year >=2014&pjmw_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
pjmw_test <- pjmw_pro[pjmw_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


pjmw_train$ds = pjmw_train$pjmw_date
pjmw_train$y = pjmw_train$pjmw_mw
pjmw_training_prophet=pjmw_train[,3:length(pjmw_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = fit.prophet(m, pjmw_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjmw_rmse <- sqrt(sum((myforecast[,'yhat'] - pjmw_test["pjmw_mw"])^2) / 5880)
pjmw_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjmw", main="Prophet fit and forecast(pjmw)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJMW")


## Prediction Plots
head(pjmw_test)
head(myforecast)
pjmw_energy = as_tibble(pjmw_test["pjmw_mw"])
mergedata = tibble(myforecast,pjmw_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjmw_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



### RSME OF ALL THE MODELS
aep_rmse
comed_rmse
dayton_rmse
deok_rmse
dom_rmse
duq_rmse
ekpc_rmse
fe_rmse
ni_rmse
pjm_load_rmse
pjme_rmse
pjmw_rmse



library(lubridate)
library(prophet)
library(tidyverse)



#### RMSE FULL MODELS ####

#### AEP data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(aep_date); head(hour); length(hour)
year = year(aep_date); head(year); length(year)
week = week(aep_date); head(week); length(week)
day = day(aep_date); head(day); length(day)
weekday = wday(aep_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(aep_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
aep_data = data.frame(year, month, day, week, weekday, hour, aep_mw)
head(aep_data)
aep_newdata = aep_data[order(year, month, day, week, weekday, hour),]
head(aep_newdata)

aep_pro <- data.frame(aep_date,aep_mw, year, month, day, week, weekday, hour)
head(aep_pro)


## Creating training testing data
aep_train <- aep_pro[aep_pro$year >=2014&aep_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
aep_test <- aep_pro[aep_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


aep_train$ds = aep_train$aep_date
aep_train$y = aep_train$aep_mw
aep_training_prophet=aep_train[,3:length(aep_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, aep_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
aep_rmse <- sqrt(sum((myforecast[,'yhat'] - aep_test["aep_mw"])^2) / 5880)
aep_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of aep", main="Prophet fit and forecast(aep)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of AEP")


## Prediction Plots
head(aep_test)
head(myforecast)
aep_energy = as_tibble(aep_test["aep_mw"])
mergedata = tibble(myforecast,aep_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=aep_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### COMED data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(comed_date); head(hour); length(hour)
year = year(comed_date); head(year); length(year)
week = week(comed_date); head(week); length(week)
day = day(comed_date); head(day); length(day)
weekday = wday(comed_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(comed_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
comed_data = data.frame(year, month, day, week, weekday, hour, comed_mw)
head(comed_data)
comed_newdata = comed_data[order(year, month, day, week, weekday, hour),]
head(comed_newdata)

comed_pro <- data.frame(comed_date,comed_mw, year, month, day, week, weekday, hour)
head(comed_pro)


## Creating training testing data
comed_train <- comed_pro[comed_pro$year >=2014&comed_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
comed_test <- comed_pro[comed_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


comed_train$ds = comed_train$comed_date
comed_train$y = comed_train$comed_mw
comed_training_prophet=comed_train[,3:length(comed_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, comed_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
comed_rmse <- sqrt(sum((myforecast[,'yhat'] - comed_test["comed_mw"])^2) / 5880)
comed_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of comed", main="Prophet fit and forecast(comed)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of COMED")


## Prediction Plots
head(comed_test)
head(myforecast)
comed_energy = as_tibble(comed_test["comed_mw"])
mergedata = tibble(myforecast,comed_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=comed_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DAYTON data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(dayton_date); head(hour); length(hour)
year = year(dayton_date); head(year); length(year)
week = week(dayton_date); head(week); length(week)
day = day(dayton_date); head(day); length(day)
weekday = wday(dayton_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dayton_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
dayton_data = data.frame(year, month, day, week, weekday, hour, dayton_mw)
head(dayton_data)
dayton_newdata = dayton_data[order(year, month, day, week, weekday, hour),]
head(dayton_newdata)

dayton_pro <- data.frame(dayton_date,dayton_mw, year, month, day, week, weekday, hour)
head(dayton_pro)


## Creating training testing data
dayton_train <- dayton_pro[dayton_pro$year >=2014&dayton_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
dayton_test <- dayton_pro[dayton_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


dayton_train$ds = dayton_train$dayton_date
dayton_train$y = dayton_train$dayton_mw
dayton_training_prophet=dayton_train[,3:length(dayton_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, dayton_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
dayton_rmse <- sqrt(sum((myforecast[,'yhat'] - dayton_test["dayton_mw"])^2) / 5880)
dayton_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of dayton", main="Prophet fit and forecast(dayton)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DAYTON")


## Prediction Plots
head(dayton_test)
head(myforecast)
dayton_energy = as_tibble(dayton_test["dayton_mw"])
mergedata = tibble(myforecast,dayton_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=dayton_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DEOK data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(deok_date); head(hour); length(hour)
year = year(deok_date); head(year); length(year)
week = week(deok_date); head(week); length(week)
day = day(deok_date); head(day); length(day)
weekday = wday(deok_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(deok_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
deok_data = data.frame(year, month, day, week, weekday, hour, deok_mw)
head(deok_data)
deok_newdata = deok_data[order(year, month, day, week, weekday, hour),]
head(deok_newdata)

deok_pro <- data.frame(deok_date,deok_mw, year, month, day, week, weekday, hour)
head(deok_pro)


## Creating training testing data
deok_train <- deok_pro[deok_pro$year >=2014&deok_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
deok_test <- deok_pro[deok_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


deok_train$ds = deok_train$deok_date
deok_train$y = deok_train$deok_mw
deok_training_prophet=deok_train[,3:length(deok_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, deok_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
deok_rmse <- sqrt(sum((myforecast[,'yhat'] - deok_test["deok_mw"])^2) / 5880)
deok_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of deok", main="Prophet fit and forecast(deok)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DEOK")


## Prediction Plots
head(deok_test)
head(myforecast)
deok_energy = as_tibble(deok_test["deok_mw"])
mergedata = tibble(myforecast,deok_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=deok_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DOM data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(dom_date); head(hour); length(hour)
year = year(dom_date); head(year); length(year)
week = week(dom_date); head(week); length(week)
day = day(dom_date); head(day); length(day)
weekday = wday(dom_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(dom_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
dom_data = data.frame(year, month, day, week, weekday, hour, dom_mw)
head(dom_data)
dom_newdata = dom_data[order(year, month, day, week, weekday, hour),]
head(dom_newdata)

dom_pro <- data.frame(dom_date,dom_mw, year, month, day, week, weekday, hour)
head(dom_pro)


## Creating training testing data
dom_train <- dom_pro[dom_pro$year >=2014&dom_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
dom_test <- dom_pro[dom_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


dom_train$ds = dom_train$dom_date
dom_train$y = dom_train$dom_mw
dom_training_prophet=dom_train[,3:length(dom_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, dom_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
dom_rmse <- sqrt(sum((myforecast[,'yhat'] - dom_test["dom_mw"])^2) / 5880)
dom_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of dom", main="Prophet fit and forecast(dom)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DOM")


## Prediction Plots
head(dom_test)
head(myforecast)
dom_energy = as_tibble(dom_test["dom_mw"])
mergedata = tibble(myforecast,dom_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=dom_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### DUQ data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(duq_date); head(hour); length(hour)
year = year(duq_date); head(year); length(year)
week = week(duq_date); head(week); length(week)
day = day(duq_date); head(day); length(day)
weekday = wday(duq_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(duq_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
duq_data = data.frame(year, month, day, week, weekday, hour, duq_mw)
head(duq_data)
duq_newdata = duq_data[order(year, month, day, week, weekday, hour),]
head(duq_newdata)

duq_pro <- data.frame(duq_date,duq_mw, year, month, day, week, weekday, hour)
head(duq_pro)

## Creating training testing data
duq_train <- duq_pro[duq_pro$year >=2014&duq_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
duq_test <- duq_pro[duq_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


duq_train$ds = duq_train$duq_date
duq_train$y = duq_train$duq_mw
duq_training_prophet=duq_train[,3:length(duq_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, duq_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
duq_rmse <- sqrt(sum((myforecast[,'yhat'] - duq_test["duq_mw"])^2) / 5880)
duq_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of duq", main="Prophet fit and forecast(duq)")
prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of DUQ")

## Prediction Plots
head(duq_test)
head(myforecast)
duq_energy = as_tibble(duq_test["duq_mw"])
mergedata = tibble(myforecast,duq_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=duq_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### EKPC data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(ekpc_date); head(hour); length(hour)
year = year(ekpc_date); head(year); length(year)
week = week(ekpc_date); head(week); length(week)
day = day(ekpc_date); head(day); length(day)
weekday = wday(ekpc_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ekpc_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
ekpc_data = data.frame(year, month, day, week, weekday, hour, ekpc_mw)
head(ekpc_data)
ekpc_newdata = ekpc_data[order(year, month, day, week, weekday, hour),]
head(ekpc_newdata)

ekpc_pro <- data.frame(ekpc_date,ekpc_mw, year, month, day, week, weekday, hour)
head(ekpc_pro)


## Creating training testing data
ekpc_train <- ekpc_pro[ekpc_pro$year >=2014&ekpc_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
ekpc_test <- ekpc_pro[ekpc_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


ekpc_train$ds = ekpc_train$ekpc_date
ekpc_train$y = ekpc_train$ekpc_mw
ekpc_training_prophet=ekpc_train[,3:length(ekpc_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, ekpc_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
ekpc_rmse <- sqrt(sum((myforecast[,'yhat'] - ekpc_test["ekpc_mw"])^2) / 5880)
ekpc_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of ekpc", main="Prophet fit and forecast(ekpc)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of EKPC")


## Prediction Plots
head(ekpc_test)
head(myforecast)
ekpc_energy = as_tibble(ekpc_test["ekpc_mw"])
mergedata = tibble(myforecast,ekpc_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=ekpc_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### FE data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(fe_date); head(hour); length(hour)
year = year(fe_date); head(year); length(year)
week = week(fe_date); head(week); length(week)
day = day(fe_date); head(day); length(day)
weekday = wday(fe_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(fe_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
fe_data = data.frame(year, month, day, week, weekday, hour, fe_mw)
head(fe_data)
fe_newdata = fe_data[order(year, month, day, week, weekday, hour),]
head(fe_newdata)

fe_pro <- data.frame(fe_date,fe_mw, year, month, day, week, weekday, hour)
head(fe_pro)


## Creating training testing data
fe_train <- fe_pro[fe_pro$year >=2014&fe_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
fe_test <- fe_pro[fe_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


fe_train$ds = fe_train$fe_date
fe_train$y = fe_train$fe_mw
fe_training_prophet=fe_train[,3:length(fe_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, fe_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
fe_rmse <- sqrt(sum((myforecast[,'yhat'] - fe_test["fe_mw"])^2) / 5880)
fe_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of fe", main="Prophet fit and forecast(fe)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of FE")


## Prediction Plots
head(fe_test)
head(myforecast)
fe_energy = as_tibble(fe_test["fe_mw"])
mergedata = tibble(myforecast,fe_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=fe_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### NI data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(ni_date); head(hour); length(hour)
year = year(ni_date); head(year); length(year)
week = week(ni_date); head(week); length(week)
day = day(ni_date); head(day); length(day)
weekday = wday(ni_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(ni_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
ni_data = data.frame(year, month, day, week, weekday, hour, ni_mw)
head(ni_data)
ni_newdata = ni_data[order(year, month, day, week, weekday, hour),]
head(ni_newdata)

ni_pro <- data.frame(ni_date,ni_mw, year, month, day, week, weekday, hour)
head(ni_pro)


## Creating training testing data
ni_train <- ni_pro[ni_pro$year >=2005&ni_pro$year<=2007,] ## select 3 years (2005.1.1 ~ 2007.12.31)
ni_test <- ni_pro[ni_pro$year==2008,][1:5880,] ## select 35 weeks (2008.1.1~2009.9.2)


ni_train$ds = ni_train$ni_date
ni_train$y = ni_train$ni_mw
ni_training_prophet=ni_train[,3:length(ni_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, ni_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
ni_rmse <- sqrt(sum((myforecast[,'yhat'] - ni_test["ni_mw"])^2) / 5880)
ni_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of ni", main="Prophet fit and forecast(ni)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of NI")


## Prediction Plots
head(ni_test)
head(myforecast)
ni_energy = as_tibble(ni_test["ni_mw"])
mergedata = tibble(myforecast,ni_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=ni_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJM_LOAD data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjm_load_date); head(hour); length(hour)
year = year(pjm_load_date); head(year); length(year)
week = week(pjm_load_date); head(week); length(week)
day = day(pjm_load_date); head(day); length(day)
weekday = wday(pjm_load_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjm_load_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjm_load_data = data.frame(year, month, day, week, weekday, hour, pjm_load_mw)
head(pjm_load_data)
pjm_load_newdata = pjm_load_data[order(year, month, day, week, weekday, hour),]
head(pjm_load_newdata)

pjm_load_pro <- data.frame(pjm_load_date,pjm_load_mw, year, month, day, week, weekday, hour)
head(pjm_load_pro)


## Creating training testing data
pjm_load_train <- pjm_load_pro[pjm_load_pro$year >=1998&pjm_load_pro$year<=2000,] ## select 2 years and 9 months (1998.4.1 ~ 2000.12.31)
pjm_load_test <- pjm_load_pro[pjm_load_pro$year==2001,][1:5880,] ## select 35 weeks (2001.1.1~2001.9.3)

pjm_load_train$ds = pjm_load_train$pjm_load_date
pjm_load_train$y = pjm_load_train$pjm_load_mw
pjm_load_training_prophet=pjm_load_train[,3:length(pjm_load_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, pjm_load_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjm_load_rmse <- sqrt(sum((myforecast[,'yhat'] - pjm_load_test["pjm_load_mw"])^2) / 5880)
pjm_load_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjm_load", main="Prophet fit and forecast(pjm_load)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJM_LOAD")


## Prediction Plots
head(pjm_load_test)
head(myforecast)
pjm_load_energy = as_tibble(pjm_load_test["pjm_load_mw"])
mergedata = tibble(myforecast,pjm_load_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjm_load_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJME data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjme_date); head(hour); length(hour)
year = year(pjme_date); head(year); length(year)
week = week(pjme_date); head(week); length(week)
day = day(pjme_date); head(day); length(day)
weekday = wday(pjme_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjme_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjme_data = data.frame(year, month, day, week, weekday, hour, pjme_mw)
head(pjme_data)
pjme_newdata = pjme_data[order(year, month, day, week, weekday, hour),]
head(pjme_newdata)

pjme_pro <- data.frame(pjme_date,pjme_mw, year, month, day, week, weekday, hour)
head(pjme_pro)


## Creating training testing data
pjme_train <- pjme_pro[pjme_pro$year >=2014&pjme_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
pjme_test <- pjme_pro[pjme_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


pjme_train$ds = pjme_train$pjme_date
pjme_train$y = pjme_train$pjme_mw
pjme_training_prophet=pjme_train[,3:length(pjme_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, pjme_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjme_rmse <- sqrt(sum((myforecast[,'yhat'] - pjme_test["pjme_mw"])^2) / 5880)
pjme_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjme", main="Prophet fit and forecast(pjme)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJME")


## Prediction Plots
head(pjme_test)
head(myforecast)
pjme_energy = as_tibble(pjme_test["pjme_mw"])
mergedata = tibble(myforecast,pjme_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjme_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



#### PJMW data ####

## Creating dataframe with features for testing
# Extract the hour, year, week, day, dow, and month
hour = hour(pjmw_date); head(hour); length(hour)
year = year(pjmw_date); head(year); length(year)
week = week(pjmw_date); head(week); length(week)
day = day(pjmw_date); head(day); length(day)
weekday = wday(pjmw_date, label=TRUE, abbr=TRUE); length(weekday)
month = factor(month.abb[month(pjmw_date)], levels=month.abb); length(month)

# Create data frame with extracted elements
pjmw_data = data.frame(year, month, day, week, weekday, hour, pjmw_mw)
head(pjmw_data)
pjmw_newdata = pjmw_data[order(year, month, day, week, weekday, hour),]
head(pjmw_newdata)

pjmw_pro <- data.frame(pjmw_date,pjmw_mw, year, month, day, week, weekday, hour)
head(pjmw_pro)


## Creating training testing data
pjmw_train <- pjmw_pro[pjmw_pro$year >=2014&pjmw_pro$year<=2016,] ## select 3 years (2014.1.1 ~ 2016.12.31)
pjmw_test <- pjmw_pro[pjmw_pro$year==2017,][1:5880,] ## select 35 weeks (2017.1.1~2017.9.2)


pjmw_train$ds = pjmw_train$pjmw_date
pjmw_train$y = pjmw_train$pjmw_mw
pjmw_training_prophet=pjmw_train[,3:length(pjmw_train)]


## Applying final prophet model
m = prophet(daily.seasonality=TRUE)
m = add_regressor(m, 'hour')
m = add_regressor(m, 'month')
m = add_regressor(m, 'weekday')
m = add_regressor(m, 'day')
m = add_regressor(m, 'week')
m = add_regressor(m, 'year')
m = fit.prophet(m, pjmw_training_prophet)


## Creating forecasting dataframe
future <- make_future_dataframe(m, periods = 5880,freq=3600)

future <- future %>%
  mutate(
    hour = (hour(ds)),
    month = (month(ds)),
    weekday = (wday(ds, label=TRUE, abbr=TRUE)),
    year = (year(ds)),
    day = (day(ds)),
    week = (week(ds)),
  )

## Forecasting
forecast <- predict(m, future)
forecast1 <- as_tibble(forecast)

myforecast<-forecast1[,c("ds","yhat", "yhat_lower", "yhat_upper")]
myforecast=myforecast[ (dim(myforecast)[1]-5879) : dim(myforecast)[1],]
myforecast
dim(myforecast)


##RMSE
pjmw_rmse <- sqrt(sum((myforecast[,'yhat'] - pjmw_test["pjmw_mw"])^2) / 5880)
pjmw_rmse

### Plots
## Forecasting plots
plot(m,forecast,xlab="Time", ylab="Energy consumption of pjmw", main="Prophet fit and forecast(pjmw)")
#prophet_plot_components(m,forecast)
#dyplot.prophet(m,forecast,main="Prophet fit and forecast",ylab="Energy consumption of PJMW")


## Prediction Plots
head(pjmw_test)
head(myforecast)
pjmw_energy = as_tibble(pjmw_test["pjmw_mw"])
mergedata = tibble(myforecast,pjmw_energy)

ggplot(data=mergedata)+
  geom_line(mapping=aes(x=ds, y=pjmw_mw))+
  geom_line(mapping=aes(x=ds, y=yhat_lower, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat_upper, color="blue"),show.legend=FALSE)+
  geom_line(mapping=aes(x=ds, y=yhat, color="red"),show.legend=FALSE)+
  labs(
    title=paste("Predicted values and prediction intervals superimposed to energy consumption"),
    caption="Energy consumption (black), forecast (blue) and prediction intervals (red), Jan 1 - Jan 28, 2018",
    x="Time",
    y="Energy consumption"
  )



### RSME OF ALL THE MODELS
aep_rmse
comed_rmse
dayton_rmse
deok_rmse
dom_rmse
duq_rmse
ekpc_rmse
fe_rmse
ni_rmse
pjm_load_rmse
pjme_rmse
pjmw_rmse











