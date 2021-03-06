# load packages
library(data.table)
library(stringr)
library(zoo)
library(lubridate)
library(dplyr)
library(geosphere)
library(xgboost)


## data: train and test ----
xtrain <- fread('./data/air_visit_data.csv')
xtest <- fread('./data/sample_submission.csv')

## store info: air ----
xstore_air <- fread('./data/air_store_info.csv',encoding = "UTF-8")

## reservations: air clean ----
reserve_air <- fread('./data/air_reserve.csv')

## store info: hpg ----
xstore_hpg <- fread('./data/hpg_store_info.csv',encoding = "UTF-8")

## reservations : hpg clean
reserve_hpg <- fread("./data/hpg_reserve.csv")

## date info ----
xdate <- fread('./data/date_info.csv')

## store_id ---
store_id <- fread("./data/store_id_relation.csv")


## weather data ---

# ---staion_info ---
# weather_station <- fread("./data/weather_info/weather_stations.csv")
# nearby_active_stations <-
#   fread("./data/weather_info/nearby_active_stations.csv")

# ---air_store nearest active station ---
xstore_air_station <-
  fread("./data/weather_info/air_store_info_with_nearest_active_station.csv",
        encoding = "UTF-8")

# ---hpg_store nearest active station---
xstore_hpg_station <-
  fread("./data/weather_info/hpg_store_info_with_nearest_active_station.csv",
        encoding = "UTF-8")

# ---station weather_info ---
weather_files <- list.files(
  path = "./data/weather_info/1-1-16_5-31-17_Weather_Translated",
  pattern = "*.csv",
  full.names = TRUE
)
weather_dt <- do.call(rbindlist, list(lapply(weather_files, fread)))

# get file name
filename <- list.files(
  path = "./data/weather_info/1-1-16_5-31-17_Weather_Translated",
  pattern = "*.csv"
)
weather_dt$station_id <- gsub(".csv","",rep(filename, each = 517))

# ---------------------------------DATA CLEAN------------------------------------
# --- weather data clean ---
# set colnames
weather_dt <- weather_dt[,c(16,1:15)]
setnames(weather_dt,"calendar_date","visit_date")

# delete NA column
weather_dt[,c(5:6,9:15)] <- NULL

# convert to date
weather_dt$visit_date <- as.Date(weather_dt$visit_date)

# fill na to 0
weather_dt[is.na(weather_dt)] <- 0

# select station_id only in stores
weather_dt <-
  weather_dt[station_id %in% unique(xstore_air_station$station_id)]

## --- reservations: air clean ----
# merge reserve and store
reserve_hpg <- merge(reserve_hpg, store_id)

# convert to datetime
reserve_air$visit_datetime <- parse_date_time(
  reserve_air$visit_datetime, 
  orders = '%Y-%m-%d H:M:S'
)
reserve_air$reserve_datetime <- parse_date_time(
  reserve_air$reserve_datetime, 
  orders = '%Y-%m-%d H:M:S'
)

## --- reservations: hpg clean ----
# convert to datetime
reserve_hpg$visit_datetime <- parse_date_time(
  reserve_hpg$visit_datetime, 
  orders = '%Y-%m-%d H:M:S'
)
reserve_hpg$reserve_datetime <- parse_date_time(
  reserve_hpg$reserve_datetime, 
  orders = '%Y-%m-%d H:M:S'
)

# time ahead = visit_datetime - reserve_datetime
reserve_air$time_ahead <- 
  as.double(reserve_air$visit_datetime - reserve_air$reserve_datetime) / 3600

reserve_hpg$time_ahead <- 
  as.double(reserve_hpg$visit_datetime - reserve_hpg$reserve_datetime) / 3600


# convert to date
reserve_air$visit_date <- as.Date(reserve_air$visit_datetime)
reserve_air$reserve_datetime <- as.Date(reserve_air$visit_datetime)

reserve_hpg$visit_date <- as.Date(reserve_hpg$visit_datetime)
reserve_hpg$reserve_datetime <- as.Date(reserve_hpg$visit_datetime)



# -----------------------------Feature Engenieer 1-------------------------------

# from surprie me
# aggregate to id x date combo
tmp1 <- reserve_air[,.(
  rv1 = sum(reserve_visitors),
  rs1 = round(mean(time_ahead), 2)
) ,
by = list(air_store_id, visit_date)]

tmp2 <- reserve_hpg[,.(
  rv2 = sum(reserve_visitors),
  rs2 = round(mean(time_ahead), 2)
) ,
by = list(air_store_id, visit_date)]


# merge tmp1,tmp2
df <- merge(tmp1, tmp2, by = c("air_store_id", "visit_date"))
rm(reserve_air,reserve_hpg,tmp1,tmp2)

### xtrain data clean ----
xtrain$visit_date <- as.Date(xtrain$visit_date)
xtrain$dow <- wday(xtrain$visit_date)
xtrain$year <- year(xtrain$visit_date)
xtrain$month <- month(xtrain$visit_date)

# Calculate number of “restaurant days”
xtrain$golden_diff <-
  as.numeric(xtrain$visit_date - as.Date('2016-04-29', '%Y-%m-%d'))
xtrain$golden_diff <- floor((xtrain$golden_diff + 700) / 7.0) - 100


### xtest data clean ----
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)

xtest$visit_date <- as.Date(xtest$visit_date)
xtest$dow <- wday(xtest$visit_date)
xtest$year <- year(xtest$visit_date)
xtest$month <- month(xtest$visit_date)

xtest$golden_diff <- 
  as.numeric(xtest$visit_date - as.Date('2017-04-29', '%Y-%m-%d'))
xtest$golden_diff <- floor((xtest$golden_diff + 700) / 7.0) - 100


unique_stores <- unique(xtest$air_store_id)
stores <- data.frame(
  air_store_id = unique_stores,
  dow = rep(seq(1:7), each = length(unique_stores))
)

# stores data clean ----
# sure it can be compressed...
tmp <- xtrain[,.(
  min_vis = min(visitors),
  max_vis = max(visitors),
  mean_vis = mean(visitors),
  median_vis = median(visitors)
),
by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

# merge stores and store_air
stores <- merge(stores,xstore_air_station)
rm(tmp)


# set air_genre_name/air_area_name to factor and then to interger
stores$air_genre_name <- factor(stores$air_genre_name)
levels(stores$air_genre_name) <- 1:nlevels(stores$air_genre_name)
stores$air_genre_name <- as.integer(stores$air_genre_name)

stores$air_area_name <- factor(stores$air_area_name)
levels(stores$air_area_name) <- 1:nlevels(stores$air_area_name)
stores$air_area_name <- as.integer(stores$air_area_name)

### date_info clean ---
# holidays at weekends are not special, right?
wkend_holidays <- which(
  xdate$day_of_week %in% c("Saturday", "Sunday") & 
    xdate$holiday_flg == 1
)
xdate[wkend_holidays, 'holiday_flg' := 0]

# add decreasing weights from now
xdate[, 'weight' := (.I/.N) ^ 7]
xdate$calendar_date <- as.Date(xdate$calendar_date)


# store id for backing up
id <- xtest$id
xtest$id <- NULL

## --- data aggregation ---
train <- rbind(xtrain, xtest)
train <- merge(train, stores, all.x = T)
train <- merge(train, xdate, by.x = 'visit_date', by.y = 'calendar_date')

# merge train and weather data
x <- merge(
  train,
  weather_dt,
  by= c("station_id","visit_date")
)

# merge df and train and aggregate rs1,rs2/rv1，rv2
train <- merge(train,df,all.x = T,by = c("air_store_id","visit_date"))

# combine rs1,rs2,rv1,rv2
train[, "total_reserv_sum" := (rv1 + rv2)]
train[, "total_reserv_mean" := (rv1 + rv2) / 2]
train[, "total_reserv_dt_diff_mean" := (rs1 + rs2) / 2]
train[, c("rs1", "rs2", "rv1", "rv2") := NULL]
train <- train[order(air_store_id, visit_date)]

# NEW FEATURES FROM JMBUL
train[,"date_int"] <- as.integer(train$visit_date)
train[,"var_max_lat"] <- max(train$latitude) - train$latitude
train[,"var_max_long"] <- max(train$longitude) - train$longitude

# NEW FEATURES FROM Georgii Vyshnia
train[,"lon_plus_lat"] <- train$longitude + train$latitude

# train$air_store_id_int <- factor(train$air_area_name)
# levels(train$air_store_id_int) <- 1:nlevels(train$air_store_id_int)
# train$air_store_id_int <- as.integer(train$air_store_id_int)


# COMPUTE DISTANCE BY LONGTITUDE/LATITUDE
others <- cbind(train$longitude,train$latitude)
center <- cbind(mean(train$longitude),mean(train$latitude))
train$dist <- distm(others,center)
rm(others,center)

# it decreases the importance of the further past data by applying 
# a weight to them
train[, 'visitors':= log1p(visitors)]
train[,.(visitors = weighted.mean(visitors, weight)),
      by = c('air_store_id', 'day_of_week', 'holiday_flg')]

# delete day_of_week、weight、station_longtidue、station_latitude
train[,weight := NULL]
train[,day_of_week := NULL]
train[,c("station_id","station_longitude","station_latitude") := NULL]

# --- set na to 0 ---
train[is.na(train)] <- 0

## -----------------------------Feature Engineer 2-------------------------------
# from lightgbm fe and validation -like sliding window
# 1.holiday in the last 3 days
train[, h3a := rollapply(
  holiday_flg,
  width = 3,
  FUN = function(s)
    sign(sum(s, na.rm = T)),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

# 2.visits totals for 14 days,21days,28days,35days
train[, vis14 := rollapply(
  log1p(visitors),
  width = 39,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[, vis21 := rollapply(
  log1p(visitors),
  width = 46,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[, vis28 := rollapply(
  log1p(visitors),
  width = 60,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[, vis35 := rollapply(
  log1p(visitors),
  width = 74,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

# aggregate vis14,vis21,vis28,vis35
train[, vLag1 := round((vis21 - vis14) / 7, 2)]
train[, vLag2 := round((vis28 - vis14) / 21, 2)]
train[, vLag3 := round((vis35 - vis14) / 35, 2)]
train[, vis14 := NULL, with = TRUE]
train[, vis21 := NULL, with = TRUE]
train[, vis28 := NULL, with = TRUE]
train[, vis35 := NULL, with = TRUE]


# 3.reservations for 7days and so on (like visitors)
train[, res7 := rollapply(
  log1p(total_reserv_sum),
  width = 7,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[,res14 := rollapply(
  log1p(total_reserv_sum),
  width = 14,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[, res21 := rollapply(
  log1p(total_reserv_sum),
  width = 21,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

train[, res28 := rollapply(
  log1p(total_reserv_sum),
  width = 28,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]


# separate train and test
x_train <- train[visitors > 0]
x_test <- train[visitors == 0]


# ----------------------------------- XGBOOST MODEL----------------------------------
## xgboost - validation ----
# x0 <- x_train[visit_date <= '2016-04-22' & visit_date >= '2016-01-01']
# x1 <- x_train[visit_date <= '2016-05-31' & visit_date >= '2016-04-29']

x0 <- x_train[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- x_train[visit_date > '2017-03-09']


# y0 -> train vistors,y1 -> validate vistors
y0 <- x0$visitors
y1 <- x1$visitors

x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

d0 <- xgb.DMatrix(
  data.matrix(x0),
  label = y0
)

d1 <- xgb.DMatrix(
  data.matrix(x1),
  label = y1
)

# set parameters 
param <- list(
  "objective" = "reg:linear",
  "eta" = 0.1,
  "max_depth" = 8,
  "subsample" = 0.886,
  'min_child_weight' = 10,
  "colsample_bytree" = 0.886,
  "scale_pos_weight" = 10,
  "gamma" = 0.5,
  "alpha" = 10,
  "lambda" = 300,
  "silent" = 1,
  "nthread" = 10,
  "seed" = 20171205
)

nround = 1000

# xgboost cross validation
bst.cv <- xgb.cv(
  params = param,
  data = d0,
  metrics = "rmse",
  nrounds = nround,
  nfold = 4,
  early_stopping_rounds = 10
)

# best nrounds
best_iteration <- bst.cv$best_iteration

bst <- xgboost(
  params = param,
  data = d0,
  metrics = "rmse",
  nrounds = best_iteration
)

# calcluate rmse
pred_val <- predict(bst, data.matrix(x1))
val_rmse_xgb <- RMSE(pred_val,y1)
print(paste('validation rmse error:', round(val_rmse_xgb, 4), sep = ' '))

## ---xgboost - full ----
x0 <- x_train
x1 <- x_test

y0 <- x0$visitors

x0$visit_date <- x0$air_store_id <- x0$visitors <-  NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <-  NULL

d0 <- xgb.DMatrix(
  data.matrix(x0),
  label = y0
)

# --- create cross validation ---
# For loop to run model 5 time with different random seeds. Using an 
# ensemble technique such as this improved the model performance

# Set i=2 because the first column is for the id variable
i = 2

# Create data frame to hold the 5 solutions developed by the model
solution.table <- data.frame(id = id)

for (i in 2:6) {
  # Set seed so that the results are reproducible
  set.seed(i)
  
  # Create model using the same parameters used in xgb.cv
  bst <- xgboost(
    params = param,
    data = d0,
    metrics = "rmse",
    nrounds = best_iteration
  )
  
  #Predict. Used the test_data because it contained the same number
  # of columns as the train.DMatrix used to build the model.
  pred_full <- predict(bst, data.matrix(x1))
  
  
  # Add the solution to column i of the solutions data frame. This creates
  # a data frame with a column for each prediction set. Each prediction is
  # a vote for that prediction. Next I will count the number of votes for
  # each prediction as use the element with the most votes as my final solution.
  solution.table[, i] <- pred_full
  
}

# Count the mean of visitors for all solution 
predict.combined <- apply(solution.table[,-1], MARGIN = 1, mean)

# result submission
result1 <- data.table(
  id = id,
  visitors = expm1(pred_full) 
)

# View the first five rows of the solution to ensure that it follows 
# submission format rules
head(result1)


# Calculate the importance of each variable to the model
# Used this function to remove variables from the model variables which 
# don't contribute to the model.
importance <- xgb.importance(feature_names = colnames(x0), model = bst)
importance

xgb.ggplot.importance(importance_matrix = importance)


# --------------------------------- WEIGHT MEANS -------------------------------
# use weight-means from hklee
date_info <- fread('./data/date_info.csv')
air_visit_data <- fread('./data/air_visit_data.csv')
sample_submission <- fread('./data/sample_submission.csv')

# holidays at weekends are not special, right?
wkend_holidays <- which(
  date_info$day_of_week %in% c("Saturday", "Sunday") & 
    date_info$holiday_flg ==1
)
date_info[wkend_holidays, 'holiday_flg' := 0]

# add decreasing weights from now
date_info[, 'weight' := (.I/.N) ^ 5]

# weighted mean visitors for each (air_store_id, day_of_week, 
# holiday_flag) or (air_store_id, day_of_week) 
visit_data <- merge(
  air_visit_data, 
  date_info, 
  by.x = 'visit_date', 
  by.y = 'calendar_date',
  all.x = TRUE
)
visit_data[, 'calendar_date' := NULL]
visit_data[, 'visitors':= log1p(visitors)]


# it decreases the importance of the further past data by applying 
# a weight to them
visitors <-
  visit_data[, .(visitors = weighted.mean(visitors, weight)),
             by = c('air_store_id', 'day_of_week', 'holiday_flg')]


# prepare to merge with date_info and visitors
sample_submission[, 'air_store_id' := str_sub(id, 1,-12)]
sample_submission[, 'calendar_date' := str_sub(id, -10)]                    
sample_submission[, 'visitors' := NULL]                  

sample_submission <- merge(
  sample_submission, 
  date_info, 
  by = 'calendar_date', 
  all.x = TRUE
)
sample_submission <- merge(
  sample_submission, visitors, 
  by = c('air_store_id', 'day_of_week', 'holiday_flg'),
  all.x = TRUE
)

# fill missings with (air_store_id, day_of_week)
missings <- which(is.na(sample_submission$visitors))
sample_submission[missings][['visitors']] <- merge(
  sample_submission[missings, -'visitors'], 
  visitors[holiday_flg==0], by = c('air_store_id', 'day_of_week'),
  all.x = TRUE)[['visitors']]


# fill missings with (air_store_id)
missings <- which(is.na(sample_submission$visitors))
sample_submission[missings][['visitors']] <- merge(
  sample_submission[missings, -'visitors'], 
  visitors[, .(visitors = mean(visitors)), by = 'air_store_id'],
  by = 'air_store_id', all.x = TRUE)[['visitors']]

# transform visitors
sample_submission[, 'visitors' := expm1(visitors)]
result2 <- sample_submission[,c("id","visitors")]
result2 <- result2[order(result2$id),]

# remove temp variable
rm(air_visit_data,visitors,visit_data,sample_submission)

# ---------------------------- MERGE RESULT -----------------------------
# merge solution1 and solution2
sample_submission_last <- data.table(
  id = id,
  vis1 = result1$visitors,
  vis2 = result2$visitors
)
sample_submission_last[, "visitors" := 0.65 * vis1 + vis2 * 0.35]
sample_submission_last[, c("vis1", "vis2") := NULL]

# Create csv submission file
write.csv(
  sample_submission_last,
  paste0("./output/submission/xgboost_surprieme_",Sys.Date(),".csv"), 
  row.names = FALSE,
  quote = F
)

# save data
save.image("./output/save/xgboost_surpriseme.RData")


# 2017-12-20 0.500 (add new feature -- longtitude and latitude)
# 2017-12-22 0.501  (create dummy variable)
# 2017-12-23 0.495 (new parameter)
# 2017-12-24 0.494 (add new feature -- dist)
# 2017-12-25 0.493 (new dummy variable strategy -- air_genre_name) 
# 2017-12-26 0.490 (blend some results)
# 2017-12-30 0.485 (blend xgb_lgb and surprise2)


# -------------------------------- FEATURE SELECT ---------------------------------
library(Boruta)

set.seed(10)
borutaAttr <- Boruta(
  visitors ~.,
  data = x_train[,-c(1:2)],
  maxRuns = 20,
  doTrace = 0
)

borutaVars <- getSelectedAttributes(borutaAttr)
boruta.formula <- formula(
  paste("visitors ~ ", paste(borutaVars, collapse = " + "))
)