# load packages
library(data.table)
library(stringr)
library(zoo)
library(lubridate)
library(dummies)
library(geosphere)
library(xgboost)


## data: train and test ----
xtrain <- fread('./data/air_visit_data.csv')
xtest <- fread('./data/sample_submission.csv')

## reservations: air clean ----
reserve_air <- fread('./data/air_reserve.csv')

## store info: air ----
xstore_air <- fread('./data/air_store_info.csv',encoding = "UTF-8")

## reservations : hpg clean
reserve_hpg <- fread("./data/hpg_reserve.csv")

## store info: hpg ----
xstore_hpg <- fread('./data/hpg_store_info.csv',encoding = "UTF-8")

## date info ----
xdate <- fread('./data/date_info.csv')

## store_id ---
store_id <- fread("./data/store_id_relation.csv")


## weather data ---




# ---------------------------------DATA CLEAN------------------------------------
# merge reserve and store
reserve_hpg <- merge(reserve_hpg, store_id)

## --- reservations: air clean ----
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


### xtest data clean ----
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)

xtest$visit_date <- as.Date(xtest$visit_date)
xtest$dow <- wday(xtest$visit_date)
xtest$year <- year(xtest$visit_date)
xtest$month <- month(xtest$visit_date)

unique_stores <- unique(xtest$air_store_id)
stores <- data.frame(
  air_store_id = unique_stores,
  dow = rep(seq(1:7), each = length(unique_stores))
)

# stores data clean ----
# sure it can be compressed...
tmp <- xtrain[,.(min_vis = min(visitors)) ,by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

tmp <- xtrain[,.(max_vis = max(visitors)) ,by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

tmp <- xtrain[,.(mean_vis = mean(visitors)) ,by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

tmp <- xtrain[,.(median_vis = median(visitors)) ,by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

tmp <- xtrain[,.(sum_vis = sum(visitors)) ,by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

# merge stores and store_air
stores <- merge(stores,xstore_air)
rm(tmp)

# set air_genre_name/air_area_name to factor and then to interger
stores$air_genre_name <- factor(stores$air_genre_name)
levels(stores$air_genre_name) <- 1:nlevels(stores$air_genre_name)

stores$air_area_name <- factor(stores$air_area_name)
levels(stores$air_area_name) <- 1:nlevels(stores$air_area_name)
stores$air_area_name <- as.integer(stores$air_area_name)

# transform category var to dummy variable
air_genre_name_dum <- dummy(stores$air_genre_name)
stores$air_genre_name <- as.integer(stores$air_genre_name)

# air_area_name_dum <- dummy(stores$air_area_name)
# stores$air_area_name <- as.integer(stores$air_area_name)

# combine air_genre_name_dum、air_area_name_dum and stores
stores <- data.table(cbind(stores,air_genre_name_dum))
stores[, c("air_genre_name") := NULL]

# rm(air_genre_name_dum)

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
train <- rbind(xtrain,xtest)
train <- merge(train,stores,all.x = T)
train <- merge(train,xdate,by.x = 'visit_date', by.y = 'calendar_date')

# merge df and train and aggregate rs1,rs2/rv1，rv2
train <- merge(train,df,all.x = T,by = c("air_store_id","visit_date"))

# combine rs1,rs2,rv1,rv2
train[, "total_reserv_sum" := (rv1 + rv2)]
train[, "total_reserv_mean" := (rv1 + rv2) / 2]
train[, "total_reserv_dt_diff_mean" := (rs1 + rs2) / 2]
train[, c("rs1", "rs2", "rv1", "rv2") := NULL]
train <- train[order(air_store_id, visit_date)]

# --- set na to 0 ---
train[is.na(train)] <- 0

# NEW FEATURES FROM JMBUL
train[,"date_int"] <- as.integer(train$visit_date)
train[,"var_max_lat"] <- max(train$latitude) - train$latitude
train[,"var_max_long"] <- max(train$longitude) - train$longitude


# NEW FEATURES FROM Georgii Vyshnia
train[,"lon_plus_lat"] <- train$longitude + train$latitude


# COMPUTE DISTANCE BY LONGTITUDE/LATITUDE
others <- cbind(train$longitude,train$latitude)
center <- cbind(mean(train$longitude),mean(train$latitude))
dist <- distm(others,center)
train[,"dist"] <- dist

# rm(others,center,dist)

# it decreases the importance of the further past data by applying 
# a weight to them
train[, 'visitors':= log1p(visitors)]
train[,.(visitors = weighted.mean(visitors, weight)), 
      by = c('air_store_id', 'day_of_week', 'holiday_flg')]

# delete day_of_week、weight
train[,day_of_week := NULL]
train[,weight := NULL]


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



# --------------------------------FEATURE SELECT---------------------------------
library(Boruta)

set.seed(10)
borutaAttr <- Boruta(
  visitors ~ . - (air_store_id + visit_date),
  data = x_train,
  maxRuns = 20,
  doTrace = 0
)

borutaVars <- getSelectedAttributes(borutaAttr)
boruta.formula <- formula(
  paste("Survived ~ ",paste(borutaVars, collapse = " + "))
)


# ----------------------------------- XGBOOST MODEL------------------------------
## xgboost - validation ----
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
  'min_child_weight' = 30,
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

# calcluate sd
pred_val <- predict(bst, data.matrix(x1))
print(paste('validation error:', round(sd(pred_val - y1), 4), sep = ' '))

# 2017-12-25 0.5073

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


bst <- xgboost(
  params = param,
  data = d0,
  metrics = "rmse",
  nrounds = best_iteration
)

# predict 
pred_full <- predict(bst, data.matrix(x1))
result1 <- data.frame(
  id = paste(xtest$air_store_id, x_test$visit_date , sep = '_'),
  visitors = expm1(pred_full)
)




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
rm(visitors,visit_data,sample_submission)

# ---------------------------- MERGE RESULT -----------------------------
# merge solution1 and solution2
sample_submission_last <- data.table(
  id = id,
  vis1 = result1$visitors,
  vis2 = result2$visitors
)
sample_submission_last[, "visitors" := (vis1 + vis2 * 1.1) / 2]
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