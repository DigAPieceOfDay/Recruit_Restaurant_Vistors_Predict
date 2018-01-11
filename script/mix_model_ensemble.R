# load packages
library(data.table)
library(stringr)
library(zoo)
library(lubridate)
library(dummies)
library(geosphere)
library(fBasics)
library(caret)

# set workdirectory
setwd("D:/Code/RStudy/DataScience/Kaggle/Recruit Restaurant Vistors Predict")


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

# convert to numeric
reserve_air$reserve_visitors <- as.numeric(reserve_air$reserve_visitors)
reserve_hpg$reserve_visitors <- as.numeric(reserve_hpg$reserve_visitors)

# -----------------------------Feature Engenieer 1-------------------------------
# from surprie me
# aggregate to id x date combo
tmp1 <- reserve_air[,.(
  rv1 = sum(reserve_visitors),
  rs1 = round(mean(time_ahead), 5)
) ,
by = list(air_store_id, visit_date)]

tmp2 <- reserve_hpg[,.(
  rv2 = sum(reserve_visitors),
  rs2 = round(mean(time_ahead), 5)
) ,
by = list(air_store_id, visit_date)]


# merge tmp1,tmp2
df <- merge(tmp1, tmp2, by = c("air_store_id", "visit_date"))
rm(tmp1,tmp2)

### xtrain data clean ----
xtrain$visitors <- as.numeric(xtrain$visitors)
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
  median_vis = median(visitors),
  var_vis = var(visitors),
  sd_vis = sd(visitors),
  mad_vis = mad(visitors),
  skewness_vis = skewness(visitors),
  kurtosis_vis = kurtosis(visitors)
),
by = list(air_store_id, dow)]
stores <- merge(stores,tmp)

# merge stores and store_air
stores <- merge(stores,xstore_air)
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
ids <- xtest$id
xtest$id <- NULL

## --- data aggregation ---
train <- rbind(xtrain,xtest)
train <- merge(train,stores,all.x = T)
train <- merge(train,xdate,by.x = 'visit_date', by.y = 'calendar_date')

# merge df and train and aggregate rs1,rs2/rv1，rv2
train <- merge(train,df, all.x = TRUE,by = c("air_store_id","visit_date"))

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

train$air_store_id_int <- factor(train$air_area_name)
levels(train$air_store_id_int) <- 1:nlevels(train$air_store_id_int)
train$air_store_id_int <- as.integer(train$air_store_id_int)

# COMPUTE DISTANCE BY LONGTITUDE/LATITUDE
others <- cbind(train$longitude,train$latitude)
center <- cbind(mean(train$longitude),mean(train$latitude))
train[,"dist":= distm(others,center)]
rm(others,center)

# it decreases the importance of the further past data by applying 
# a weight to them
train[, "visitors":= log1p(visitors)]

# delete day_of_week、weight
train[,weight := NULL]
train[,day_of_week := NULL]

## -----------------------------Feature Engineer 2-------------------------------
# from lightgbm fe and validation like sliding window
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

# --- set na to 0 ---
train[is.na(train)] <- 0

# separate train and test
x_train <- train[visitors > 0]
x_test <- train[visitors == 0]


# filter outliers
outliers <-
  abs(x_train$visitors - mean(x_train$visitors)) > 2 * sd(x_train$visitors)

x_train <-  x_train[which(outliers == FALSE)]

# ----------------------------- XGBOOST MODEL-----------------------------
library(xgboost)

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

param <- list(
  "objective" = "reg:linear",
  "eta" = 0.05,
  "max_depth" = 8,
  "subsample" = 0.886,
  'min_child_weight' = 10,
  "colsample_bytree" = 0.886,
  "gamma" = 0.5,
  "alpha" = 10,
  "lambda" = 30,
  "silent" = 1,
  "nthread" = 8,
  "seed" = 20171205
)

nround = 1000

# xgboost cross validation
bst.cv <- xgb.cv(
  params = param,
  data = d0,
  metrics = "rmse",
  nrounds = nround,
  nfold = 5,
  early_stopping_rounds = 20
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

x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL


d0 <- xgb.DMatrix(
  data.matrix(x0),
  label = y0
)


# Set i=2 because the first column is for the id variable
i = 2
solution_xgb <- data.frame(id = ids)

# cross validation for full data
for (i in 2:6) {
  set.seed(i)
  bst <- xgboost(
    params = param,
    data = d0,
    metrics = "rmse",
    nrounds = best_iteration
  )
  pred_ful <- predict(bst, data.matrix(x1))
  solution_xgb[, i] <- pred_ful
  
}

# Count the mean of visitors for all solution 
pred_ful <- apply(solution_xgb[,-1], MARGIN = 1, mean)

# PREDICT FULL
result_xgb <- data.frame(
  id = ids,
  visitors = expm1(pred_ful)
)

# --------------------------- Lightgbm Model ------------------------------
library(lightgbm)

## lgbm - validation ----
x0 <- x_train[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- x_train[visit_date > '2017-03-09']


# y0 trian vistors for vistors
y0 <- x0$visitors
y1 <- x1$visitors

x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

cat_features <- c('air_genre_name', 'air_area_name')
d0 <- lgb.Dataset(
  as.matrix(x0),
  label = y0,
  categorical_feature = cat_features,
  free_raw_data = TRUE
)
d1 <- lgb.Dataset(
  as.matrix(x1),
  label = y1,
  categorical_feature = cat_features,
  free_raw_data = TRUE
)

# x0$wgt <- ((1 + mx2)/(1  + mx1))^5

param <- list(
  objective = 'regression',
  metric = 'mse',
  max_depth = 7,
  feature_fraction = 0.7,
  bagging_fraction = 0.8,
  min_data_in_leaf = 30,
  learning_rate = 0.02,
  num_threads = 4,
  weight = 'wgt'
)

nround <- 1000
valids <- list(valid = d1)

# valid train
lgb_cv <- lgb.train(
  params = param,
  data = d0,
  valids = valids,
  nrounds = nround,
  nfold = 4,
  early_stopping_rounds = 10
)

# BEST ITERATORS
best_iteration <- lgb_cv$best_iter


# calcluate rmse
pred_val <- predict(lgb_cv, as.matrix(x1))
val_rmse_lgbm <- RMSE(pred_val,y1)
print(paste('validation rmsle error:', round(val_rmse_lgbm, 4),sep = ' '))


## ---lgbm - full ----

x0 <- x_train
x1 <- x_test

y0 <- x0$visitors

x0$visit_date <- x0$air_store_id <- x0$visitors <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

cat_features <- c('air_genre_name', 'air_area_name')
d0 <- lgb.Dataset(
  as.matrix(x0),
  label = y0,
  categorical_feature = cat_features,
  free_raw_data = FALSE
)

# cross validation for full data
solution_lightgbm <- data.frame(id = ids)

for (i in 2:6) {
  set.seed(i)
  lgb <- lgb.train(
    params = param,  
    data = d0, 
    nrounds = best_iteration
  )
  pred_ful <- predict(lgb, as.matrix(x1))
  solution_lightgbm[, i] <- pred_ful
  
}

# Count the mean of visitors for all solution 
pred_ful <- apply(solution_lightgbm[,-1], MARGIN = 1, mean)

# save submit data
result_lightbgm <- data.frame(
  id = ids,
  visitors = expm1(pred_ful)
)


### ----------------------------- GBM MODEL -------------------------------
library(gbm)

## gbm - validation ----
x0 <- x_train[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- x_train[visit_date > '2017-03-09']

# y0 trian vistors for vistors
y0 <- x0$visitors
y1 <- x1$visitors

x0$visit_date <- x0$air_store_id <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <-  NULL


# Validation TRAIN MODEL
gbm_model <- gbm(
  visitors ~ .,
  data = x0,
  distribution = "gaussian",
  n.trees = 3000,
  shrinkage = 0.01,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  n.minobsinnode = 10,
  cv.folds = 5,
  keep.data = TRUE,
  verbose = TRUE,
  n.cores = 8
)

# check performance using 5-fold cross-validation
best_iteration <- gbm.perf(gbm_model, method = "cv")
print(best_iteration)

# valid predict
pred_val <- predict(gbm_model,x1,2500)


# calcluate rmse
val_rmse_gbm <- RMSE(pred_val,y1)
print(paste('validation rmse error:', round(val_rmse_gbm, 4),sep = ' '))


## ---GBM - full ----
x0 <- x_train
x1 <- x_test

x0$visit_date <- x0$air_store_id <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <-  NULL

# FULL TRAIN MODEL
gbm_model <- gbm(
  visitors ~ .,
  data = x0,
  distribution = "gaussian",
  n.trees = 1000,
  shrinkage = 0.05,
  interaction.depth = 3,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  n.minobsinnode = 10,
  cv.folds = 5,
  keep.data = TRUE,
  verbose = TRUE,
  n.cores = 4
)


# PREDICT FULL
pred_ful <- predict(gbm_model,x1,2500)

# SAVE RESULT
result_gbm <- data.table(
  id = ids,
  visitors = expm1(pred_ful)
)


# ------------------------------- KNN Model ------------------------------
library(kknn)

## randomforest - validation ----
x0 <- x_train[visit_date <= '2017-03-09' & visit_date > '2016-04-01']
x1 <- x_train[visit_date > '2017-03-09']

# y0 trian vistors for vistors
y0 <- x0$visitors
y1 <- x1$visitors

x0$visit_date <- x0$air_store_id <-  NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <- NULL

# set seed for keeping result same
set.seed(20171226)

# Validation TRAIN MODEL
knn_model <- train.kknn(
  visitors ~.,
  x0,
  kmax = 15,
  distance = 1,
  kernel = "optimal"
)

# valid predict
pred_val <- predict(knn_model,x1)

# calcluate rmse
val_rmse_knn <- RMSE(pred_val,y1)
print(paste('validation rmse error:', round(val_rmse_knn, 4),sep = ' '))


## ---GBM - full ----
x0 <- x_train
x1 <- x_test

x0$visit_date <- x0$air_store_id <- NULL
x1$visit_date <- x1$air_store_id <- x1$visitors <-  NULL

knn_model <- train.kknn(
  visitors ~.,
  x0,
  kmax = 15,
  distance = 1,
  kernel = "optimal"
)

# full predict
pred_val <- predict(knn_model,x1)

result_knn <- data.table(
  id = id,
  visitors = expm1()
)


# MERGE Three MODEL PREDICT RESULT
result1 <- data.frame(
  id = ids,
  visitors = result_xgb$visitors * 0.4 + result_lightbgm$visitors * 0.3 
)


# Create csv submission file
write.csv(
  result1,
  paste0("./submission/xgb_lgb",".csv"), 
  row.names = FALSE,
  quote = F
)


# ----------------------------- WEIGHT MEANS ----------------------------
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

# ------------------------------ BLEND RESULT ------------------------------
# merge result1 and result2
blend_result1 <- data.table(
  id = ids,
  visitors = result1$visitors * 0.6 + result2$visitors * 0.4 * 1.1
)

# Blend surprise2 result
blend_result2 <- fread("./output/blend/sample/SubmissonK.csv")

sample_submission <- data.table(
  id = ids,
  visitors = blend_result1$visitors * 0.4 + blend_result2$visitors * 0.6
)


# Create csv submission file
write.csv(
  sample_submission,
  paste0("./submission/mix_ensemble_",Sys.Date(),".csv"), 
  row.names = FALSE,
  quote = F
)


# save data
save.image("./output/save/mix_model_ensemble.RData")


# 2018-01-05  0.483 (blend -- tune parameters and new blend)
# 2018-01-08  0.485 (add new feature)
# 2018-01-08  0.485 (tune nrounds)