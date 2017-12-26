## wd etc ----
require(data.table)
require(stringr)
require(lubridate)
require(zoo)
require(lightgbm)

## data: train and test ----
xtrain <- fread('./data/air_visit_data.csv')
xtest <- fread('./data/sample_submission.csv')

# reservations: air clean
reserve_air <- fread('./data/air_reserve.csv')

## store info: air ----
xstore <- fread('./data/air_store_info.csv',encoding = "UTF-8")

## date info ---
xdate <- fread('./data/date_info.csv')


# ----------------------------DATA CLEAN--------------------------------
# --- 1.train and test data clean ---
# align the columns (test has the store id and date concatenated)
xtest$air_store_id <- str_sub(xtest$id, 1,-12)
xtest$visit_date <- str_sub(xtest$id, -10)
xtest$id <- NULL

# format date 
xtrain$visit_date <- as.Date(xtrain$visit_date)
xtest$visit_date <- as.Date(xtest$visit_date)

# combine train and test 
xtrain <- rbind(xtrain, xtest)

## --- 2.reservations: air clean ----
# convert to datetime
reserve_air$visit_datetime <- parse_date_time(
  reserve_air$visit_datetime, 
  orders = '%Y-%m-%d H:M:S'
)
reserve_air$reserve_datetime <- parse_date_time(
  reserve_air$reserve_datetime, 
  orders = '%Y-%m-%d H:M:S'
)

# time ahead = visit_datetime - reserve_datetime
reserve_air$time_ahead <- 
  as.double(reserve_air$visit_datetime - reserve_air$reserve_datetime) / 3600

# round to day
reserve_air$visit_date <- as.Date(reserve_air$visit_datetime)
reserve_air$reserve_datetime <- as.Date(reserve_air$visit_datetime)

# aggregate to id x date combo
res_air_agg <-
  reserve_air[j = list(
    air_res_visitors = sum(reserve_visitors),
    air_mean_time_ahead = round(mean(time_ahead), 2)
  ) ,
  by = list(air_store_id, visit_date)]
rm(reserve_air)


# set air_air_genre_name to factor 
xstore$air_genre_name <- factor(xstore$air_genre_name)
levels(xstore$air_genre_name) <- 1:nlevels(xstore$air_genre_name)

# set air_genre_name to integer
xstore$air_genre_name <- as.integer(xstore$air_genre_name)

# set air_genre_name to factor and then to interger
xstore$air_area_name <- factor(xstore$air_area_name)
levels(xstore$air_area_name) <- 1:nlevels(xstore$air_area_name)
xstore$air_area_name <- as.integer(xstore$air_area_name)

## --- 3.date info clean ---
# holidays at weekends are not special, right?
wkend_holidays <- which(
  xdate$day_of_week %in% c("Saturday", "Sunday") & 
    xdate$holiday_flg ==1
)
xdate[wkend_holidays, 'holiday_flg' := 0]

# add decreasing weights from now
xdate[, 'weight' := (.I/.N) ^ 7]
xdate$calendar_date <- as.Date(xdate$calendar_date)

## --- 4.data aggregation ---
xtrain <- merge(xtrain, res_air_agg, all.x = T)
xtrain <- merge(xtrain, xstore, all.x = T, by = 'air_store_id' )
xtrain <- merge(xtrain, xdate, by.x = 'visit_date', by.y = 'calendar_date')

# weight means for vistors
xtrain[, 'visitors':= log1p(visitors)]
xtrain[,.(visitors = weighted.mean(visitors, weight)), 
       by = c('air_store_id', 'day_of_week', 'holiday_flg')]

# delete day_of_week
xtrain[,day_of_week := NULL]
rm(res_air_agg, xstore, xdate)

# --- 5.set na to 0 ---
xtrain[is.na(xtrain)] <- 0
xtrain <- xtrain[order(air_store_id, visit_date)]

## --------------------------Feature Engineer --------------------------
# 1.holiday in the last 3 days
xtrain[, h3a := rollapply(
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
xtrain[, vis14 := rollapply(
  log1p(visitors),
  width = 39,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[, vis21 := rollapply(
  log1p(visitors),
  width = 46,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[, vis28 := rollapply(
  log1p(visitors),
  width = 60,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[, vis35 := rollapply(
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
xtrain[, vLag1 := round((vis21 - vis14) / 7, 2)]
xtrain[, vLag2 := round((vis28 - vis14) / 21, 2)]
xtrain[, vLag3 := round((vis35 - vis14) / 35, 2)]
xtrain[, vis14 := NULL, with = TRUE]
xtrain[, vis21 := NULL, with = TRUE]
xtrain[, vis28 := NULL, with = TRUE]
xtrain[, vis35 := NULL, with = TRUE]

# 3.reservations for 7days and so on (like visit data)
xtrain[, res7 := rollapply(
  log1p(air_res_visitors),
  width = 7,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[,res14 := rollapply(
  log1p(air_res_visitors),
  width = 14,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[, res21 := rollapply(
  log1p(air_res_visitors),
  width = 21,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

xtrain[, res28 := rollapply(
  log1p(air_res_visitors),
  width = 28,
  FUN = function(s)
    sum(s, na.rm = T),
  partial = TRUE,
  fill = 0,
  align = 'right'
),
by = c('air_store_id')]

# separate train and test
x_train <- xtrain[visitors > 0]
x_test <- xtrain[visitors == 0]
rm(xtrain,xtest)

# -----------------------------BUILD MODEL------------------------------
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

params <- list(
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

ntrx <- 1000
valids <- list(valid = d1)
model <- lgb.train(
  params = params,
  data = d0,
  valids = valids,
  nrounds = ntrx,
  early_stopping_rounds = 10
)

pred_val <- predict(model, as.matrix(x1))
print( paste('validation error:', round(sd(pred_val - y1),4), sep = ' ' ))

# 0.5863

ntrx <- model$best_iter

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

# set params
params <- list(
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

model <- lgb.train(params = params,  data = d0, nrounds = ntrx)
pred_full <- predict(model, as.matrix(x1))

# save submit data
result1 <- data.frame(
  id = paste(x_test$air_store_id, x_test$visit_date , sep = '_'),
  vis1 = expm1(pred_full)
)


# ------------------------use weight means from hklee---------------------
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
date_info[, 'weight' := (.I/.N) ^ 7]

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
sample_submission[, 'vis2' := expm1(visitors)]
result2 <- sample_submission[,c("id","vis2")]
result2 <- result2[order(result2$id),]

# remove temp variable
rm(air_visit_data,date_info,visitors,visit_data,sample_submission)


# -------------------------------SAVE RESULT-----------------------------
# merge solution1 and solution2
sample_submission_last <- data.table(
  id = result1$id,
  vis1 = result1$vis1,
  vis2 = result2$vis2
)
sample_submission_last$visitors <- apply(sample_submission_last[,-1],1,mean)
sample_submission_last <- sample_submission_last[,c("id","visitors")]



write.csv(
  result1, 
  paste0("./output/submission_data/lightgbm_",".csv"), 
  row.names = F, 
  quote = F
)


save.image(
  paste0("./output/save/lightgbm_",".RData")
)