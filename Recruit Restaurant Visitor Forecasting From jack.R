library(data.table)
library(xgboost)

dates <- as.character(seq(as.Date("2016-01-01"), as.Date("2017-05-31"),by = "day"))
test.dates <- as.character(seq(as.Date("2017-04-23"), as.Date("2017-05-31"), by = "day"))

##### history of number of visit #####

air_visit <- fread("./data/air_visit_data.csv")
air_visit_dcast <- dcast(
  air_visit,
  air_store_id ~ visit_date,
  value.var = "visitors",
  fill = NA
)

cols <- c("date", "air_store_id", paste0("b", 0:(7 * 53), "d_visit"))

data.visit <- data.table()
for (d in dates) {
  temp <- air_visit_dcast
  colnames(temp)[-1] <-
    paste0("b", as.Date(d) - as.Date(colnames(temp)[-1]), "d_visit")
  temp <- cbind(date = d, temp)
  
  na.cols <- setdiff(cols, colnames(temp))
  for (col in na.cols) {
    temp[[col]] <- NA
  }
  temp <- temp[, cols, with = FALSE]
  data.visit <- rbind(data.visit, temp)
}
data.visit <-
  data.visit[!is.na(b0d_visit) | date >= as.character("2017-04-23")]

##### prepare other data #####
# air_reserve clean
air_reserve <- fread("./data/air_reserve.csv")
air_reserve[, date := as.character(as.Date(visit_datetime))]


# date_info clean
date_info <- fread("./data/date_info.csv")
date_info[, dow := wday(as.Date(calendar_date))]
date_info[, holiday_flg := ifelse(dow %in% c(1, 7), 1, holiday_flg)]
date_info[, 
 holiday_flgs3 := paste0(shift(holiday_flg, 1),
 holiday_flg,
 shift(holiday_flg, 1, type = "lead"))
]
normal.date <- date_info[
  dow == 1 & holiday_flgs3 == "110" |
  dow == 2 & holiday_flgs3 == "100" |
  dow == 3 & holiday_flgs3 == "000" |
  dow == 4 & holiday_flgs3 == "000" |
  dow == 5 & holiday_flgs3 == "000" |
  dow == 6 & holiday_flgs3 == "001" |
  dow == 7 & holiday_flgs3 == "011"
]

# store clean -- 
air_store_info <- fread("./data/air_store_info.csv", drop = 3)
air_store_info[, air_genre_name := as.integer(as.factor(air_genre_name))]
for (j in 1:max(air_store_info$air_genre_name)) {
  if (j %in% c(1, 6, 10)) {
    next
  }
  air_store_info[[paste0("genre_", j)]] <- (air_store_info$air_genre_name == j) * 1
}
air_store_info[, air_genre_name := NULL]

##### make xgboost model and prediction for each day #####

key <- c("date", "air_store_id")
target <- "b0d_visit"

prediction <- data.table()
for (day in 1:length(test.dates)) {
  print(test.dates[day])
  test.dow <- (day - 1) %% 7 + 1
  
  ##### select dates for train #####
  
  if (day %in% 10:13) {
    train.dates <- normal.date[dow == test.dow]$calendar_date
  } else {
    if (test.dow == 1) {
      train.dates <- normal.date[dow %in% c(1, 7)]$calendar_date
    }
    if (test.dow == 2) {
      train.dates <- normal.date[dow %in% c(2, 3, 5)]$calendar_date
    }
    if (test.dow == 3) {
      train.dates <- normal.date[dow %in% c(2, 3, 4, 5)]$calendar_date
    }
    if (test.dow == 4) {
      train.dates <- normal.date[dow %in% c(3, 4, 5)]$calendar_date
    }
    if (test.dow == 5) {
      train.dates <- normal.date[dow %in% c(3, 4, 5)]$calendar_date
    }
    if (test.dow == 6) {
      train.dates <- normal.date[dow %in% c(4, 6, 7)]$calendar_date
    }
    if (test.dow == 7) {
      train.dates <- normal.date[dow %in% c(1, 6, 7)]$calendar_date
    }
  }
  train.dates <- train.dates[
    "2016-01-04" < train.dates & train.dates < "2016-12-31" |
     "2017-01-04" < train.dates & train.dates < "2017-04-23"
  ]
  train.dates <- train.dates[order(train.dates, decreasing = TRUE)]
  train.dates <- train.dates[1:min(150, length(train.dates))]
  
  ##### history of number of reserve #####
  cols <- c("date", "air_store_id", paste0("b", 0:(7 * 10), "d_reserve"))
  
  data.reserve <- data.table()
  for (d in c(train.dates, test.dates[day])) {
    temp <- air_reserve[
      reserve_datetime < as.character(as.Date(d) + 1 - day), 
      .(reserve_visitors = sum(reserve_visitors)),
      by = .(air_store_id, date)
    ]
    if (nrow(temp) == 0) {
      next
    }
    temp <- dcast(
      temp,
      air_store_id ~ date,
      value.var = "reserve_visitors",
      fill = NA
    )
    colnames(temp)[-1] <- paste0(
      "b",
      as.Date(d) - as.Date(colnames(temp)[-1]),
      "d_reserve"
    )
    
    temp <- cbind(date = d, temp)
    
    na.cols <- setdiff(cols, colnames(temp))
    for (col in na.cols) {
      temp[[col]] <- NA
    }
    temp <- temp[, cols, with = FALSE]
    data.reserve <- rbind(data.reserve, temp)
  }
  
  ##### train data #####
  data.train <- data.visit[date %in% train.dates]
  
  if (day == 10) {
    # regard 2017-05-02 as Friday
    data.train[, b0d_visit := b4d_visit]
  } else if (day == 11) {
    # regard 2017-05-03 as Saturday
    data.train[, b0d_visit := b4d_visit]
  } else if (day == 12) {
    # regard 2017-05-04 as Saturday
    data.train[, b0d_visit := b5d_visit]
  } else if (day == 13) {
    # regard 2017-05-05 as Saturday
    data.train[, b0d_visit := b6d_visit]
  }
  
  data.train <- data.train[!is.na(data.train[[target]])]
  data.train <- merge(
    data.train,
    data.reserve,
    by = c("date", "air_store_id"),
    all.x = TRUE,
    sort = FALSE
  )
  
  ##### test data #####
  
  data.test <- data.visit[date == test.dates[day]]
  data.test <- merge(
    data.test,
    data.reserve,
    by = c("date", "air_store_id"),
    all.x = TRUE,
    sort = FALSE
  )
  
  ##### log transform #####
  
  for (j in 3:ncol(data.train)) {
    data.train[[j]] <- log(data.train[[j]] + 1)
  }
  for (j in 3:ncol(data.test)) {
    data.test[[j]] <- log(data.test[[j]] + 1)
  }
  
  ##### select and add features #####
  
  days <- unique(c(0:20 + day, c(1:53) * 7))
  days <- days[days >= day]
  exp.vars <- paste0("b", days, "d_visit")
  days <- unique(c(0:7, (0:10) * 7))
  exp.vars <- c(exp.vars, paste0("b", days, "d_reserve"))
  
  for (j in 1:5) {
    days <- day:(day + 6) + (j - 1) * 7
    data.train[[paste0("b", j, "w")]] <-
      apply(data.train[, paste0("b", days, "d_visit"), with = FALSE], 1, mean, na.rm =TRUE)
    data.test[[paste0("b", j, "w")]] <-
      apply(data.test[, paste0("b", days, "d_visit"), with = FALSE], 1, mean, na.rm = TRUE)
    exp.vars <- c(exp.vars, paste0("b", j, "w"))
  }
  
  data.train$dow_sum8w <- apply(data.train[, paste0("b", (seq(1, 8) + (day - 1) %/% 7) * 7, "d_visit"), with =
                                             FALSE], 1, mean, na.rm = TRUE)
  data.test$dow_sum8w <-
    apply(data.test[, paste0("b", (seq(1, 8) + (day - 1) %/% 7) * 7, "d_visit"), with =
                      FALSE], 1, mean, na.rm = TRUE)
  
  exp.vars <- c(exp.vars, "dow_sum8w")
  exp.vars <- intersect(exp.vars, colnames(data.train))
  
  data.train <- data.train[, c(key, target, exp.vars), with = FALSE]
  data.test <- data.test[, c(key, exp.vars), with = FALSE]
  
  data.train <- merge(
    data.train,
    air_store_info,
    by = "air_store_id",
    all.x = TRUE,
    sort = FALSE
  )
  data.test <- merge(
    data.test,
    air_store_info,
    by = "air_store_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  temp <- wday(as.Date(data.train$date))
  if (length(unique(temp)) <= 2) {
    data.train[, dow := wday(as.Date(date))]
    data.test[, dow := wday(as.Date(date))]
  } else {
    for (j in 1:7) {
      if (sum(temp == j) > 0) {
        data.train[[paste0("dow_", j)]] <- (temp == j) * 1
        data.test[[paste0("dow_", j)]] <-
          (wday(as.Date(data.test$date)) == j) * 1
      }
    }
  }
  
  data.train$day <- as.integer(substr(data.train$date, 9, 10))
  data.test$day <- as.integer(substr(data.test$date, 9, 10))
  
  exp.vars <- setdiff(colnames(data.train), c(key, target))
  
  ##### modeling #####
  
  x.train <- data.train[, exp.vars, with = FALSE]
  x.train <- as.matrix(apply(x.train, 2, as.numeric))
  y.train <- data.train[[target]]
  dtrain <- xgb.DMatrix(data = x.train, label = y.train)
  
  x.test <- data.test[, exp.vars, with = FALSE]
  x.test <- as.matrix(apply(x.test, 2, as.numeric))
  
  params <- list(
    "eta"               = 0.02,
    "max_depth"         = 8,
    "min_child_weight"  = 16,
    "subsample"         = 0.9,
    "colsample_bylevel" = 0.1,
    "objective"         = "reg:linear",
    "eval_metric"       = "rmse",
    "base_score"        = mean(y.train)
  )
  
  set.seed(0)
  model.xgb <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = ifelse(day %in% 10:13, 300, 700),
    watchlist = list(train = dtrain),
    print_every_n = 50,
    nthread = 4
  )
  
  ##### predict #####
  temp <- cbind(
    data.test[, key, with = FALSE], 
    visitors = predict(model.xgb, x.test, reshape =TRUE)
  )
  temp[, id := paste0(air_store_id, "_", date)]
  temp[, visitors := round(exp(visitors) - 1, 5)]
  prediction <- rbind(prediction, temp[, .(id, visitors)])
  
}

##### make submission #####

submission <- fread("./data/sample_submission.csv", select = 1)
submission <- merge(submission, prediction, by = "id", all.x = TRUE)
submission[visitors < 1, visitors := 1]
write.csv(
  submission, 
  "./submission/submission_from_jack.csv", 
  row.names = FALSE
)