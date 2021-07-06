library(readxl)
library(forecast)
library(dplyr)
library(rlist)

# Load the data
setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/per kpw")
data <- data.frame(read_xlsx("pemusnahan_uang.xlsx", sheet="MO"))
data.inflow <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet="MO"))
names(data)[names(data) == 'pemusnahan'] <- 'sum_all_uk_ul'
names(data.inflow)[names(data.inflow) == 'inflow'] <- 'sum_all_uk_ul'


#______________________Cross Validation RMSE or MSE______________________#

evaluation <- function(y_test, depan) {
  pu <- data.frame(data[, c(y_test)])
  
  #Forecast Evaluation
  farima <- function(x, h) {
    forecast(auto.arima(x), h=h)
  }
  fholt <- function(x, h) {
    forecast(holt(x), h=h)
  }
  fhw <- function(x, h) {
    forecast(hw(x), seasonal="multiplication", h=h)
  }
  fets <- function(x, h) {
    forecast(ets(x), h = h)
  }
  pu.ts <- ts(pu, start = c(2010, 1), end = c(2021, 3), frequency = 12)
  lpu.ts <- log(pu.ts)
  
  # Error forecast
  e1 <- tsCV(lpu.ts, stlf, method="arima", h=depan)
  e2 <- tsCV(lpu.ts, stlf, method="ets", h=depan)
  e3 <- tsCV(lpu.ts, stlf, method="rwdrift", h=depan)
  e4 <- tsCV(lpu.ts, farima, h=depan)
  e5 <- tsCV(lpu.ts, fholt, h=depan)
  e6 <- tsCV(lpu.ts, fhw, h=depan)
  e7 <- tsCV(lpu.ts, fets, h=depan)
  
  error <- list(e1, e2, e3, e4, e5, e6, e7)
  mape <- function(x) {
    mape <- mean(abs(x/lpu.ts))*100
  }
  mape.list <- lapply(error, mape)
  names(mape.list) <- c("stlf.arima", "stlf.ets", "stlf.rwdirft",
                        "arima", "holt", "hw", "ets")
  min.mape <- which.min(mape.list)
  list_all <-list(min.mape, mape.list)
  return(list_all)
}

data.test <- data[,-1]
data.test <- data.test[, 1:9]
uk_jumlah <- data[,12]
pemusnahan_uang <- data[,23]
data.test <- cbind(data.test, uk_jumlah, pemusnahan_uang)
colomn.names <- colnames(data.test)

min.eval.list <- list()
for (i in colomn.names) {
  min.eval <- evaluation(i, 6)
  min.eval.list[[i]] <- min.eval
}

#_____________SPACE FOR TRIAL AND ERROR_______________________#

pu <- data.frame(data[, c("pemusnahan")])

#Forecast Evaluation
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
fholt <- function(x, h) {
  forecast(holt(x), h=h)
}
fhw <- function(x, h) {
  forecast(hw(x), seasonal="multiplication", h=h)
}
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
pu.ts <- ts(pu, start = c(2010, 1), end = c(2021, 3), frequency = 12)
lpu.ts <- log(pu.ts)

# Error forecast
e1 <- tsCV(lpu.ts, stlf, method="arima", h=6)
e2 <- tsCV(lpu.ts, stlf, method="ets", h=6)
e3 <- tsCV(lpu.ts, stlf, method="rwdrift", h=6)
e4 <- tsCV(lpu.ts, farima, h=6)
e5 <- tsCV(lpu.ts, fholt, h=6)
e6 <- tsCV(lpu.ts, fhw, h=6)
e7 <- tsCV(lpu.ts, fets, h=6)

error <- list(e1, e2, e3, e4, e5, e6, e7)
mape <- function(x) {
  mape <- mean(abs(x/lpu.ts))*100
}
mape.list <- lapply(error, mape)
names(mape.list) <- c("stlf.arima", "stlf.ets", "stlf.rwdirft",
                      "arima", "holt", "hw", "ets")
min.mape <- which.min(mape.list)
min.mape

# create training data
train2 <- window(lpu.ts, end = c(2018, 12))

# create specific test data of interest
test <- window(lpu.ts, start = c(2019, 1), end = c(2021, 3))

farima <- forecast(auto.arima(train2), h=6)
mean(accuracy(farima, test)[,c("MAPE")])
fholt <- forecast(holt(train2), h=6)
mean(accuracy(fholt, test)[,c("MAPE")])
stlf.f <- stlf(train2, method="arima", h=6)
akurasi <- accuracy(stlf.f, test)
mean(akurasi[,c("MAPE")])
sd(akurasi[,c("MAPE")])

#_____________________MAPE and MASE_______________________#

  evaluasi <- function(y_test, h){
    pu <- data.frame(data[, c(y_test)])
    inflow <- data.frame(data.inflow[, c(y_test)])
    outflow <- data.frame(data.outflow[, c(y_test)])
    pu.ts <- ts(pu, start = c(2010, 1), end = c(2021, 3), frequency = 12)
    inflow.ts <- ts(inflow, start = c(2010, 1), end = c(2021, 3), frequency = 12)
    outflow.ts <- ts(outflow, start = c(2010, 1), end = c(2021, 3), frequency = 12)
    x <- log(outflow.ts)
  
    # create training data
    train.inflow <- window(inflow.ts, end = c(2018, 12))
    train2 <- window(x, end = c(2018, 12))
    # train2 <- train2/train.inflow
    
    # create specific test data of interest
    rasio <- pu.ts/inflow.ts
    test <- window(x, start = c(2019, 1), end = c(2019, h))
    test.inflow <- window(inflow.ts, start = c(2019, 1), end = c(2019, h))
    test.outflow <- window(outflow.ts, start = c(2019, 1), end = c(2019, h))
  
    # Forecast
    stlf.arima <- stlf(train2, method="arima", h=h)
    stlf.ets <- stlf(train2, method="ets", h=h)
    stlf.rwdrift <- stlf(train2, method="rwdrift", h=h)
    farima <- forecast(auto.arima(train2), h=h)
    fholt <- forecast(holt(train2), h=h)
    fhw <- forecast(hw(train2), h=h)
    fets <- forecast(ets(train2), h = h)
    f.list <- list(stlf.arima, stlf.ets, stlf.rwdrift,
                    farima, fholt, fhw, fets)
  
    # Accuracy
    rata2 <- list()
    standev <- list()
    for (i in f.list) {
      mean1 <- mean(accuracy(i, test)[,c("MAPE")])
      sd1 <- sd(accuracy(i, test)[,c("MAPE")])
      rata2 <- list.append(rata2, mean1)
      standev <- list.append(standev, sd1)
    }
    rata2.df <- data.frame(rata2)
    colnames(rata2.df) <- c("stlf arima", "stlf ets", "stlf rwdrift",
                                "arima", "holt", "holt-winter", "ets")
    sd.df <- data.frame(standev)
    colnames(sd.df) <- c("stlf arima", "stlf ets", "stlf rwdrift",
                              "arima", "holt", "holt-winter", "ets")
    mape <- rbind(rata2.df, sd.df)
    # return(mape)
    
    # Accuracy 2
    rata2 <- list()
    for (i in f.list) {
      fcst.test <- exp(i$mean) 
      mean1 <- accuracy(fcst.test, test.outflow)[,c("MAPE")]
      rata2 <- list.append(rata2, mean1)
    }
    rata2.df <- data.frame(rata2)
    colnames(rata2.df) <- c("stlf arima", "stlf ets", "stlf rwdrift",
                            "arima", "holt", "holt-winter", "ets")
    mape <- rata2.df
    mape
    return(mape)
  }
  
  evaluasi_sumflow <- function(data.test, y_test, h){
    sumflow <- data.frame(data.test[, c(y_test)])
    sumflow.ts <- ts(sumflow, start = c(2010, 1), end = c(2021, 3), frequency = 12)
    x <- sumflow.ts
    
    # create training data
    train2 <- window(x, end = c(2020, 12))
    
    # create specific test data of interest
    test <- window(x, start = c(2021, 1), end = c(2021, 3))
    
    # Forecast
    stlf.arima <- stlf(train2, method="arima", h=h)
    stlf.ets <- stlf(train2, method="ets", h=h)
    stlf.rwdrift <- stlf(train2, method="rwdrift", h=h)
    farima <- forecast(auto.arima(train2), h=h)
    fholt <- forecast(holt(train2), h=h)
    fhw <- forecast(hw(train2), h=h)
    fets <- forecast(ets(train2), h = h)
    f.list <- list(stlf.arima, stlf.ets, stlf.rwdrift,
                   farima, fholt, fhw, fets)
    
    # Accuracy 2
    rata2 <- list()
    for (i in f.list) {
      mean1 <- accuracy(i, x)["Test set", "MAPE"]
      rata2 <- list.append(rata2, mean1)
    }
    rata2.df <- data.frame(rata2)
    colnames(rata2.df) <- c("stlf arima", "stlf ets", "stlf rwdrift",
                            "arima", "holt", "holt-winter", "ets")
    mape <- rata2.df
    
    # Accuracy 3
    rata2 <- list()
    for (i in f.list) {
      mean1 <- accuracy(i, x)["Test set", "MASE"]
      rata2 <- list.append(rata2, mean1)
    }
    rata2.df <- data.frame(rata2)
    colnames(rata2.df) <- c("stlf arima", "stlf ets", "stlf rwdrift",
                            "arima", "holt", "holt-winter", "ets")
    mase <- rata2.df
    
    return(mase)
  }
  
  #Pemusnahan Uang
  data.pu <- data.frame(read_xlsx("pemusnahan_uang.xlsx", sheet="NAS"))
  data.inflow <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet="MDN"))
  names(data)[names(data) == 'pemusnahan'] <- 'sum_all_uk_ul'
  names(data.inflow)[names(data.inflow) == 'inflow'] <- 'sum_all_uk_ul'
  names(data.outflow)[names(data.outflow) == 'outflow'] <- 'sum_all_uk_ul'
  
  data.test <- data.pu[,-1]
  data.test <- data.test[, 1:7]
  uk_jumlah <- data.pu[,12]
  # sum_all_uk_ul <- data[,23]
  data.test <- cbind(data.test, uk_jumlah)
  colomn.names <- colnames(data.test)
  
  # Outflow and Inflow
  data.sumflow <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet="MDN"))
  names(data.sumflow)[names(data.sumflow) == 'inflow'] <- 'sumflow' # Harus diganti2 inflow dan outflow
  data.test <- data.sumflow[, 1:7]
  uk_jumlah <- data.sumflow[,12]
  ul_jumlah <- data.sumflow[,22]
  sumflow <- data.sumflow[,23]
  data.test <- cbind(data.test, uk_jumlah, data.sumflow["ul_500"], ul_jumlah, sumflow)
  data.test <- data.test[,-1]
  colomn.names <- colnames(data.test)
  
  # Evaluation
  eval.list <- list()
  for (i in colomn.names) {
    eval <- evaluasi_sumflow(data.pu, i, 4)
    eval.list[[i]] <- data.frame(eval)
  }

  eval.mean.min <-list()
  for (i in colomn.names) {
    min.mean <- which.min(eval.list[[i]][1,])
    eval.mean.min[[i]] <- min.mean
  }

  # Mode function
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  # Best forecast method based on the MAPE and MASE evaluation
  getmode(eval.mean.min)

  library(writexl)
  setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/per kpw")
  write_xlsx(eval.list, "evaluation_list_pu_mase_nas.xlsx")
  
  
  