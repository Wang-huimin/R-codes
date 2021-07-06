library(ggplot2)
library(forecast)
library(bsts)
library(repr)
library(readxl)
library(writexl)


## Choosing a theme for clear and consistent data plots 
theme_set(theme_bw())
par(mfrow=c(1,1))
setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/Analisis")

#FUNCTION THAT RETURN TO FORECAST VALUE OUTFLOW
dekompos <- function(x, nahead, tipe) {
  combine_data <- list()
  #loop over data
  for (i in 1:ncol(x)) {
    train_1 <- window(x[ ,i], end = c(2020, 12))
    STL <- stl(x[ ,i], s.window="periodic", robust=TRUE)
    assign(paste("stl", i, sep = ""), STL)
    STL_ARIMA <- stlf(train_1, h=nahead, method="arima", level=c(80,95))
    assign(paste("stl_arima", i, sep = ""), STL_ARIMA)
    combine_data[[i]] <- assign(paste("stl_arima", i, sep = ""), STL_ARIMA)$mean
  }
  names(combine_data) <- c("uk_100k", "uk_50k", "uk_20k", "uk_10k", "uk_5k",
                           "uk_2k", "uk_1k", "uk_0.5k", "uk_0.1k", "uk_u0.1k", "sum_uk", 
                           "ul_1k", "ul_0.5k", "ul_0.2k", "ul_0.1k", "ul_0.05k", "ul_0.025k",
                           "ul_0.01k", "ul_0.0.005k", "ul_u0.005k", "sum_ul", "sum_flow")
  combine_data <- data.frame(combine_data)
  write_xlsx(combine_data, "Nilai Tengah Proyeksi per Pecahan.xlsx")
  
  if (tipe=="Outflow") {
    #Grafik Uang Kertas Outflow
    par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
    plot(stl_arima1, main="Proyeksi Outflow 100.000")
    plot(stl_arima2, main="Proyeksi Outflow 50.000")
    plot(stl_arima3, main="Proyeksi Outflow 20.000")
    plot(stl_arima4, main="Proyeksi Outflow 10.000")
    mtext("Proyeksi Outflow Uang Kertas", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Outflow Uang Kertas 1.png")
    dev.off()
    par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    plot(stl_arima5, main="Proyeksi Outflow 5.000")
    plot(stl_arima6, main="Proyeksi Outflow 2.000")
    plot(stl_arima7, main="Proyeksi Outflow 1.000")
    mtext("Proyeksi Outflow Uang Kertas", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Outflow Uang Kertas 2.png")
    dev.off()
    
    #Grafik Uang Logam Outflow
    par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
    plot(stl_arima12, main="Proyeksi Outflow 1.000")
    plot(stl_arima13, main="Proyeksi Outflow 500")
    plot(stl_arima14, main="Proyeksi Outflow 200")
    plot(stl_arima15, main="Proyeksi Outflow 100")
    mtext("Proyeksi Outflow Uang Logam", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Outflow Uang Logam 1.png")
    dev.off()
    par(mfrow=c(2,1), oma = c(0, 0, 2, 0))
    plot(stl_arima16, main="Proyeksi Outflow 50")
    plot(stl_arima20, main="Proyeksi Outflow <50")
    mtext("Proyeksi Outflow Uang Logam", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Outflow Uang Logam 2.png")
    dev.off()
  } else if (tipe=="Inflow") {
    #Grafik Uang Kertas Inflow
    par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
    plot(stl_arima1, main="Proyeksi Inflow 100.000")
    plot(stl_arima2, main="Proyeksi Inflow 50.000")
    plot(stl_arima3, main="Proyeksi Inflow 20.000")
    plot(stl_arima4, main="Proyeksi Inflow 10.000")
    mtext("Proyeksi Inflow Uang Kertas", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Inflow Uang Kertas 1.png")
    dev.off()
    par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    plot(stl_arima5, main="Proyeksi Inflow 5.000")
    plot(stl_arima6, main="Proyeksi Inflow 2.000")
    plot(stl_arima7, main="Proyeksi Inflow 1.000")
    mtext("Proyeksi Inflow Uang Kertas", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Inflow Uang Kertas 2.png")
    dev.off()
    
    #Grafik Uang Logam Inflow
    par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
    plot(stl_arima12, main="Proyeksi Inflow 1.000")
    plot(stl_arima13, main="Proyeksi Inflow 500")
    plot(stl_arima14, main="Proyeksi Inflow 200")
    plot(stl_arima15, main="Proyeksi Inflow 100")
    mtext("Proyeksi Inflow Uang Logam", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Inflow Uang Logam 1.png")
    dev.off()
    par(mfrow=c(2,1), oma = c(0, 0, 2, 0))
    plot(stl_arima16, main="Proyeksi Inflow 50")
    plot(stl_arima20, main="Proyeksi Inflow <50")
    mtext("Proyeksi Inflow Uang Logam", outer = TRUE, cex = 1)
    dev.copy(png,"Proyeksi Inflow Uang Logam 2.png")
    dev.off()
  } else {
    return(FALSE)
  }
  
  return(combine_data)
}

## Export Data
dir()
setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/Analisis")
data <- data.frame(read_xlsx("kpw_dekomposisi.xlsx", sheet="Outflow PMS"))

#drop kolom bulan
drops <- c("Bulan", "kpw")
data <- data.frame(data[ , !(names(data) %in% drops)] )

#Setting time series
data_ts <- ts(data, start=c(2010, 1), end=c(2020, 12), frequency=12)
setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/Analisis/Pematang Siantar")
forecast_value <- dekompos(data_ts, 6, "Outflow")
write_xlsx(forecast_value, "forecast_value Outflow PMS.xlsx")


#__________________________________ New Decomposition______________________________________#

dekompos2 <- function(x, nahead) {
  combine_data <- list()
  #loop over data
  for (i in 1:ncol(x)) {
    train_1 <- window(x[ ,i], end = c(2021, 3))
    STL <- stl(x[ ,i], s.window="periodic", robust=TRUE)
    assign(paste("stl", i, sep = ""), STL)
    STL_ARIMA <- stlf(train_1, h=nahead, method="arima", level=c(80,95))
    assign(paste("stl_arima", i, sep = ""), STL_ARIMA)
    combine_data[[i]] <- assign(paste("stl_arima", i, sep = ""), STL_ARIMA)$mean
  }
  names(combine_data) <- c("uk_100k", "uk_50k", "uk_20k", "uk_10k", "uk_5k",
                           "uk_2k", "uk_1k", "uk_0.5k", "uk_0.1k", "uk_u0.1k", "sum_uk", 
                           "ul_1k", "ul_0.5k", "ul_0.2k", "ul_0.1k", "ul_0.05k", "ul_0.025k",
                           "ul_0.01k", "ul_0.0.005k", "ul_u0.005k", "sum_ul", "sum_flow")
  combine_data <- data.frame(combine_data)
  return(combine_data)
}

x <- quarterly
nahead <- 11

dekompos3 <- function(x, nahead) {
  fstl <- stlf(x, h=nahead, method="rwdrift", level=50)
  # fhw <- forecast(hw(x), h = nahead)
  # fets <- forecast(ets(x), h = nahead)
  forecast_outflow <- fstl$mean
  
  return(forecast_outflow)
}

dekompos4 <- function(x, nahead) {
  # fstl <- stlf(x, h=nahead, method="rwdrift", level=50)
  # fhw <- forecast(hw(x), h = nahead)
  fets <- forecast(ets(x), h = nahead)
  forecast_outflow <- fets$mean
  
  return(forecast_outflow)
}

seas_adj <- function(x) {
  combine_data <- list()
  #loop over data
  for (i in 1:ncol(x)) {
    STL <- stl(x[ ,i], s.window="periodic", robust=TRUE)
    seas_adj <- seasadj(STL)
    combine_data[[i]] <- seas_adj
  }
  names(combine_data) <- c("uk_100k", "uk_50k", "uk_20k", "uk_10k", "uk_5k",
                           "uk_2k", "uk_1k", "uk_0.5k", "uk_0.1k", "uk_u0.1k", "sum_uk", 
                           "ul_1k", "ul_0.5k", "ul_0.2k", "ul_0.1k", "ul_0.05k", "ul_0.025k",
                           "ul_0.01k", "ul_0.0.005k", "ul_u0.005k", "sum_ul", "sum_flow")
  combine_data <- data.frame(combine_data)
  return(combine_data)
}


## Loop over excel sheets
  ## harusnya tanpa export gambar
  data_excel <- list()
  setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/per kpw")
  sheet_list <- excel_sheets("outflow_kpw.xlsx") #diganti dulu 
  sheet_list <- sheet_list[2:length(sheet_list)]
  
  # Decomposition version 2
  for( i in sheet_list) {
    data <- data.frame(read_xlsx("outflow_kpw.xlsx", sheet=i))
    drops <- c("Bulan")
    data <- data.frame(data[ , !(names(data) %in% drops)] )
    data_ts <- ts(data, start=c(2010, 1), end=c(2021, 3), frequency=12)
    decomposition_result <- dekompos2(data_ts, 6)
    decomposition_result[decomposition_result<0] <- 0
    data_excel[[i]] <- decomposition_result
  }
  
  nasional <- data.frame(Reduce(f="+", x=data_excel, accumulate = FALSE))
  data_excel[["NAS"]] <- nasional

  # Exporting list into excel file
  write_xlsx(data_excel, "outflow_fcst_kpw.xlsx")
  
  # Seasonal Adjustment Only
  for( i in sheet_list) {
    data <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet=i))
    drops <- c("Bulan")
    data <- data.frame(data[ , !(names(data) %in% drops)] )
    data_ts <- ts(data, start=c(2010, 1), end=c(2021, 3), frequency=12)
    decomposition_result <- seas_adj(data_ts)
    decomposition_result[decomposition_result<0] <- 0
    data_excel[[i]] <- decomposition_result
  }
  
  nasional <- data.frame(Reduce(f="+", x=data_excel, accumulate = FALSE))
  data_excel[["NAS"]] <- nasional
  
  # Exporting list into excel file
  write_xlsx(data_excel, "inflow_seasADJ_kpw.xlsx")

  # Convert monthly data into quarterly data and forecast only outflow
  data_excel <- list()
  for( i in sheet_list) {
    data <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet=i))$inflow
    monthly <- ts(data, start=c(2010, 1), end=c(2021, 3), frequency=12)
    quarterly <- aggregate(monthly, nfrequency=4)
    decomposition_result <- dekompos3(quarterly, 11)
    decomposition_result[decomposition_result<0] <- 0
    data_excel[[i]] <- data.frame(decomposition_result)
    colnames(data_excel[[i]]) <-"inflow"
  }
  
  nasional <- data.frame(Reduce(f="+", x=data_excel, accumulate = FALSE))
  data_excel[["NAS"]] <- nasional
  
  raw_nasional <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet="NAS"))
  outflow_nasional <- data.frame(raw_nasional$inflow)
  outflow_nasional_ts <- ts(outflow_nasional, start=c(2010, 1), end=c(2021, 3), frequency=12)
  quarterly_on <- aggregate(outflow_nasional_ts, nfrequency=4)
  quarterly_on_df <- data.frame(quarterly_on)
  colnames(quarterly_on_df) <- "inflow"
  combine_outflow <- rbind(quarterly_on_df, data_excel[["NAS"]])
  combine_ts <- ts(combine_outflow, start=c(2010, 1), end=c(2023, 4), frequency=4)
  annually <- aggregate(combine_ts, nfrequency = 1)
  annually[12:14,1]/1000000
  
  # Bottom up monthly data inflow
  data_excel <- list()
  for( i in sheet_list) {
    data <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet=i))$inflow
    monthly <- ts(data, start=c(2010, 1), end=c(2021, 3), frequency=12)
    decomposition_result <- dekompos3(monthly, 35)
    decomposition_result[decomposition_result<0] <- 0
    data_excel[[i]] <- data.frame(decomposition_result)
    colnames(data_excel[[i]]) <-"inflow"
  }
  
  nasional <- data.frame(Reduce(f="+", x=data_excel, accumulate = FALSE))
  data_excel[["NAS"]] <- nasional
  
  raw_nasional <- data.frame(read_xlsx("inflow_kpw.xlsx", sheet="NAS"))
  outflow_nasional <- data.frame(raw_nasional$inflow)
  colnames(outflow_nasional) <- "inflow"
  combine_outflow <- rbind(outflow_nasional, data_excel[["NAS"]])
  combine_ts <- ts(combine_outflow, start=c(2010, 1), end=c(2023, 12), frequency=12)
  annually <- aggregate(combine_ts, nfrequency = 1)
  annually[12:14]/1000000
  
  # Bottom up monthly data outflow
  data_excel <- list()
  for( i in sheet_list) {
    data <- data.frame(read_xlsx("outflow_kpw.xlsx", sheet=i))$outflow
    monthly <- ts(data, start=c(2010, 1), end=c(2021, 3), frequency=12)
    decomposition_result <- dekompos4(monthly, 35)
    decomposition_result[decomposition_result<0] <- 0
    data_excel[[i]] <- data.frame(decomposition_result)
    colnames(data_excel[[i]]) <-"outflow"
  }
  
  nasional <- data.frame(Reduce(f="+", x=data_excel, accumulate = FALSE))
  data_excel[["NAS"]] <- nasional
  
  raw_nasional <- data.frame(read_xlsx("outflow_kpw.xlsx", sheet="NAS"))
  outflow_nasional <- data.frame(raw_nasional$outflow)
  colnames(outflow_nasional) <- "outflow"
  combine_outflow <- rbind(outflow_nasional, data_excel[["NAS"]])
  combine_ts <- ts(combine_outflow, start=c(2010, 1), end=c(2023, 12), frequency=12)
  annually <- aggregate(combine_ts, nfrequency = 1)
  annually[12:14]/1000000
  
  