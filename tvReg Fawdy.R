library(Matrix)
library(zoo)
library(tvReg)
library(vars)
library(stats)
library(readxl)
library(dplyr)
library(forecast)

set.seed(0101)

setwd("D:/Kerja/Asisten Akademik/BKF Kemenkeu")
bkf_raw <- read_excel("r_data_bkf.xlsx", sheet = "nominal")
bkf_raw.ts <-  ts(bkf_raw[,-1], start=c(2010, 1), end=c(2020, 12), frequency=12)
bkf_raw.ts <-  window(bkf_raw.ts, start=c(2011, 1), end=c(2020, 12), frequency=12)

pct <- function(x) {
  (x-lag(x, n=1))/lag(x, n=1)*100
}

logn <- function(x) {
  log(x)
}

int1 <- diff(bkf_raw$int1/100, lag=1)
int2 <- diff(bkf_raw$int2/100, lag=1)
int3 <- diff(bkf_raw$int3, lag=1)

int1_raw <- bkf_raw$int1
int2_raw <- bkf_raw$int2
int3_raw <- bkf_raw$int3

bkf.1 <- bkf_raw %>%
  mutate_each(funs(pct), c(gov, cons, gdp, cpi, exr, pegawai, barang, modal, sosial, total_spending))
ts.plot(bkf.1$cpi)

bkf <- cbind(bkf.1$barang, bkf.1$gdp, bkf.1$cpi, int3, bkf.1$exr)
colnames(bkf) <- c("gov", "gdp", "cpi", "int", "exr")
bkf.ts <- ts(bkf, start=c(2010, 1), end=c(2020, 12), frequency=12)
bkf.ts <- window(bkf.ts, start=c(2011, 1), end=c(2020, 12), frequency=12)

# Dates in string format
tml <- paste0(floor(time(bkf.ts)),"M", (1+12*( time(bkf.ts) - floor(time(bkf.ts)))))
class(bkf.ts) # just to make sure that the dataset is time series

head(bkf.ts, n=10)

# Use ts.plot
ts.plot(bkf.ts, col = 1:length(bkf.ts), xlab = "Monthly Data", ylab = "Percentage (%)", 
        main = "Fiscal Policy Data, 2011-2020")

# Add a legend to ts.plot
legend("topright", colnames(bkf.ts), lty = 1, col = 1:length(bkf.ts), bty = "n",
       cex=0.65)

tvVAR.fit <- tvVAR(bkf.ts, p=4, type = "const")

##Cumulative impulse response function
TVIRF1 <- tvIRF(tvVAR.fit, n.ahead = 25, cumulative = TRUE,
                impulse = "gov", response = "gov")

TVIRF2 <- tvIRF(tvVAR.fit, n.ahead = 25, cumulative = TRUE,
                impulse = "gov", response = "gdp")

irf1 <- data.frame(TVIRF1$irf$gov)
irf2 <- data.frame(TVIRF2$irf$gov)

irf1_num <- TVIRF1$irf$gov
irf2_num <- TVIRF2$irf$gov

# Plot IRF manually
periods <- seq(1:26)
mean_gov <- colMeans(irf1)
kelompok <- rep("mean", 26)
gov1 <- data.frame(cbind(mean_gov, periods, kelompok))
colnames(gov1) <- c("responses", "periods", "group")

mei_2020_gov <- irf1_num[112, 1,]
kelompok <- rep("Agustus 2020", 26)
gov2 <- data.frame(cbind(mei_2020_gov, periods, kelompok))
colnames(gov2) <- c("responses", "periods", "group")

october_2010_gov <- irf1_num[25, 1,]
kelompok <- rep("Mei 2013", 26)
gov3 <- data.frame(cbind(october_2010_gov, periods, kelompok))
colnames(gov3) <- c("responses", "periods", "group")

mean_gdp <- colMeans(irf2)
kelompok <- rep("mean", 26)
gdp1 <- data.frame(cbind(mean_gdp, periods, kelompok))
colnames(gdp1) <- c("responses", "periods", "group")

mei_2020_gdp <- irf2_num[112, 1,]
kelompok <- rep("Agustus 2020", 26)
gdp2 <- data.frame(cbind(mei_2020_gdp, periods, kelompok))
colnames(gdp2) <- c("responses", "periods", "group")

october_2010_gdp <- irf2_num[25, 1,]
kelompok <- rep("Mei 2013", 26)
gdp3 <- data.frame(cbind(october_2010_gdp, periods, kelompok))
colnames(gdp3) <- c("responses", "periods", "group")

  # GOV and GDP responses
  gov_responses <- bind_rows(list(gov1, gov2, gov3))
  gov_responses$responses <- as.numeric(gov_responses$responses)
  gov_responses$periods <- as.numeric(gov_responses$periods)

  gdp_responses <- bind_rows(list(gdp1, gdp2, gdp3))
  gdp_responses$responses <- as.numeric(gdp_responses$responses)
  gdp_responses$periods <- as.numeric(gdp_responses$periods)
  
  # Plot GOV and GDP responses
  ggplot(gov_responses, aes(x = periods, y = responses, group=group, col=group, fill=group)) + 
    geom_line() 

    # + scale_y_continuous(limits=c(-15, 100))
  
  ggplot(gdp_responses, aes(x = periods, y = responses, group=group, col=group, fill=group)) + 
    geom_line()
  
    # + scale_y_continuous(limits=c(-0.5, 1))
  
gdp <- bkf_raw.ts[, "gdp"]
g <- bkf_raw.ts[, "total_spending"]
conversion <- g/gdp
conversion_log <- log(g)/log(gdp)
conversion.log <- log(conversion)
ts.plot(conversion_log)
ts.plot(conversion)
ts.plot(gdp)

bkf_2 <- read_excel("r_data_bkf.xlsx", sheet = "seasonally adjusted")
bkf_2.ts <-  ts(bkf_2[,-1], start=c(2010, 1), end=c(2020, 12), frequency=12)
bkf_2.ts <-  window(bkf_2.ts, start=c(2011, 1), end=c(2020, 12), frequency=12)

gdp2 <- bkf_2.ts[,"gdp"]
g2 <- bkf_2.ts[,"barang"]
conversion2 <- g2/gdp2
ts.plot(conversion2)

# Multiplier for every t
multiplier_list <- list()
for (i in 1:25) {
  multiplier_i <- irf2[,i]/irf1[,i]
  multiplier_list[[i]] <- multiplier_i
}

ts.plot(multiplier_list[[24]])

timeline <- seq(2011.416,2021,1/12)
multiplier_fix <- list()
for (o in 1:25) {
  conversion_result <- multiplier_list[[o]]/conversion2[5:120]
  conversion_result_df <- data.frame(conversion_result)
  identifier <- data.frame(rep(o, length(timeline)))
  test_df <- cbind(timeline, identifier, conversion_result_df)
  colnames(test_df) <- c("timeline", "identifier", "multiplier")
  multiplier_fix[[o]] <- data.frame(test_df)
}
ts.plot(multiplier_fix[[12]]$multiplier)

estimating_multipliers <- function(belanja, belanja_plot) {
  bkf <- cbind(belanja, bkf.1$gdp, bkf.1$cpi, int3, bkf.1$exr)
  colnames(bkf) <- c("gov", "gdp", "cpi", "int", "exr")
  bkf.ts <- ts(bkf, start=c(2010, 1), end=c(2020, 12), frequency=12)
  bkf.ts <- window(bkf.ts, start=c(2011, 1), end=c(2020, 12), frequency=12)
  
  # Dates in string format
  tml <- paste0(floor(time(bkf.ts)),"M", (1+12*( time(bkf.ts) - floor(time(bkf.ts)))))
  
  tvVAR.fit <- tvVAR(bkf.ts, p=4, type = "const")
  
  ##Cumulative impulse response function
  TVIRF1 <- tvIRF(tvVAR.fit, n.ahead = 25, cumulative = TRUE,
                  impulse = "gov", response = "gov")
  TVIRF2 <- tvIRF(tvVAR.fit, n.ahead = 25, cumulative = TRUE,
                  impulse = "gov", response = "gdp")
  irf1 <- data.frame(TVIRF1$irf$gov)
  irf2 <- data.frame(TVIRF2$irf$gov)
  
  gdp <- bkf_raw.ts[, "gdp"]
  g <- belanja_plot
  conversion <- g/gdp
  conversion_log <- log(g)/log(gdp)
  
  bkf_2 <- read_excel("r_data_bkf.xlsx", sheet = "seasonally adjusted")
  bkf_2.ts <-  ts(bkf_2[,-1], start=c(2010, 1), end=c(2020, 12), frequency=12)
  bkf_2.ts <-  window(bkf_2.ts, start=c(2011, 1), end=c(2020, 12), frequency=12)
  
  gdp2 <- bkf_2.ts[,"gdp"]
  g2 <- belanja_plot
  conversion2 <- g2/gdp2
  
  # Multiplier for every t
  multiplier_list <- list()
  for (i in 1:25) {
    multiplier_i <- irf2[,i]/irf1[,i]
    multiplier_list[[i]] <- multiplier_i
  }
  
  timeline <- seq(2011.416,2021,1/12)
  multiplier_fix <- list()
  for (o in 1:25) {
    conversion_result <- multiplier_list[[o]]/conversion2[5:120]
    conversion_result_df <- data.frame(conversion_result)
    identifier <- data.frame(rep(o, length(timeline)))
    test_df <- cbind(timeline, identifier, conversion_result_df)
    colnames(test_df) <- c("timeline", "time horizon", "multiplier")
    multiplier_fix[[o]] <- data.frame(test_df)
  }
  return(multiplier_fix)
}

multipliers_total_spending <- estimating_multipliers(belanja = bkf.1$total_spending, 
                                                     belanja_plot = bkf_2.ts[, "total_spending"])
multipliers_pegawai <- estimating_multipliers(belanja = bkf.1$pegawai, 
                                              belanja_plot = bkf_2.ts[, "pegawai"])
multipliers_barang <- estimating_multipliers(belanja = bkf.1$barang, 
                                             belanja_plot = bkf_2.ts[, "barang"])
multipliers_modal <- estimating_multipliers(belanja = bkf.1$modal, 
                                            belanja_plot = bkf_2.ts[, "modal"])
multipliers_sosial <- estimating_multipliers(belanja = bkf.1$sosial, 
                                            belanja_plot = bkf_2.ts[, "sosial"])
# 3D Graph
rbind_multipliers <- bind_rows(multipliers_modal)
library(plotly)

data_test <- rbind_multipliers
fig <- plot_ly(data_test, x = ~timeline, y = ~time.horizon, z = ~multiplier,
               intensity = ~multiplier,
               type = "mesh3d", colors = colorRamp(rainbow(5)))
fig

#2D plots for every type of spending
library(ggplot2)
export_excel <- function(datax1) {
  rbind_multipliers <- bind_rows(datax1)
  data_test <- rbind_multipliers
  data_test$time.horizon <- as.factor(data_test$time.horizon)
  data_test <- data_test %>%
    filter(time.horizon==3 | time.horizon==6 | time.horizon==12)
  return(data_test)
}


total_spending <- export_excel(multipliers_total_spending)
pegawai <- export_excel(multipliers_pegawai)
barang <- export_excel(multipliers_barang)
modal <- export_excel(multipliers_modal)
sosial <- export_excel(multipliers_sosial)
export_excel_all <- list(total_spending, pegawai, barang, modal, sosial)

library(writexl)
write_xlsx(export_excel_all, "multipliers 2d.xlsx")

ggplot(total_spending, aes(x = timeline, y = multiplier, group=time.horizon, 
                           col=time.horizon, fill=time.horizon)) +
  geom_line()

ggplot(pegawai, aes(x = timeline, y = multiplier, group=time.horizon, 
                           col=time.horizon, fill=time.horizon)) +
  geom_line()

ggplot(barang, aes(x = timeline, y = multiplier, group=time.horizon, 
                           col=time.horizon, fill=time.horizon)) +
  geom_line()

ggplot(modal, aes(x = timeline, y = multiplier, group=time.horizon, 
                           col=time.horizon, fill=time.horizon)) +
  geom_line()

ggplot(sosial, aes(x = timeline, y = multiplier, group=time.horizon, 
                           col=time.horizon, fill=time.horizon)) +
  geom_line()


#2D plots for 12 months multipliers

twod_rbinds <- function(mulval, group_val) {
  group <- rep(group_val, 116)
  group.df <- data.frame(group)
  rbind_2d <- cbind(mulval, group.df)
  return(rbind_2d)
}

ts_2d <- twod_rbinds(multipliers_total_spending[[12]], "total spending")
pegawai_2d <- twod_rbinds(multipliers_pegawai[[12]], "pegawai")
barang_2d <- twod_rbinds(multipliers_barang[[12]], "barang")
modal_2d <- twod_rbinds(multipliers_modal[[12]], "modal")
sosial_2d <- twod_rbinds(multipliers_sosial[[12]], "sosial")
all_2d <- list(pegawai_2d, barang_2d, modal_2d)
all_2d_rbinds <- bind_rows(all_2d)

ggplot(all_2d_rbinds, aes(x = timeline, y = multiplier, group=group, col=group, fill=group)) +
  geom_line()

ggplot(sosial_2d, aes(x = timeline, y = multiplier)) +
  geom_line()



