library(readxl)
library(zoo)
library(forecast)
library(bsts)
library(repr)
library(urca)
library(strucchange)
library(sandwich)
library(lmtest)
library(vars)
library(writexl)
library(ggplot2)

setwd("D:/Kerja/Asisten Akademik/Riset Bank Indonesia/Analisis")
dir()

MD <- read_excel("Rdata.xlsx")
head(MD, n=10)
money_demand <- ts(MD, start=c(2007, 1), end=c(2020, 2), frequency=4)
money_demand <- window(money_demand[,-1], start=c(2007, 2), end=c(2019, 4), frequency=4)

# Decompose the time series 
outflow_STL <- stl(money_demand[,"Outflows"], s.window="periodic", robust=TRUE)
inflow_STL <- stl(money_demand[,"Inflows"], s.window="periodic", robust=TRUE)
cob_STL <- stl(money_demand[,"COBS"], s.window="periodic", robust=TRUE)
civs_STL <- stl(money_demand[,"Civs"], s.window="periodic", robust=TRUE)
cc_STL <- stl(money_demand[,"CC"], s.window="periodic", robust=TRUE)
atm_deb_STL <- stl(money_demand[,"Atm_deb"], s.window="periodic", robust=TRUE)
emon_STL <- stl(money_demand[,"Emon"], s.window="periodic", robust=TRUE)

plot(outflow_STL, main="Outflow Decomposition")
dev.copy(png,"Outflow Decomposition.png")
dev.off()
plot(inflow_STL, main="Inflow Decomposition")
dev.copy(png,"Inflow Decomposition.png")
dev.off()

outflow_stl <- seasadj(outflow_STL)
seas_outflow <- seasonal(outflow_STL)
inflow_stl <- seasadj(inflow_STL)
seas_inflow <- seasonal(inflow_STL)
civs_stl <- seasadj(civs_STL)
atm_deb_stl <- seasadj(atm_deb_STL)
cc_stl <- seasadj(cc_STL)
emon_stl <- seasadj(emon_STL)
cob_stl <- seasadj(cob_STL)

#Extrapolating data seasonal component
seas_outflow_fcst <- stlf(seas_outflow, h=16, method="arima")
seas_inflow_fcst <- stlf(seas_inflow, h=16, method="arima")
seas_inout.df <- data.frame(seas_outflow_fcst, seas_inflow_fcst)

plot(seas_inflow_fcst, main="Inflow Seasonal Component+Forecast")
dev.copy(png,"Inflow Seasonal Component.png")
dev.off()

plot(seas_outflow_fcst, main="Outlow Seasonal Component+Forecast")
dev.copy(png,"Outlow Seasonal Component.png")
dev.off()

#Extract data NT(Non Tunai) STL
nt.df <- data.frame(atm_deb_stl, cc_stl, emon_stl)
write_xlsx(nt.df, "non tunai decompos.xlsx")

#Extract data outflow inflow cob STL
stl.list <- list(outflow_stl, inflow_stl, cob_stl)
names(stl.list) <- c("outflow_stl", "inflow_stl", "cob_stl")
stl.df <- as.data.frame(stl.list)
write_xlsx(stl.df, "stl_decompos.xlsx")

#Outflow and Inflow Riil
outflowr <- outflow_stl/money_demand[,"CPI"]
inflowr <- inflow_stl/money_demand[,"CPI"]
civsr <- civs_stl/money_demand[,"CPI"]

#transform into log
loutflowr <- log(outflowr)
linflowr <- log(inflowr)
lcpi <- log(money_demand[,"CPI"])
lexrl <- log(money_demand[,"Exrl"])
depl <- money_demand[,"Depl"]
lcc <- log(cc_stl)
lemon <- na.approx(log(emon_stl))
latm <- log(atm_deb_stl)
lcivs <- log(civsr)
lgdp <- log(money_demand[,"GDP"])

plot(loutflowr)
plot(linflowr)

data.df <- data.frame(cbind(loutflowr, linflowr, lcpi, lexrl, 
                  depl, lcc, lemon, latm, lcivs, lgdp))
names(data.df) <- c("log.outflowr", "log.inflowr", "log.cpi","log.exrl", "depl","log.cc",
                      "log.emon","log.atm", "lcvis", "log.gdp")
waktu <- c(1:length(data.df$log.outflowr))
plot(waktu, data.df$log.outflowr)

#MODEL OUTFLOW ATM-Debet    
    #Cointegration Test
    outflow_atm <- cbind(loutflowr,lgdp, lexrl, depl, latm)
    colnames(outflow_atm) <- c("log.outflowr","log.gdp", "log.exrl", "depl", "log.atm")
    outflow_atm <- na.trim(outflow_atm)

    #Best lag
    outflow_atm.VAR.IC <- VARselect(outflow_atm,lag.max = 4, type="const")
    nlags <- outflow_atm.VAR.IC$selection["HQ(n)"]
    nlags

    #perform cointegrating test
    outflow_atm <- window(outflow_atm, start=c(2007, 2), end=c(2019, 4), frequency=4)
    outflow_atm.CA <- ca.jo(outflow_atm, ecdet="trend", type="trace", 
                            K=nlags, spec="transitory")
    summary(outflow_atm.CA)
    outflow_atm.CA <- ca.jo(outflow_atm, ecdet="trend", type="eigen", 
                            K=nlags, spec="transitory")
    summary(outflow_atm.CA)

    # estimate unrestriced VEC model
    outflow_atm.VEC <- cajorls(outflow_atm.CA, r=4)
    outflow_atm.VEC

    #test coefficient alpha
    summary(outflow_atm.VEC$rlm)

    outflow_atm.VAR <- vec2var(outflow_atm.CA, r=4)

    # forecast using VAR in levels
    outflow_atm.VAR.fcst <- predict(outflow_atm.VAR, n.ahead=16, ci=0.95)
    par( mar=c(4,4,2,1), cex=0.75)

    plot(outflow_atm.VAR.fcst)
    dev.copy(png,"Outflow ATM-Debet Forecast.png")
    dev.off()

    #Extract antilog dan dikalikan dengan ARIMA CPI
    out_atm_fcst <- outflow_atm.VAR.fcst$fcst
    outflow_atm_fit <- out_atm_fcst$log.outflowr
    out_atm <- data.frame(exp(outflow_atm_fit[,-4]))
    cpi_arima <- auto.arima(money_demand[,"CPI"])
    summary(cpi_arima)
    cpi_fcst <- forecast(cpi_arima, h=16)
    plot(cpi_fcst)
    cpi.arima.fcst <- data.frame(cbind(cpi_fcst$mean, cpi_fcst$lower[,"95%"], 
                                       cpi_fcst$upper[,"95%"]))
    
    cpi_stlf <- stlf(money_demand[,"CPI"], method="arima", h=16)
    cpi.stlf.fcst <- data.frame(cbind(cpi_fcst$mean, cpi_fcst$lower[,"95%"], 
                                      cpi_fcst$upper[,"95%"]))
    
    #Forecast Outflow riil * CPI + Seasonal Component
    outflow_atm_fix <- data.frame((out_atm*cpi.arima.fcst)+seas_inout.df$Point.Forecast)
    outflow_atm_fix <- ts(outflow_atm_fix, start=c(2020,1), end=c(2023,4), frequency = 4)
    outflow_atm_fix.an <- data.frame(aggregate(outflow_atm_fix, nfrequency = 1))
    write_xlsx(outflow_atm_fix.an, "outflow atm forecast annualy.xlsx")
    outflow_atm_fix.df <- data.frame(outflow_atm_fix)
    write_xlsx(outflow_atm_fix.df, "outflow atm forecast quarterly.xlsx")

#MODEL INFLOW Baseline
    #Cointegration Test
    inflow_b <- cbind(linflowr, lcivs, latm)
    colnames(inflow_b) <- c("log.inflowr","log.civ", "log.atm")
    
    #Best lag
    inflow_b.VAR.IC <- VARselect(inflow_b,lag.max = 4, type="const")
    nlags <- inflow_b.VAR.IC$selection["HQ(n)"]
    nlags
    
    #perform cointegrating test
    inflow_b <- window(inflow_b, start=c(2007, 2), end=c(2019, 4), frequency=4)
    inflow_b.CA <- ca.jo(inflow_b, ecdet="trend", type="trace", K=nlags, spec="transitory")
    summary(inflow_b.CA)
    inflow_b.CA <- ca.jo(inflow_b, ecdet="trend", type="eigen", K=nlags, spec="transitory")
    summary(inflow_b.CA)
    
    inflow_b.VAR <- vec2var(inflow_b.CA, r=2)
    
    # forecast using VAR in levels
    inflow_b.VAR.fcst <- predict(inflow_b.VAR, n.ahead=16, ci=0.95)
    par( mar=c(4,4,2,1), cex=0.75)
    
    plot(inflow_b.VAR.fcst)
    dev.copy(png,"Inflow Baseline Forecast.png")
    dev.off()
    
    #Extract antilog and multiplied by ARIMA CPI
    in_b_fcst <- inflow_b.VAR.fcst$fcst
    inflow_b_fit <- in_b_fcst$log.inflowr
    in_b <- data.frame(exp(inflow_b_fit[,-4]))
    
    #Forecast Outflow riil * CPI + Seasonal Component
    inflow_b_fix <- data.frame((in_b*cpi.arima.fcst)+seas_inout.df$Point.Forecast.1)
    inflow_b_fix <- ts(inflow_b_fix, start=c(2020,1), end=c(2023,4), frequency = 4)
    inflow_b_fix.an <- data.frame(aggregate(inflow_b_fix, nfrequency = 1))
    write_xlsx(inflow_b_fix.an, "inflow forecast annualy.xlsx")
    inflow_b_fix.df <- data.frame(inflow_b_fix)
    write_xlsx(inflow_b_fix.df, "inflow forecast quarterly.xlsx")

# Creating a table
    tabel_inflow <- data.frame(cbind(c(2020, 2021, 2022, 2023), inflow_b_fix.an[,1]/1000, 
                    c(745.6, 793.6, 839.2, 888.6), c(784.8, 835.4, 883.4, 935.3), 
                    c(824.1, 877.1, 927.6, 982.1)))
    colnames(tabel_inflow) <- c("Tahun", "Inflow", "Batas Bawah DKEM", "Nilai Tengah DKEM",
                                "Batas Atas DKEM")
    
    tabel_outflow <- data.frame(cbind(c(2020, 2021, 2022, 2023), outflow_atm_fix.an[,1]/1000, 
                                      c(796.9, 846, 839.9, 946.4), c(838.9, 890.5, 941, 996.2), 
                                      c(880.8, 935, 988, 1046.1)))
    colnames(tabel_outflow) <- c("Tahun", "Outflow", "Batas Bawah DKEM", "Nilai Tengah DKEM",
                                "Batas Atas DKEM")
    netflow <- data.frame((outflow_atm_fix.an/1000)-(inflow_b_fix.an/1000))
    
    tabel_netflow <- data.frame(cbind(c(2020, 2021, 2022, 2023), netflow[,1], 
                                      c(51.3, 52.4, 54.7, 57.9), c(54, 55.1, 57.6, 60.96), 
                                      c(56.7, 57.9, 60.4, 64)))
    colnames(tabel_netflow) <- c("Tahun", "Netflow", "Batas Bawah DKEM", "Nilai Tengah DKEM",
                                 "Batas Atas DKEM")
    tabel_inflow
    tabel_outflow
    tabel_netflow
    
# Forecast univariate
    ## Outflow
    farima <- function(x, h) {
        forecast(auto.arima(x), h=h)
    }
    
    outflow_self <- ts(money_demand[, "Outflows"], start = c(2007,2), 
                       end = c(2019,4), frequency = 4)
    loutflow_self <- na.approx(log(outflow_self))
    
    e1 <- tsCV(loutflow_self, stlf, method="arima", h=4)
    e2 <- tsCV(loutflow_self, stlf, method="ets", h=4)
    e3 <- tsCV(loutflow_self, stlf, method="rwdrift", h=4)
    e4 <- tsCV(loutflow_self, farima, h=4)
    
    #RMSE
    sqrt(mean(e1^2, na.rm = TRUE))
    sqrt(mean(e2^2, na.rm = TRUE))
    sqrt(mean(e3^2, na.rm = TRUE))
    sqrt(mean(e4^2, na.rm = TRUE))
    
    ## Inflow
    inflow_self <- ts(money_demand[, "Inflows"], start = c(2007,2), 
                      end = c(2019,4), frequency = 4)
    linflow_self <- na.approx(log(inflow_self))
    
    e5 <- tsCV(linflow_self, stlf, method="arima", h=4)
    e6 <- tsCV(linflow_self, stlf, method="ets", h=4)
    e7 <- tsCV(linflow_self, stlf, method="rwdrift", h=4)
    e8 <- tsCV(linflow_self, farima, h=4)
    
    #RMSE
    sqrt(mean(e5^2, na.rm = TRUE))
    sqrt(mean(e6^2, na.rm = TRUE))
    sqrt(mean(e7^2, na.rm = TRUE))
    sqrt(mean(e8^2, na.rm = TRUE))
    
    
    #MODEL Outflow EMON
    #Cointegration Test
    lemon[is.na(lemon)] <- 0
    outflow_emon <- cbind(loutflowr,lgdp, lexrl, depl, lemon)
    colnames(outflow_emon) <- c("log.outflowr","log.gdp", "log.exrl", "depl", "log.emon")
    outflow_emon <- na.trim(outflow_emon)
    
    #Best lag
    outflow_emon.VAR.IC <- VARselect(outflow_emon,lag.max = 4, type="const")
    nlags <- outflow_emon.VAR.IC$selection["HQ(n)"]
    nlags
    
    #perform cointegrating test
    outflow_emon <- window(outflow_emon, start=c(2007, 2), end=c(2019, 4), frequency=4)
    outflow_emon.CA <- ca.jo(outflow_emon, ecdet="const", type="trace", K=nlags, spec="transitory")
    summary(outflow_emon.CA)
    outflow_emon.CA <- ca.jo(outflow_emon, ecdet="const", type="eigen", K=nlags, spec="transitory")
    summary(outflow_emon.CA)
    
    # estimate unrestriced VEC model
    outflow_emon.VEC <- cajorls(outflow_emon.CA, r=4)
    outflow_emon.VEC
    
    #test coefficient alpha
    summary(outflow_emon.VEC$rlm)
    
    outflow_emon.VAR <- vec2var(outflow_emon.CA, r=4)
    
    # forecast using VAR in levels
    outflow_emon.VAR.fcst <- predict(outflow_emon.VAR, n.ahead=16, ci=0.95)
    par( mar=c(4,4,2,1), cex=0.75)
    
    plot(outflow_emon.VAR.fcst)
    
    #Extract antilog dan dikalikan dengan ARIMA CPI
    out_emon_fcst <- outflow_emon.VAR.fcst$fcst
    outflow_emon_fit <- out_emon_fcst$log.outflowr
    out_emon <- exp(outflow_emon_fit)
    
    #Forecast Outflow riil * CPI
    outflow_emon_fix <- as.data.frame(out_emon*cpi_fcst) 
    write_xlsx(outflow_emon_fix, "outflow_emon_forecast.xlsx")
    
    #MODEL Outflow CC
    #Cointegration Test
    outflow_cc <- cbind(loutflowr,lgdp, lexrl, depl, lcc)
    colnames(outflow_cc) <- c("log.outflowr","log.gdp", "log.exrl", "depl", "log.cc")
    outflow_cc <- na.trim(outflow_cc)
    
    #Best lag
    outflow_cc.VAR.IC <- VARselect(outflow_cc,lag.max = 4, type="const")
    nlags <- outflow_cc.VAR.IC$selection["HQ(n)"]
    nlags
    
    #perform cointegrating test
    outflow_cc <- window(outflow_cc, start=c(2007, 2), end=c(2019, 4), frequency=4)
    outflow_cc.CA <- ca.jo(outflow_cc, ecdet="const", type="trace", K=nlags, spec="transitory")
    summary(outflow_cc.CA)
    outflow_cc.CA <- ca.jo(outflow_cc, ecdet="const", type="eigen", K=nlags, spec="transitory")
    summary(outflow_cc.CA)
    
    # estimate unrestriced VEC model
    outflow_cc.VEC <- cajorls(outflow_cc.CA, r=4)
    outflow_cc.VEC
    
    #test coefficient alpha
    summary(outflow_cc.VEC$rlm)
    
    outflow_cc.VAR <- vec2var(outflow_cc.CA, r=4)
    
    # forecast using VAR in levels
    outflow_cc.VAR.fcst <- predict(outflow_cc.VAR, n.ahead=16, ci=0.95)
    par( mar=c(4,4,2,1), cex=0.75)
    
    plot(outflow_cc.VAR.fcst)
    
    #Extract antilog dan dikalikan dengan ARIMA CPI
    out_cc_fcst <- outflow_cc.VAR.fcst$fcst
    outflow_cc_fit <- out_cc_fcst$log.outflowr
    out_cc <- exp(outflow_cc_fit)
    
    #Forecast Outflow riil * CPI
    outflow_cc_fix <- as.data.frame(out_cc*cpi_fcst) 
    write_xlsx(outflow_cc_fix, "outflow_cc_forecast.xlsx")
    