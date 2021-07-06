library(ggplot2)

t_adf <- c(1:10000)

for (n in 1: 10000) {
  et <- rnorm(100)
  yrn <- rnorm(100)
  y <- yrn[100/2]
    for (i in 1:100) {
        y[i+1] <- y[i]+et[i+1] 
    }
  yl1 <- lag(y, n=1L)
  yl1 <- c(yl1[2:101])
  ydiff <- diff(y)
  reg <- lm(formula= ydiff ~ yl1)
  sumd_ydiff <- sum((reg$fitted.values-ydiff)^2, na.rm = TRUE)
  dev_yl1 <- (yl1-mean(yl1, na.rm = TRUE))^2
  sumd_yl1 <- sum(dev_yl1, na.rm = TRUE)
  coeff <- reg$coefficients[2]
  n_obs <- length(ydiff)-2
  se <- sqrt((sumd_ydiff/n_obs)/sumd_yl1)
  t_val <- coeff/se
  t_adf[n] <- t_val
}
t_adf <- data.frame(t_adf)

ggplot(t_adf, aes(x=t_adf)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
