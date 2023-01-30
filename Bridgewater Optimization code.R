library(quantmod)
library(ggplot2)
stock_1 <- getSymbols("PG",auto.assign = FALSE)
stock_2 <- getSymbols("JNJ",auto.assign = FALSE)
stock_3 <- getSymbols("PEP",auto.assign = FALSE)
stock_4 <- getSymbols("KO",auto.assign = FALSE)
stock_5 <- getSymbols("IEMG",auto.assign = FALSE)
stock_6 <- getSymbols("IVV",auto.assign = FALSE)
stock_7 <- getSymbols("COST",auto.assign = FALSE)
stock_8 <- getSymbols("VWO",auto.assign = FALSE)
stock_9 <- getSymbols("WMT",auto.assign = FALSE)
stock_10 <- getSymbols("SPY",auto.assign = FALSE)

#merging
joined_prices <- merge.xts(stock_1,stock_2,stock_3,stock_4,stock_5,stock_6,stock_7,stock_8,stock_9,stock_10)
joined_prices_only <- joined_prices[,seq(from = 6, to= 60, by = 6)]

#plot(joined_prices_only)
#chartSeries(stock_1['2022-09'])
#chartSeries(stock_2['2022-09'])
#chartSeries(fixed_income['2022-09'])

#Session 3 - fixing the issues with dates by ROR

##We are not using this dataframe
library(dplyr)
joined_returns<-as.data.frame(joined_prices_only) %>%
  mutate(PG_ROR = log(PG.Adjusted/lag(PG.Adjusted))) %>%
  mutate(JNJ_ROR = log(JNJ.Adjusted/lag(JNJ.Adjusted))) %>%
  mutate(PEP_ROR = log(PEP.Adjusted/lag(PEP.Adjusted))) %>%
  mutate(KO_ROR = log(KO.Adjusted/lag(KO.Adjusted))) %>%
  mutate(IEMG_ROR = log(IEMG.Adjusted/lag(IEMG.Adjusted))) %>%
  mutate(IVV_ROR = log(IVV.Adjusted/lag(IVV.Adjusted))) %>%
  mutate(COST_ROR = log(COST.Adjusted/lag(COST.Adjusted))) %>%
  mutate(VWO_ROR = log(VWO.Adjusted/lag(VWO.Adjusted))) %>%
  mutate(WMT_ROR = log(WMT.Adjusted/lag(WMT.Adjusted))) %>%
  mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted)))
tail(joined_returns)

## This dataframe will be used in the whole assignment
stock_1_returns <- monthlyReturn(getSymbols("PG",auto.assign = FALSE))
stock_2_returns <- monthlyReturn(getSymbols("JNJ",auto.assign = FALSE))
stock_3_returns <- monthlyReturn(getSymbols("PEP",auto.assign = FALSE))
stock_4_returns <- monthlyReturn(getSymbols("KO",auto.assign = FALSE))
stock_5_returns <- monthlyReturn(getSymbols("IEMG",auto.assign = FALSE))
stock_6_returns <- monthlyReturn(getSymbols("IVV",auto.assign = FALSE))
stock_7_returns <- monthlyReturn(getSymbols("COST",auto.assign = FALSE))
stock_8_returns <- monthlyReturn(getSymbols("VWO",auto.assign = FALSE))
stock_9_returns <- monthlyReturn(getSymbols("WMT",auto.assign = FALSE))
stock_10_returns <- monthlyReturn(getSymbols("SPY",auto.assign = FALSE))

#adding a benchmark to our joined_monthlyreturns
benchmark_returns <- monthlyReturn(getSymbols("^GSPC", auto.assign = FALSE))
benchmark_returns
joined_monthlyreturns <- merge.xts(stock_1_returns,stock_2_returns,stock_3_returns,stock_4_returns,
                                   stock_5_returns,stock_6_returns,stock_7_returns,stock_8_returns,
                                   stock_9_returns,stock_10_returns,benchmark_returns)
barplot(colMeans(joined_monthlyreturns),names.arg = col_names, main = "Mean monthly returns",col = c("#1b98e0"))
tail(joined_monthlyreturns)
#plot(joined_monthlyreturns)

## WEIGHTED PORTFOLIO
a <- c(4.23,3.9,3.32,3.25,3.24,3.19,2.87,2.73,2.65,2.65)
total <- sum(a)
b <- a/total
print(b)
b
## b is weights
joined_portfolio_ret <- as.data.frame(joined_monthlyreturns)%>%
  mutate(Portfolio = b[1]*monthly.returns+
           b[2]*monthly.returns.1+
           b[3]*monthly.returns.2+
           b[4]*monthly.returns.3+
           b[5]*monthly.returns.4+
           b[6]*monthly.returns.5+
           b[7]*monthly.returns.6+
           b[8]*monthly.returns.7+
           b[9]*monthly.returns.8+
           b[10]*monthly.returns.9
        )
tail(joined_portfolio_ret)
#plot(joined_portfolio_ret$Portfolio)
plot(na.omit(joined_portfolio_ret$Portfolio), type="l",main = "Montly Returns of the portfolio",ylab = "Monthly Returns",xlab = "Months")
returns <- na.omit(joined_portfolio_ret)

return_line <- ggplot(data = returns,aes(x= index(returns), y = Portfolio))
print(return_line)

## STANDARD DEVIATION/RISK OF ASSETS
time_index <- nrow(joined_monthlyreturns)

stock_1_sd <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])*sqrt(12)
stock_2_sd <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
stock_3_sd <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
stock_4_sd <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
stock_5_sd <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
stock_6_sd <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
stock_7_sd <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])*sqrt(12)
stock_8_sd <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])*sqrt(12)
stock_9_sd <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])*sqrt(12)
stock_10_sd <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])*sqrt(12)
portfolio_sd <- sd(joined_portfolio_ret$Portfolio[time_index:(time_index-11)])*sqrt(12)

sd_ = c(stock_1_sd,stock_2_sd,stock_3_sd,stock_4_sd,
                   stock_5_sd,stock_6_sd,stock_7_sd,stock_8_sd,
                   stock_9_sd,stock_10_sd,portfolio_sd)
sd_ = round(sd_,3)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
standard_deviation <- as.data.frame(sd_,row.names = col_names)
standard_deviation
barplot(standard_deviation$sd_,names.arg = col_names)

##TRACKING ERROR
benchmark_sd <- joined_monthlyreturns$monthly.returns.10[time_index:(time_index-11)]
stock_1_sd_te <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_2_sd_te <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_3_sd_te <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_4_sd_te <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_5_sd_te <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_6_sd_te <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_7_sd_te <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_8_sd_te <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_9_sd_te <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_10_sd_te <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
portfolio_sd_te <- sd(joined_portfolio_ret$Portfolio[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
portfolio_sd_te
t_e = c(stock_1_sd_te,stock_2_sd_te,stock_3_sd_te,stock_4_sd_te,
      stock_5_sd_te,stock_6_sd_te,stock_7_sd_te,stock_8_sd_te,
      stock_9_sd_te,stock_10_sd_te,portfolio_sd_te)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
tracking_error <- round(tracking_error,3)
tracking_error <- as.data.frame(t_e,row.names = col_names)
tracking_error
barplot(tracking_error$t_e,names.arg = col_names)

total <- tracking_error
total$s_d <- sd_

## SHARPE RATIO
riskfree <- 0.001
stock_1_sharpe <- (mean(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])-riskfree)/stock_1_sd
stock_2_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])-riskfree)/stock_2_sd
stock_3_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])-riskfree)/stock_3_sd
stock_4_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])-riskfree)/stock_4_sd
stock_5_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])-riskfree)/stock_5_sd
stock_6_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])-riskfree)/stock_6_sd
stock_7_sharpe <- (mean(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])-riskfree)/stock_7_sd
stock_8_sharpe <- (mean(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])-riskfree)/stock_8_sd
stock_9_sharpe <- (mean(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])-riskfree)/stock_9_sd
stock_10_sharpe <- (mean(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])-riskfree)/stock_10_sd
portfolio_sharpe <- (mean(joined_portfolio_ret$Portfolio[time_index:(time_index-11)]))/portfolio_sd
s_r <- c(stock_1_sharpe,stock_2_sharpe,stock_3_sharpe,stock_4_sharpe,stock_5_sharpe,stock_6_sharpe,stock_7_sharpe,
                  stock_8_sharpe,stock_9_sharpe,stock_10_sharpe,portfolio_sharpe)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
sharpe_ratio <- as.data.frame(s_r,row.names = col_names)
sharpe_ratio
barplot(sharpe_ratio$sharpe_ratio,names.arg = col_names)
total$sharpe_ratio <- s_r
total <- t(total)
total



##Barplot for Tracking Error, Standard Deviation and Sharpe Ratio
barplot(total,col = c("#1b98e0", "#353436", '#005366'),beside = TRUE,legend.text = c('Tracking Error','Standard Deviation','Sharpe Ratio'))
?barplot
###looking at diversification using correlation
named <- joined_monthlyreturns

colnames(named) <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","S&P500")
cor(named[time_index:(time_index-59)],use = 'complete.obs')
##this will make a plot of correlations
heatmap(cor(named[time_index:(time_index-59)],use = 'complete.obs'))
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(named)


#heatmap(cor(joined_monthlyreturns[time_index : (time_index-29)]))
##ploting ror
compare_ror <- ggplot(data=joined_monthlyreturns)+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns), color="blue")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.1), color="red")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.2), color="green4")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.3), color="orange")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.4), color="yellow")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.5), color="black")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.6), color="brown")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.7), color="pink")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.8), color="purple")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.9), color="grey")+
  geom_line(aes(x=index(joined_monthlyreturns), y=monthly.returns.10), color="blue4")
print(compare_ror)

library(plotly)
library(tseries)
ggplotly(compare_ror)


#################################################################
######### Using ADF to test for stationarity    #################
#################################################################
adf.test(joined_monthlyreturns$monthly.returns)
adf.test(joined_monthlyreturns$monthly.returns.1)
adf.test(joined_monthlyreturns$monthly.returns.2)
adf.test(joined_monthlyreturns$monthly.returns.3)
#adf.test(joined_monthlyreturns$monthly.returns.4) #have NA values
adf.test(joined_monthlyreturns$monthly.returns.5)
adf.test(joined_monthlyreturns$monthly.returns.6)
adf.test(joined_monthlyreturns$monthly.returns.7)
adf.test(joined_monthlyreturns$monthly.returns.8)
adf.test(joined_monthlyreturns$monthly.returns.9)
adf.test(joined_monthlyreturns$monthly.returns.10) #for the benchmark


##creating models
time_index <- nrow(joined_monthlyreturns)
last_12_months <- joined_monthlyreturns[time_index: (time_index-11),]

stock_1_reg <- lm(monthly.returns~monthly.returns.10,data = last_12_months)
summary(stock_1_reg)

stock_2_reg <- lm(monthly.returns.1~monthly.returns.10,data = last_12_months)
summary(stock_2_reg)

stock_3_reg <- lm(monthly.returns.2~monthly.returns.10,data = last_12_months)
summary(stock_3_reg)

stock_4_reg <- lm(monthly.returns.3~monthly.returns.10,data = last_12_months)
summary(stock_4_reg)

stock_5_reg <- lm(monthly.returns.4~monthly.returns.10,data = last_12_months)
summary(stock_5_reg)

stock_6_reg <- lm(monthly.returns.5~monthly.returns.10,data = last_12_months)
summary(stock_6_reg)

stock_7_reg <- lm(monthly.returns.6~monthly.returns.10,data = last_12_months)
summary(stock_7_reg)

stock_8_reg <- lm(monthly.returns.7~monthly.returns.10,data = last_12_months)
summary(stock_8_reg)

stock_9_reg <- lm(monthly.returns.8~monthly.returns.10,data = last_12_months)
summary(stock_9_reg)

stock_10_reg <- lm(monthly.returns.9~monthly.returns.10,data = last_12_months)
summary(stock_10_reg)




#####################################################
##############Portfolio Optimization#################

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, stock3_returns,
                                   stock4_returns, stock5_returns, stock6_returns, 
                                   stock7_returns, stock8_returns, stock9_returns, stock10_returns )

joined_monthlyreturns_ef <- joined_monthlyreturns

#adding a benchmark to our joined_monthlyreturns
benchmark_returns <- monthlyReturn(getSymbols("^GSPC", from="2013-01-01", auto.assign = FALSE))
benchmark_returns
joined_monthlyreturns <- merge.xts(stock1_returns,stock2_returns,stock3_returns,stock4_returns,
                                   stock5_returns,stock6_returns,stock7_returns,stock8_returns,
                                   stock9_returns,stock10_returns,benchmark_returns)
tail(joined_monthlyreturns)

#Adding portfolio returns
total <- 4.23+3.9+3.32+3.25+3.24+3.19+2.87+2.73+2.65+2.65

PG_alloc <- 4.23/total
JNJ_alloc <- 3.9/total
PEP_alloc <- 3.32/total
KO_alloc <- 3.25/total
IEMG_alloc <- 3.24/total
IVV_alloc <- 3.19/total
COST_alloc <- 2.87/total
VWO_alloc <- 2.73/total
WMT_alloc <- 2.65/total
SPY_alloc <- 2.65/total

#These have to add to 1
joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio=PG_alloc*monthly.returns+
           JNJ_alloc*monthly.returns.1+
           PEP_alloc*monthly.returns.2+
           KO_alloc*monthly.returns.3+
           IEMG_alloc*monthly.returns.4+
           IVV_alloc*monthly.returns.5+
           COST_alloc*monthly.returns.6+
           VWO_alloc*monthly.returns.7+
           WMT_alloc*monthly.returns.8+
           SPY_alloc*monthly.returns.9)

tail(joined_portfolio_ret)
mean(joined_portfolio_ret$portfolio)
## STANDARD DEVIATION/RISK OF ASSETS
time_index <- nrow(joined_monthlyreturns)

stock_1_sd <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])*sqrt(12)
stock_2_sd <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
stock_3_sd <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
stock_4_sd <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
stock_5_sd <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
stock_6_sd <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
stock_7_sd <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])*sqrt(12)
stock_8_sd <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])*sqrt(12)
stock_9_sd <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])*sqrt(12)
stock_10_sd <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])*sqrt(12)
portfolio_sd <- sd(joined_portfolio_ret$portfolio[time_index:(time_index-11)])*sqrt(12)

standard_deviation = c(stock_1_sd,stock_2_sd,stock_3_sd,stock_4_sd,
                       stock_5_sd,stock_6_sd,stock_7_sd,stock_8_sd,
                       stock_9_sd,stock_10_sd,portfolio_sd)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
standard_deviation <- as.data.frame(standard_deviation,row.names = col_names)
standard_deviation


##TRACKING ERROR
benchmark_sd <- joined_monthlyreturns$monthly.returns.10[time_index:(time_index-11)]
stock_1_sd_te <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_2_sd_te <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_3_sd_te <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_4_sd_te <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_5_sd_te <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_6_sd_te <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_7_sd_te <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_8_sd_te <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_9_sd_te <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_10_sd_te <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
portfolio_sd_te <- sd(joined_portfolio_ret$portfolio[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)

tracking_error = c(stock_1_sd_te,stock_2_sd_te,stock_3_sd_te,stock_4_sd_te,
                   stock_5_sd_te,stock_6_sd_te,stock_7_sd_te,stock_8_sd_te,
                   stock_9_sd_te,stock_10_sd_te,portfolio_sd_te)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
tracking_error <- as.data.frame(tracking_error,row.names = col_names)
tracking_error


## SHARPE RATIO
riskfree <- 0.001
stock_1_sharpe <- (mean(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])-riskfree)/stock_1_sd
stock_2_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])-riskfree)/stock_2_sd
stock_3_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])-riskfree)/stock_3_sd
stock_4_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])-riskfree)/stock_4_sd
stock_5_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])-riskfree)/stock_5_sd
stock_6_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])-riskfree)/stock_6_sd
stock_7_sharpe <- (mean(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])-riskfree)/stock_7_sd
stock_8_sharpe <- (mean(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])-riskfree)/stock_8_sd
stock_9_sharpe <- (mean(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])-riskfree)/stock_9_sd
stock_10_sharpe <- (mean(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])-riskfree)/stock_10_sd
portfolio_sharpe <- (mean(joined_portfolio_ret$portfolio[time_index:(time_index-11)]))/portfolio_sd
sharpe_ratio <- c(stock_1_sharpe,stock_2_sharpe,stock_3_sharpe,stock_4_sharpe,stock_5_sharpe,stock_6_sharpe,stock_7_sharpe,
                  stock_8_sharpe,stock_9_sharpe,stock_10_sharpe,portfolio_sharpe)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
sharpe_ratio <- as.data.frame(sharpe_ratio,row.names = col_names)
sharpe_ratio

###looking at diversification using correlation
named <- joined_monthlyreturns
colnames(named) <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","S&P500")
cor(named[time_index:(time_index-59)],use = 'complete.obs')

#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

# construct the data
asset.names = c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY")
er_0 <- mean(joined_monthlyreturns_ef$monthly.returns)
er_1 <- mean(joined_monthlyreturns_ef$monthly.returns.1)
er_2 <- mean(joined_monthlyreturns_ef$monthly.returns.2)
er_3 <- mean(joined_monthlyreturns_ef$monthly.returns.3)
er_4 <- mean(joined_monthlyreturns_ef$monthly.returns.4)
er_5 <- mean(joined_monthlyreturns_ef$monthly.returns.5)
er_6 <- mean(joined_monthlyreturns_ef$monthly.returns.6)
er_7 <- mean(joined_monthlyreturns_ef$monthly.returns.7)
er_8 <- mean(joined_monthlyreturns_ef$monthly.returns.8)
er_9 <- mean(joined_monthlyreturns_ef$monthly.returns.9)
cov(joined_monthlyreturns_ef)
er = c(er_0, er_1, er_2, er_3, er_4, er_5, er_6, er_7, er_8, er_9)
names(er) = asset.names
covmat = cov(joined_monthlyreturns_ef)
r.free = 0.001
dimnames(covmat) = list(asset.names, asset.names)

# tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free, shorts = FALSE)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)

# compute portfolio frontier
ef <- efficient.frontier(er, covmat, alpha.min=-2,
                         alpha.max=1.5, nport=20, shorts = FALSE)
attributes(ef)

plot(ef)
plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)
sr.tan = (tan.port$er - r.free)/tan.port$sd
abline(a=r.free, b=sr.tan, col="green", lwd=2)

#New Weights Portfolio Calculations
#Adding portfolio returns

PG_alloc <- 0.040468
JNJ_alloc <- 0.29354
PEP_alloc <- 0.03856
KO_alloc <- 0
IEMG_alloc <- 0
IVV_alloc <- 0
COST_alloc <- 0.550885
VWO_alloc <- 0
WMT_alloc <- 0
SPY_alloc <- 0.076546

#These have to add to 1
joined_portfolio_ret_new <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio=PG_alloc*monthly.returns+
           JNJ_alloc*monthly.returns.1+
           PEP_alloc*monthly.returns.2+
           KO_alloc*monthly.returns.3+
           IEMG_alloc*monthly.returns.4+
           IVV_alloc*monthly.returns.5+
           COST_alloc*monthly.returns.6+
           VWO_alloc*monthly.returns.7+
           WMT_alloc*monthly.returns.8+
           SPY_alloc*monthly.returns.9)
tail(joined_portfolio_ret_new)

## STANDARD DEVIATION/RISK OF ASSETS
time_index <- nrow(joined_monthlyreturns)

stock_1_sd <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])*sqrt(12)
stock_2_sd <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)
stock_3_sd <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)
stock_4_sd <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)
stock_5_sd <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])*sqrt(12)
stock_6_sd <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])*sqrt(12)
stock_7_sd <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])*sqrt(12)
stock_8_sd <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])*sqrt(12)
stock_9_sd <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])*sqrt(12)
stock_10_sd <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])*sqrt(12)
portfolio_sd <- sd(joined_portfolio_ret_new$portfolio[time_index:(time_index-11)])*sqrt(12)

standard_deviation_new = c(stock_1_sd,stock_2_sd,stock_3_sd,stock_4_sd,
                           stock_5_sd,stock_6_sd,stock_7_sd,stock_8_sd,
                           stock_9_sd,stock_10_sd,portfolio_sd)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
standard_deviation_new <- as.data.frame(standard_deviation_new,row.names = col_names)
standard_deviation_new


##TRACKING ERROR
benchmark_sd <- joined_monthlyreturns$monthly.returns.10[time_index:(time_index-11)]
stock_1_sd_te <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_2_sd_te <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_3_sd_te <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_4_sd_te <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_5_sd_te <- sd(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_6_sd_te <- sd(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_7_sd_te <- sd(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_8_sd_te <- sd(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_9_sd_te <- sd(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
stock_10_sd_te <- sd(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)
portfolio_sd_te <- sd(joined_portfolio_ret_new$portfolio[time_index:(time_index-11)]-benchmark_sd)*sqrt(12)

tracking_error_new = c(stock_1_sd_te,stock_2_sd_te,stock_3_sd_te,stock_4_sd_te,
                       stock_5_sd_te,stock_6_sd_te,stock_7_sd_te,stock_8_sd_te,
                       stock_9_sd_te,stock_10_sd_te,portfolio_sd_te)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
tracking_error_new <- as.data.frame(tracking_error_new,row.names = col_names)
tracking_error_new


## SHARPE RATIO
riskfree <- 0.001
stock_1_sharpe <- (mean(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])-riskfree)/stock_1_sd
stock_2_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])-riskfree)/stock_2_sd
stock_3_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])-riskfree)/stock_3_sd
stock_4_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])-riskfree)/stock_4_sd
stock_5_sharpe <- (mean(joined_monthlyreturns$monthly.returns.4[time_index:(time_index-11)])-riskfree)/stock_5_sd
stock_6_sharpe <- (mean(joined_monthlyreturns$monthly.returns.5[time_index:(time_index-11)])-riskfree)/stock_6_sd
stock_7_sharpe <- (mean(joined_monthlyreturns$monthly.returns.6[time_index:(time_index-11)])-riskfree)/stock_7_sd
stock_8_sharpe <- (mean(joined_monthlyreturns$monthly.returns.7[time_index:(time_index-11)])-riskfree)/stock_8_sd
stock_9_sharpe <- (mean(joined_monthlyreturns$monthly.returns.8[time_index:(time_index-11)])-riskfree)/stock_9_sd
stock_10_sharpe <- (mean(joined_monthlyreturns$monthly.returns.9[time_index:(time_index-11)])-riskfree)/stock_10_sd
portfolio_sharpe <- (mean(joined_portfolio_ret_new$portfolio[time_index:(time_index-11)]))/portfolio_sd
sharpe_ratio_new <- c(stock_1_sharpe,stock_2_sharpe,stock_3_sharpe,stock_4_sharpe,stock_5_sharpe,stock_6_sharpe,stock_7_sharpe,
                      stock_8_sharpe,stock_9_sharpe,stock_10_sharpe,portfolio_sharpe)
col_names <- c("PG","JNJ","PEP","KO","IEMG","IVV","COST","VWO","WMT","SPY","Portfolio")
sharpe_ratio_new <- as.data.frame(sharpe_ratio_new,row.names = col_names)
sharpe_ratio_new

final <- tracking_error_new
final$s_d <- standard_deviation_new
final$sharpe_ratio <- sharpe_ratio_new
final <- t(final)
final
barplot(final,col = c("#1b98e0", "#353436", '#005366'),beside =
          TRUE,legend.text = c('Standard Deviation','Tracking Error','Sharpe Ratio'),main = "New Optimized Portfolio")


final[1:3,11]
total[1:3,11]
a 



