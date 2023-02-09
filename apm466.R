install.packages("jrvFinance")
library("jrvFinance")
install.packages("ggplot2")
library(ggplot2)

library(readxl)
bond <- read_excel("/Users/tommyzhang/Downloads/APM466 A1 data (1).xlsx", sheet=3)
View(bond)

coupon <- bond$Coupon
maturity_date <- bond$`Maturity Date`
price_date <- c("2023/1/16", "2023/1/17", "2023/1/18", "2023/1/19", "2023/1/20", "2023/1/23", "2023/1/24", "2023/1/25", "2023/1/26", "2023/1/27")
clean_price_matrix = matrix(c(bond$"44942", bond$"44943", bond$"44944", bond$"44945", bond$"44946", bond$"44949", bond$"44950", bond$"44951", bond$"44952", bond$"44953"), nrow=10,ncol = 10)


# Q4 bonds’ yield (YTM)
# matrix where rows represent ytm of each bond and columns represent each bond

YTM_matrix = matrix('numeric', nrow=10, ncol=10)
for(j in c(1:10)){
  close_price = clean_price_matrix[,j]
  for(i in c(1:10)){
    YTM_matrix[i,j] <- bond.yield(price_date[i], maturity_date[j], coupon[j], freq=2, clean_price_matrix[i,j])
  }
}
View(YTM_matrix)

#plot YTM curve
YTM_matrix<-as.data.frame(YTM_matrix)
year_digit <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(year_digit, YTM_matrix[1,], type="o", main="Yield Curve", xlab = "Maturity year", ylab = "YTM", lwd=1.0)
color=c(1:10)
for (i in c(2:10)) {
  lines(year_digit, YTM_matrix[i,], type = "o", col=color[i-1])
}
legend("topright",legend = c('2023-01-16','2023-01-17','2023-01-18','2023-01-19','2023-01-20','2023-01-23','2023-01-24','2023-01-25','2023-01-26','2023-01-27'), col=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), lty=1, cex=0.5)

#interpolation technique

## spot rate
# dirty price
dirty_matrix <- matrix("numeric",nrow = 10, ncol = 10)
for (i in 1:10){
  for (j in 1:10) {
    dirty_matrix[i,j] <- bond.TCF(price_date[i],
                                        maturity_date[j],
                                        coupon[j],
                                        freq = 2,
                                        convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                        redemption_value = 100)$accrued + clean_price_matrix[i,j]}}
View(dirty_matrix)

# cash flow
cash_flow <- list()
for (i in 1:10){
  cash_flow =  bond.TCF(price_date[i],
                        maturity_date[i],
                        coupon[i],
                        freq = 2,
                        convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                        redemption_value = 100)$cf
  print(cash_flow)
  assign(paste0("cash_flow_",i), cash_flow)
}




year_frac_matrix <- matrix("numeric",nrow = 10, ncol = 10)
for ( i in c(1:10)){
  for (j in c(1:10)){
    year_frac_matrix[i,j] = yearFraction(price_date[i], maturity_date[j], freq = 2,
                                         convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E")
    )
  }}


spot_rate_matrix = matrix(nrow=10,ncol=10)
for (i in 1:10){
  t_1 <- as.numeric(year_frac_matrix[i,1])
  sf_1 <- function(x) as.numeric(dirty_matrix[i,1])-cash_flow_1[1]*(1+x/2)^(-2*t_1)
  s_1 <- uniroot(sf_1,c(0,1))$root
  
  t_2 <- as.numeric(year_frac_matrix[i,2])
  sf_2 <- function(x) as.numeric(dirty_matrix[i,2])-cash_flow_2[1]*(1+s_1/2)^(-2*(t_2-0.5*1))-cash_flow_2[2]*(1+x/2)^(-2*t_2)
  s_2 <- uniroot(sf_2,c(0,1))$root
  
  t_3 <- as.numeric(year_frac_matrix[i,3])
  sf_3 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_3[1]*(1+s_1/2)^(-2*(t_3-0.5*2))-cash_flow_3[2]*(1+s_2/2)^(-2*(t_3-0.5*1))-cash_flow_3[3]*(1+x/2)^(-2*t_3)
  s_3 <- uniroot(sf_3,c(0,1))$root
  
  t_4 <- as.numeric(year_frac_matrix[i,4])
  sf_4 <- function(x) as.numeric(dirty_matrix[i,4])-cash_flow_4[1]*(1+s_1/2)^(-2*(t_4-0.5*3))-cash_flow_4[2]*(1+s_2/2)^(-2*(t_4-0.5*2))-cash_flow_4[3]*(1+s_3/2)^(-2*(t_4-0.5*1))-cash_flow_4[4]*(1+x/2)^(-2*t_4)
  s_4 <- uniroot(sf_4,c(0,1))$root
  
  t_5 <- as.numeric(year_frac_matrix[i,5])
  sf_5 <- function(x) as.numeric(dirty_matrix[i,5])-cash_flow_5[1]*(1+s_1/2)^(-2*(t_5-0.5*4))-cash_flow_5[2]*(1+s_2/2)^(-2*(t_5-0.5*3))-cash_flow_5[3]*(1+s_3/2)^(-2*(t_5-0.5*2))-cash_flow_5[4]*(1+s_4/2)^(-2*(t_5-0.5*1))-cash_flow_5[5]*(1+x/2)^(-2*t_5)
  s_5 <- uniroot(sf_5,c(0,1))$root

  t_6 <- as.numeric(year_frac_matrix[i,6])
  sf_6 <- function(x) as.numeric(dirty_matrix[i,6])-cash_flow_6[1]*(1+s_1/2)^(-2*(t_5-0.5*5))-cash_flow_6[2]*(1+s_2/2)^(-2*(t_6-0.5*4))-cash_flow_6[3]*(1+s_3/2)^(-2*(t_6-0.5*3))-cash_flow_6[4]*(1+s_4/2)^(-2*(t_6-0.5*2))-cash_flow_6[5]*(1+s_5/2)^(-2*(t_6-0.5*1))-cash_flow_6[6]*(1+x/2)^(-2*t_6)
  s_6 <- uniroot(sf_6,c(0,1))$root
  
  t_7 <- as.numeric(year_frac_matrix[i,7])
  sf_7 <- function(x) as.numeric(dirty_matrix[i,7])-cash_flow_7[1]*(1+s_1/2)^(-2*(t_7-0.5*6))-cash_flow_7[2]*(1+s_2/2)^(-2*(t_7-0.5*5))-cash_flow_7[3]*(1+s_3/2)^(-2*(t_7-0.5*4))-cash_flow_7[4]*(1+s_4/2)^(-2*(t_7-0.5*3))-cash_flow_7[5]*(1+s_5/2)^(-2*(t_7-0.5*2))-cash_flow_7[6]*(1+s_6/2)^(-2*(t_7-0.5*1))-cash_flow_7[7]*(1+x/2)^(-2*t_7)
  s_7 <- uniroot(sf_7,c(0,1))$root
  
  t_8 <- as.numeric(year_frac_matrix[i,8])
  sf_8 <- function(x) as.numeric(dirty_matrix[i,8])-cash_flow_8[1]*(1+s_1/2)^(-2*(t_8-0.5*7))-cash_flow_8[2]*(1+s_2/2)^(-2*(t_8-0.5*6))-cash_flow_8[3]*(1+s_3/2)^(-2*(t_8-0.5*5))-cash_flow_8[4]*(1+s_4/2)^(-2*(t_8-0.5*4))-cash_flow_8[5]*(1+s_5/2)^(-2*(t_8-0.5*3))-cash_flow_8[6]*(1+s_6/2)^(-2*(t_8-0.5*2))-cash_flow_8[7]*(1+s_7/2)^(-2*(t_8-0.5*1))-cash_flow_8[8]*(1+x/2)^(-2*t_8)
  s_8 <- uniroot(sf_8,c(0,1))$root
  
  t_9 <- as.numeric(year_frac_matrix[i,9])
  sf_9 <- function(x) as.numeric(dirty_matrix[i,9])-cash_flow_9[1]*(1+s_1/2)^(-2*(t_9-0.5*8))-cash_flow_9[2]*(1+s_2/2)^(-2*(t_9-0.5*7))-cash_flow_9[3]*(1+s_3/2)^(-2*(t_9-0.5*6))-cash_flow_9[4]*(1+s_4/2)^(-2*(t_9-0.5*5))-cash_flow_9[5]*(1+s_5/2)^(-2*(t_9-0.5*4))-cash_flow_9[6]*(1+s_6/2)^(-2*(t_9-0.5*3))-cash_flow_9[7]*(1+s_7/2)^(-2*(t_9-0.5*2))-cash_flow_9[8]*(1+s_8/2)^(-2*(t_9-0.5*1))-cash_flow_9[9]*(1+x/2)^(-2*t_9)
  s_9 <- uniroot(sf_9,c(0,1))$root
  
  t_10 <- as.numeric(year_frac_matrix[i,10])
  sf_10 <- function(x) as.numeric(dirty_matrix[i,10])-cash_flow_10[1]*(1+s_1/2)^(-2*(t_10-0.5*9))-cash_flow_10[2]*(1+s_2/2)^(-2*(t_10-0.5*8))-cash_flow_10[3]*(1+s_3/2)^(-2*(t_10-0.5*7))-cash_flow_10[4]*(1+s_4/2)^(-2*(t_10-0.5*6))-cash_flow_10[5]*(1+s_5/2)^(-2*(t_10-0.5*5))-cash_flow_10[6]*(1+s_6/2)^(-2*(t_10-0.5*4))-cash_flow_10[7]*(1+s_7/2)^(-2*(t_10-0.5*3))-cash_flow_10[8]*(1+s_8/2)^(-2*(t_10-0.5*2))-cash_flow_10[9]*(1+s_9/2)^(-2*(t_10-0.5*1))-cash_flow_10[10]*(1+x/2)^(-2*t_10)
  s_10 <- uniroot(sf_10,c(0,1))$root
  
  s <- rbind(s_1,s_2,s_3,s_4,s_5,s_6,s_7,s_8,s_9,s_10)
  spot_rate_matrix[i,] <- s}
View(spot_rate_matrix)

plot(year_digit, spot_rate_matrix[1,], main = "Spot Curve", xlab = "Year", ylab = "Spot", type = "o", col = "black")
color <- c(1:10)
for (i in 2:10) {
  points(year_digit, spot_rate_matrix[i,], type = "o", col = color[i - 1])
  lines(year_digit, spot_rate_matrix[i,], type = "o", col = color[i - 1])}

