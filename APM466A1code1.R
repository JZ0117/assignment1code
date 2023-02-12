install.packages("jrvFinance")
library("jrvFinance")
install.packages("ggplot2")
library(ggplot2)

library(readxl)
bond <- read_excel("/Users/tommyzhang/Desktop/466bondsdata.xlsx", sheet = 2)
View(bond)

coupon <- bond$Coupon
maturity_date <- bond$`Maturity Date`
price_date <- c("2023/1/16", "2023/1/17", "2023/1/18", "2023/1/19", "2023/1/20", "2023/1/23", "2023/1/24", "2023/1/25", "2023/1/26", "2023/1/27")
clean_price_matrix = matrix(c(bond$"16", bond$"17", bond$"18", bond$"19", bond$"20", bond$"23", bond$"24", bond$"25", bond$"26", bond$"27"), nrow=10,ncol = 10, byrow = TRUE)


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
year_digit <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
plot(year_digit, YTM_matrix[1,], type="o", main="Yield Curve", xlab = "Maturity year", ylab = "YTM", col="black")
colors <- c(1:10)
for (i in c(2:10)) {
  lines(year_digit, YTM_matrix[i,], type = "o", col=colors[i-1])
}
legend("topright",legend = c('2023-01-16','2023-01-17','2023-01-18','2023-01-19','2023-01-20','2023-01-23','2023-01-24','2023-01-25','2023-01-26','2023-01-27'), col=c(1:10), lty=1, cex=0.75)

#interpolation technique

## spot rate
# dirty price
dirty_matrix <- matrix("numeric",nrow = 10, ncol = 10)
for (i in c(1:10)){
  for (j in c(1:10)) {
    dirty_matrix[i,j] <- bond.TCF(price_date[i],
                                  maturity_date[j],
                                  coupon[j],
                                  freq = 2,
                                  convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                  redemption_value = 100)$accrued + clean_price_matrix[i,j]}}
View(dirty_matrix)

# cash flow
for (i in 1:10){
  cash_flow =  bond.TCF(price_date[i],
                        maturity_date[i],
                        coupon[i],
                        freq = 2,
                        convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                        redemption_value = 100)$cf
  assign(paste0("cash_flow_",i), cash_flow)
}


year_frac_matrix <- matrix("numeric",nrow = 10, ncol = 10)
for ( i in c(1:10)){
  for (j in c(1:10)){
    year_frac_matrix[i,j] <- yearFraction(price_date[i], maturity_date[j], freq = 2,
                                         convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E")
    )
  }}


spot_rate_matrix <- matrix(nrow=10,ncol = 10)
for (i in 1:10){
  t1 <- as.numeric(year_frac_matrix[i,1])
  sf1 <- function(x) as.numeric(dirty_matrix[i,1])-cash_flow_1[1]*(1+x/2)^(-2*t1)
  s1 <- uniroot(sf1,c(0,1))$root
  
  t2 <- as.numeric(year_frac_matrix[i,2])
  sf2 <- function(x) as.numeric(dirty_matrix[i,2])-cash_flow_2[1]*(1+s1/2)^(-2*(t2-0.5*1))-cash_flow_2[2]*(1+x/2)^(-2*t2)
  s2 <- uniroot(sf2,c(0,1))$root
  
  t3 <- as.numeric(year_frac_matrix[i,3])
  sf3 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_3[1]*(1+s1/2)^(-2*(t3-0.5*2))-cash_flow_3[2]*(1+s2/2)^(-2*(t3-0.5*1))-cash_flow_3[3]*(1+x/2)^(-2*t3)
  s3 <- uniroot(sf3,c(0,1))$root
  
  t4 <- as.numeric(year_frac_matrix[i,4])
  sf4 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_4[1]*(1+s1/2)^(-2*(t4-0.5*3))-cash_flow_4[2]*(1+s2/2)^(-2*(t4-0.5*2))-cash_flow_4[3]*(1+s3/2)^(-2*(t4-0.5*1))-cash_flow_4[4]*(1+x/2)^(-2*t4)
  s4 <- uniroot(sf4,c(0,1))$root
  
  t5 <- as.numeric(year_frac_matrix[i,5])
  sf5 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_5[1]*(1+s1/2)^(-2*(t5-0.5*4))-cash_flow_5[2]*(1+s2/2)^(-2*(t5-0.5*3))-cash_flow_5[3]*(1+s3/2)^(-2*(t5-0.5*2))-cash_flow_5[4]*(1+s4/2)^(-2*(t5-0.5*1))-cash_flow_5[5]*(1+x/2)^(-2*t5)
  s5 <- uniroot(sf5,c(0,1))$root
  
  t6 <- as.numeric(year_frac_matrix[i,6])
  sf6 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_6[1]*(1+s1/2)^(-2*(t5-0.5*5))-cash_flow_6[2]*(1+s2/2)^(-2*(t6-0.5*4))-cash_flow_6[3]*(1+s3/2)^(-2*(t6-0.5*3))-cash_flow_6[4]*(1+s4/2)^(-2*(t6-0.5*2))-cash_flow_6[5]*(1+s5/2)^(-2*(t6-0.5*1))-cash_flow_6[6]*(1+x/2)^(-2*t6)
  s6 <- uniroot(sf6,c(0,1))$root
  
  t7 <- as.numeric(year_frac_matrix[i,7])
  sf7 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_7[1]*(1+s1/2)^(-2*(t7-0.5*6))-cash_flow_7[2]*(1+s2/2)^(-2*(t7-0.5*5))-cash_flow_7[3]*(1+s3/2)^(-2*(t7-0.5*4))-cash_flow_7[4]*(1+s4/2)^(-2*(t7-0.5*3))-cash_flow_7[5]*(1+s5/2)^(-2*(t7-0.5*2))-cash_flow_7[6]*(1+s6/2)^(-2*(t7-0.5*1))-cash_flow_7[7]*(1+x/2)^(-2*t7)
  s7 <- uniroot(sf7,c(0,1))$root
  
  t8 <- as.numeric(year_frac_matrix[i,8])
  sf8 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_8[1]*(1+s1/2)^(-2*(t8-0.5*7))-cash_flow_8[2]*(1+s2/2)^(-2*(t8-0.5*6))-cash_flow_8[3]*(1+s3/2)^(-2*(t8-0.5*5))-cash_flow_8[4]*(1+s4/2)^(-2*(t8-0.5*4))-cash_flow_8[5]*(1+s5/2)^(-2*(t8-0.5*3))-cash_flow_8[6]*(1+s6/2)^(-2*(t8-0.5*2))-cash_flow_8[7]*(1+s7/2)^(-2*(t8-0.5*1))-cash_flow_8[8]*(1+x/2)^(-2*t8)
  s8 <- uniroot(sf8,c(0,1))$root
  
  t9 <- as.numeric(year_frac_matrix[i,9])
  sf9 <- function(x) as.numeric(dirty_matrix[i,3])-cash_flow_9[1]*(1+s1/2)^(-2*(t9-0.5*8))-cash_flow_9[2]*(1+s2/2)^(-2*(t9-0.5*7))-cash_flow_9[3]*(1+s3/2)^(-2*(t9-0.5*6))-cash_flow_9[4]*(1+s4/2)^(-2*(t9-0.5*5))-cash_flow_9[5]*(1+s5/2)^(-2*(t9-0.5*4))-cash_flow_9[6]*(1+s6/2)^(-2*(t9-0.5*3))-cash_flow_9[7]*(1+s7/2)^(-2*(t9-0.5*2))-cash_flow_9[8]*(1+s8/2)^(-2*(t9-0.5*1))-cash_flow_9[9]*(1+x/2)^(-2*t9)
  s9 <- uniroot(sf9,c(0,1))$root
  
  t10 <- as.numeric(year_frac_matrix[i,10])
  sf10 <- function(x) as.numeric(dirty_matrix[i,10])-cash_flow_10[1]*(1+s1/2)^(-2*(t10-0.5*9))-cash_flow_10[2]*(1+s2/2)^(-2*(t10-0.5*8))-cash_flow_10[3]*(1+s3/2)^(-2*(t10-0.5*7))-cash_flow_10[4]*(1+s4/2)^(-2*(t10-0.5*6))-cash_flow_10[5]*(1+s5/2)^(-2*(t10-0.5*5))-cash_flow_10[6]*(1+s6/2)^(-2*(t10-0.5*4))-cash_flow_10[7]*(1+s7/2)^(-2*(t10-0.5*3))-cash_flow_10[8]*(1+s8/2)^(-2*(t10-0.5*2))-cash_flow_10[9]*(1+s9/2)^(-2*(t10-0.5*1))-cash_flow_10[10]*(1+x/2)^(-2*t10)
  s10 <- uniroot(sf10,c(0,1))$root
  
  s <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
  spot_matrix[i,] <- s}


plot(year_digit, spot_matrix[1,], main = "Spot Curve", xlab = "Maturity Year", ylab = "Spot%", type = "o", col = "black")
color <- c(1:10)
for (i in 2:10) {
  points(year_digit, spot_matrix[i,], type = "o", col = color[i-1])
  lines(year_digit, spot_matrix[i,], type = "o", col = color[i-1])}
legend("topright",legend = c('2023-01-16','2023-01-17','2023-01-18','2023-01-19','2023-01-20','2023-01-23','2023-01-24','2023-01-25','2023-01-26','2023-01-27'), col=c(1:10), lty=1, cex=0.5)


# forward rate


spot_each_year=spot_matrix[,c(2,4,6,8,10)]
fwd_matrix=matrix(nrow=10,ncol=4)
for(j in c(1:10)){
  for(i in c(1:4)){
    f1_i=function(x) (1+spot_each_year[j,i]/2)^(2*i)*(1+x/2)^(2)-(1+spot_each_year[j,i+1]/2)^(2*(i+1))
    fwd_matrix[j,i]=uniroot(f1_i,c(-1,1))$root
  }
}
year_fwd=c(1,2,3,4)
plot(year_fwd,fwd_matrix[1,],type="o", main="Forward Curve", col="black", xlab="Year", ylab="Forward Rate")
colour = c(1:10)
for (i in c(2:10)){
  lines(year,fwd_matrix[i,], type="o", col=colour[i-1])
}
legend("topleft",legend=c('2023-01-16','2023-01-17','2023-01-18','2023-01-19','2023-01-20','2023-01-23','2023-01-24','2023-01-25','2023-01-26','2023-01-27'), col=c(1:10), lty=1, cex=0.7)


# Q5
log_YTM_matrix=matrix(nrow=9,ncol=5)
for (i in c(1:5)){
  for (j in c(1:9)){
    log_YTM_matrix[j,i]=log(as.numeric(YTM_matrix[i,j+1])/as.numeric(YTM_matrix[i,j]))
  }
}
print(log_YTM_matrix)
YTM_cov=cov(log_YTM_matrix,log_YTM_matrix)
print(YTM_cov)

log_fwd_matrix <- matrix(nrow = 9, ncol =4)
for (i in c(1:4)){
  for (j in c(1:9)){
    log_fwd_matrix[j,i] <- log(fwd_matrix[(j+1),i]/fwd_matrix[j,i])
  }}
print(log_fwd_matrix)
fwd_cov=cov(log_fwd_matrix,log_fwd_matrix)
print(fwd_cov)

#Q6
print(eigen(YTM_cov)$values)
print(eigen(YTM_cov)$vectors)

print(eigen(fwd_cov)$values)
print(eigen(fwd_cov)$vectors)

