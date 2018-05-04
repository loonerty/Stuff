path <- "E:/Data/HQDR/"
setwd(path)
library(fpp)
library(changepoint)
library(quantreg)
files <- list.files(path=path, pattern=".txt")

counter <- 0
med.slope.agree <- 0
med.int.agree <- 0
boot.slope.agree <- 0
boot.int.agree <- 0
linear.assumptions <- 0
boot.assumptions <- 0
median.assumptions <- 0
all.slope.agree <- 0
all.int.agree <- 0
fails <- 0
medall.slope.agree <- 0
medall.int.agree <- 0

for(i in 1:length(files))
{
  station_no <- substr(files[i],8,13)
  print(station_no)
  rain <- read.table(files[i], skip=1, na.strings="99999.9")
  
  colnames(rain) <- c("Date", "Rainfall")
  
  #splitting date to 3 columns
  rain <- cbind(substr(rain$Date,7,8), rain)
  rain <- cbind(substr(rain$Date,5,6), rain)
  rain <- cbind(substr(rain$Date,1,4), rain)
  
  rain <- rain[,-4]
  
  colnames(rain) <- c("Year", "Month", "Day", "Rainfall")
  
  rain$Year <- as.numeric(as.character(rain$Year))
  rain$Month <- as.numeric(as.character(rain$Month))
  
  rain$Year[which(rain$Month == 12)] <- rain$Year[which(rain$Month == 12)] + 1
  
  #split into seasons
  summer <- rain[which(rain$Month %in% c(1,2,12)),]
  autumn <- rain[which(rain$Month %in% c(3,4,5)),]
  winter <- rain[which(rain$Month %in% c(6,7,8)),]
  spring <- rain[which(rain$Month %in% c(9,10,11)),]
  
  season_list <- list(summer, autumn, winter, spring)
  
  for(j in 1:4)
  {
    counter <- counter + 1
    if(j==1){season <- "Summer"}
    if(j==2){season <- "Autumn"}
    if(j==3){season <- "Winter"}
    if(j==4){season <- "Spring"}
    
    quantiles <- numeric(0)
    boot.means <- numeric(0)
    start <- season_list[[j]]$Year[1]
    end <- season_list[[j]]$Year[length(season_list[[j]]$Year)]
    years <- start:end
    for(k in start:end)
    {
      quantiles[k-start+1] <- quantile(season_list[[j]]$Rainfall[which(season_list[[j]]$Year == k)], probs=0.95, na.rm=TRUE)
      boot.quantiles <- numeric(0)
      for(m in 1:1000)
      {
        sample <- sample(season_list[[j]]$Rainfall[which(season_list[[j]]$Year == k)], size=90, replace=TRUE)
        boot.quantiles[m] <- quantile(sample, probs=0.95, na.rm=TRUE)
      }
      boot.means[k-start+1] <- mean(boot.quantiles, na.rm=TRUE)
    }
    
    #Move on to plotting the shit
    #Check if there's a small number of bad data
    newstart <- which(quantiles > 0)[1]
    length <- length(quantiles)
       
    quantiles <- quantiles[newstart:length]
    boot.means <- boot.means[newstart:length]
    years <- years[newstart:length]
    index1 <- 1:length(years)
    
    linear <- lm(quantiles ~ index1)
    boot.model <- lm(boot.means ~ years)
    median.fit <- rq(quantiles ~ index1, tau=0.5)
    # 
    # #SLOPES
    slope <- coef(summary(linear))[2,1]
    slope.lower <- slope - coef(summary(linear))[2,2]
    slope.upper <- slope + coef(summary(linear))[2,2]
    # 
    boot.slope <- coef(summary(boot.model))[2,1]
    boot.slope.lower <- boot.slope - coef(summary(boot.model))[2,2]
    boot.slope.upper <- boot.slope + coef(summary(boot.model))[2,2]
    # 
    med.slope <- coef(summary(median.fit))[2,1]
    med.slope.lower <- coef(summary(median.fit))[2,2]
    med.slope.upper <- coef(summary(median.fit))[2,3]
    # 
    # #INTERCEPTS
    int <- coef(summary(linear))[1,1]
    int.lower <- int - coef(summary(linear))[1,2]
    int.upper <- int + coef(summary(linear))[1,2]
    # 
    boot.int <- coef(summary(boot.model))[1,1]
    boot.int.lower <- boot.int - coef(summary(boot.model))[1,2]
    boot.int.upper <- boot.int + coef(summary(boot.model))[1,2]
    # 
    med.int <- coef(summary(median.fit))[1,1]
    med.int.lower <- coef(summary(median.fit))[1,2]
    med.int.upper <- coef(summary(median.fit))[1,3]
    
    plot(index1, quantiles, main=paste("Quantiles for season",j,"at", station_no, sep=" "))
    abline(linear, col="red")
    abline(median.fit, col="blue")
    
    
    #COMPARE LEAST SQUARES TO BOOTSTRAPPED
    #SLOPE
    if(slope >= boot.slope.lower && slope <= boot.slope.upper)
    {
      if(boot.slope >= slope.lower && boot.slope <= slope.upper)
      {
        #print("Least squares and bootstrapped slopes agree")
        boot.slope.agree <- boot.slope.agree + 1
      }
      # else
      # {
      #   print("Least squares within error bounds of bootstrapped slope only")
      # }
    }
    # # else if(boot.slope >= slope.lower && boot.slope <= slope.upper)
    # # {
    # #   print("Bootstrapped slope within error bounds of least squares slope only")
    # # }
    # # else
    # # {
    # #   print("Bootstrapped and least squares Slopes do not agree")
    # # }
    # 
    # #INTERCEPT
    if(int >= boot.int.lower && int <= boot.int.upper)
    {
      if(boot.int >= int.lower && boot.int <= int.upper)
      {
        #print("Least squares and bootstrapped ints agree")
        boot.int.agree <- boot.int.agree + 1
      }
      # else
      # {
      #   print("Least squares within error bounds of bootstrapped int only")
      # }
    }
    # # else if(boot.int >= int.lower && boot.int <= int.upper)
    # # {
    # #   print("Bootstrapped int within error bounds of least squares int only")
    # # }
    # # else
    # # {
    # #   print("Bootstrapped and least squares Ints do not agree")
    # # }
    # 
    #COMPARE LEAST SQUARES TO MEDIAN
    #SLOPE
    if(slope >= med.slope.lower && slope <= med.slope.upper)
    {
      if(med.slope >= slope.lower && med.slope <= slope.upper)
      {
        #print("Least squares and median slopes agree")
        med.slope.agree <- med.slope.agree + 1
        print(med.slope.agree)
      }
      # else
      # {
      #   print("Least squares within error bounds of median slope only")
      # }
    }
    # # else if(med.slope >= slope.lower && med.slope <= slope.upper)
    # # {
    # #   print("median slope within error bounds of least squares slope only")
    # # }
    # # else
    # # {
    # #   print("median and least squares slopes do not agree")
    # # }
    # 
    #INTERCEPT
    if(int >= med.int.lower && int <= med.int.upper)
    {
      if(med.int >= int.lower && med.int <= int.upper)
      {
        #print("Least squares and median ints agree")
        med.int.agree <- med.int.agree + 1
        print(med.int.agree)
      }
      # else
      # {
      #   print("Least squares within error bounds of median int only")
      # }
    }
    # # else if(med.int >= int.lower && med.int <= int.upper)
    # # {
    # #   print("median int within error bounds of least squares int only")
    # # }
    # # else
    # # {
    # #   print("Median and least squares ints do not agree")
    # # }
    # 
    # if(shapiro.test(linear$residuals)$p.value >= 0.05 && mean(linear$residuals) < 10^(-13))
    # {
    #   linear.assumptions <- linear.assumptions + 1
    # }
    # if(shapiro.test(boot.model$residuals)$p.value >= 0.05 && mean(boot.model$residuals) < 10^(-13))
    # {
    #   boot.assumptions <- boot.assumptions + 1
    # }
    # if(shapiro.test(median.fit$residuals)$p.value >= 0.05 && mean(median.fit$residuals) < 10^(-13))
    # {
    #   median.assumptions <- median.assumptions + 1
    # }
    
    #Quantile Regression of all the data
    index <- 1:length(season_list[[j]]$Rainfall)
    qr <- rq(season_list[[j]]$Rainfall ~ index, tau = 0.95)
    
    if(qr$coefficients[1] != 0 && qr$coefficients[2] != 0)
    {
      all.slope <- 365*coef(summary(qr))[2,1]
      all.slope.lower <- 365*coef(summary(qr))[2,2]
      all.slope.upper <- 365*coef(summary(qr))[2,3]
      
      all.int <- coef(summary(qr))[1,1]
      all.int.lower <- coef(summary(qr))[1,2]
      all.int.upper <- coef(summary(qr))[1,3]
      
      #SLOPE
      if(slope >= all.slope.lower && slope <= all.slope.upper)
      {
        if(all.slope >= slope.lower && all.slope <= slope.upper)
        {
          #print("Least squares and QR slopes agree")
          all.slope.agree <- all.slope.agree + 1
        }
      #   # else
      #   # {
      #   #   print("Least squares within error bounds of QR slope only")
      #   # }
      }
      # else if(all.slope >= slope.lower && all.slope <= slope.upper)
      # {
      #   print("QR slope within error bounds of least squares slope only")
      # }
      # else
      # {
      #   print("QR and least squares Slopes do not agree")
      # }
      
      #INTERCEPT
      if(int >= all.int.lower && int <= all.int.upper)
      {
        if(all.int >= int.lower && all.int <= int.upper)
        {
          #print("Least squares and QR ints agree")
          all.int.agree <- all.int.agree + 1
        }
        # else
        # {
        #   print("Least squares within error bounds of QR int only")
        # }
      }
      # else if(all.int >= int.lower && all.int <= int.upper)
      # {
      #   print("QR int within error bounds of least squares int only")
      # }
      # else
      # {
      #   print("QR and least squares Ints do not agree")
      # }
      
      #COMPARING MEDIAN TO 95% QR
      #SLOPE
      if(med.slope >= all.slope.lower && med.slope <= all.slope.upper)
      {
        if(all.slope >= med.slope.lower && all.slope <= med.slope.upper)
        {
          #print("Least squares and QR slopes agree")
          medall.slope.agree <- medall.slope.agree + 1
        }
        # else
        # {
        #   print("Least squares within error bounds of QR slope only")
        # }
      }
      # else if(all.slope >= slope.lower && all.slope <= slope.upper)
      # {
      #   print("QR slope within error bounds of least squares slope only")
      # }
      # else
      # {
      #   print("QR and least squares Slopes do not agree")
      # }
      
      #INTERCEPT
      if(med.int >= all.int.lower && med.int <= all.int.upper)
      {
        if(all.int >= med.int.lower && all.int <= med.int.upper)
        {
          #print("Least squares and QR ints agree")
          medall.int.agree <- medall.int.agree + 1
        }
        # else
        # {
        #   print("Least squares within error bounds of QR int only")
        # }
      }
      # else if(all.int >= int.lower && all.int <= int.upper)
      # {
      #   print("QR int within error bounds of least squares int only")
      # }
      # else
      # {
      #   print("QR and least squares Ints do not agree")
      # }
    }
    else
    {
      fails <- fails + 1
    }
    
  }
}

#ASSUMPTIONS RESULTS
#Linear
print(paste(linear.assumptions*100/counter, "% of least squares results pass residual assumptions", sep=""))
# 
# #Bootstrapped
print(paste(boot.assumptions*100/counter, "% of bootstrapped least squares results pass residual assumptions", sep=""))
# 
# #Median
print(paste(median.assumptions*100/counter, "% of median models pass residual assumptions", sep=""))
# 
# #AGREEMENT RESULTS
# #Linear and bootstrapped
print(paste(boot.slope.agree*100/counter, "% of bootstrapped slopes agree with raw data least squares", sep=""))
print(paste(boot.int.agree*100/counter, "% of bootstrapped intercepts agree with raw data least squares", sep=""))
# 
#Linear and median
print(paste(med.slope.agree*100/counter, "% of median slopes agree with raw data least squares", sep=""))
print(paste(med.int.agree*100/counter, "% of median intercepts agree with raw data least squares", sep=""))

#QR and Linear
print(paste(all.slope.agree*100/counter, "% of QR slopes agree with raw data least squares", sep=""))
print(paste(all.int.agree*100/counter, "% of QR intercepts agree with raw data least squares", sep=""))
print(paste(fails*100/counter, "% of seasons had 0 slope or 0 intercept and so failed", sep=""))

#QR and Median
print(paste(medall.slope.agree*100/counter, "% of 0.95 QR slopes agree with median QR", sep=""))
print(paste(medall.int.agree*100/counter, "% of 0.95 QR intercepts agree with median QR", sep=""))
