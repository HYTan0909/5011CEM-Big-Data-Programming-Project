install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("parallel")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(doParallel)
library(parallel)
library(foreach)

#parallel processing
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores())

# Activate cluster for foreach library
doParallel::registerDoParallel(cl)
time_foreach <- system.time({
  r <- foreach::foreach(i = 1:1,
                        .combine = rbind) %dopar% {
                          oswardJan<-read.csv('Jan_osward_grocery.csv')
                          oswardFeb<-read.csv('Feb_osward_grocery.csv')
                          oswardMar<-read.csv('Mar_osward_grocery.csv')
                          oswardApr<-read.csv('Apr_osward_grocery.csv')
                          oswardMay<-read.csv('May_osward_grocery.csv')
                          oswardJun<-read.csv('Jun_osward_grocery.csv')
                        }
})
time_foreach[3]
# Stop cluster to free up resources
parallel::stopCluster(cl)

#sequential processing
#runtime
start<-Sys.time()

oswardJan<-read.csv('Jan_osward_grocery.csv')
oswardFeb<-read.csv('Feb_osward_grocery.csv')
oswardMar<-read.csv('Mar_osward_grocery.csv')
oswardApr<-read.csv('Apr_osward_grocery.csv')
oswardMay<-read.csv('May_osward_grocery.csv')
oswardJun<-read.csv('Jun_osward_grocery.csv')

end<-Sys.time()

runtime<-end - start
runtime

#descriptive analysis 
#Jan (age 0 to 17)
minJanChild<-min(oswardJan$age_0_17, na.rm = TRUE)
meanJanChild<-mean(oswardJan$age_0_17, na.rm = TRUE)
medianJanChild<-median(oswardJan$age_0_17, na.rm = TRUE)
sdJanChild<-sd(oswardJan$age_0_17, na.rm = TRUE)

#Jan (age 18 to 64)
minJanAdult<-min(oswardJan$age_18_64, na.rm = TRUE)
meanJanAdult<-mean(oswardJan$age_18_64, na.rm = TRUE)
medianJanAdult<-median(oswardJan$age_18_64, na.rm = TRUE)
sdJanAdult<-sd(oswardJan$age_18_64, na.rm = TRUE)

#Jan (age 65+)
minJanElder<-min(oswardJan$age_65., na.rm = TRUE)
meanJanElder<-mean(oswardJan$age_65., na.rm = TRUE)
medianJanElder<-median(oswardJan$age_65., na.rm = TRUE)
sdJanElder<-sd(oswardJan$age_65., na.rm = TRUE)

#Feb (age 0 to 17)
minFebChild<-min(oswardFeb$age_0_17, na.rm = TRUE)
meanFebChild<-mean(oswardFeb$age_0_17, na.rm = TRUE)
medianFebChild<-median(oswardFeb$age_0_17, na.rm = TRUE)
sdFebChild<-sd(oswardFeb$age_0_17, na.rm = TRUE)

#Feb (age 18 to 64)
minFebAdult<-min(oswardFeb$age_18_64, na.rm = TRUE)
meanFebAdult<-mean(oswardFeb$age_18_64, na.rm = TRUE)
medianFebAdult<-median(oswardFeb$age_18_64, na.rm = TRUE)
sdFebAdult<-sd(oswardFeb$age_18_64, na.rm = TRUE)

#Feb (age 65+)
minFebElder<-min(oswardFeb$age_65., na.rm = TRUE)
meanFebElder<-mean(oswardFeb$age_65., na.rm = TRUE)
medianFebElder<-median(oswardFeb$age_65., na.rm = TRUE)
sdFebElder<-sd(oswardFeb$age_65., na.rm = TRUE)

#Mar (age 0 to 17)
minMarChild<-min(oswardMar$age_0_17, na.rm = TRUE)
meanminMarChild<-mean(oswardMar$age_0_17, na.rm = TRUE)
medianminMarChild<-median(oswardMar$age_0_17, na.rm = TRUE)
sdminMarChild<-sd(oswardMar$age_0_17, na.rm = TRUE)

#Mar (age 18 to 64)
minMarAdult<-min(oswardMar$age_18_64, na.rm = TRUE)
meanminMarAdult<-mean(oswardMar$age_18_64, na.rm = TRUE)
medianminMarAdult<-median(oswardMar$age_18_64, na.rm = TRUE)
sdminMarAdult<-sd(oswardMar$age_18_64, na.rm = TRUE)

#Mar (age 65+)
minMarElder<-min(oswardMar$age_65., na.rm = TRUE)
meanMarElder<-mean(oswardMar$age_65., na.rm = TRUE)
medianMarElder<-median(oswardMar$age_65., na.rm = TRUE)
sdMarElder<-sd(oswardMar$age_65., na.rm = TRUE)

#Apr (age 0 to 17)
minAprChild<-min(oswardApr$age_0_17, na.rm = TRUE)
meanAprChild<-mean(oswardApr$age_0_17, na.rm = TRUE)
medianAprChild<-median(oswardApr$age_0_17, na.rm = TRUE)
sdAprChild<-sd(oswardApr$age_0_17, na.rm = TRUE)

#Apr (age 18 to 64)
minAprAdult<-min(oswardApr$age_18_64, na.rm = TRUE)
meanAprAdult<-mean(oswardApr$age_18_64, na.rm = TRUE)
medianAprAdult<-median(oswardApr$age_18_64, na.rm = TRUE)
sdAprAdult<-sd(oswardApr$age_18_64, na.rm = TRUE)

#Apr (age 65+)
minAprElder<-min(oswardApr$age_65., na.rm = TRUE)
meanAprElder<-mean(oswardApr$age_65., na.rm = TRUE)
medianAprElder<-median(oswardApr$age_65., na.rm = TRUE)
sdAprElder<-sd(oswardApr$age_65., na.rm = TRUE)

#May (age 0 to 17)
minMayChild<-min(oswardMay$age_0_17, na.rm = TRUE)
meanMayChild<-mean(oswardMay$age_0_17, na.rm = TRUE)
medianMayChild<-median(oswardMay$age_0_17, na.rm = TRUE)
sdMayChild<-sd(oswardMay$age_0_17, na.rm = TRUE)

#May (age 18 to 64)
minMayAdult<-min(oswardMay$age_18_64, na.rm = TRUE)
meanMayAdult<-mean(oswardMay$age_18_64, na.rm = TRUE)
medianMayAdult<-median(oswardMay$age_18_64, na.rm = TRUE)
sdMayAdult<-sd(oswardMay$age_18_64, na.rm = TRUE)

#May (age 65+)
minMayElder<-min(oswardMay$age_65., na.rm = TRUE)
meanMayElder<-mean(oswardMay$age_65., na.rm = TRUE)
medianMayElder<-median(oswardMay$age_65., na.rm = TRUE)
sdMayElder<-sd(oswardMay$age_65., na.rm = TRUE)

#Jun (age 0 to 17)
minJunChild<-min(oswardJun$age_0_17, na.rm = TRUE)
meanJunChild<-mean(oswardJun$age_0_17, na.rm = TRUE)
medianJunChild<-median(oswardJun$age_0_17, na.rm = TRUE)
sdJunChild<-sd(oswardJun$age_0_17, na.rm = TRUE)

#Jun (age 18 to 64)
minJunAdult<-min(oswardJun$age_18_64, na.rm = TRUE)
meanJunAdult<-mean(oswardJun$age_18_64, na.rm = TRUE)
medianJunAdult<-median(oswardJun$age_18_64, na.rm = TRUE)
sdJunAdult<-sd(oswardJun$age_18_64, na.rm = TRUE)

#Jun (age 65+)
minJunElder<-min(oswardJun$age_65., na.rm = TRUE)
meanJunElder<-mean(oswardJun$age_65., na.rm = TRUE)
medianJunElder<-median(oswardJun$age_65., na.rm = TRUE)
sdJunElder<-sd(oswardJun$age_65., na.rm = TRUE)

#correlation test
#Jan
cor.test(oswardJan$num_transactions, oswardJan$age_0_17)
cor.test(oswardJan$num_transactions, oswardJan$age_18_64)
cor.test(oswardJan$num_transactions, oswardJan$age_65.)

#Feb
cor.test(oswardFeb$num_transactions, oswardFeb$age_0_17)
cor.test(oswardFeb$num_transactions, oswardFeb$age_18_64)
cor.test(oswardFeb$num_transactions, oswardFeb$age_65.)

#Mar
cor.test(oswardMar$num_transactions, oswardMar$age_0_17)
cor.test(oswardMar$num_transactions, oswardMar$age_18_64)
cor.test(oswardMar$num_transactions, oswardMar$age_65.)

#Apr
cor.test(oswardApr$num_transactions, oswardApr$age_0_17)
cor.test(oswardApr$num_transactions, oswardApr$age_18_64)
cor.test(oswardApr$num_transactions, oswardApr$age_65.)

#May
cor.test(oswardMay$num_transactions, oswardMay$age_0_17)
cor.test(oswardMay$num_transactions, oswardMay$age_18_64)
cor.test(oswardMay$num_transactions, oswardMay$age_65.)

#Jun
cor.test(oswardJun$num_transactions, oswardJun$age_0_17)
cor.test(oswardJun$num_transactions, oswardJun$age_18_64)
cor.test(oswardJun$num_transactions, oswardJun$age_65.)


