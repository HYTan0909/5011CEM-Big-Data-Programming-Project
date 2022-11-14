install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("parallel")
install.packages('microbenchmark')
install.packages("tidyverse")
install.packages("caTools")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(doParallel)
library(parallel)
library(microbenchmark)
library(tidyverse)
library(caTools)

#sequential processing 
setwd("/Users/USER/Documents/5011CEM-Big-Data-Programming-Project/5011CEM-Big-Data-Programming-Project/AreaPurchase")
df <-
  list.files(path = "/Users/USER/Documents/5011CEM-Big-Data-Programming-Project/5011CEM-Big-Data-Programming-Project/AreaPurchase", pattern = "*.csv") %>% 
  map_df(~read_csv(.))

summary(df)

func<-function(f){
  list.files(path = "/Users/USER/Documents/5011CEM-Big-Data-Programming-Project/5011CEM-Big-Data-Programming-Project/AreaPurchase", pattern = "*.csv") %>% 
  map_df(~read_csv(.))
}

system.time(microbenchmark(lapply(1:1, func), times = 1L))

#parallel processing
n.cores<-8
cluster<-parallel::makeCluster(n.cores, types = "PSOCK")

clusterEvalQ(cluster, {
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(parallel)
  library(microbenchmark)
  library(tidyverse)
})

system.time(microbenchmark(parLapply(cluster, 1:1, func), times = 1L))


#comparison
compareMbm<-microbenchmark(seqData(), parallelData())

autoplot(compareMbm)

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

#linear regression
dataset <-
  list.files(path = "/Users/USER/Documents/5011CEM-Big-Data-Programming-Project/5011CEM-Big-Data-Programming-Project/AreaPurchase", pattern = "*.csv") %>% 
  map_df(~read_csv(.))

View(dataset)

childrenSet<-lm(num_transactions~age_0_17, dataset)
adultSet<-lm(num_transactions~age_18_64, dataset)
elderSet<-lm(num_transactions~age_65+ , dataset)

print(summary(childrenSet))
print(summary(adultSet))
print(summary(elderSet))




