# Just for fun finding out the Page View of the Wikipedia Pages of 10 Indian Actors.
# The Actors that I have included are:
# 1. Shahrukh Khan
# 2. Akshay Kumar
# 3. Hritik Roshan
# 4. Amitabh Bachchan
# 5. Aamir Khan
# 6. Irfan Khan
# 7. Ranbir Kapoor
# 8. Shahid Kapoor
# 9. Ajay Devgan
# 10. Salman Khan

# The data has been collected using Wikipedia wp_trend Library in R.

# Installation Steps

install.packages("devtools")
devtools::install_github("petermeissner/wikipediatrend")
devtools::install_github("twitter/AnomalyDetection")


#Loading Libraries

library(wikipediatrend)
library(AnomalyDetection)
library(ggplot2)

print("Your current working directory is: ")
getwd()
print("Now we will create a new folder called Actor Analysis in this folder.")
dir.create("Actors Analysis")
setwd("./Actors Analysis")
print("Now the working directory is:")
getwd()

# Collecting the data

srk<- wp_trend("Shah Rukh Khan", from="2008-01-01", lang = "en")
akku<- wp_trend("Akshay Kumar", from="2008-01-01", lang = "en")
hr<- wp_trend("Hritik Roshan", from="2008-01-01", lang = "en")
ab<- wp_trend("Amitabh Bachchan", from="2008-01-01", lang = "en")
aak<- wp_trend("Aamir Khan", from="2008-01-01", lang = "en")
ik<- wp_trend("Irfan Khan", from="2008-01-01", lang = "en")
rk<- wp_trend("Ranbir Kapoor", from="2008-01-01", lang = "en")
sk<- wp_trend("Shahid Kapoor", from="2008-01-01", lang = "en")
ad<- wp_trend("Ajay Devgan", from="2008-01-01", lang = "en")
sak<- wp_trend("Salman Khan", from="2008-01-01", lang = "en")

# Saving the data into CSV files.

write.csv(srk, file = "srk.csv")
write.csv(akku, file = "akku.csv")
write.csv(hr, file = "hr.csv")
write.csv(ab, file = "ab.csv")
write.csv(aak, file = "aak.csv")
write.csv(ik, file = "ik.csv")
write.csv(rk, file = "rk.csv")
write.csv(sk, file = "sk.csv")
write.csv(ad, file = "ad.csv")
write.csv(sak, file = "sak.csv")


## Plotting data
png('srk.png')
ggplot(srk, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('akku.png')
ggplot(akku, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('hr.png')
ggplot(hr, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('ab.png')
ggplot(ab, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('aak.png')
ggplot(aak, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('ik.png')
ggplot(ik, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('rk.png')
ggplot(rk, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('sk.png')
ggplot(sk, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('ad.png')
ggplot(ad, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

png('sak.png')
ggplot(sak, aes(x=date, y=count, color=count)) + geom_line()
dev.off()

#Converting the Date in correct format

srk$date = as.POSIXct(srk$date)
akku$date = as.POSIXct(akku$date)
hr$date = as.POSIXct(hr$date)
ab$date = as.POSIXct(ab$date)
aak$date = as.POSIXct(aak$date)
ik$date = as.POSIXct(ik$date)
rk$date = as.POSIXct(rk$date)
sk$date = as.POSIXct(sk$date)
ad$date = as.POSIXct(ad$date)
sak$date = as.POSIXct(sak$date)


# Taking the data that is required.

srk<-srk[,c(1,2)]
akku<-akku[,c(1,2)]
hr<-hr[,c(1,2)]
ab<-ab[,c(1,2)]
aak<-aak[,c(1,2)]
ik<-ik[,c(1,2)]
rk<-rk[,c(1,2)]
sk<-sk[,c(1,2)]
ad<-ad[,c(1,2)]
sak<-sak[,c(1,2)]

############################# Anomaly Detection #############################


# 1. SRK


data_anomaly = AnomalyDetectionTs(srk, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("srk_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"srk_anomaly.txt")


# 2. AKKU


data_anomaly = AnomalyDetectionTs(akku, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("akku_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"akku_anomaly.txt")


# 3. HR 


data_anomaly = AnomalyDetectionTs(hr, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("hr_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"hr_anomaly.txt")


# 4. AB


data_anomaly = AnomalyDetectionTs(ab, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("ab_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"ab_anomaly.txt")


# 5. AAK


data_anomaly = AnomalyDetectionTs(aak, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("aak_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"srk_anomaly.txt")


# 6. IK


data_anomaly = AnomalyDetectionTs(ik, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("ik_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"ik_anomaly.txt")


# 7. RK


data_anomaly = AnomalyDetectionTs(rk, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("rk_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"rk_anomaly.txt")


# 8. SK


data_anomaly = AnomalyDetectionTs(sk, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("sk_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"sk_anomaly.txt")


# 9. AD


data_anomaly = AnomalyDetectionTs(ad, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("ad_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"ad_anomaly.txt")


# 10. SAK


data_anomaly = AnomalyDetectionTs(sak, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
jpeg("sak_anomaly.jpg", width= 8.25, height= 5.25, units="in", res=500, pointsize = 4)
data_anomaly$plot
dev.off()

## Calculate deviation percentage from the expected value 
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)

## Plot anomalies table
anomaly_table=data_anomaly$anoms
write.table(anomaly_table,"sak_anomaly.txt")