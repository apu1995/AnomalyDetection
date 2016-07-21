# Use this file if you want to read the CSV files provided. And not use the Wikipedia Trend Library.
# This one is better as fetching the data took a lot of time for me approximately 20 mins.

library(AnomalyDetection)
library(ggplot2)

# Reading form the CSV files

srk=read.csv("srk.csv")
akku=read.csv("akku.csv")
hr=read.csv("hr.csv")
ab=read.csv("ab.csv")
aak=read.csv("aak.csv")
ik=read.csv("ik.csv")
rk=read.csv("rk.csv")
sk=read.csv("sk.csv")
ad=read.csv("ad.csv")
sak=read.csv("sak.csv")

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