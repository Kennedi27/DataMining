#Set Lokasi Kerja
setwd("Documents/WD/TB-DataMining")
getwd()
#Load Dataset
dataset1 <- read.csv("Immunotherapy.csv", sep = ",")
#lihat Struktur Dataset
str(dataset1)
#Mengubah Struktur Dataset Result_of_Treatment ke tipe factor
dataset1$Result_of_Treatment <- as.factor(dataset1$Result_of_Treatment)
#load Library
library("dplyr")
library("caret")
library("lattice")
library("ggplot2")
library("rpart")
library("rpart.plot")
#Cek Data Sample Sebanyak 10
sample_n(dataset1, 10)
#Membagi Data Training dan Testing dengan persentase 80%
set.seed(123)
training.sample <- dataset1$Result_of_Treatment%>%createDataPartition(p=0.8, list = FALSE)
train.data <- dataset1[training.sample,]
test.data <- dataset1[-training.sample,]
#Membuat Model
model <- rpart(Result_of_Treatment~., data = train.data, method = "class", control = rpart.control(minsplit = 0, cp = 0))
#Menampilkan Plot/Pohon Keputusan dan tingkat probabilitas class
prp(model, extra = 4)
#Melakukan Prediksi terhadap data test
prediksi <- model %>% predict(test.data, type = "class")
#menampilkan sebagian data prediksi
head(prediksi)
#Menampilkan table Prediksi data test
table(prediksi, test.data$Result_of_Treatment)
#Menampilkan Tingkat Akurasi Data
mean(prediksi == test.data$Result_of_Treatment)
#Menampilkan hasil Prediksi
summary(prediksi)
