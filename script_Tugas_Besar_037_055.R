#Set Area Kerja
setwd("C:/wd/TB")
getwd()
#Load Data Set yang akan digunakan
dataset3 <- read.csv("Immunotherapy.csv", sep = ",")

#Install Library
#Jika Sudah terinstall abaikan saja
install.packages("C50") 
install.packages("printr")

#Jalankan/Panggil library C50 dan printr
library(C50)
library(printr)
library(dplyr)
#cek Struktur dataset
str(dataset3)

#Cek class/tipe dari kolom Result_of_Treatment
class(dataset3$Result_of_Treatment)

#Mengubah tipe class ke factor
dataset3$Result_of_Treatment <- as.factor(dataset3$Result_of_Treatment)

#Membagi Data Training dan Testing dengan persentase 80%
set.seed(123)
training.sample3 <- sample(nrow(dataset3)*0.8)
train.data3 <- dataset3[training.sample3,]
test.data3 <- dataset3[-training.sample3,]

#Membuat Model dan Melihat isi model
set.seed(123)
model3 <- C5.0(Result_of_Treatment ~., data=train.data3, control = C5.0Control(minCases = 0))
model3
summary(model3)

#Menampilkam gambar/pohon model
plot(model3)

#Prediksi data test 
datatesting3 <- test.data3[,1:7]
predictions3 <- predict(model3, datatesting3)

#Melihat beberapa hasil prediksi
head(predictions3)
#Membandingkan hasil prediksi dari datatesting dengan dataset
table(predictions3, test.data3$Result_of_Treatment)

#Melihat tingkat akurasi
mean(predictions3  == test.data3$Result_of_Treatment)
