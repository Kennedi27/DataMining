#Set Area Kerja
setwd("Documents/WD/TB-DataMining")
#Load Data Set yang akan digunakan
dataset3 <- read.csv("Immunotherapy.csv", sep = ",")
#Install Library
#Jika Sudah terinstall abaikan saja
install.packages("C50") 
install.packages("printr")
#Jalankan/Panggil library C50 dan printr
library(C50)
library(printr)
#Membuat model menggunakan C5.0
model3 <- C5.0(Result_of_Treatment ~., data=dataset3)
#jika model error, lanjutkan langkah berikut
#cek class/tipe dari kolom Result_of_Trearment
class(dataset3$Result_of_Treatment)
#mengubah tipe class ke factor dan jalankan kembali model <- C5.0(Result_of_Treatment ~., data=dataset)
dataset3$Result_of_Treatment <- as.factor(dataset3$Result_of_Treatment)
model3 <- C5.0(Result_of_Treatment ~., data=dataset3)
#melihat hasil model
model3
summary(model3)
#menampilkam gambar/pohon model
plot(model3)
#membuat data Testing dari dataset3
datatesting3 <- dataset3[,1:7]
#prediksi
predictions3 <- predict(model3, datatesting3)
head(predictions3)
#membandingkan hasil prediksi dari datatesting dengan dataset
table(predictions3, dataset3$Result_of_Treatment)
#Melihat tingkat akurasi
mean(predictions3  == dataset3$Result_of_Treatment)
#Memngetahui rule dari model yang di buat
rulemodel <- C5.0(Result_of_Treatment ~., data = dataset3, rules = TRUE)
rulemodel
summary(rulemodel)
