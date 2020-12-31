#Set Area Kerja
setwd("D:/Documents/WD/TB-DataMining")
#Data Set yang akan digunakan
dataset <- read.csv("Immunotherapy.csv", sep = ",")
#Aktifkan library C50 dan printr 
library(C50)
library(printr)
#Membuat model menggunakan C5.0
model <- C5.0(Result_of_Treatment ~., data=dataset)
#jika model error, lanjutkan langkah berikut
#cek class/tipe dari kolom Result_of_Trearment
class(dataset$Result_of_Treatment)
#mengubah tipe class ke factor dan jalankan kembali model <- C5.0(Result_of_Treatment ~., data=dataset)
dataset$Result_of_Treatment <- as.factor(dataset$Result_of_Treatment)
#melihat hasil model
model
summary(model)
#menampilkam gambar/pohon model
plot(model)
#membuat dataset
datatesting <- dataset[,1:7]
#prediksi
predictions <- predict(model, datatesting)
#membandingkan hasil prediksi dari datatesting dengan dataset
table(predictions, dataset$Result_of_Treatment)

#Memngetahui rule model
rulemodel <- C5.0(Result_of_Treatment ~., data = dataset, rules = TRUE)
rulemodel
summary(rulemodel)
