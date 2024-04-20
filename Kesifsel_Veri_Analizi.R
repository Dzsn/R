# Gerekli kütüphaneler indirilir ve eklenir
install.packages("readxl")
install.packages("readr")
install.packages("tidyverse")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("caret")
install.packages("corrplot")
install.packages("caTools")
library(caTools)
library(caret)
library(corrplot)
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(glmnet)

# e+10 gibi değerlerin açılması için bu komut girilir
options(scipen = 999)

# Verinin Tekrarlanabilmesi için seed komutu eklenir
set.seed(123)

# Gözlem Sayısı, bağımsız ve bağımlı değişkenler girilir
n = 100 #Gözlem sayısı belirleme
x1 = rnorm(n, mean = 15, sd = 1.3) # Bağımsız değişken 1
x2 = rnorm(n, mean = 10, sd = 3) # Bağımsız değişken 2
x3 = rnorm(n, mean = 30, sd = 0.2) # Bağımsız değişken 3
x4 = rnorm(n, mean = 21, sd = 3) # Bağımsız değişken 4
x5 = rnorm(n, mean = 35, sd = 1) # Bağımsız değişken 5

y = 15 - 5.2*x1+ 3.1*x2 + 1.19*x3 - 7.8*x4 + 5.5*x5 - rnorm(n, mean = 0 , sd = 1.5) #Bağımlı değişken oluşturulur

# Oluşturalan veri setini Dataframe'e çevirme
veri = data.frame(y,x1,x2,x3,x4,x5)

# Oluşturulan Veride NA bir değer var mı diye kontrol edilir
print(is.na(veri))

# Verinin Çeşitli Grafikleri oluşturulur.
boxplot(veri) #Kutu-Bıyık Grafiği çizilir
hist(veri$y) #Histogramı çizilir

# Q-Q plotu oluşturulabilir
qqnorm(veri$y) #Q-Q plot çizimi
qqline(veri$y) #Q-Q plot çizimi çizgi ile

# Çeşitli değişkenlere göre sıralamalar yapılabilir
sort(veri$y) #y değişkenini düşükten yükseğe sıralama

# Verinin özeti alınabilir
summary(veri) 

# Verinin z skorları bulunabilir. +-3 Sigma üstü değer var mı diye bakılabilir
sigma = scale(veri) #Z skoruna dönüştürülür
outliners = abs(sigma) > 3 #Mutlak değer olarak 3'ten büyük z skorları değişkene atanır
summary(sigma) #Özet alınır

# Q1,  Medyan ve Q3 dışında Veri Sıralandığında %x'lik dilimden buyuk degeri elde etmek ilin quantile fonksiyonu kullanilabilir.
# Örneğin %30'luk dilimdeki veriyi elde etmek için aşağıdaki kod kullanılabilir
print(quantile(veri$y,0.30))

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(veri$y, 0.25) #Q1 değeri
Q3 <- quantile(veri$y, 0.75) #Q3 değeri

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1 # Kartiller arası mesafe ölçülr
lower_bound <- Q1 - 1.5 * IQR # Alt sınır
upper_bound <- Q3 + 1.5 * IQR # Üst sınır

# Aykırı değerleri belirleme
print(veri$y < lower_bound | veri$y > upper_bound)

correlation_table_1 = cor(veri) #Korelasyon DataFrame'e çevrilir
print(correlation_table_1) #Korelasyon tablosu ekrana yazdırılır

#Çoklu doğrusal regresyon modeli oluşturma
deneme_modeli = lm(y ~ x1+x2+x3+x4+x5)

#Modelin özetini alınabilir
summary(deneme_modeli)
#x3 harici tÜm bağımsız değişkenlerin y bağımlı değişkeni ile arasında anlamlı ilişki bulunmuştur

# Grafiklerin yan yana yerleştirilmesi ekran 2 sati 3 sütuna bölünür
par(mfrow=c(2,3)) #Ekran Bölünür
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16) #x1 kıyas
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16) #x2 kıyas
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16) #x3 kıyas
plot(x4, y, main = "x4 vs. y", xlab = "x4", ylab = "y", col = "orange", pch = 16) #x4 kıyas
plot(x5, y, main = "x5 vs. y", xlab = "x5", ylab = "y", col = "magenta", pch = 16) #x5 kıyas

# Veriyi test ve eğitim alt kümelerine bölme
split <- sample.split(y, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE) #Eğitim datasının ataması
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE) #Test verisinin ataması

# Eğitim ve Test Verilerinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)




# Ödev 2.kısım Hazır Veri Yükleme
par(mfrow=c(1,1)) # Bir önceki kısımdan kalan ekranı bölme işlemi kaldırılır




# Hazır veri Data kısmına yüklenir
veri = read_excel("C:/Users/deniz/Downloads/world_population_excel_workbook.xlsx")

# İleride değiştirilme ihtimaline karşı orijinal verinin kopyası oluşturulur
kopya = veri

# Veride NA değerleri ve başka sorun olabilecek tipte veriler var mı diye göz atılır
View(kopya)

# Veride NA değerleri olmadığından modifikasyonlar yapılabilir.
# Sütun başlıkları Türkçeleştirilebilir. Bunun için yeni başlıklar bir vektöre kaydedilip isimlere atanır.
yeni_basliklar = c("Sira","CCA3","Ulke","Baskent","Kita","2022 Nufusu","2020 Nufusu","2015 Nufusu","2010 Nufusu","2000 Nufusu","1990 Nufusu","1980 Nufusu","1970 Nufusu","Yuzolcumu (km²)","Yogunluk (km² basina)","Buyume Orani","Nufusunun Dunyadaki Yuzdesi")
names(kopya) = yeni_basliklar #Yeni başlıklar sütun adları yerine atanır
names(kopya) # Yeni başlıklar yazdırılır 

# Ülkelerin 52 yıllık yüzdelik nüfus değişimlerini bulmak için yeni bir sütun eklenebilir.
kopya$"Yuzdelik Degisim" = (kopya$`2022 Nufusu`- kopya$`1970 Nufusu`)/kopya$`1970 Nufusu`*100
names(kopya) # Yeni başlıklar yazdırılır 

# Ülkelerin nüfusları 1 milyon altı Düşük, 1-100 milyon arası Orta ve 50 Milyon üstü Yüksek olarak sınıflanır
kategorilenmis = mutate(kopya,
             Nufus_kategorileri = ifelse(
               kopya$`2022 Nufusu` < 1000000, "Dusuk",
               ifelse(
                 kopya$`2022 Nufusu` <=100000000, "Orta",
                 "Yuksek")
             )
       )
# 2022 Nüfus verilerinin özetini almak için summary fonksiyonu kullanılabilir.
# Bu sayede Min, Maks, Medyan, Ortalama ve Kartillerin bilgilerine ulaşabiliriz
# İstenilirse summary(kategorilenmis) seklinde fonksiyonu içine verinin kendisi  yazılabilir fakat fazla detaylı olduğundan istenilen sütunu yazmak daha uygundur
summary(kategorilenmis$`2022 Nufusu`)

# Veri, Korelasyon analizi öncesi sadece nümerik değişkenlerin bırakılacağı bir forma sokulur
is_numeric_nu_na <- function(col){  # Fonksiyon oluşturulur
  is_numeric <- is.numeric(col)     # Nümerikler alt değişkene atanır
  no_na <- !anyNA(col)              # NA olmayan değişkenler alt değişkene atanır
  return(is_numeric & no_na)        # Bu iki değişkeninin koşulunu sağlayan dönüt
}

Numerik <- Filter(is_numeric_nu_na, kategorilenmis) #Yukarıdaki fonksiyonun koşulları kullanılarak veri filtrelenir

# Nümerik değişkenlerin kendi aralarındaki korelasyonların olduğu tablo oluşturulur
correlation_table_2 = cor(Numerik)
view(correlation_table_2) # Korelasyon tablosu yazdırılır ve 0.7 üstü korelasyon var mı diye göz atılır

# Sonuca göre nüfusların birbiri arasındaki korelasyon harici anlamlı bir korelasyon yoktur.

# Yüksek korelasyonlar yazdırılır.
yuksek_cor = findCorrelation(correlation_table_2,cutoff = 0.7)
print(Numerik[,yuksek_cor]) # 0.7 üstü korelasyon olan sütunlar yazdırılır

# Sonuca göre nüfusların birbiri arasındaki korelasyon harici anlamlı bir korelasyon yoktur.


# Korelasyonların grafiği çizdirilir
corr_plot = corrplot(correlation_table_2,method = "color")