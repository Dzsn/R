#F stringlerin görevine benzer glue fonksiyonunu yüklüyoruz
install.packages("glue")

# F stringlerin g"glue"# F stringlerin görevine benzer glue fonksiyonunu çağırıyoruz
library(glue)

# Hesap makinesi 

# Toplama
toplama <- function(a, b) {
  return(a + b)
}

# Çıkarma 
cikarma <- function(a, b) {
  return(a - b)
}

# Çarpma 
carpma <- function(a, b) {
  return(a * b)
}

# Bölme 
bolme <- function(a, b) {
  if (b == 0) {
    return("Payda 0 olamaz!")
  } else {
    return(a / b)
  }
}

# Kuvvet
kuvvet <- function(a,b) {
  return(a^b)
}

sayi1 <- as.numeric(readline("Birinci değeri giriniz: "))
sayi2 <- as.numeric(readline("İkinci değeri giriniz: "))

# İşlem
cat("Yapmak istediğiniz işlemi seçin:
      1. Toplama
      2. Çıkarma
      3. Çarpma
      4. Bölme
      5. Kuvvet")

secim <- as.integer(readline("Seçiminizi girin: "))

switch(secim,
       "1" = glue("Toplamanın sonucu: {toplama(sayi1, sayi2)}"),
       "2" = glue("Çıkarmanın Sonucu: {cikarma(sayi1, sayi2)}"),
       "3" = glue("Çarpımın Sonucu: {carpma(sayi1, sayi2)}"),
       "4" = glue("Bölümün Sonucu: {bolme(sayi1, sayi2)}"),
       "5" = glue("Üssünü Almanın Sonucu: {kuvvet(sayi1,sayi2)}"),
       print("Geçersiz seçim!")
)



# Mod değerini kolayca hesaplamak için DescTools paketi yüklenir
install.packages(c("DescTools","glue"))

# Paket kullanıma çağrılır
library(DescTools)
library(glue)

# Verilen Vektörün Maks, Min, Medyan, Mod değerleri ve sıralanmış halini gösteren fonksiyon
vektor_ozeti = function(vektor) {
  # Maksimumun Hesaplanması
  maks = max(vektor)
  
  # Minumumun Hesaplanması
  minimum = min(vektor)
    
  # Medyanın Hesaplanması
  medyan = median(vektor)
  
  # Modun Hesaplanması
  mod_degeri = Mode(vektor)
  
  
  # Ortalamanın Hesaplanması
  ortalama = mean(vektor)
  
  # Vektörün Sıralanmış Hali
  siralanmis = sort(vektor)
  
  return(list(maksimum_deger = maks,min_deger = minimum,medyan_deger = medyan,mod_deger = mod_degeri,ortalama_deger = ortalama ,siralanmis_vektor = siralanmis))
}

vektor = c(15,6,24,84,943,23,74,23,97,56,23)

sonuc = vektor_ozeti(vektor)

glue("Maksimum: {sonuc$maksimum_deger}
      Minumum: {sonuc$min_deger}
      Medyan: {sonuc$medyan_deger}
      Mod: {sonuc$mod_deger}
      Ortalama: {round(sonuc$ortalama_deger,2)}
      Vektörün Sıralanmış Hali: {glue_collapse(sonuc$siralanmis_vektor, sep = ', ')}")
