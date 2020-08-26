# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Gergana Daskalova 06/11/2016 University of Edinburgh

# Loading the necessary packages - here dplyr by Hadley Whickam from which we will use the filter() function
library(dplyr)

# Memanggil file edidiv.csv
edidiv <- read.csv('Intro_to_R/edidiv.csv')

# memanggil 6 row teratas
head(edidiv)
# memanggil 6 row terbawah
tail(edidiv)
# menunjukan struktur data yang di data.frame
str(edidiv)
dim(edidiv) # mengetahui dimensi variabel
summary(edidiv) #ringkasan 5 statistika

head(edidiv$taxonGroup) # menampilkan 6 baris paling atas pada kolom pertama saja
class(edidiv$taxonGroup) # mengecheck jenis tipe variable

edidiv$taxonGroup <- as.factor(edidiv$taxonGroup) # Mengubah jenis tipe variabel dari chr ke factor
summary(edidiv$taxonGroup) # menampilkan summary dari kolom taxongroup

Bettle <- filter(edidiv, taxonGroup == 'Beetle') # Memfilter untuk mengambil kondisi jenis tertentu
Bird <- filter(edidiv, taxonGroup == 'Bird') # Memfilter untuk mengambil taxa burung
Butterfly <- filter(edidiv, taxonGroup == 'Butterfly')
Dragonfly <- filter(edidiv, taxonGroup == 'Dragonfly')
Flowering.Plants <- filter(edidiv, taxonGroup == 'Flowering.Plants')
Fungus <- filter(edidiv, taxonGroup == 'Fungus')
Hymenopteran <- filter(edidiv, taxonGroup == 'Hymenopteran')
Lichen <- filter(edidiv, taxonGroup == 'Lichen')
Liverwort <- filter(edidiv, taxonGroup == 'Liverwort')
Mammal <- filter(edidiv, taxonGroup == 'Mammal')
Mollusc <- filter(edidiv, taxonGroup == 'Mollusc')


bettle_sp <- length(unique(Bettle$taxonName)) # Unique() untuk menghilangkan spesies yang double/ganda
bird_sp <- length(unique(Bird$taxonName)) # length() untuk menghitung total spesies
butterfly_sp <- length(unique(Butterfly$taxonName))
dragonfly_sp <- length(unique(Dragonfly$taxonName))
flowering.plants_sp <- length(unique(Flowering.Plants$taxonName))
fungus_sp <- length(unique(Fungus$taxonName))
hymenopteran_sp <- length(unique(Hymenopteran$taxonName))
lichen_sp <- length(unique(Lichen$taxonName))
liverwort_sp <- length(unique(Liverwort$taxonName))
mammal_sp <- length(unique(Mammal$taxonName))
mollusc_sp <- length(unique(Mollusc$taxonName))

biodiv <- c(bettle_sp, bird_sp, butterfly_sp, dragonfly_sp, flowering.plants_sp, 
            fungus_sp, hymenopteran_sp, lichen_sp, liverwort_sp, mammal_sp, mollusc_sp) # Membuat vector dengan c()
names(biodiv) <- c('Bettle',
                   'Bird',
                   'Butterfly',
                   'Dragonfly',
                   'Flowering.Plants',
                   'Fungus',
                   'Hymenopteran',
                   'Lichen',
                   'Liverwort',
                   'Mammal',
                   'Mollusc')

?barplot # untuk membuka petunjuk 
png('barplot.png', width = 1600, height = 600) # save file image to png
barplot(biodiv, xlab = 'Taxa', ylab = 'Number of species', ylim = c(0,600), cex.names = 1.5, 
        cex.axis = 1.5, cex.lab = 1.5) # visualisasi dengan barplot
dev.off()

# Membuat object vector bernama taxa yang mengandung nama taxa
taxa <- c('Bettle',
          'Bird',
          'Butterfly',
          'Dragonfly',
          'Flowering.Plants',
          'Fungus',
          'Hymenopteran',
          'Lichen',
          'Liverwort',
          'Mammal',
          'Mollusc')
#  Mengubah object ini menjadi factor, sebagai contoh categorical variabel
taxa_f <- factor(taxa)

# Menggabungkan semua value dari jumlah spesies masing masing ke dalam variabel richness
richness <- c(bettle_sp, 
              bird_sp, 
              butterfly_sp, 
              dragonfly_sp, 
              flowering.plants_sp, 
              fungus_sp, 
              hymenopteran_sp, 
              lichen_sp, 
              liverwort_sp, 
              mammal_sp, 
              mollusc_sp)

# Membuat data frame dari 2 vectors
biodata <- data.frame(taxa_f, richness)
biodata
# Menyimpan file
write.csv(biodata, file = 'biodata.csv')

png('barplot2.png', width = 1600, height = 600)
# names.arg untuk label bar, xlab untuk label x axis, ylab untuk label y axis
barplot(biodata$richness, names.arg = c('Bettle',
                                        'Bird',
                                        'Butterfly',
                                        'Dragonfly',
                                        'Flowering.Plants',
                                        'Fungus',
                                        'Hymenopteran',
                                        'Lichen',
                                        'Liverwort',
                                        'Mammal',
                                        'Mollusc'),
        xlab = 'Taxa', ylab = 'Number of species', ylim = c(0, 600))
dev.off()

# membuat vector bird_sp berisi spesies burung
bird_sp <- c('sparrow',
             'kingfisher',
             'eagle',
             'hummingbird',
             'sparrow',
             'kingfisher',
             'eagle',
             'hummingbird',
             'sparrow',
             'kingfisher',
             'eagle',
             'hummingbird')
# membuat vector wingspan berisi nilai sayang burung
wingspan <- c(22, 26, 195, 8, 24, 23, 201, 9, 21, 25, 185, 9)
# membuat data frame
birds <- data.frame(bird_sp, wingspan)
# Mencari nilai mean dengan menfilter berdasarkan spesies burung
sparrow_sp <- mean(filter(birds, birds$bird_sp == 'sparrow')$wingspan)
kingfisher_sp <- mean(filter(birds, birds$bird_sp == 'kingfisher')$wingspan)
eagle_sp <- mean(filter(birds, birds$bird_sp == 'eagle')$wingspan)
hummingbird_sp <- mean(filter(birds, birds$bird_sp == 'hummingbird')$wingspan)
# Menggabungkan nilai mean menjadi data vector
wingspan_means <- c(sparrow_sp,
                    kingfisher_sp,
                    eagle_sp,
                    hummingbird_sp)

# mengambil nama burung agar tidak ganda
bird_sp <- unique(bird_sp)

# membuat data frame dengan menggabungkan jenis burung dan mean wingspan
birds_data <- data.frame(bird_sp, wingspan_means)

# Melihat isi dataframe bird_data
birds_data
class(birds_data$bird_sp)

# menyimpan ke dalam file csv
write.csv(birds_data, file = 'wingspan_mean.csv')

# Meng-export barplot ke png file
png(filename = 'barplot_wingspan.png', width = 800, height = 600)
# Membuat barplot
barplot(birds_data$wingspan_means, names.arg = bird_sp,
        xlab = 'Bird Species', ylab = 'Average of Wingspan', ylim = c(0, 250),
        col = 'gold')
# menghentikan proses png
dev.off()
