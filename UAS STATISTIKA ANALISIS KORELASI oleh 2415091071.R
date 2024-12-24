# Nama: Ni Wayan Risca Yulita Dewi
# Kelas: 1DPS
# NIM: 2415091071

# ANALISIS KORELASI

# 1. Data
# Membuat data simulasi tentang jam tidur dan produktivitas
data <- data.frame(
  jam_tidur = c(6, 7, 8, 5, 6.5, 7.5, 8, 6, 5.5, 7, 6.2, 8, 6.8, 7.3, 5.7, 8.5, 6.5, 7.8, 5.2, 7),
  produktivitas = c(75, 80, 85, 70, 78, 82, 88, 77, 73, 81, 76, 86, 79, 84, 72, 89, 80, 85, 68, 83)
)

# Tampilkan data untuk memastikan data sudah benar
print(data)

# 2. Uji Asumsi
# a. Uji normalitas untuk jam_tidur
shapiro_tidur <- shapiro.test(data$jam_tidur)
# Uji normalitas untuk produktivitas
shapiro_produktivitas <- shapiro.test(data$produktivitas)

# Menampilkan hasil uji normalitas
print("Hasil uji normalitas untuk jam tidur:")
print(shapiro_tidur)
print("Hasil uji normalitas untuk produktivitas:")
print(shapiro_produktivitas)

# b. Uji Linieritas: Membuat scatter plot untuk melihat hubungan antara jam tidur dan produktivitas
# Dengan scatter plot, kita dapat mengecek apakah ada pola linier antara kedua variabel
plot(data$jam_tidur, data$produktivitas,
     main = "Hubungan Jam Tidur dan Produktivitas",
     xlab = "Jam Tidur (X)",
     ylab = "Produktivitas (Y)",
     pch = 19, col = "blue")

# c. Deteksi Outlier: Membuat boxplot untuk memeriksa adanya outlier pada masing-masing variabel
# Boxplot membantu kita melihat distribusi dan nilai-nilai ekstrim
boxplot(data$jam_tidur, main = "Boxplot Jam Tidur", col = "lightblue")
boxplot(data$produktivitas, main = "Boxplot Produktivitas", col = "lightgreen")

# 3. Analisis
# Menghitung korelasi antara jam_tidur dan produktivitas menggunakan uji Pearson
# Ini membantu kita mengetahui sejauh mana hubungan linier antara kedua variabel
correlation <- cor.test(data$jam_tidur, data$produktivitas, method = "pearson")
print("Hasil Uji Korelasi:")
print(correlation)

# 4. Visualisasi
library(ggplot2)
# Membuat visualisasi scatter plot dengan garis regresi
# Menambahkan garis regresi untuk melihat hubungan linier yang lebih jelas
ggplot(data, aes(x = jam_tidur, y = produktivitas)) +
  geom_point(color = "blue") + # Titik data
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Garis regresi
  labs(title = "Hubungan Jam Tidur dan Produktivitas dengan Garis Regresi",
       x = "Jam Tidur (X)",
       y = "Produktivitas (Y)")

# 5. Interpretasi
cat("\nHasil analisis korelasi:\n")
cat("Korelasi Pearson:", correlation$estimate, "\n")
cat("P-value:", correlation$p.value, "\n")

# Menyimpulkan apakah korelasi antara jam tidur dan produktivitas signifikan
if (correlation$p.value < 0.05) {
  cat("Terdapat korelasi signifikan antara jam tidur dan produktivitas.\n")
} else {
  cat("Tidak terdapat korelasi signifikan antara jam tidur dan produktivitas.\n")
}

