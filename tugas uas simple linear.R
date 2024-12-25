#nama:putu agus dhion devandra
#nim:2415091133
#kelas:SI 1 DPS

# 1. Pembuatan Data
# Menggunakan data simulasi untuk hubungan antara 'X' (misalnya jam belajar) dan 'Y' (nilai ujian)
set.seed(123) # Untuk pengulangan hasil yang sama
n <- 30
X <- rnorm(n, mean = 5, sd = 2)  # Variabel independen
Y <- 2.5 * X + rnorm(n, mean = 0, sd = 2)  # Variabel dependen dengan noise
data <- data.frame(X, Y)  # Membuat data frame
head(data)  # Melihat 6 data pertama

# 2. Visualisasi Awal
# Plot scatterplot untuk melihat hubungan
plot(data$X, data$Y, pch = 19, col = "blue", 
     main = "Scatterplot X vs Y", 
     xlab = "Jam Belajar (X)", 
     ylab = "Nilai Ujian (Y)")

# 3. Analisis Regresi Linear Sederhana
# Membuat model regresi
model <- lm(Y ~ X, data = data)
summary(model)  # Melihat hasil model

# 4. Uji Asumsi
# (a) Uji normalitas residual
residuals <- resid(model)
shapiro.test(residuals)  # Uji Shapiro-Wilk untuk normalitas

# (b) Homoskedastisitas (variansi residual konstan)
plot(fitted(model), residuals, pch = 19, 
     main = "Fitted Values vs Residuals", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Garis horizontal pada 0
library(car)
ncvTest(model)  # Uji formal Non-constant Variance Test

# (c) Uji independensi residual
durbinWatsonTest(model)  # Uji Durbin-Watson untuk autokorelasi

# 5. Visualisasi Hasil Model
# Scatterplot dengan garis regresi
plot(data$X, data$Y, pch = 19, col = "blue", 
     main = "Regresi Linear Sederhana", 
     xlab = "Jam Belajar (X)", 
     ylab = "Nilai Ujian (Y)")
abline(model, col = "red", lwd = 2)  # Garis regresi

# 6. Interpretasi
cat("\nInterpretasi Model:\n")
cat("Koefisien konstanta (intersep):", coef(model)[1], "\n")
cat("Koefisien slope (X):", coef(model)[2], "\n")
cat("Artinya, setiap penambahan 1 jam belajar, nilai ujian bertambah sekitar", round(coef(model)[2], 2), "poin.\n")
cat("R-squared:", summary(model)$r.squared, 
    "- Sekitar", round(summary(model)$r.squared * 100, 2), "% variasi dalam nilai ujian dijelaskan oleh jam belajar.\n")

