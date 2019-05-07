install.packages("xlsx", dep = T)
library("xlsx")

A <- read.xlsx("D:/Paveloom/Лабораторные работы/2 - №39/III (объединение таблиц, часть 2)/Сравнение опорных спектров.xlsx",sheetIndex = 1)

plot(A[,1], A[,2], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Интенсивность, I, %', ylim = c(0,100))
lines(A[,1],A[,3], col="gray")

plot(A[,1], A[,4], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Коэффициент пропускания света, T, %', ylim = c(0,100))

plot(A[,1], A[,5], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Оптическая плотность среды', ylim = c(0,2))


# Сравнение опорных спектров
plot(A[,1], A[,2], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Интенсивность, I, %', ylim = c(0,60))
lines(A[,1],A[,3], col="green")
lines(A[,1],A[,4], col="red")
lines(A[,1],A[,5], col="blue")

# Сравнение экстинкций
plot(A[,1], A[,2], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Оптическая плотность среды', ylim = c(0,1.1))
lines(A[,1],A[,3], col="green")
lines(A[,1],A[,4], col="red")

which.max(A[,4])
A[768,]

plot(A[,1], Mod(A[,3]-A[,2]), type='l', xlab = 'Длина волны, λ, нм', ylab = 'Разность интенсивностей, ΔI, %', ylim = c(0,2.5), col='green')
lines(A[,1],Mod(A[,4]-A[,2]), col="red")
lines(A[,1],Mod(A[,5]-A[,2]), col="blue")

max(Mod(A[,4]-A[,2]))
which.max(Mod(A[,4]-A[,2]))
A[871,]

A[772,]

sd(Mod(A[,3]-A[,2]))
sd(Mod(A[,4]-A[,2]))
sd(Mod(A[,5]-A[,2]))
sd(Mod(A[,6]-A[,2]))
sd(Mod(A[,7]-A[,2]))

plot(A[,1], A[,2], type='l', xlab = 'Длина волны, λ, нм', ylab = 'Коэффициент пропускания света, T, %', ylim = c(0,100),col='green')
lines(A[,1],A[,3], col="yellow")
lines(A[,1],A[,4], col="black")

plot(A[,1], Mod(A[,2]*A[,3]/100 - A[,4]), type='l', xlab = 'Длина волны, λ, нм', ylab = 'Разность коэффициентов пропускания света, ΔT, %', ylim = c(0,15))
plot(A[,1], Mod(A[,5]*A[,6]/100 - A[,7]), type='l', xlab = 'Длина волны, λ, нм', ylab = 'Разность коэффициентов пропускания света, ΔT, %', ylim = c(0,15))


# Интерполяция
library(splines)

C <- data.frame(cum.pop = c(0.33, 0.713, 0.966),
                cum.cost = c(1.00*10^(-4), 2.00*10^(-4), 3.0*10^(-4)))
plot(C, type = 'lines')

x=seq(0.25,1.1,0.001)
y=0.0000634711 + 0.0000410821*x + 0.000210944*x^2
plot(x,y, type = 'lines', ylab = 'Концентрация, C', xlab = 'Экстинкция, E')
points(c(0.33, 0.713, 0.966),c(1.00*10^(-4), 2.00*10^(-4), 3.0*10^(-4)))
abline(v = 0.816, col = 'red')
x=0.816
y

ispl <- interpSpline(cum.pop ~ cum.cost,  C)

# plots the interpolated spline
plot(ispl, xlab = 'Концентрация, C', ylab = 'Экстинкция, E')    
points(C[,2], C[,1], pch=19)
