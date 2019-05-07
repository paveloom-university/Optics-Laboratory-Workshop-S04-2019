# Часть 1

I_1=c(59, 60, 56, 50, 43, 33, 24, 16, 9, 6, 59, 56, 50, 42, 32, 23, 14, 9, 6, 6)
I_2=c(59, 60, 57, 51, 43, 33, 24, 16, 10, 6, 59, 56, 50, 42, 32, 23, 15, 9, 6, 6)
I_3=c(59, 59, 56, 50, 42, 33, 24, 15, 9, 6, 59, 55, 49, 41, 32, 23, 14, 9, 6, 6)
delta_I=1

I_mean = (I_1+I_2+I_3)/3
I_mean

delta_I_mean = sqrt((I_1-I_mean)^2+(I_2-I_mean)^2+(I_3-I_mean)^2+delta_I^2)
delta_I_mean

I_0_mean = 59.666667
delta_I_0_mean = 1.290994

A = I_mean/I_0_mean
A

delta_A = (I_mean*delta_I_0_mean+I_0_mean*delta_I_mean)/I_0_mean^2
delta_A

phi = c(0, -10, -20, -30, -40, -50, -60, -70, -80, -90, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
delta_phi = 1

x=seq(-90,90,0.1)
x_rad=x*pi/180

library(splines)

df <- data.frame(a=phi[2:20], b=A[2:20])
ispl <- interpSpline(b ~ a, df)

plot(ispl, xlab = "φ, ˚", ylab = "γ")
points(phi, A)
lines(x, (cos(x_rad))^2, col='lightgray')
lines(x-6, (cos(x_rad))^2, col='darkgray')
arrows(phi, A+delta_A, phi, A-delta_A, code = 3, angle = 90, length = "0.05")
arrows(phi-delta_phi, A, phi+delta_phi, A, code = 3, angle = 90, length = "0.05")

B = (I_mean-6)/I_0_mean
max_B=max(B)

koef = 1/max_B
koef

delta_B = ((I_mean-6)*delta_I_0_mean+I_0_mean*delta_I_mean)/I_0_mean^2

C = koef*B
delta_C = koef*delta_B

plot(phi, C, xlab = "φ, ˚", ylab = "γ′")
lines(x-6, (cos(x_rad))^2, col='darkgray')
points(phi, B, pch=24)
arrows(phi, C+delta_C, phi, C-delta_C, code = 3, angle = 90, length = "0.05")
arrows(phi-delta_phi, C, phi+delta_phi, C, code = 3, angle = 90, length = "0.05")

# Часть 2

phi_r_r = c(54, 54, 54, 54, 53)
phi_r_g = c(32, 34, 32, 33, 34)
phi_r_y = c(47, 48, 46, 47, 47)
phi_l_r = c(128, 130, 128, 128, 130)
phi_l_g = c(142, 142, 142, 142, 142)
phi_l_y = c(134, 134, 134, 134, 134)

phi_r_r_mean = mean(phi_r_r)
phi_r_g_mean = mean(phi_r_g)
phi_r_y_mean = mean(phi_r_y)
phi_l_r_mean = mean(phi_l_r)
phi_l_g_mean = mean(phi_l_g)
phi_l_y_mean = mean(phi_l_y)

phi_r_r_mean
phi_r_g_mean
phi_r_y_mean
phi_l_r_mean
phi_l_g_mean
phi_l_y_mean

delta_phi = 1

delta_phi_r_r_mean = sqrt((phi_r_r[1]-phi_r_r_mean)^2+(phi_r_r[2]-phi_r_r_mean)^2+(phi_r_r[3]-phi_r_r_mean)^2+(phi_r_r[4]-phi_r_r_mean)^2+(phi_r_r[5]-phi_r_r_mean)^2+delta_phi^2)
delta_phi_r_g_mean = sqrt((phi_r_g[1]-phi_r_g_mean)^2+(phi_r_g[2]-phi_r_g_mean)^2+(phi_r_g[3]-phi_r_g_mean)^2+(phi_r_g[4]-phi_r_g_mean)^2+(phi_r_g[5]-phi_r_g_mean)^2+delta_phi^2)
delta_phi_r_y_mean = sqrt((phi_r_y[1]-phi_r_y_mean)^2+(phi_r_y[2]-phi_r_y_mean)^2+(phi_r_y[3]-phi_r_y_mean)^2+(phi_r_y[4]-phi_r_y_mean)^2+(phi_r_y[5]-phi_r_y_mean)^2+delta_phi^2)
delta_phi_l_r_mean = sqrt((phi_l_r[1]-phi_l_r_mean)^2+(phi_l_r[2]-phi_l_r_mean)^2+(phi_l_r[3]-phi_l_r_mean)^2+(phi_l_r[4]-phi_l_r_mean)^2+(phi_l_r[5]-phi_l_r_mean)^2+delta_phi^2)
delta_phi_l_g_mean = sqrt((phi_l_g[1]-phi_l_g_mean)^2+(phi_l_g[2]-phi_l_g_mean)^2+(phi_l_g[3]-phi_l_g_mean)^2+(phi_l_g[4]-phi_l_g_mean)^2+(phi_l_g[5]-phi_l_g_mean)^2+delta_phi^2)
delta_phi_l_y_mean = sqrt((phi_l_y[1]-phi_l_y_mean)^2+(phi_l_y[2]-phi_l_y_mean)^2+(phi_l_y[3]-phi_l_y_mean)^2+(phi_l_y[4]-phi_l_y_mean)^2+(phi_l_y[5]-phi_l_y_mean)^2+delta_phi^2)

delta_phi_r_r_mean
delta_phi_r_g_mean
delta_phi_r_y_mean
delta_phi_l_r_mean
delta_phi_l_g_mean
delta_phi_l_y_mean

phi_0 = 99

phi_r_r_final = abs(phi_r_r_mean - phi_0)
phi_r_g_final = abs(phi_r_g_mean - phi_0)
phi_r_y_final = abs(phi_r_y_mean - phi_0)
phi_l_r_final = abs(phi_l_r_mean - phi_0)
phi_l_g_final = abs(phi_l_g_mean - phi_0)
phi_l_y_final = abs(phi_l_y_mean - phi_0)

phi_r_r_final
phi_r_g_final
phi_r_y_final
phi_l_r_final
phi_l_g_final
phi_l_y_final

delta_phi_r_r_final = delta_phi_r_r_mean + 1
delta_phi_r_g_final = delta_phi_r_g_mean + 1
delta_phi_r_y_final = delta_phi_r_y_mean + 1
delta_phi_l_r_final = delta_phi_l_r_mean + 1
delta_phi_l_g_final = delta_phi_l_g_mean + 1
delta_phi_l_y_final = delta_phi_l_y_mean + 1

K_r = phi_r_r_final/phi_l_r_final
K_g = phi_r_g_final/phi_l_g_final
K_y = phi_r_y_final/phi_l_y_final

K_r
K_g
K_y

delta_K_r = (delta_phi_r_r_final*phi_l_r_final+delta_phi_l_r_final*phi_r_r_final)/phi_l_r_final^2
delta_K_g = (delta_phi_r_g_final*phi_l_g_final+delta_phi_l_g_final*phi_r_g_final)/phi_l_g_final^2
delta_K_y = (delta_phi_r_y_final*phi_l_y_final+delta_phi_l_y_final*phi_r_y_final)/phi_l_y_final^2

delta_K_r
delta_K_g
delta_K_y

# Часть 3

phi_1_vd_r = c(71,72,72,71,71)
phi_1_vd_g = c(57,57,58,57,56)
phi_1_vd_y = c(66,66,66,66,65)
phi_1_p_r = c(90,91,90,90,90)
phi_1_p_g = c(86,86,86,86,86)
phi_1_p_y = c(88,88,89,89,88)

phi_2_vd_r = c(82,82,82,82,82)
phi_2_vd_g = c(73,74,74,73,74)
phi_2_vd_y = c(79,79,79,78,78)
phi_2_p_r = c(94,94,94,94,94)
phi_2_p_g = c(91,91,91,91,91)
phi_2_p_y = c(93,92,92,93,93)

phi_1_vd_r_mean = mean(phi_1_vd_r)
phi_1_vd_g_mean = mean(phi_1_vd_g)
phi_1_vd_y_mean = mean(phi_1_vd_y)
phi_1_p_r_mean = mean(phi_1_p_r)
phi_1_p_g_mean = mean(phi_1_p_g)
phi_1_p_y_mean = mean(phi_1_p_y)

phi_2_vd_r_mean = mean(phi_2_vd_r)
phi_2_vd_g_mean = mean(phi_2_vd_g)
phi_2_vd_y_mean = mean(phi_2_vd_y)
phi_2_p_r_mean = mean(phi_2_p_r)
phi_2_p_g_mean = mean(phi_2_p_g)
phi_2_p_y_mean = mean(phi_2_p_y)

phi_1_vd_r_mean
phi_1_vd_g_mean
phi_1_vd_y_mean
phi_1_p_r_mean
phi_1_p_g_mean
phi_1_p_y_mean

phi_2_vd_r_mean
phi_2_vd_g_mean
phi_2_vd_y_mean
phi_2_p_r_mean
phi_2_p_g_mean
phi_2_p_y_mean

delta_phi = 1

delta_phi_1_vd_r_mean = sqrt((phi_1_vd_r[1]-phi_1_vd_r_mean)^2+(phi_1_vd_r[2]-phi_1_vd_r_mean)^2+(phi_1_vd_r[3]-phi_1_vd_r_mean)^2+(phi_1_vd_r[4]-phi_1_vd_r_mean)^2+(phi_1_vd_r[5]-phi_1_vd_r_mean)^2+delta_phi^2)
delta_phi_1_vd_g_mean = sqrt((phi_1_vd_g[1]-phi_1_vd_g_mean)^2+(phi_1_vd_g[2]-phi_1_vd_g_mean)^2+(phi_1_vd_g[3]-phi_1_vd_g_mean)^2+(phi_1_vd_g[4]-phi_1_vd_g_mean)^2+(phi_1_vd_g[5]-phi_1_vd_g_mean)^2+delta_phi^2)
delta_phi_1_vd_y_mean = sqrt((phi_1_vd_y[1]-phi_1_vd_y_mean)^2+(phi_1_vd_y[2]-phi_1_vd_y_mean)^2+(phi_1_vd_y[3]-phi_1_vd_y_mean)^2+(phi_1_vd_y[4]-phi_1_vd_y_mean)^2+(phi_1_vd_y[5]-phi_1_vd_y_mean)^2+delta_phi^2)
delta_phi_1_p_r_mean = sqrt((phi_1_p_r[1]-phi_1_p_r_mean)^2+(phi_1_p_r[2]-phi_1_p_r_mean)^2+(phi_1_p_r[3]-phi_1_p_r_mean)^2+(phi_1_p_r[4]-phi_1_p_r_mean)^2+(phi_1_p_r[5]-phi_1_p_r_mean)^2+delta_phi^2)
delta_phi_1_p_g_mean = sqrt((phi_1_p_g[1]-phi_1_p_g_mean)^2+(phi_1_p_g[2]-phi_1_p_g_mean)^2+(phi_1_p_g[3]-phi_1_p_g_mean)^2+(phi_1_p_g[4]-phi_1_p_g_mean)^2+(phi_1_p_g[5]-phi_1_p_g_mean)^2+delta_phi^2)
delta_phi_1_p_y_mean = sqrt((phi_1_p_y[1]-phi_1_p_y_mean)^2+(phi_1_p_y[2]-phi_1_p_y_mean)^2+(phi_1_p_y[3]-phi_1_p_y_mean)^2+(phi_1_p_y[4]-phi_1_p_y_mean)^2+(phi_1_p_y[5]-phi_1_p_y_mean)^2+delta_phi^2)

delta_phi_2_vd_r_mean = sqrt((phi_2_vd_r[1]-phi_2_vd_r_mean)^2+(phi_2_vd_r[2]-phi_2_vd_r_mean)^2+(phi_2_vd_r[3]-phi_2_vd_r_mean)^2+(phi_2_vd_r[4]-phi_2_vd_r_mean)^2+(phi_2_vd_r[5]-phi_2_vd_r_mean)^2+delta_phi^2)
delta_phi_2_vd_g_mean = sqrt((phi_2_vd_g[1]-phi_2_vd_g_mean)^2+(phi_2_vd_g[2]-phi_2_vd_g_mean)^2+(phi_2_vd_g[3]-phi_2_vd_g_mean)^2+(phi_2_vd_g[4]-phi_2_vd_g_mean)^2+(phi_2_vd_g[5]-phi_2_vd_g_mean)^2+delta_phi^2)
delta_phi_2_vd_y_mean = sqrt((phi_2_vd_y[1]-phi_2_vd_y_mean)^2+(phi_2_vd_y[2]-phi_2_vd_y_mean)^2+(phi_2_vd_y[3]-phi_2_vd_y_mean)^2+(phi_2_vd_y[4]-phi_2_vd_y_mean)^2+(phi_2_vd_y[5]-phi_2_vd_y_mean)^2+delta_phi^2)
delta_phi_2_p_r_mean = sqrt((phi_2_p_r[1]-phi_2_p_r_mean)^2+(phi_2_p_r[2]-phi_2_p_r_mean)^2+(phi_2_p_r[3]-phi_2_p_r_mean)^2+(phi_2_p_r[4]-phi_2_p_r_mean)^2+(phi_2_p_r[5]-phi_2_p_r_mean)^2+delta_phi^2)
delta_phi_2_p_g_mean = sqrt((phi_2_p_g[1]-phi_2_p_g_mean)^2+(phi_2_p_g[2]-phi_2_p_g_mean)^2+(phi_2_p_g[3]-phi_2_p_g_mean)^2+(phi_2_p_g[4]-phi_2_p_g_mean)^2+(phi_2_p_g[5]-phi_2_p_g_mean)^2+delta_phi^2)
delta_phi_2_p_y_mean = sqrt((phi_2_p_y[1]-phi_2_p_y_mean)^2+(phi_2_p_y[2]-phi_2_p_y_mean)^2+(phi_2_p_y[3]-phi_2_p_y_mean)^2+(phi_2_p_y[4]-phi_2_p_y_mean)^2+(phi_2_p_y[5]-phi_2_p_y_mean)^2+delta_phi^2)

delta_phi_1_vd_r_mean
delta_phi_1_vd_g_mean
delta_phi_1_vd_y_mean
delta_phi_1_p_r_mean
delta_phi_1_p_g_mean
delta_phi_1_p_y_mean

delta_phi_2_vd_r_mean
delta_phi_2_vd_g_mean
delta_phi_2_vd_y_mean
delta_phi_2_p_r_mean
delta_phi_2_p_g_mean
delta_phi_2_p_y_mean

phi_0 = 99

phi_1_vd_r_final = abs(phi_1_vd_r_mean - phi_0)
phi_1_vd_g_final = abs(phi_1_vd_g_mean - phi_0)
phi_1_vd_y_final = abs(phi_1_vd_y_mean - phi_0)
phi_1_p_r_final = abs(phi_1_p_r_mean - phi_0)
phi_1_p_g_final = abs(phi_1_p_g_mean - phi_0)
phi_1_p_y_final = abs(phi_1_p_y_mean - phi_0)

phi_2_vd_r_final = abs(phi_2_vd_r_mean - phi_0)
phi_2_vd_g_final = abs(phi_2_vd_g_mean - phi_0)
phi_2_vd_y_final = abs(phi_2_vd_y_mean - phi_0)
phi_2_p_r_final = abs(phi_2_p_r_mean - phi_0)
phi_2_p_g_final = abs(phi_2_p_g_mean - phi_0)
phi_2_p_y_final = abs(phi_2_p_y_mean - phi_0)

phi_1_vd_r_final
phi_1_vd_g_final
phi_1_vd_y_final
phi_1_p_r_final
phi_1_p_g_final
phi_1_p_y_final

phi_2_vd_r_final
phi_2_vd_g_final
phi_2_vd_y_final
phi_2_p_r_final
phi_2_p_g_final
phi_2_p_y_final

delta_phi_1_vd_r_final = delta_phi_1_vd_r_mean + 1
delta_phi_1_vd_g_final = delta_phi_1_vd_g_mean + 1
delta_phi_1_vd_y_final = delta_phi_1_vd_y_mean + 1
delta_phi_1_p_r_final = delta_phi_1_p_r_mean + 1
delta_phi_1_p_g_final = delta_phi_1_p_g_mean + 1
delta_phi_1_p_y_final = delta_phi_1_p_y_mean + 1

delta_phi_2_vd_r_final = delta_phi_2_vd_r_mean + 1
delta_phi_2_vd_g_final = delta_phi_2_vd_g_mean + 1
delta_phi_2_vd_y_final = delta_phi_2_vd_y_mean + 1
delta_phi_2_p_r_final = delta_phi_2_p_r_mean + 1
delta_phi_2_p_g_final = delta_phi_2_p_g_mean + 1
delta_phi_2_p_y_final = delta_phi_2_p_y_mean + 1

K_1_r = phi_1_vd_r_final/phi_1_p_r_final
K_1_g = phi_1_vd_g_final/phi_1_p_g_final
K_1_y = phi_1_vd_y_final/phi_1_p_y_final

K_2_r = phi_2_vd_r_final/phi_2_p_r_final
K_2_g = phi_2_vd_g_final/phi_2_p_g_final
K_2_y = phi_2_vd_y_final/phi_2_p_y_final

K_1_r
K_1_g
K_1_y

K_2_r
K_2_g
K_2_y

delta_K_1_r = (phi_1_vd_r_final*delta_phi_1_p_r_final + phi_1_p_r_final*delta_phi_1_vd_r_final)/phi_1_p_r_final^2
delta_K_1_g = (phi_1_vd_g_final*delta_phi_1_p_g_final + phi_1_p_g_final*delta_phi_1_vd_g_final)/phi_1_p_g_final^2
delta_K_1_y = (phi_1_vd_y_final*delta_phi_1_p_y_final + phi_1_p_y_final*delta_phi_1_vd_y_final)/phi_1_p_y_final^2

delta_K_2_r = (phi_2_vd_r_final*delta_phi_2_p_r_final + phi_2_p_r_final*delta_phi_2_vd_r_final)/phi_2_p_r_final^2
delta_K_2_g = (phi_2_vd_g_final*delta_phi_2_p_g_final + phi_2_p_g_final*delta_phi_2_vd_g_final)/phi_2_p_g_final^2
delta_K_2_y = (phi_2_vd_y_final*delta_phi_2_p_y_final + phi_2_p_y_final*delta_phi_2_vd_y_final)/phi_2_p_y_final^2

delta_K_1_r
delta_K_1_g
delta_K_1_y

delta_K_2_r
delta_K_2_g
delta_K_2_y

K_real_1 = 8
K_real_2 = 2.5

delta_K_real_i = 0.05

K_real = K_real_1/K_real_2
K_real

delta_K_real = (K_real_1*delta_K_real_i+K_real_2*delta_K_real_i)/K_real_2^2
delta_K_real
