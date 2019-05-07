
# Часть 1

sigma_t = c(0.415, 0.402, 0.382)

sigma = mean(sigma_t)

sigma_r = 0
for (i in 1:3) {
sigma_r = sigma_r + abs(sigma_t[i] - sigma)
}

sigma_o = 0.002

delta_sigma = sqrt(sigma_o^2 + sigma_r^2)
delta_sigma

sigma

R_1_t = c(2.05, 1.91, 1.74)

R_1 = mean(R_1_t)
R_1

R_1_r = 0
for (i in 1:3) {
  R_1_r = R_1_r + abs((R_1_t[i] - R_1))
}

R_1_o = 0.01

delta_R_1 = sqrt(R_1_o^2 + R_1_r^2)  
delta_R_1


  
R_2_t = c(1.42, 1.45, 1.14)

R_2 = mean(R_2_t)
R_2

R_2_r = 0
for (i in 1:3) {
  R_2_r = R_2_r + abs((R_2_t[i] - R_2))
}

R_2_o = 0.01

delta_R_2 = sqrt(R_2_o^2 + R_2_r^2)  
delta_R_2

D_1_t = c(27.5, 27.75, 28.50)

D_1 = mean(D_1_t)
D_1

D_1_r = 0
for (i in 1:3) {
  D_1_r = D_1_r + abs((D_1_t[i] - D_1))
}

D_1_o = 0.05

delta_D_1 = sqrt(D_1_o^2 + D_1_r^2)  
delta_D_1


D_2_t = c(49.6, 47.8, 45)

D_2 = mean(D_2_t)
D_2

D_2_r = 0
for (i in 1:3) {
  D_2_r = D_2_r + abs((D_2_t[i] - D_2))
}

D_2_o = 0.05

delta_D_2 = sqrt(D_2_o^2 + D_2_r^2)  
delta_D_2

# Короч, путаница. Вверху усреднение, но оно не нужно
# Вычисляю для измерения №2


sigma = 0.402
delta_sigma = 0.002
R_1 = 1.91
R_2 = 1.45
delta_R = 0.01

D_1 = 27.75
D_2 = 47.80
delta_D = 0.05

D_1_mm = D_1 * 10
D_2_mm = D_2 * 10

delta_D_mm = delta_D * 10

lambda = sigma * sqrt(R_1 * R_2) / (D_2_mm - D_1_mm) * (sqrt(R_1) - sqrt(R_2)) / (sqrt(R_1) + sqrt(R_2))
lambda

lambda_nm = lambda * 1000000
lambda_nm

p1 = delta_sigma / sigma
p1

p2 = delta_D_mm / (D_2_mm - D_1_mm)
p2  
  
p3 = delta_D_mm / (D_2_mm - D_1_mm)
p3

p4 = delta_R / 2 * (1 / ( sqrt(R_1) * (sqrt(R_1) - sqrt(R_2)) ) - 1 / ( sqrt(R_1) * (sqrt(R_1) + sqrt(R_2)) ) + 1/R_1 ) 
p4

p5 = delta_R / 2 * (-1 / ( sqrt(R_2) * (sqrt(R_1) - sqrt(R_2)) ) + 1 / ( sqrt(R_2) * (sqrt(R_1) + sqrt(R_2)) ) + 1/R_2 ) 
p5

rel_delta_lambda = sqrt(p1^2 + p2^2 + p3^2 + p4^2 +p5^2)
rel_delta_lambda

delta_lambda = rel_delta_lambda * lambda

delta_lambda_nm = delta_lambda * 1000000
delta_lambda_nm

i=seq(1,3)

plot(i, lambda_nm, xlab = " ", ylab = " ", ylim = c(550, 950))
arrows(i, lambda_nm+delta_lambda_nm, i, lambda_nm-delta_lambda_nm, code = 3, angle = 90, length = "0.05")

library(ggplot2)

# pd <- position_dodge(0.1)

# ggplot() + geom_point(aes(x=i, y=lambda_nm)) + ylim(540,950) + xlim(0.9, 3.1) + 
#  xlab(' ') + ylab(' ') + geom_errorbar(aes(x=i,ymin=lambda_nm-delta_lambda_nm, ymax=lambda_nm+delta_lambda_nm), position=pd, color = 'black', width = 0.05) + 
#  geom_hline(yintercept=725.5, color = "blue", size = 0.8) + geom_smooth(aes(x=i, y=lambda_nm), method = 'lm', formula = y~y, fullrange = TRUE, level = 0.58, color = 'blue')
# df1 = data.frame(x=i, y=lambda_nm)

# model1 = lm(data=df1, y ~ y)
# summary(model1)


# Часть 2

y = c(4.59, 3.72, 2.63, 1.63, 0.65)
s = c(1.94, 2.28, 2.16, 2.65, 3.00)

delta_y = 0.01
delta_s = 0.04

pd <- position_dodge(0.1)

ggplot() + geom_point(aes(x=s, y=y)) + geom_smooth(aes(x=s, y=y), method = 'lm', fullrange = TRUE) + 
  geom_errorbar(aes(x=s,ymin=y-delta_y, ymax=y+delta_y), position=pd, color = 'black', width = 0.0) + 
  geom_errorbarh(aes(y=y,xmin=s-delta_s, xmax=s+delta_s), position=pd, color = 'black', height = 0.2) + 
  xlab(' ') + ylab(' ')

df2 = data.frame(x=s, y=y)

model2 = lm(data=df2, y ~ x)
y_2_hat = fitted(model2)
cor(y,y_2_hat)
summary(model2)

dy_ds = 3.5001
delta_dy_ds = 0.7838

sigma = 0.402
delta_sigma = 0.002

ds_dy = 1/dy_ds

b_m = sigma * ds_dy
b_m


delta_b_m = delta_sigma * ds_dy + delta_dy_ds / (dy_ds)^2
delta_b_m

n = 1.52
lambda = 230
delta_lambda = 7

lambda_mm = lambda / 1000000
delta_lambda_mm = delta_lambda / 1000000

lambda_mm
delta_lambda_mm

alpha = 1 / (n - 1) * lambda_mm / (2 * sigma) * (dy_ds + 1)
alpha

delta_alpha = 1 / (n - 1) * ( ((delta_lambda_mm * sigma + delta_sigma * lambda_mm) / (2 * sigma^2)) * (dy_ds + 1) + (delta_dy_ds + 1) * lambda_mm / (2 * sigma) )
delta_alpha

omega = lambda_mm / b_m
omega

delta_omega = 1/2 * (delta_lambda_mm*b_m + delta_b_m*lambda_mm) / b_m^2
delta_omega
