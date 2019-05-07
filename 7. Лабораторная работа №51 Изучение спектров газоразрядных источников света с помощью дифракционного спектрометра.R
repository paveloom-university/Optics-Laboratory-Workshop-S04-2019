# Часть 1

phi_1_n = c(15.33, 15.72, 16.35, 17.20, 20.32, 23.23, 24.63, 31.97, 34.08, 35.82, 36.62, 44.38, 52.70)
phi_2_n = c(195.33, 195.72, 196.33, 197.20, 200.30, 203.23, 204.62, 211.93, 214.08, 215.80, 216.62, 224.37, 232.70)

phi_n = ((phi_2_n-180)+phi_1_n)/2
phi_n

phi_1 = c(15.33, 15.72, 16.34, 17.20, 20.31, 23.23)
phi_2 = c(31.95, 34.08, 35.81, 36.62, 44.38, 52.70)

lambda = c(4471.5, 4713.1, 4921.9, 5047.7, 5875.6, 6678.2)

x_1 = lambda
x_2 = 2*lambda
x_2

delta_phi_1 = 0.01
delta_phi_2 = 0.01

rad_1 = phi_1*pi/180
rad_2 = phi_2*pi/180

delta_rad_1 = delta_phi_1*pi/180
delta_rad_2 = delta_phi_2*pi/180

y_1 = sin(rad_1)
y_2 = sin(rad_2)
y_1
y_2

delta_y_1 = cos(rad_1)*delta_rad_1
delta_y_2 = cos(rad_2)*delta_rad_2
delta_y_1
delta_y_2

library(ggplot2)

pd <- position_dodge(0.1)

ggplot() + geom_point(aes(x=x_1, y=y_1)) + geom_smooth(aes(x=x_1, y=y_1), method = 'lm', fullrange = TRUE) + 
  xlab(' ') + ylab(' ') + geom_errorbar(aes(x=x_1,ymin=y_1-delta_y_1, ymax=y_1+delta_y_1), position=pd, color = 'black', width = 0.05) + 
  geom_errorbarh(aes(y=y_1,xmin=x_1-delta_x_1, xmax=x_1+delta_x_1), position=pd, color = 'black', height = 0.05)

model_1 = lm(y_1~x_1)
y_1_hat = fitted(model_1)
cor(y_1,y_1_hat)
summary(model_1)

ggplot() + geom_point(aes(x=x_2, y=y_2)) + geom_smooth(aes(x=x_2, y=y_2), method = 'lm', fullrange = TRUE) + 
  xlab(' ') + ylab(' ')

model_2 = lm(y_2~x_2)
y_2_hat = fitted(model_2)
cor(y_2,y_2_hat)
summary(model_2)

# Часть 2

phi_1_n_2 = c(15.17, 16.1, 16.53, 17.53, 22.05, 22.45)
phi_2_n_2 = c(195.15, 196.1, 196.52, 197.55, 202.05, 202.42)

phi_n_2 = ((phi_2_n_2-180)+phi_1_n_2)/2
phi_n_2

delta_phi_n_2 = 0.01

rad_3 = phi_n_2*pi/180
delta_rad_3 = delta_phi_n_2*pi/180

y_3 = sin(rad_3)
y_3
delta_y_3 = cos(rad_3)*delta_rad_3
delta_y_3

b_1 = -1.537e-02
delta_b_1 = 1.207e-02

n_1 = 6.138e-05
delta_n_1 = 2.261e-06

lambda_calc = (y_3 - b_1)/n_1
lambda_calc

delta_y_3_minus_b_1 = delta_y_3 + delta_b_1

delta_lambda_calc = (delta_y_3_minus_b_1*n_1+delta_n_1*(y_3 - b_1))/n_1^2
delta_lambda_calc

pd <- position_dodge(0.1)

ggplot() + geom_point(aes(x=x_1, y=y_1)) + geom_smooth(aes(x=x_1, y=y_1), method = 'lm', fullrange = TRUE) + 
  xlab(' ') + ylab(' ') + geom_hline(yintercept=y_3, color = "red") + geom_vline(xintercept=lambda_calc, color = 'green') + 
  geom_errorbarh(aes(y=y_3,xmin=lambda_calc-delta_lambda_calc, xmax=lambda_calc+delta_lambda_calc), position=pd, color = 'black')

# Улучшение для части 2

b_2 = -8.949e-03
delta_b_2 = 4.774e-03

n_2 = 6.023e-05
delta_n_2 = 4.471e-07

fix_lambda_calc = (y_3 - b_2)/n_2
fix_lambda_calc

delta_y_3_minus_b_2 = delta_y_3 + delta_b_2

delta_fix_lambda_calc = (delta_y_3_minus_b_2*n_2+delta_n_2*(y_3 - b_2))/n_2^2
delta_fix_lambda_calc

ggplot() + geom_point(aes(x=x_2, y=y_2)) + geom_smooth(aes(x=x_2, y=y_2), method = 'lm', fullrange = TRUE) + 
  xlab(' ') + ylab(' ') + geom_hline(yintercept=y_3, color = "red") + geom_vline(xintercept=fix_lambda_calc, color = 'green') + 
  geom_errorbarh(aes(y=y_3,xmin=fix_lambda_calc-delta_fix_lambda_calc, xmax=fix_lambda_calc+delta_fix_lambda_calc), position=pd, color = 'black')

# Просмотр диапазонов погрешностей

lambda_tabl = c(4413.0, 4678.1, 4799.9, 5154.7, 6325.2, 6438.5)

ggplot() + geom_vline(xintercept=lambda_tabl, color = "red") + 
  geom_rect(aes(xmin=fix_lambda_calc-delta_fix_lambda_calc, xmax=fix_lambda_calc+delta_fix_lambda_calc, ymin=0, ymax=1), alpha=0.2, fill="blue")
