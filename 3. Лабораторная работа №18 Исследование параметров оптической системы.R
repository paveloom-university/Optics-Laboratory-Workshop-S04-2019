library(ggplot2)

# Собирающая линза

A = c(0.8, 0.200, 0.300, 1.100, 0.700, 0.500, 0.400)
y=-0.5

B=A/y
B

delta = 0.025

C=(A*delta+abs(y)*delta)/y^2
C

D=1/B
D

E=C/B^2
E

p=441
q=c(608.0, 765.0, 722.0, 590.0, 617.0, 644, 665)
r=c(882, 917, 884, 927, 867, 853, 860)

u=p-q
b=r-q
u
b


df1 <- data.frame(b,B)
df2 <- data.frame(u,D)

err_B_min=B-C
err_B_max=B+C
err_D_min=D-E
err_D_max=D+E

pd <- position_dodge(0.1)

ggplot() + geom_point(data=df1, aes(x=b, y=B)) + geom_smooth(data=df1, aes(x=b, y=B), method = 'lm', fullrange = TRUE) + 
  geom_point(data=df2, aes(x=u, y=D)) + geom_smooth(data=df2, aes(x=u, y=D), method = 'lm', colour = 'red', fullrange = TRUE) + 
  ylim(-2.1,1.6) + xlim(-300, 300) + xlab(' ') + ylab(' ') + geom_errorbar(aes(x=b,ymin=err_B_min, ymax=err_B_max), width=.1, position=pd) + 
  geom_errorbar(aes(x=u,ymin=err_D_min, ymax=err_D_max), width=.1, position=pd)

model1 = lm(data=df1, B~b)
y1 = df1$B
y1_hat = fitted(model1)
cor(y1,y1_hat)
coef(model1)
summary(model1)

model2 = lm(data=df2, D~u)
y2 = df2$D
y2_hat = fitted(model2)
cor(y2,y2_hat)
coef(model2)
summary(model2)

# Рассеивающая линза

A = c(0.6, 0.7, 1, 0.5, 0.4, 0.6, 0.8)
y=0.5

B=A/y
B

delta = 0.025

C=(A*delta+abs(y)*delta)/y^2
C

D=1/B
D

E=C/B^2
E

p=854
q=c(826, 832, 758, 856, 896, 822, 780)
r=c(859, 874, 940, 855, 862, 860, 899)

u=p-q
u
b=r-q
b

df1 <- data.frame(b,B)
df2 <- data.frame(u,D)

err_B_min=B-C
err_B_max=B+C
err_D_min=D-E
err_D_max=D+E

pd <- position_dodge(0.1)

ggplot() + geom_point(data=df1, aes(x=b, y=B)) + geom_smooth(data=df1, aes(x=b, y=B), method = 'lm', fullrange = TRUE) + 
  geom_point(data=df2, aes(x=u, y=D)) + geom_smooth(data=df2, aes(x=u, y=D), method = 'lm', colour = 'red', fullrange = TRUE) + 
  ylim(-1,3) + xlim(-300, 300) + xlab(' ') + ylab(' ') + geom_errorbar(aes(x=b,ymin=err_B_min, ymax=err_B_max), width=.1, position=pd) + 
  geom_errorbar(aes(x=u,ymin=err_D_min, ymax=err_D_max), width=.1, position=pd)

model1 = lm(data=df1, B~b)
y1 = df1$B
y1_hat = fitted(model1)
cor(y1,y1_hat)
coef(model1)
summary(model1)

model2 = lm(data=df2, D~u)
y2 = df2$D
y2_hat = fitted(model2)
cor(y2,y2_hat)
coef(model2)
summary(model2)




L = 146.2
delta_L = 0.3

P_1 = 0.0103
delta_P_1 = 0.0006

P_2 = -0.0053
delta_P_2 = 0.0006

a1 = 1 - P_1*L
a1

delta_a1 = delta_P_1*L + delta_L * P_1
delta_a1

a3 = - P_1 - P_2 + P_1*P_2*L
a3

delta_a3 = delta_P_1 + delta_P_2 + (delta_P_1*P_2 + delta_P_2*P_1)*L + delta_L*P_1*P_2
delta_a3

a4 = 1 - P_2*L
a4

delta_a4 = delta_P_2*L + delta_L * P_2
delta_a4

A = -0.51
delta_A = 0.09

B = 146.2
delta_B = 0.3

C = -0.013
delta_C = 0.002

D = 1.78
delta_D = 0.09

s1 = D/C
s1

delta_s1 = (delta_D*C + delta_C*D)/C^2
delta_s1

f1 = 1/C
f1

delta_f1 = delta_C/C^2
delta_f1

t1 = (D - 1)/C
t1

delta_t1 = delta_s1
delta_t1

s2 = -A/C
s2

delta_s2 = (delta_A*C + delta_C*A)/C^2 
delta_s2

t2 = -(A-1)/C
t2

delta_t2 = -(delta_A*C + delta_C*A)/C^2
delta_t2


A = c(0.9, 1.1, 1.2, 1.5, 0.65, 0.4, 0.2)
y=-0.5

B=A/y
B

delta = 0.025

C=(A*delta+abs(y)*delta)/y^2
C

D=1/B
D

E=C/B^2
E

p=371
q=c(712, 699, 696, 690, 722, 762, 870)
r=c(829, 857, 873, 910, 810, 806, 876)

u=p-q
u
b=r-q
b

df1 <- data.frame(b,B)
df2 <- data.frame(u,D)

err_B_min=B-C
err_B_max=B+C
err_D_min=D-E
err_D_max=D+E

pd <- position_dodge(0.1)

ggplot() + geom_point(data=df1, aes(x=b, y=B)) + geom_smooth(data=df1, aes(x=b, y=B), method = 'lm', fullrange = TRUE) + 
  geom_point(data=df2, aes(x=u, y=D)) + geom_smooth(data=df2, aes(x=u, y=D), method = 'lm', colour = 'red', fullrange = TRUE) + 
  ylim(-3.5,3) + xlim(-510, 240) + xlab(' ') + ylab(' ') + geom_errorbar(aes(x=b,ymin=err_B_min, ymax=err_B_max), width=.1, position=pd) + 
  geom_errorbar(aes(x=u,ymin=err_D_min, ymax=err_D_max), width=.1, position=pd)

model1 = lm(data=df1, B~b)
y1 = df1$B
y1_hat = fitted(model1)
cor(y1,y1_hat)
coef(model1)
summary(model1)

model2 = lm(data=df2, D~u)
y2 = df2$D
y2_hat = fitted(model2)
cor(y2,y2_hat)
coef(model2)
summary(model2)




A = c(0.25, 0.2, 0.45, 0.35, 0.15, 0.3, 0.4)
y=-0.5

B=A/y
B

delta = 0.025

C=(A*delta+abs(y)*delta)/y^2
C

D=1/B
D

E=C/B^2
E

p=371
q=c(655, 703, 580, 610, 736, 634, 588)
r=c(847, 880, 805, 818, 905, 832, 808)

u=p-q
u
b=r-q
b

df1 <- data.frame(b,B)
df2 <- data.frame(u,D)

err_B_min=B-C
err_B_max=B+C
err_D_min=D-E
err_D_max=D+E

pd <- position_dodge(0.1)

ggplot() + geom_point(data=df1, aes(x=b, y=B)) + geom_smooth(data=df1, aes(x=b, y=B), method = 'lm', fullrange = TRUE) + 
  geom_point(data=df2, aes(x=u, y=D)) + geom_smooth(data=df2, aes(x=u, y=D), method = 'lm', colour = 'red', fullrange = TRUE) + 
  ylim(-4.5,4.5) + xlim(-400, 240) + xlab(' ') + ylab(' ') + geom_errorbar(aes(x=b,ymin=err_B_min, ymax=err_B_max), width=.1, position=pd) + 
  geom_errorbar(aes(x=u,ymin=err_D_min, ymax=err_D_max), width=.1, position=pd)

model1 = lm(data=df1, B~b)
y1 = df1$B
y1_hat = fitted(model1)
cor(y1,y1_hat)
coef(model1)
summary(model1)

model2 = lm(data=df2, D~u)
y2 = df2$D
y2_hat = fitted(model2)
cor(y2,y2_hat)
coef(model2)
summary(model2)





A = -0.29
delta_A = 0.04

B = 146.2
delta_B = 0.3

C = -0.012
delta_C = 0.0004

D = 1.44
delta_D = 0.09

s1 = D/C
s1

delta_s1 = (delta_D*C + delta_C*D)/C^2
delta_s1

f1 = 1/C
f1

delta_f1 = delta_C/C^2
delta_f1

t1 = (D - 1)/C
t1

delta_t1 = delta_s1
delta_t1

s2 = -A/C
s2

delta_s2 = (delta_A*C + delta_C*A)/C^2 
delta_s2

t2 = -(A-1)/C
t2

delta_t2 = -(delta_A*C + delta_C*A)/C^2
delta_t2


test = c(0.0005, 0.001, 0.0003, 0.0003)
mean(test)
sd(test)
