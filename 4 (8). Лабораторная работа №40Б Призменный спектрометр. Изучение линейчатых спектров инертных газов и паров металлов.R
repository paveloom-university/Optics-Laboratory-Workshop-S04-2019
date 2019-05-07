
# Часть 1

phi_1_n = c(63.67, 311.08)
phi_2_n = c(243.65, 131.08)

phi_2_n[2] = phi_2_n[2] + 360
phi_n = ((phi_2_n - 180) + phi_1_n)/2
phi_n

alpha = abs(phi_n[2] - phi_1_n[1])
alpha

A = abs(180 - alpha)
A

# Часть 2

phi_1_n_2 = c(59.07, 60.25, 61.2, 61.45)
phi_2_n_2 = c(239.03, 240.25, 241.17, 241.42)

phi_n_2 = (phi_2_n_2 - 180 + phi_1_n_2)/2
phi_n_2

rad_1 = ((phi_n_2 + A)/2)*pi/180
rad_2 = (A/2)*pi/180

n = sin(rad_1)/sin(rad_2)
n

delta_A = 0.02
delta_phi = 0.01

delta_A_rad = delta_A*pi/180
delta_phi_rad = delta_phi*pi/180

delta_n = (cos(rad_1)*(delta_phi_rad + delta_A_rad)*sin(rad_2) + cos(rad_2)*delta_A_rad*sin(rad_1))/(2*(sin(rad_2))^2)
delta_n


plot(phi_n_2, n, xlab = ' ', ylab = ' ', ylim = c(1.565,1.67))
lines(phi_n_2, n)
arrows(phi_n_2, n+delta_n, phi_n_2, n-delta_n, code = 3, angle = 90, length = "0.05")
arrows(phi_n_2-delta_phi, n, phi_n_2+delta_phi, n, code = 3, angle = 90, length = "0.05")
