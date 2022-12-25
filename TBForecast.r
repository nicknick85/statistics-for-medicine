scr <- c(62.5, 63.3, 64.4, 65.7, 65.8, 66.6, 68.1, 69.3, 72.7, 72.7, 73.7, 66.7, 70.8)
mort <- c(16.8, 15.4, 14.2, 12.5, 11.3, 10.1, 9.2, 7.8, 6.5, 5.9, 5.2, 4.5, 4.7)
incd <- c(82.6, 77.2, 73, 68.1, 63, 59.5, 57.7, 53.3, 48.3, 44.4, 41.2, 32.5, 31.1)

tsm <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

## Mortality analysis
###############################################################################

d_log_mort <- log(mort[2:13])-log(mort[1:12])

pdf(file = "d_log_mort.pdf",width=15, height=5)
boxplot(d_log_mort, outpch = 21, outbg="red", outcex=1.5, horizontal = TRUE)
stripchart(d_log_mort[1:11], pch = 19, col = 4, add = TRUE)
dev.off()

Box.test(d_log_mort[1:11], type = "Ljung-Box")
shapiro.test(d_log_mort[1:11])

library(outliers)
grubbs.test(d_log_mort, type = 10)

library(bsts)
ss <- list()
ss <- AddLocalLevel(ss, mort[2:13])
mdl_mort <- bsts(mort[2:13] ~ scr[1:12], timestamps = tsm[2:13], state.specification = ss, niter = 1500, seed = 2022)

pdf(file = "mort_fitting.pdf", width=15, height=8)
plot(mdl_mort)
dev.off()

pdf(file = "mort_diag.pdf", width=15, height=10)
par(mfrow=c(2,1), mai = c(1,1,0.1,0.1))
qqdist(draws = residuals(mdl_mort))
AcfDist(draws = residuals(mdl_mort))
dev.off()

cons_mort <- predict(mdl_mort, horizon = 5, newdata = c(70.8, 47.37, 58.95, 60.00, 62.11))$mean
base_mort <- predict(mdl_mort, horizon = 5, newdata = c(70.8, 75.89, 76.74, 77.04, 77.66))$mean
optm_mort <- predict(mdl_mort, horizon = 5, newdata = c(70.8, 82.11, 85.26, 86.32, 88.42))$mean

## Incidence analysis
#########################################################################

d_log_incd <- log(incd[2:13])-log(incd[1:12])

pdf(file = "d_log_incd.pdf",width=15, height=5)
boxplot(d_log_incd, outpch = 21, outbg="red", outcex=1.5, horizontal = TRUE)
stripchart(d_log_incd[c(1:10, 12)], pch = 19, col = 4, add = TRUE)
dev.off()

Box.test(d_log_incd[c(1:10, 12)], type = "Ljung-Box")
shapiro.test(d_log_incd[c(1:10, 12)])

library(outliers)
grubbs.test(d_log_incd, type = 10)

library(bsts)
ss <- list()
ss <- AddAutoAr(ss, incd, lag = 5)
ss <- AddSemilocalLinearTrend(ss, incd)
mdl_incd <- bsts(incd ~ scr, state.specification = ss, timestamps = tsm, family = "student", niter = 1500, seed = 2022)

pdf(file = "incd_fitting.pdf", width=15, height=6)
plot(mdl_incd)
dev.off()

pdf(file = "incd_diag.pdf", width=15, height=10)
par(mfrow=c(2,1), mai = c(1,1,0.1,0.1))
qqdist(draws = residuals(mdl_incd), ylim = c(-5,5))
AcfDist(draws = residuals(mdl_incd))
dev.off()

cons_incd <- predict(mdl_incd, horizon = 5, newdata = c(47.37, 58.95, 60.00, 62.11, 63.16))$mean
base_incd <- predict(mdl_incd, horizon = 5, newdata = c(75.89, 76.74, 77.04, 77.66, 78.15))$mean
optm_incd <- predict(mdl_incd, horizon = 5, newdata = c(82.11, 85.26, 86.32, 88.42, 89.47))$mean
