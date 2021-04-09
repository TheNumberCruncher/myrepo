#install.packages('forecast')
require(ggplot2)
require(forecast)
require(quantmod)
require(fBasics)

pop_data = read.csv('/Users/shanemcintyre/Desktop/Wolf Time Series Project/Wolf_Population_Data.csv')

Yellowstone_Population = pop_data$YNP.Population
basicStats(Yellowstone_Population)
plot(density(Yellowstone_Population), main = 'Population Distribution')
x = seq(0,200)
y = dnorm(x, mean(Yellowstone_Population), sd(Yellowstone_Population))
lines(x,y, lty = 2, col = 'red')
legend(-20, 0.012, legend = c('Wolf Population Distribution', 'Normal Distribution'), col = c('black', 'red'), lty = c(1,2), cex = 0.5)

r = Yellowstone_Population - mean(Yellowstone_Population)
normalTest(r)
qqnorm(r)
qqline(r)

# Start of real work
pop = pop_data$YNP.Population
pop
n_pop = pop_data$Northern.Range
yrs = pop_data$Year

g = ggplot(data = pop_data, aes(x = Year)) + 
  geom_point(aes(y = YNP.Population, color = 'Yellowstone'), shape = 18) + geom_line(aes(y = YNP.Population, color = 'Yellowstone')) +
  geom_point(aes(y = Northern.Range, color = 'Northern Range'), shape = 16) + geom_line(aes(y = Northern.Range, color = 'Northern Range')) +
  xlab('Year') + ylab('Population') + ggtitle('Wolf Population from 1995-2019') + scale_color_manual(values = c('Yellowstone' = 'blue',
                                                                                                                'Northern Range' = 'skyblue')) +
  labs(color = 'Location')
g

# Can already tell data is nonstationary
acf(Yellowstone_Population)
Box.test(pop, lag = 3, type = 'Ljung-Box')

# Seasonality coeeficient of 1
p = 5
q = 5
aic = matrix(rep(0, p*q), p, q)
bic = matrix(rep(0, p*q), p, q)
for (i in 1:p) {
  for (j in 1:q) {
    m = arima(pop, order = c(i,1,j))
    aic[i,j] = AIC(m)
    bic[i,j] = BIC(m)
  }
}
aic
bic
which(aic == min(aic), arr.ind = T)
which(bic == min(bic), arr.ind = T)
# Suggests arima(2,1,2)

model = arima(pop, order = c(2,1,2))
model
resid = model$residuals
fitted_model = pop - resid
Box.test(resid, lag = 3, 'Ljung-Box')
g2 = ggplot() + geom_point(aes(x = yrs, y = pop, color = 'Yellowstone Population')) + geom_line(aes(x = yrs, y = pop, color = 'Yellowstone Population')) +
  geom_point(aes(x = yrs, y = fitted_model, color = 'Fitted Model')) + geom_line(aes(x = yrs, y = fitted_model, color = 'Fitted Model')) + xlab('Year') +
  ylab('Population') + ggtitle('YNP Population vs Fitted Model ARIMA(2,1,2)') + scale_color_manual(values = c('Yellowstone Population' = 'blue',
                                                                                               'Fitted Model' = 'red')) +
  labs(color = '') + theme(legend.text = element_text(size = 7))
g2

# Goodness of fit between +/- 1.96/sqrt(N)
1.96/sqrt(length(pop))
acf(resid)
# Predictions
frcst = forecast(model, h = 5)
frcst
frcst_val = data.frame(frcst$mean)
frcst_val
low = data.frame(frcst$lower[,])
low
upper = data.frame(frcst$upper[,])
upper
new_yrs = seq(2019,2024, by = 1)
frcst_YNP = c(tail(pop, n = 1), frcst_val$frcst.mean)
frcst_YNP
frcst_vals = data.frame(cbind(new_yrs, frcst_YNP))
g3 = ggplot() + geom_point(aes(x = yrs, y = pop, color = 'Yellowstone Population')) + 
  geom_line(aes(x = yrs, y = pop, color = 'Yellowstone Population')) + 
  geom_line(data = frcst_vals, aes(x = new_yrs, y = frcst_YNP, color = 'Forecasted Values')) + 
  scale_color_manual(values = c('Yellowstone Population' = 'blue',
                                'Forecasted Values' = 'coral'))
g3
PI_yrs = seq(2020,2024, by = 1)
PI = cbind(PI_yrs, low, upper)
PI
names(PI)[2] = 'low_80'
names(PI)[3] = 'low_95'
names(PI)[4] = 'high_80'
names(PI)[5] = 'high_95'
PI
g4 = g3 + geom_line(data = PI, aes(x = PI_yrs, y = low_80, color = '80% PI', lty = '80% PI')) +
  geom_line(data = PI, aes(x = PI_yrs, y = low_95, color = '95% PI', lty = '95% PI')) +
  geom_line(data = PI, aes(x = PI_yrs, y = high_80, color = '80% PI', lty = '80% PI')) +
  geom_line(data = PI, aes(x = PI_yrs, y = high_95, color = '95% PI', lty = '95% PI')) + scale_color_manual(values = c('Yellowstone Population' = 'blue',
                                                                                                                'Forecasted Values' = 'coral',
                                                                                                                '80% PI' = 'red',
                                                                                                                '95% PI' = 'darkgreen')) +
  scale_linetype_manual(values = c('80% PI' = 2,
                                   '95% PI' = 3)) +
  labs(color = '') +
  theme(legend.text = element_text(size = 7)) +
  xlab('Year') +
  ylab('Population') + ggtitle('5 Year Forecast of Wolf Population')
g4

# Seasonality coeeficient of 2
p = 5
q = 5
aic = matrix(rep(0, p*q), p, q)
bic = matrix(rep(0, p*q), p, q)
for (i in 1:p) {
  for (j in 1:q) {
    m = arima(pop, order = c(i,2,j))
    aic[i,j] = AIC(m)
    bic[i,j] = BIC(m)
  }
}
aic
bic
which(aic == min(aic), arr.ind = T)
which(bic == min(bic), arr.ind = T)
# Suggests arima(2,2,1)


model2 = arima(pop, order = c(2,2,1))
model2
resid2 = model2$residuals
fitted_model2 = pop - resid2
Box.test(resid2, lag = 3, type = 'Ljung-Box')
acf(resid2)
g5 = ggplot() + geom_point(aes(x = yrs, y = pop, color = 'Yellowstone Population')) + geom_line(aes(x = yrs, y = pop, color = 'Yellowstone Population')) +
  geom_point(aes(x = yrs, y = fitted_model2, color = 'Second Fitted Model')) + geom_line(aes(x = yrs, y = fitted_model2, color = 'Second Fitted Model')) + 
  xlab('Year') + ylab('Population') + scale_color_manual(values = c('Yellowstone Population' = 'blue',
                                                                    'Second Fitted Model' = 'red')) +
  labs('') + ggtitle('YNP Population vs Fitted Model ARIMA(2,2,1)') +
  theme(legend.text = element_text(size = 7))
g5

#From Box test choose arima(2,1,2)
