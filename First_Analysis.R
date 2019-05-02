#needed clean3 from Third_EDA

clean3

# not autocorrelation

acf(clean3$THERMAL_COUNT_RATE,1000)

# skip first value because it is very big
fft(clean3$THERMAL_COUNT_RATE)[2:length(clean3)] %>% Re() %>%plot(type = "l")
fft(clean3$THERMAL_COUNT_RATE)[2:length(clean3)] %>% Im() %>% lines(col = 2)

#epithermal case
acf(clean3$CF_ATM_EPI,100)
fft(clean3$CF_ATM_EPI)[2:length(clean3)] %>% Re() %>%plot(type = "l")
fft(clean3$CF_ATM_EPI)[2:length(clean3)] %>% Im() %>% lines(col = 2)