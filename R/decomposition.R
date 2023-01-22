library(fpp3)
d <-
qasis %>%
  select(date, num_rating) %>%
  group_by(date) %>%
  summarize(num_rating = sum(num_rating))

series <- tsibble(d, index = date)

autoplot(series)

dcmp <-
  series %>%
  model(stl = STL(num_rating))

components(dcmp) %>% autoplot()
