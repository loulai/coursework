# DATA FRAME PREP & ADDITIONS

#load model                              
load('model.Rdata')

#read in weather
weather_2015 <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

weather_2015 <- select(weather_2015, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather_2015) <- tolower(names(weather_2015))
weather_2015 <- mutate(weather_2015,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather_2015 <- tbl_df(weather_2015)

# add additional features (is_weekend, is_holiday)
holiday_dates <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25",
                   "2015-01-01", "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25")

weather_2015 <- mutate(weather_2015, is_hot = (tmax*10) > 80,
             is_heavy_rain = (prcp*10) > 4,
             is_holiday = ymd %in% as.Date(holiday_dates),
             is_weekend = wday(ymd) == 1 | wday(ymd) == 7)

############################
weather_2015$predicted <- predict(lm.fit8, weather_2015)
ggplot() + geom_point(aes(ymd, predicted), data = weather_2015)
View(weather_2015)
save(lm.fit8, file = "model.Rdata")
