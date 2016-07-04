library(dplyr)
library(readr)

# define a function to turn strings into datetimes
parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

########################################
# load and clean trip data
########################################

# load each month of the trip data into one big data frame
csvs <- Sys.glob('*-tripdata.csv')
trips_2015 <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')

  # the date format changed to something ugly in 2014-09 which read_csv doesn't recognize as a datetime,
  # so manually convert the date from a string to a datetime
  if (typeof(tmp$starttime) == "character")
    tmp <- mutate(tmp,
                  starttime=parse_datetime(starttime, "%m/%d/%Y %H:%M:%S"),
                  stoptime=parse_datetime(stoptime, "%m/%d/%Y %H:%M:%S"))

  trips_2015 <- rbind(trips_2015, tmp)
}

# replace spaces in column names with underscores
names(trips_2015) <- gsub(' ', '_', names(trips_2015))

# add a column for year/month/day (without time of day)
trips_2015 <- mutate(trips_2015, ymd=as.Date(starttime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips_2015 <- mutate(trips_2015, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))

########################################
# load and clean weather data
########################################

# load weather data from belvedere tower in central park
# https://www.ncei.noaa.gov/orders/cdo/762757.csv
# ordered from
# http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00094728/detail
weather_2015 <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
weather_2015 <- select(weather_2015, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather_2015) <- tolower(names(weather_2015))
weather_2015 <- mutate(weather_2015,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather_2015 <- tbl_df(weather_2015)

# save data frame for easy loading in the future
save(trips_2015, weather_2015, file='trips_2015.RData')

