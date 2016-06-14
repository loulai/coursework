#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations
cut -d, -f5 201402-citibike-tripdata.csv | sort | uniq | wc -l
330

# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq | wc -l
5700

# extract all of the trip start times
cut -d, -f2 201402-citibike-tripdata.csv

# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | cut -c2-11 | uniq -c

# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -c2-11 | uniq -c | sort -n | tail -n1

# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -c2-11 | uniq -c | sort -n | head -n2 | tail -n1

# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | sort -n | uniq -c | sort -n | tail -n1

# count the number of riders by gender and birth year
cut -d, -f15 201402-citibike-tripdata.csv | sort -n | uniq -c #gender
cut -d, -f14 201402-citibike-tripdata.csv | sort -n | uniq -c #birth year

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
awk -F, '$5 ~ /.*[0-9].*&.*[0-9].*/' 201402-citibike-tripdata.csv | wc -l

# compute the average trip duration
awk -F, '/[0-9]/ {gsub(/"/, "", $1); sum+=$1; count++} END {print sum/count}' 201402-citibike-tripdata.csv

#compute the average trip duration of trips starting on a station on Broadway
awk -F, '/[0-9]/ {gsub(/"/, "", $1); if($5 ~ /.*Broadway.*/) {sum+=$1; count++}} END {print sum/count}' 201402-citibike-tripdata.csv

