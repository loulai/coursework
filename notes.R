Join

inner_join

anti_join

left_join()

right_join()

spread(dtaframe, thingtospread, vlaue) #takes long data frame and makes it wide, for tidying

gather(df_wide, "sex", "y", 2:3) #collect from 1 and 3, flatten it out, call the new column name sex, call the value y

spread(trips_by... , gender, numtrips) %>% mutate(ration=Male/Femalr)

ggplot2(plit, aes(x=birth_year, y=ratio)) + geom_line() _xlim(X(a950),2000)) + ylim(c(0,10))