# 1
# 請問資料檔中共有幾筆資料（觀察值）？幾個變項？
# How many data (observed values) are there in the data file? How many variables?

str(df)  # 10450 obs. of  4 variables:

# 2
#這些資料分別來自哪些媒體？
# Which media do these materials come from? 

table(df$media) # 政府新聞稿    蘋果日報 PTT分享新聞        中時 
                #        409         402        7385        2254  

# 3
# 最多來自哪個媒體？
# Which media does it come from at most?

names(table(df$media))[grep(1,match(unname(table(df$media)), 
                                           as.character(max(table(df$media))),
                                          nomatch = 0))]                        # "PTT分享新聞"

# 4
# 這些資料分別來自哪個時間段？
# What time period do these data come from? 

max(as.Date(df$date))                                                           # first "2019-12-31"
min(as.Date(df$date))                                                           # last "2020-06-25" 

max(as.Date(df$date)) - min(as.Date(df$date))                                   # Time difference of 177 days

# 哪一天的報導量最大？
# Which day has the largest number of reports? 

names(table(df$date))[grep(1,match(unname(table(df$date)), 
                                    as.character(max(table(df$date))),
                                    nomatch = 0))]                              # "2020-02-10"



max_date = names(table(df$date))[grep(1,match(unname(table(df$date)), 
                                   as.character(max(table(df$date))),
                                   nomatch = 0))]  


# 從哪一天報導量開始緩慢下降？
# Since when did the report volume start to decline slowly?

df1 = as.data.frame(table(df$date))
df1$Var1 = as.Date(df1$Var1)
df1$highlight = "N"
df1$highlight[df1$Var1 == max_date] = "Y" 

library(ggplot2) # install.packages("ggplot2")
library(scales) # install.packages("scales")

ggplot( df1, aes( x = Var1, y = Freq, fill = highlight ) ) +
  geom_bar( stat = "identity" ) +
  scale_fill_manual( values = c( "Y"="tomato", "N"="gray" ), guide = FALSE ) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  geom_line(aes(y=))

library(data.table) # install.packages("data.table")

df1$mov_avg = frollmean(df1$Freq, 10)
df1$mov_avg[is.na(df1$mov_avg)] = 0

df1$Var1[df1$mov_avg == max(df1$mov_avg, na.rm = TRUE)]                 # "2020-01-25"

ggplot( df1, aes( x = Var1, y = Freq, fill = highlight ) ) +
  geom_bar( stat = "identity" ) +
  scale_fill_manual( values = c( "Y"="tomato", "N"="gray" ), guide = FALSE ) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  geom_line(aes(y=df1$mov_avg))



  



