#install required packages
#install.packages("qdapTools")
#install.packages("plyr")
install.packages("readr")

#Install all required libraries
library(dplyr)
library(tidyr)
library(qdapTools)
library(lubridate)
library(readr)



#load source data
#DataGroupAssignment <- load("C:/Users/HP PC/Desktop/IESEG_Classes/Buisness Analytics Tools_OPenSource/GroupAssignment/DataGroupAssignment.Rdata")

#Analyze table structure
str(UserDailyAggregation)
#check nulls in dataframe
summary(UserDailyAggregation)
colSums(is.na.data.frame(UserDailyAggregation))
#There are no nulls in this dataframe

#Date is of char datatype. convert it to date type and load it to dummy table
UserDailyAggregation$Date <- ymd(UserDailyAggregation$Date)
str(UserDailyAggregation)

#remove(Products)
#Add product IDs desription for easy understanding
#step1 : Create a temp table with product description as per appendix 1
Products <- data.frame(matrix(ncol = 2, nrow = 8))
colnames(Products)<- c("ProductID","Product_Description")
Products$ProductID <- c(1, 2, 3, 4, 5, 6, 7, 8)
Products$"Product_Description" <- c("Sports book fixed-odd" ,"Sports book live-action" ,"Poker BossMedia" ,"Casino BossMedia" ,"Supertoto" ,"Games VS" ,"Games bwin" ,"Casino Chartwell")
Products$ProductID <- as.numeric(Products$ProductID)
str(Products)

#Creating New Columns for each customer
#creating another temp table as temp_mean for mean calculation
temp_mean <- as_tibble(UserDailyAggregation)
temp_mean$Date <- ymd(temp_mean$Date)

temp_mean <- temp_mean %>% 
  group_by(UserID,ProductID) %>% 
  summarise(cust_mean_Stakes = round(mean(Stakes),2), 
            cust_mean_Winnings = round(mean(Winnings),2),
            cust_mean_Bets = round(mean(Bets),2),
            cust_mean_Stakes = round(max(Stakes),2)
  )

#Creating New Columns for each customer
#creating another temp table as temp_total for mean calculation
temp_total <- as_tibble(UserDailyAggregation)
temp_total$Date <- ymd(temp_total$Date)

temp_total <- temp_total %>% 
  group_by(UserID,ProductID) %>% 
  summarise(cust_Total_Stakes = round(sum(Stakes),2), 
            cust_Total_Winnings = round(sum(Winnings),2),
            cust_Total_Bets = round(sum(Bets),2),
            cust_Total_Stakes = round(sum(Stakes),2),
            cust_Total_Days = n_distinct(Date)
  )
#Merge temp_total and temp_mean with each other as Mean_Total
Mean_Total <- merge(temp_total, temp_mean)

#Merge Mean_Total with product table to add product description
Cust_Mean_Total_Prod <- merge(Mean_Total, Products)


###########
#############
#############
#Create more variables based on each product
#common categories of product are sports,poker, casino and games
#Create a separate entity for Casino
casino <- as_tibble(UserDailyAggregation)
casino$Date <- ymd(casino$Date)

str(casino)

casino <- casino %>% 
  filter( ProductID == 4 | ProductID == 8 ) %>% 
  group_by(UserID,ProductID) %>% 
  summarise(Total_Stakes_Casino = round(sum(Stakes),2), 
            Total_Win_Casino = round(sum(Winnings),2),
            Total_Bets_Casino = round(sum(Bets),2),
            Total_Stakes_Casino = round(sum(Stakes),2),
            Total_Days_played_Casino = n_distinct(Date),
            mean_Stakes_Casino = round(mean(Stakes),2), 
            mean_Winnings_Casino = round(mean(Winnings),2),
            mean_Bets_Casino = round(mean(Bets),2),
            Highest_Played_Month_casino = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_casino =   names(which.max(table(weekdays(as.Date(Date)))))
  ) 

#renaming Product_ID column for easy join
casino<- rename(casino,ProductID_casino=ProductID)
#Replace all NAs with 0
casino <- casino %>% replace(is.na(.), 0)


###Create a aspearate entity for sports
sports <- as_tibble(UserDailyAggregation)
sports$Date <- ymd(sports$Date)

sports <- sports %>% 
  filter(ProductID == 1 | ProductID == 2) %>%
  group_by(UserID,ProductID) %>% 
  summarise(Total_Stakes_sports = round(sum(Stakes),2), 
            Total_Win_sports = round(sum(Winnings),2),
            Total_Bets_sports = round(sum(Bets),2),
            Total_Stakes_sports = round(sum(Stakes),2),
            Total_Days_played_sports = n_distinct(Date),
            mean_Stakes_sports = round(mean(Stakes),2), 
            mean_Winnings_sports = round(mean(Winnings),2),
            mean_Bets_sports = round(mean(Bets),2),
            Highest_Played_Month_sports = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_sports =   names(which.max(table(weekdays(as.Date(Date)))))
  )   

#renaming Product_ID column for easy join
sports<- rename(sports,ProductID_sports=ProductID)
#Replace all NAs with 0
sports <- sports %>% replace(is.na(.), 0)


###Create a aspearate entity for poker
poker <- as_tibble(UserDailyAggregation)
poker$Date <- ymd(poker$Date)

poker <- poker %>% 
  filter(ProductID == 3) %>%
  group_by(UserID,ProductID) %>% 
  summarise(Total_Stakes_poker = round(sum(Stakes),2), 
            Total_Win_poker = round(sum(Winnings),2),
            Total_Bets_poker = round(sum(Bets),2),
            Total_Stakes_poker = round(sum(Stakes),2),
            Total_Days_played_poker = n_distinct(Date),
            mean_Stakes_poker = round(mean(Stakes),2), 
            mean_Winnings_poker = round(mean(Winnings),2),
            mean_Bets_poker = round(mean(Bets),2),
            Highest_Played_Month_poker = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_poker =   names(which.max(table(weekdays(as.Date(Date)))))
  )   

###Create a aspearate entity for Games
games <- as_tibble(UserDailyAggregation)
games$Date <- ymd(games$Date)

games <- games %>% 
  filter(ProductID == 6 | ProductID == 7) %>%
  group_by(UserID,ProductID) %>% 
  summarise(Total_Stakes_games = round(sum(Stakes),2), 
            Total_Win_games = round(sum(Winnings),2),
            Total_Bets_games = round(sum(Bets),2),
            Total_Stakes_games = round(sum(Stakes),2),
            Total_Days_played_games = n_distinct(Date),
            mean_Stakes_games = round(mean(Stakes),2), 
            mean_Winnings_games = round(mean(Winnings),2),
            mean_Bets_games = round(mean(Bets),2),
            Highest_Played_Month_games = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_games =   names(which.max(table(weekdays(as.Date(Date)))))
  )  

#renaming Product_ID column for easy join
games<- rename(games,ProductID_games=ProductID)

###Create a aspearate entity for supertoto
supertoto <- as_tibble(UserDailyAggregation)
supertoto$Date <- ymd(supertoto$Date)

supertoto <- supertoto %>% 
  filter(ProductID == 5) %>%
  group_by(UserID,ProductID) %>% 
  summarise(Total_Stakes_supertoto = round(sum(Stakes),2), 
            Total_Win_supertoto = round(sum(Winnings),2),
            Total_Bets_supertoto = round(sum(Bets),2),
            Total_Stakes_supertoto = round(sum(Stakes),2),
            Total_Days_played_supertoto = n_distinct(Date),
            mean_Stakes_supertoto = round(mean(Stakes),2), 
            mean_Winnings_supertoto = round(mean(Winnings),2),
            mean_Bets_supertoto = round(mean(Bets),2),
            Highest_Played_Month_supertoto = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_supertoto =   names(which.max(table(weekdays(as.Date(Date)))))
  ) 

#renaming Product_ID column for easy join
supertoto<- rename(supertoto,ProductID_supertoto=ProductID)

#merge all products table together and #Drop duplicated column
casino_sports <- full_join(casino,sports,by =c("UserID"))
str(casino_sports)


ca_sp_ga <- full_join(x=casino_sports, y=games, by="UserID")
str(ca_sp_ga)

all_products <- full_join(x=ca_sp_ga,y=supertoto, by="UserID")
str(all_products)
#rm(all_products)

#Highest Stakes Among All products
all_products$Highest_Stakes <- pmax(all_products$Total_Stakes_sports,all_products$Total_Stakes_Casino,all_products$Total_Stakes_games,all_products$Total_Stakes_supertoto)  
#Highest Winnings Among All products
all_products$Highest_Wins <- pmax(all_products$Total_Win_sports,all_products$Total_Win_Casino,all_products$Total_Win_games,all_products$Total_Win_supertoto)  
#Highest Bets Among All products
all_products$Highest_Bets <- pmax(all_products$Total_Bets_sports,all_products$Total_Bets_Casino,all_products$Total_Bets_games,all_products$Total_Bets_supertoto)  

#Replacing all NA values in all the columns of all_products dataframe to 0

all_products <- all_products %>% 
  as.data.frame() %>%
  replace(is.na(.), 0)


#####Plotting Graphs
#install.packages("gganimate")
library(ggplot2)
library(gganimate)


#Best play month for sports
sports_M_plot <- all_products %>%
  filter (Highest_Played_Month_sports != 0) %>%
  select (UserID, Highest_Played_Month_sports)

ggplot(sports_M_plot,aes(x = Highest_Played_Month_sports, fill = Highest_Played_Month_sports)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for sports

sports_D_plot <- all_products %>%
  filter (Highest_Played_Day_sports != 0) %>%
  select (UserID, Highest_Played_Day_sports)

ggplot(sports_D_plot,aes(x = Highest_Played_Day_sports, fill = Highest_Played_Day_sports)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")


#Best play month for games
games_M_plot <- all_products %>%
  filter (Highest_Played_Month_games != 0) %>%
  select (UserID, Highest_Played_Month_games)

ggplot(games_M_plot,aes(x = Highest_Played_Month_games, fill = Highest_Played_Month_games)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for games
games_D_plot <- all_products %>%
  filter (Highest_Played_Day_games != 0) %>%
  select (UserID, Highest_Played_Day_games)

ggplot(games_D_plot,aes(x = Highest_Played_Day_games, fill = Highest_Played_Day_games)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")

#Best play month for supertoto
supertoto_M_plot <- all_products %>%
  filter (Highest_Played_Month_supertoto != 0) %>%
  select (UserID, Highest_Played_Month_supertoto)

ggplot(supertoto_M_plot,aes(x = Highest_Played_Month_supertoto, fill = Highest_Played_Month_supertoto)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for supertoto
supertoto_D_plot <- all_products %>%
  filter (Highest_Played_Day_supertoto != 0) %>%
  select (UserID, Highest_Played_Day_supertoto)

ggplot(supertoto_D_plot,aes(x = Highest_Played_Day_supertoto, fill = Highest_Played_Day_supertoto)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")

#Best play month for casino
casino_M_plot <- all_products %>%
  filter (Highest_Played_Month_casino != 0) %>%
  select (UserID, Highest_Played_Month_casino)

ggplot(casino_M_plot,aes(x = Highest_Played_Month_casino, fill = Highest_Played_Month_casino)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for casino
casino_D_plot <- all_products %>%
  filter (Highest_Played_Day_casino != 0) %>%
  select (UserID, Highest_Played_Day_casino)

ggplot(casino_D_plot,aes(x = Highest_Played_Day_casino, fill = Highest_Played_Day_casino)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")

str(all_products)

#####Exporting Staging intermediate Data 
write.table(casino, file = "C:/Users/HP PC/Desktop/IESEG_Classes/Semester1/Buisness Analytics Tools_OPenSource/GroupAssignment/TestMarkdown/casino.csv", sep=",")
write.table(sports, file = "C:/Users/HP PC/Desktop/IESEG_Classes/Semester1/Buisness Analytics Tools_OPenSource/GroupAssignment/TestMarkdown/sports.csv", sep=",")
write.table(games, file = "C:/Users/HP PC/Desktop/IESEG_Classes/Semester1/Buisness Analytics Tools_OPenSource/GroupAssignment/TestMarkdown/games.csv", sep=",")
write.table(supertoto, file = "C:/Users/HP PC/Desktop/IESEG_Classes/Semester1/Buisness Analytics Tools_OPenSource/GroupAssignment/TestMarkdown/supertoto.csv", sep=",")
