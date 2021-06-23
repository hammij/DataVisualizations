#Necessary Libraries
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("usmap")
#install.packages("stringr")
#install.packages("openintro")
#install.packages("ggpubr")

library(ggplot2)
library(lubridate)
library(dplyr)
library(usmap)
library(stringr)
library(openintro)
library(ggpubr)

#Data exploration
#MAY NEED TO CHANGE FILE PATH ON HW SUBMISSION
d = read.csv("D:/Downloads/Gurp_HW_Help.csv")
d = na.omit(d)
head(d)
summary(d)
colnames(d)

#Converting Dates to Date time and making new variables for years
d$Coverage_Start_Date = mdy(d$Coverage_Start_Date)
d$Coverage_End_Date = mdy(d$Coverage_End_Date)
d$Year_Start = year(d$Coverage_Start_Date)
d$Year_End = year(d$Coverage_End_Date)
d = d[d$Year_Start <= 2020 & d$Year_Start >=2006, ]

#DF Only REP DEM OTHER
d$Cand_Party_Affiliation = as.character(d$Cand_Party_Affiliation)
d$Cand_Party_Affiliation[(d$Cand_Party_Affiliation != "REP" & d$Cand_Party_Affiliation != "DEM")] = "OTHER"
d$Cand_Party_Affiliation = as.factor(d$Cand_Party_Affiliation)
d$Cand_Party_Affiliation = factor(d$Cand_Party_Affiliation, levels = c("DEM", "REP", "OTHER"))
levels(d$Cand_Party_Affiliation)
d$Cand_Office = as.factor(d$Cand_Office)
d = na.omit(d)

#Total Disbursement by Party
summary(d$Total_Disbursement)   
hist(d$Total_Disbursement)
#data seems pretty skewed so going to remove outliers
Q = quantile(d$Total_Disbursement, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(d$Total_Disbursement)
d_fixed =  subset(d, d$Total_Disbursement > (Q[1] - 1.5*iqr) & 
                      d$Total_Disbursement < (Q[2]+1.5*iqr))
d_fixed = na.omit(d_fixed)

Q = quantile(d_fixed$Total_Receipt, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(d_fixed$Total_Receipt)
d_fixed =  subset(d_fixed, d_fixed$Total_Receipt > (Q[1] - 1.5*iqr) & 
                    d_fixed$Total_Receipt < (Q[2]+1.5*iqr))

#better lets visualize

gd_dis = d_fixed %>%
  group_by(Cand_Party_Affiliation, Cand_Office, Year_Start) %>%
  summarise(Total_Disbursement = median(Total_Disbursement))

levels(gd_dis$Cand_Party_Affiliation)

#Bar plot Avg Disbersement by party for each office  
p = ggplot(gd_dis, aes(Cand_Office, Total_Disbursement/1000))
p + geom_col(aes(fill = Cand_Party_Affiliation), position = 'dodge')+
  scale_fill_manual(values=c("DEM" = "#111df2", "REP" =  "#f14010", "OTHER" = "#11f224"), name ="Political Party")+
  ylab("Avg Disbursement Total (Per 1k USD)")+
  xlab("Office")+
  ggtitle("Average Total spent by each Political Party for each Office Race")


#Bar plot Fake
p = ggplot(gd_dis, aes(Cand_Office, log(Total_Disbursement)))
p + geom_col(aes(fill = Cand_Party_Affiliation), position = 'dodge')+
  scale_fill_manual(values=c("DEM" = "#111df2", "REP" =  "#f14010", "OTHER" = "#11f224"), name ="Political Party")+
  ylab("Avg Disbursement Total (Per 1k USD)")+
  xlab("Office")+
  ggtitle("Log of Average Total spent by each Political Party for each Office Race")
  
  

#Time series avg spent by each party for each office by year
p = ggplot(gd_dis, aes(Year_Start, Total_Disbursement))
p + geom_line(aes(color = Cand_Office)) + 
  facet_grid(rows = gd_dis$Cand_Party_Affiliation)+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 10))+
  scale_x_continuous(breaks = c(2006,2008,2010,2012,
                                2014,2016,2018,2020))+
  ggtitle("Party Dibursement Totals vs Campaign Start Year",
          subtitle = "Average amount in USD spent by each party over a 15 year 
          period, grouped by office.")+
  ylab("Avg Disbursement Total (Per 10k USD)")+
  xlab("Campaign Start Year")+
  labs(color = "Office")

#Fake Time Series
p = ggplot(gd_dis, aes(Year_Start, log(Total_Disbursement)))
p + geom_line(aes(color = Cand_Office)) + 
  facet_grid(rows = gd_dis$Cand_Party_Affiliation)+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 10))+
  scale_x_continuous(breaks = c(2006,2008,2010,2012,
                                2014,2016,2018,2020))+
  ggtitle("Party Dibursement Totals vs Campaign Start Year",
          subtitle = "Log of Average amount in USD spent by each party over a 15 year 
          period, grouped by office.")+
  ylab("Avg Disbursement Total (Per USD)")+
  xlab("Campaign Start Year")+
  labs(color = "Office")
  


#Scatter plot 
p = ggplot(d_fixed,aes((Total_Disbursement/1000), Total_Receipt/1000))
p + geom_point(aes(color = Cand_Office))+
  facet_wrap(~d_fixed$Cand_Party_Affiliation, nrow = 3)+
  geom_smooth(method = 'lm')+
  stat_cor(method = "pearson", label.x = 0, label.y = 2000)+
  stat_regline_equation(label.x = 0, label.y = 1500)+
  xlab("Total Spent (Per 1k USD)")+
  ylab("Total Raised (Per 1k USD)")+
  ggtitle("Total Raised (Receipt) vs Total Spent (Disbursement) by each Party")+
  labs(color = "Office")

#Fake scatter
p = ggplot(d_fixed,aes((Total_Disbursement), Total_Receipt/1000))
p + geom_point(aes(color = Cand_Office))+
  facet_wrap(~d_fixed$Cand_Party_Affiliation, nrow = 3)+
  geom_smooth(method = 'lm')+
  stat_cor(method = "pearson", label.x = 0, label.y = 1500000)+
  stat_regline_equation(label.x = 0, label.y = 1000000)+
  xlab("Total Spent")+
  ylab("Total Raised (Per 1k USD)")+
  ggtitle("Total Raised (Receipt) Per 1000 USD vs Total Spent (Disbursement) by each Party")+
  labs(color = "Office")+
  ylim(c(0, max(d_fixed$Total_Receipt)))



#USMAP
d_fixed$Cand_State = as.factor(d_fixed$Cand_State)

d_states = d_fixed[d_fixed$Cand_State %in% unique(usmap::statepop$abbr), ]
states_spent = d_states[,c(18,11)]
states_spent = states_spent %>% 
  group_by(Cand_State) %>%
  summarise(Total_Disbursement = median(Total_Disbursement))
states_spent$Cand_State = as.character(states_spent$Cand_State)

states_spent$States = abbr2state(states_spent$Cand_State)
states_spent$fips = fips(states_spent$States)
states_spent = states_spent[,c(4,3,1,2)]


plot_usmap(regions = 'states',data = states_spent, values= "Total_Disbursement")+
  scale_fill_viridis_c(name = "Median Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")

states_by_party = d_states[,c(18,11,8)]
states_by_party <- states_by_party %>%
  group_by(Cand_State, Cand_Party_Affiliation) %>%
  summarise(Total_Disbursement = mean(Total_Disbursement))

dem_states = states_by_party[states_by_party$Cand_Party_Affiliation == "DEM", ]
dem_states$fips = fips(dem_states$Cand_State)
plot_usmap(regions = 'states',data = dem_states, values= "Total_Disbursement")+
  scale_fill_viridis_c(name = "Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average Democrat Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))

rep_states = states_by_party[states_by_party$Cand_Party_Affiliation == "REP", ]
rep_states$fips = fips(rep_states$Cand_State)
plot_usmap(regions = 'states',data = rep_states, values= "Total_Disbursement")+
  scale_fill_viridis_c(name = "Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average Republican Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))

other_states = states_by_party[states_by_party$Cand_Party_Affiliation == "OTHER", ]
other_states$fips = fips(other_states$Cand_State)
plot_usmap(regions = 'states',data = other_states, values= "Total_Disbursement")+
  scale_fill_viridis_c(name = "Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average 3rd Party Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))


#Fake Maps
states_by_party = d_states[,c(18,11,8)]
states_by_party <- states_by_party %>%
  group_by(Cand_State, Cand_Party_Affiliation) %>%
  summarise(Total_Disbursement = log(mean(Total_Disbursement)))

dem_states = states_by_party[states_by_party$Cand_Party_Affiliation == "DEM", ]
dem_states$fips = fips(dem_states$Cand_State)
plot_usmap(regions = 'states',data = dem_states, values= "Total_Disbursement")+
  scale_fill_continuous(name = "Log Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average Democrat Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))

rep_states = states_by_party[states_by_party$Cand_Party_Affiliation == "REP", ]
rep_states$fips = fips(rep_states$Cand_State)
plot_usmap(regions = 'states',data = rep_states, values= "Total_Disbursement")+
  scale_fill_continuous(name = "Log Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average Republican Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))

other_states = states_by_party[states_by_party$Cand_Party_Affiliation == "OTHER", ]
other_states$fips = fips(other_states$Cand_State)
plot_usmap(regions = 'states',data = other_states, values= "Total_Disbursement")+
  scale_fill_continuous(name = "Log Disbursement Spending", label = scales::comma) + 
  theme(legend.position = "right")+
  ggtitle("Average 3rd Party Spending by State")+
  theme(plot.title = element_text(size = 15, face = "bold"))


         