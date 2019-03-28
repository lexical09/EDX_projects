##### Investigate flight performance from 2008-2018 #####
# dataset was obtained from the Bureau of Transportation Statistics
# https://www.transtats.bts.gov/OT_Delay/OT_DelayCause1.asp?pn=1

###Load necessary packages for data exploration and analysis###
require(ggplot2)
require(grid)
require(scales)
require(dplyr)
require(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)
library(rmarkdown)
library(knitr)

#Import dataset as a dataframe
flight <- read.csv("airline_delay_causes.csv", header=T, check.names=F)

#Get a summary of datatype and data info using summary and str
str(flight)
summary(flight)
    
#Remove Column with NA values
flight <- flight[,colSums(is.na(flight))<nrow(flight)]
#Check how many carriers in this dataset
print(paste("There are", length(unique(flight$carrier_name)), "carriers in this dataset."))

# Summary dataframe with total number of arrivals, delayed flights, cancelled flights,
# on-time flights that each carrier has by year.
flight_summary <- flight %>%
  group_by(year, carrier_name) %>%
  summarize(arrivals = sum(arr_flights), 
            delayed = sum(arr_del15), 
            cancelled = sum(arr_cancelled), 
            diverted = sum(arr_diverted)) %>%
  transform(on_time = 1 - delayed/arrivals) %>%
  transform(delayed_percent = delayed/arrivals)

#Remove all row with NA values from the flight_summary dataframe
flight_summary <- na.omit(flight_summary)

#Line plot by year for each carrier
ggplot(data = flight_summary, aes(x=year, y=on_time))+
  scale_x_continuous(name = "Year", 
                     breaks = seq(2008, 2018, 1))+
  geom_line(aes(color=carrier_name))

#Make new dataset that includes the average number of arrivals
#delayed flights, cancelled flights, and diverted flights and the proportion of 
#on_time flights in the last 10 years by carrier
flight_summary_average <- flight_summary %>%
  group_by(carrier_name) %>%
  summarize(ave_arrivals = mean(arrivals), 
            ave_delayed = mean(delayed_percent), 
            ave_cancelled = mean(cancelled),
            ave_diverted = mean(diverted), 
            ave_ontime = mean(on_time)) 

top_ten_delayed <- flight_summary_average%>%
  arrange(desc(ave_delayed))%>%
  top_n(10, ave_delayed)

top_ten_ontime <- flight_summary_average%>%
  arrange((desc(ave_ontime)))%>%
  top_n(10, ave_ontime)

#Average proportion of delayed flights from Dec 2008 to Dec 2018
ggplot(data = top_ten_delayed, aes(x=reorder(carrier_name,ave_delayed), ave_delayed))+
  geom_bar(stat = 'identity', position = 'dodge', fill="#FF9999", colour="black")+
  geom_text(mapping = aes(label = ave_ontime), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.4,size=10))+
  labs(x="Airline Name", y='Average proportion of delayed flights')+
  ggtitle("Average proportion of delayed flights (Dec 2008 - Dec 2018)") + coord_flip()

#Average proportion of on-time flights from Dec 2008 to Dec 2018
ggplot(data = top_ten_ontime, aes(x=reorder(carrier_name, ave_ontime), y=ave_ontime))+
  geom_bar(stat = 'identity', position = 'dodge', fill="#56B4E9", colour="black")+
  geom_text(mapping = aes(label = ave_ontime), size = 3) +
  labs(x='Airline Name', y='Average proportion of on-time flights')+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.4))+
  ggtitle('Average proportion of on-time flights (Dec 2008 - Dec 2018)')+ coord_flip()


#Subseting all data point related to airlines belonging to the top_five_delay list
subset_delay <- filter(flight, 
                       carrier_name %in%
                         top_ten_delayed[['carrier_name']])

#Create new summary for each delay cause for each carrier
delay_summary <- subset_delay %>%
  group_by(carrier_name, year) %>%
  summarize(arr_delay = sum(` arr_delay`), 
            carrier_delay = sum(` carrier_delay`), 
            weather_delay = sum(weather_delay), 
            nas_delay = sum(nas_delay), 
            security_delay = sum(security_delay), 
            late_aircraft_delay = sum(late_aircraft_delay),
            sum_delay = sum(` arr_delay`,` carrier_delay`,weather_delay,
                            nas_delay,security_delay,late_aircraft_delay))%>%
  transform(arr_delay_per = arr_delay/sum_delay,
            carrier_delay_per = carrier_delay/sum_delay,
            weather_delay_per = weather_delay/sum_delay,
            nas_delay_per = nas_delay/sum_delay,
            security_delay_per = security_delay/sum_delay,
            late_aircraft_delay_per = late_aircraft_delay/sum_delay)

#Remove NA rows from delay_summary dataset
delay_summary <- na.omit(delay_summary)

#Calcuate the average number of delayed flight by each category from 2005-2017
average_delay_summary <- delay_summary %>%
  group_by(carrier_name)%>%
  summarize(arrival_delay = mean(arr_delay_per), 
            carrier_delay = mean(carrier_delay_per), 
            weather_delay = mean(weather_delay_per), 
            nas_delay = mean(nas_delay_per), 
            security_delay = mean(security_delay_per), 
            late_aircraft_delay = mean(late_aircraft_delay_per))

#Create grouped bar plots
library(reshape2)

average_delay_summary <- data.frame(average_delay_summary)
average_delay_summary <- melt(average_delay_summary, 
                              id.vars = "carrier_name")

ggplot(data = average_delay_summary,
       aes(x=carrier_name, y=value, fill=variable,width=.5))+
  geom_bar(stat = 'identity',
           colour="black",
           width = 2,
           position = 'dodge',
           aes(color = variable))+
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.4, size=12))+
  ggtitle("Comparison Between Different Delay Among Airlines")+
  labs(x="Proportion",y="Airline Name") + coord_flip()

#Subseting info for the top five airlines with the highest average number of 
#delayed flights 
p1 <- filter(flight_summary,carrier_name %in% top_ten_delayed[['carrier_name']]) %>%
  mutate(label = if_else(year == max(year), as.character(carrier_name), NA_character_)) %>%
  ggplot(aes(x=year, y=arrivals, color=carrier_name))+
  scale_x_continuous(name = "Year", 
                     breaks = seq(2008, 2018, 1))+
  geom_line()+
  geom_label_repel(aes(label = label), nudge_x = 0.5, na.rm = TRUE)

p2 <- filter(flight_summary,carrier_name %in% top_ten_delayed[['carrier_name']]) %>%
  mutate(label = if_else(year == max(year), as.character(carrier_name), NA_character_)) %>%
  ggplot(aes(x=year, y=delayed, color=carrier_name))+
  scale_x_continuous(name = "Year", 
                     breaks = seq(2008, 2018, 1))+
  geom_line()+
  geom_label_repel(aes(label = label), nudge_x = 0.5, na.rm = TRUE)

p3 <- filter(flight_summary,carrier_name %in% top_ten_delayed[['carrier_name']]) %>%
  mutate(label = if_else(year == max(year), as.character(carrier_name), NA_character_)) %>%
  ggplot(aes(x=year, y=on_time, color=carrier_name))+
  scale_x_continuous(name = "Year", 
                     breaks = seq(2008, 2018, 1))+
  geom_line()+
  geom_label_repel(aes(label = label), nudge_x = 0.5, na.rm = TRUE)

grid.arrange(p1, p2, p3)

##Save data as CSV files
write.csv(top_ten_delayed, file="top_ten_delay.csv", row.names = FALSE)
write.csv(top_ten_ontime, file="top_ten_on_time.csv", row.names = FALSE)
write.csv(average_delay_summary, file="most_common_delay_cause.csv", row.names = FALSE)

################## Linear Regression ##################
#Copy the data
Airlines <- flight

# Train set (%70) and test set (30%)
set.seed(15071)
spl <- sample(nrow(Airlines), 0.7*nrow(Airlines))
AirlinesTrain <- Airlines[spl, ]
temp <- Airlines[-spl, ]

#Ensure trainset contains carrier_name and airport from testset
#with no NA values
temp <- temp%>%
  semi_join(AirlinesTrain, by = "carrier_name")%>% 
  semi_join(AirlinesTrain, by = "airport")%>% 
  na.omit(cols = c("arr_del15"))

AirlinesTest <- temp
rm(temp) #remove temp

#train model using all of the other variables as independent variables
delayLR <- lm(arr_del15 ~., data = AirlinesTrain)

#The root mean squared error
RMSE <- function(true_values, predicted_values){
  sqrt(mean((true_values - predicted_values)^2))}

#Predict number of delays on Test set
delayLRpred <- predict.lm(delayLR, newdata = AirlinesTest)

#Get R-squared
Rsquared <-summary(delayLR)$r.squared

#Get Adjusted R-squared
AdjRsquared <-summary(delayLR)$adj.r.squared

# Results for RMSE, R-squared, Adj R-squared
rmse <- RMSE(AirlinesTest$arr_del15, delayLRpred)
results <- tibble(statistics = "RMSE", results = rmse)
results <- bind_rows(results, tibble(statistics="R-squared",
                                     results = Rsquared ))
results <- bind_rows(results, tibble(statistics="Adjusted R-squared",
                                     results = AdjRsquared ))

results

#combine Test set and predicted number of delays
output <- cbind(AirlinesTest, delayLRpred)

#Show top 15 preditec scores
output %>% select(c(year ,carrier_name,airport,arr_del15,delayLRpred)) %>% 
  head(15) %>% knitr::kable()

#Save file 
write.csv(output,"Flights_with_predicted_delays.csv", na = "", row.names=FALSE)




