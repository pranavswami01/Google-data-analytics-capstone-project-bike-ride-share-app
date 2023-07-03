# Google-data-analytics-capstone-project-bike-ride-share-app

# INTRODUCTION TO THE CYCLISTIC CASE STUDY

This project is a part of the capstone project of the Google Data Analytics Professional Certificate. It will follow the 6 steps of Data analysis learned during the online classes.

## Step 1: Ask

### 1.Cyclistic

Cyclistic is a fictional bike-sharing company based in Chicago, Illinois. They possess 5800 bikes (and also cargo-bikes, hand-tricycles and reclining bikes) and 600 docked stations throughout the city.

### 2.Business task

Analyzing historical trip data from Cyclistic to identify trends and connections between casual riders and annual members.

### 3.Key Stakeholders

**Lily Moreno**: She is the manager and the director of marketing. She is handling the development of campaigns and the ways to promote the bike sharing program.

**Cyclistic Executive team**: A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy.

## Step 2: Prepare 

### 1.Information on data 

The data has been shared publicly by Motivate International inc under strict licence. It is composed of monthly/ quarterly spreadsheet files from April 2020 to August 2022. The data have been collected from the use of Motival International inc's bikes and it includes Ride id, type of bikes, member status, date of ride, hour of ride, start station, end station, station id, coordinate of the station ...

### 2.Limitations of data. 

The data obtained is from April 2020 to August 2022. I choose to work on two years worth of data from April 2020 to April 2022 due to the huge amount of data, the analysis can only be judged as a multi-year analysis. As it is publicly shared on Kaggle, I have no doubt on its integrity. 

### 3.Is the data ROCC ?

A good data source is ROCCC which stands for Reliable, Original, Comprehensive, Current, and Cited.

Reliable - High - It has thousands of rides.

Original - High - It is first-party data. 

Comprehensive - MED - It matches most of Cyclistic's product parameters. 

Current - High - The data is from 2020 to 2022.

Cited - Med - It is first-party data. 

# Step 3: Process 

I used R to conduct my analysis due to the accessibility of the program, amount of data and being able to create the visualisations.

### 1.Installing packages and library 

```{r}

install.packages("tidyverse")

install.packages("ggplot2")

install.packages("lubridate")

install.packages("janitor")

install.packages("ggpubr")

install.packages("skimr")

install.packages("here")

install.packages("ggrepel")

```

```{r}

library(tidyverse)

library(janitor)

library(here)

library(skimr)

library(ggplot2)

library(lubridate)

```

I chose these packages to help me with my analysis.

### 2.Importing and preparing the data

```{r}

X202004_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202004-divvy-tripdata.csv"),

X202005_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202005-divvy-tripdata.csv"),

X202006_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202006-divvy-tripdata.csv"),

X202007_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202007-divvy-tripdata.csv"),

X202008_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202008-divvy-tripdata.csv"),

X202009_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202009-divvy-tripdata.csv"),

X202010_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202010-divvy-tripdata.csv"),

X202011_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202011-divvy-tripdata.csv"),

X202012_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202012-divvy-tripdata.csv"),

X202101_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202101-divvy-tripdata.csv"),

X202102_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202102-divvy-tripdata.csv"),

X202103_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202103-divvy-tripdata.csv"),

X202104_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202104-divvy-tripdata.csv"),

X202105_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202105-divvy-tripdata.csv"),

X202106_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202106-divvy-tripdata.csv"), 

X202107_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202107-divvy-tripdata.csv"), 

X202108_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202108-divvy-tripdata.csv"), 

X202109_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202109-divvy-tripdata.csv"), 

X202110_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202110-divvy-tripdata.csv"), 

X202111_divvy_tropdata <- read_csv("cyclistic spreadsheet files /202111-divvy-tripdata.csv"), 

X202112_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202112-divvy-tripdata.csv"), 

X202201_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202201-divvy-tripdata.csv"), 

X202202_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202202-divvy-tripdata.csv"), 

X202203_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202203-divvy-tripdata.csv")

X202204_divvy_tripdata <- read_csv("cyclistic spreadsheet files /202204-divvy-tripdata.csv")

```

I also checked the data for inconsistencies in col datatypes. I noticed that some dataframes were wrongly formatted and I had to change that. I did the same code for every table. 

```{r}

X202102_divvy_tripdata < mutate(X202102_divvy_tripdata, started_at = as.character(started_at), ended_at = as.character(ended_at))

```

### 3.Data cleaning 

I checked the type of each bike offered and the member status of every table. 

```{r}

n_unique(X202102_divvy_tripdata$rideable_type)

n_unique(X202102_divvy_tripdata$member_casual)

```

> [1] 3

> [1] 2

I checked for duplicated rides in every table. 

```{r}

sum(duplicated(X202102_divvy_tripdata))

```

> [1] 0

I removed negative time of every table. 

```{r}

X202102_divvy_tripdata %>% filter(ended_at < started_at) %>% count()

bike_tripdata_202102 <- bike_tripdata_202102 %>% filter(ended_at > started_at)

```

I removed rows with NA values in every table. 

```{r}

bike_tripdata_202102 <- bike_tripdata_202102 %>% drop_na()

```

I cleaned and formatted the column names of every table. 

```{r}

clean_names(bike_tripdata_202102)

bike_tripdata_202102 <- rename_with(bike_tripdata_202102, tolower)

```

I formatted the datetime column from chr to datetime in every table. 

```{r}

bike_tripdata_202102 <- bike_tripdata_202102 %>% mutate(started_at = ymd_hms(started_at), ended_at = ymd_hms(ended_at))

```

I merged and renamed the file. 

```{r}

bike_tripdata <- bind_rows (X202004_divvy_tripdata, X202005_divvy_tripdata, X202006_divvy_tripdata,, X202007_divvy_tripdata, X202008_divvy_tripdata, X202009_divvy_tripdata, X202010_divvy_tripdata, X202011_divvy_tripdata, X202012_divvy_tripdata, X202101_divvy_tripdata, X202102_divvy_tripdata, X202103_divvy_tripdata, X202104_divvy_tripdata, X202105_divvy_tripdata, X202106_divvy_tripdata,  X202107_divvy_tripdata, X202108_divvy_tripdata,  X202109_divvy_tripdata, X202110_divvy_tripdata, X202111_divvy_tropdata, X202112_divvy_tripdata, X202201_divvy_tripdata, X202202_divvy_tripdata, 

X202203_divvy_tripdata, X202204_divvy_tripdata)

```

I extracted the day from the started_at column. 

```{r}

bike_tripdata <- bike_tripdata %>%  mutate(weekday = wday(started_at, label = TRUE, abbr = TRUE))

```

I extracted the time from the started_at and ended_at columns. 

```{r}

bike_tripdata <- bike_tripdata %>% mutate(start_time = format(started_at, "%H:%M:%S"))%>% mutate(end_time = format(ended_at, "%H:%M:%S")) %>% mutate(start_time = hms(start_time))%>% mutate(end_time = hms(end_time))

```

I extracted the hour from the started_at column.

```{r}

bike_tripdata <- bike_tripdata %>% mutate(hour = hour(start_time))

```

I created a duration column. 

```{r}

bike_tripdata$duration <- difftime(bike_tripdata$ended_at, bike_tripdata$started_at, units = "mins")

```

# Step 4: Analyse

### 1.Determine the number of members vs casual riders.

I wanted to determine the number of actual members vs the number of casual riders.

```{r}

table(bike_tripdata$member_casual)

table(bike_tripdata$rideable_type)

```

Casual : 1,763,917

Member : 2,802,582

I plotted the results.

[Count of users.pdf]

From April 2020 to April 2022, the company had more members (customers that pay a membership fee) than casual riders.

### 2. Number of rides per user and type of bikes.

Then, I wanted to see the number of rides per user (member and casual) by rides offered (electric, classic and docked bikes) and also the percentage. 

```{r}

bike_percentage <- bike_tripdata %>% select(ride_id, member_casual, rideable_type) %>% group_by(rideable_type, member_casual) %>% count() %>% mutate(percentage = (n/42994*100)) %>% arrange(-percentage)

```

I also plotted the results. 

[Usage of bikes between casual and members riders.pdf]

Docked bikes are the go-to bikes from casual riders and member riders. Classic bikes (which are dockless are number 2 in casual but close second for members) and electric bikes are third. Both casual and members seem to like to rely on dock bikes and trust the system to go around. 

### 3. The number of rides per month.

I wondered which months had the most users.

```{r}

bike_trip_month <- bike_tripdata %>% select(member_casual, month) %>% group_by(member_casual, month)%>% count()%>% arrange(member_casual)

member_month <-  filter(bike_trip_month, member_casual =="member")

casual_month <-  filter(bike_trip_month, member_casual =="casual")

```

I plotted the results.

[Number of rides per months of member.pdf]
[Number of rides per months of casual users.pdf]


May, July and September are casual’s busiest months. It is no surprise to witness spring and summer months being the busiest month for the bike sharing company. The members are again more consistent in their use of the bike throughout the year with May, July and November being the busiest but nine month out of the twelves have at least 200,000 rides which is surprising. 

### 4.Determine the number of rides for each day. 

I wanted to determine which day of the week was the busiest for each type of membership. 

```{r}
bike_trip_weekdays <- bike_tripdata %>% select(member_casual, weekday) %>% group_by(member_casual, weekday)%>% count()%>% arrange(member_casual)

member_weekday <-  filter(bike_trip_weekdays, member_casual =="member")

casual_weekday <-  filter(bike_trip_weekdays, member_casual =="casual")

```
And I plotted the results. 

[Number of rides per days of casual users.pdf]
[Number of rides per days of member.pdf]
According to the plots, member riders are the most consistent in using the bikes. 5 days of the week exceeding the 400,000 rides in the member category with Tuesday, Wednesday, Thursday in the top 3. The casual riders mostly use the bikes on Saturday with roughly 400000 rides. 

The difference between member and casual riders can be explained through the idea of commute + leisure for the member but only for leisure for the casual riders. 

### 5.Getting the number of rides per hour. 

I wanted to be more precise by having the number of rides per hour for each membership.  

```{r}

bike_trip_hour <- bike_tripdata %>% select(member_casual, hour) %>% group_by(member_casual, hour)%>% count()%>% arrange(hour)

member_hour <-  filter(bike_trip_hour, member_casual =="member")

casual_hour <- filter(bike_trip_hour, member_casual =="casual")

```

And I plotted the results.

[Number of rides per hours of member.pdf]
[Number of rides per hours of casual users.pdf]

Member riders mostly use the bike to commute back home with a massive peak of use between 3 pm and 8pm (15h to 20h). The same time slot for the casual riders is the highest peak but can’t be compared to the member’s usage in terms of number. It can be said that members use bikes throughout the day with some consistency to the difference of the casual users that have a growing pattern from 10 am to 3 pm.

### 6. Mapping of data coordinates of member and casual.

I thought that it would be a very interesting way to show ride data. I used Tableau to do it using the map graph.

[Most used bike routes by member.pdf]
[Most used bike stations by casual users.pdf]

As the graphs show, Member riders are along the lake-shore and a bit more inland but use more stations overall and have more ride from a station a to a station b. Casual riders on the other hand ride more along the lake-shore and have more rides from station a and return to that same station a.

### 7.Mean duration for member and casual riders. 

```{r}

bike_trip_mean_duration <- bike_tripdata %>% select(member_casual, duration)%>% summarise(mean_duration = mean(duration))

casual_mean_duration <-  bike_tripdata %>% filter(member_casual == 'casual')

casual_mean_duration %>% select(member_casual, duration) %>% summarise(mean_duration = mean(duration))

member_mean_duration <-  bike_tripdata %>% filter(member_casual == 'member')

member_mean_duration %>% select(member_casual, duration)%>% summarise(mean_duration = mean(duration))

mean_duration <- bike_tripdata %>% group_by(member_casual)%>% summarise(mean_duration = mean(duration))

```

And I plotted the results. 

[Mean duration travelled by riders.pdf]

I can confirm that the mean duration of a bike trip by casual riders is more than 40 minutes long compared to the 15 minutes of the member riders. It confirmes that the casual riders use the bike for leisure. 

# Step 5: Share phase

See the graphs throughout this document

# Step 6: Act phase

As I said, over the two-year period that I covered (2020/2022), there were more members than casual riders. The members preferred the docked bikes and the classic bike being close second than electrical bikes.

Members ride more during the week than casual members that ride mostly during the weekend, but casual members have a longer ride duration than members. It can be explained by the fact that members ride to commute to work or to go home while casuals ride for leisure.

Members and casual riders ride more in the evening period, but members also ride a lot during the morning of the work week.

Recommendations :

- Plan campaigns to promote the health benefits of using bikes to commute to work and the environmental benefits.

- Create an app that could track the activity of riders (i.e. calories burned or CO2 rejections prevented).

- Create partnerships with local companies to promote the usage of bikes through discount memberships or stations nearby.

- Increase bike availability in the top 10 start stations.
