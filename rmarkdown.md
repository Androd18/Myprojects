Report R-markdown
================
Andres R.
12/19/2021

> **Loading environment, cleaning data and performing calculations**

**We load our libraries to start the data cleaning**

``` r
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)
```

**Now we load the 12 months data from the
[link](https://divvy-tripdata.s3.amazonaws.com/index.html)**

``` r
Dec_2020 <- read_csv("202012-divvy-tripdata.csv")
Jan_2021 <- read_csv("202101-divvy-tripdata.csv")
Feb_2021 <- read_csv("202102-divvy-tripdata.csv")
Mar_2021 <- read_csv("202103-divvy-tripdata.csv")
Apr_2021 <- read_csv("202104-divvy-tripdata.csv")
May_2021 <- read_csv("202105-divvy-tripdata.csv")
Jun_2021 <- read_csv("202106-divvy-tripdata.csv")
Jul_2021 <- read_csv("202107-divvy-tripdata.csv")
Aug_2021 <- read_csv("202108-divvy-tripdata.csv")
Sep_2021 <- read_csv("202109-divvy-tripdata.csv")
Oct_2021 <- read_csv("202110-divvy-tripdata.csv")
Nov_2021 <- read_csv("202111-divvy-tripdata.csv")
```

**We compare columns in the data frames loaded to see if there are
inconsistencies**

``` r
compare_df_cols(Dec_2020, Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021,
                Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, Nov_2021,
                return = "mismatch")
```

    ##  [1] column_name Dec_2020    Jan_2021    Feb_2021    Mar_2021   
    ##  [6] Apr_2021    May_2021    Jun_2021    Jul_2021    Aug_2021   
    ## [11] Sep_2021    Oct_2021    Nov_2021   
    ## <0 rows> (or 0-length row.names)

**We did not find any inconsistency so we proceed to stack all the data
frames into one data frame**

``` r
all_trips <- bind_rows(Dec_2020,Jan_2021,Feb_2021,Mar_2021,Apr_2021,May_2021,
                       Jun_2021,Jul_2021,Aug_2021,Sep_2021,Oct_2021,Nov_2021)
```

-   We delete the columns (start_lat, start_lng, end_lat and end_lng)for
    the new data frame as they will not be relevant for our analysis.

``` r
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

**Inspection of the new data frame**

**Adding new columns for further calculations(date, month, day, year,
day_of_week and ride_length)**

``` r
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

**The new column ‘ride_length’ is changed to a numeric mode for
calculations**

``` r
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
```

-   As some of the bikes are taken out from the docks to perform quality
    tests we remove them as same as the NA values and a new data frame
    is created with the new data.

``` r
all_trips_new <- all_trips[!(all_trips$start_station_name == "HQ QR" | 
                              all_trips$ride_length<0),]
all_trips_new<-na.omit(all_trips_new)
```

> **Performing calculations:**

**Analysis on ride length**

``` r
mean(all_trips_new$ride_length)/60
```

    ## [1] 21.95237

``` r
median(all_trips_new$ride_length)/60
```

    ## [1] 12.31667

``` r
max(all_trips_new$ride_length)/60
```

    ## [1] 55944.15

``` r
min(all_trips_new$ride_length)/60
```

    ## [1] 0

**Comparing ride_length between Casual and Members**

``` r
aggregate(all_trips_new$ride_length/60 ~ all_trips_new$member_casual, FUN = mean)
```

    ##   all_trips_new$member_casual all_trips_new$ride_length/60
    ## 1                      casual                     32.62101
    ## 2                      member                     13.29073

``` r
aggregate(all_trips_new$ride_length/60 ~ all_trips_new$member_casual, FUN = median)
```

    ##   all_trips_new$member_casual all_trips_new$ride_length/60
    ## 1                      casual                        16.75
    ## 2                      member                         9.80

``` r
aggregate(all_trips_new$ride_length/60 ~ all_trips_new$member_casual, FUN = max)
```

    ##   all_trips_new$member_casual all_trips_new$ride_length/60
    ## 1                      casual                    55944.150
    ## 2                      member                     1499.833

``` r
aggregate(all_trips_new$ride_length/60 ~ all_trips_new$member_casual, FUN = min)
```

    ##   all_trips_new$member_casual all_trips_new$ride_length/60
    ## 1                      casual                            0
    ## 2                      member                            0

**Average trip duration and number of rides between both users**

``` r
all_trips_new %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday)   
```

    ## # A tibble: 14 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              402526             37.7
    ##  2 casual        Mon              226837             32.7
    ##  3 casual        Tue              214196             28.8
    ##  4 casual        Wed              215210             28.3
    ##  5 casual        Thu              219883             28.1
    ##  6 casual        Fri              284477             31.1
    ##  7 casual        Sat              464622             35.0
    ##  8 member        Sun              310483             15.3
    ##  9 member        Mon              341706             12.8
    ## 10 member        Tue              386435             12.5
    ## 11 member        Wed              387918             12.6
    ## 12 member        Thu              362173             12.5
    ## 13 member        Fri              355300             12.9
    ## 14 member        Sat              353590             14.9

**Rider data by month**

``` r
all_trips_new %>%
  group_by(member_casual, month) %>%
  summarise(num_of_rides = n(), average_duration = mean(ride_length)/60)%>%
  arrange(member_casual, month) %>%
  print(n = 24)
```

    ## # A tibble: 24 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual month num_of_rides average_duration
    ##    <chr>         <chr>        <int>            <dbl>
    ##  1 casual        01           14690             26.4
    ##  2 casual        02            8613             47.1
    ##  3 casual        03           75641             38.5
    ##  4 casual        04          120420             38.4
    ##  5 casual        05          216829             39.6
    ##  6 casual        06          304189             38.5
    ##  7 casual        07          369407             33.3
    ##  8 casual        08          341469             28.6
    ##  9 casual        09          292926             28.1
    ## 10 casual        10          189117             26.3
    ## 11 casual        11           69958             22.5
    ## 12 casual        12           24492             27.6
    ## 13 member        01           68819             12.0
    ## 14 member        02           34383             14.8
    ## 15 member        03          130048             13.7
    ## 16 member        04          177783             14.3
    ## 17 member        05          234164             14.3
    ## 18 member        06          304585             14.1
    ## 19 member        07          322902             13.8
    ## 20 member        08          332916             13.6
    ## 21 member        09          328192             13.1
    ## 22 member        10          288855             12.0
    ## 23 member        11          185909             11.0
    ## 24 member        12           89049             12.3

**Rider data by type of bike used**

``` r
all_trips_new %>%
  group_by(member_casual, rideable_type) %>%
  summarise(num_of_rides = n(), average_duration = mean(ride_length)/60)%>%
  arrange(member_casual, rideable_type)
```

    ## # A tibble: 6 × 4
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type num_of_rides average_duration
    ##   <chr>         <chr>                <int>            <dbl>
    ## 1 casual        classic_bike       1253104             26.0
    ## 2 casual        docked_bike         312100             77.7
    ## 3 casual        electric_bike       462547             20.1
    ## 4 member        classic_bike       1958931             13.6
    ## 5 member        docked_bike           7774             12.2
    ## 6 member        electric_bike       530900             12.2

**Creating separate summary data frames**

``` r
summary_membercasual <- all_trips_new %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)/60) %>% 
  arrange(member_casual, weekday)   %>%
  print(n = 14)
```

    ## # A tibble: 14 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              402526             37.7
    ##  2 casual        Mon              226837             32.7
    ##  3 casual        Tue              214196             28.8
    ##  4 casual        Wed              215210             28.3
    ##  5 casual        Thu              219883             28.1
    ##  6 casual        Fri              284477             31.1
    ##  7 casual        Sat              464622             35.0
    ##  8 member        Sun              310483             15.3
    ##  9 member        Mon              341706             12.8
    ## 10 member        Tue              386435             12.5
    ## 11 member        Wed              387918             12.6
    ## 12 member        Thu              362173             12.5
    ## 13 member        Fri              355300             12.9
    ## 14 member        Sat              353590             14.9

``` r
summary_month <- all_trips_new %>%
  group_by(member_casual, month) %>%
  summarise(num_of_rides = n(), average_duration = mean(ride_length)/60)%>%
  arrange(member_casual, month) %>%
  print(n = 24)
```

    ## # A tibble: 24 × 4
    ## # Groups:   member_casual [2]
    ##    member_casual month num_of_rides average_duration
    ##    <chr>         <chr>        <int>            <dbl>
    ##  1 casual        01           14690             26.4
    ##  2 casual        02            8613             47.1
    ##  3 casual        03           75641             38.5
    ##  4 casual        04          120420             38.4
    ##  5 casual        05          216829             39.6
    ##  6 casual        06          304189             38.5
    ##  7 casual        07          369407             33.3
    ##  8 casual        08          341469             28.6
    ##  9 casual        09          292926             28.1
    ## 10 casual        10          189117             26.3
    ## 11 casual        11           69958             22.5
    ## 12 casual        12           24492             27.6
    ## 13 member        01           68819             12.0
    ## 14 member        02           34383             14.8
    ## 15 member        03          130048             13.7
    ## 16 member        04          177783             14.3
    ## 17 member        05          234164             14.3
    ## 18 member        06          304585             14.1
    ## 19 member        07          322902             13.8
    ## 20 member        08          332916             13.6
    ## 21 member        09          328192             13.1
    ## 22 member        10          288855             12.0
    ## 23 member        11          185909             11.0
    ## 24 member        12           89049             12.3

``` r
summary_type <- all_trips_new %>%
  group_by(member_casual, rideable_type) %>%
  summarise(num_of_rides = n(), average_duration = mean(ride_length)/60)%>%
  arrange(member_casual, rideable_type) %>%
  print(n = 6)
```

    ## # A tibble: 6 × 4
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type num_of_rides average_duration
    ##   <chr>         <chr>                <int>            <dbl>
    ## 1 casual        classic_bike       1253104             26.0
    ## 2 casual        docked_bike         312100             77.7
    ## 3 casual        electric_bike       462547             20.1
    ## 4 member        classic_bike       1958931             13.6
    ## 5 member        docked_bike           7774             12.2
    ## 6 member        electric_bike       530900             12.2

**Creating visualization for the data**

-   Plot for average ride per day of the week

``` r
          ggplot(summary_membercasual, 
          aes(x = weekday, y = average_duration, fill = member_casual,
          color = member_casual)) +
          ylim(0,45) +
          geom_col(width = 0.6, position = position_dodge(0.7)) +
          theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
          theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
          scale_fill_manual("member_casual", values = c("casual" = "#556B2F", "member" = "#A2CD5A")) +
          labs(title = "Average Ride Length by Weekday") + 
          theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0)) +
          ylab("Ride length") + 
          xlab("Day of Week")
```

-   Plot for average number of rides per day of the week

``` r
          ggplot(summary_membercasual, 
                aes(x = weekday, y = number_of_rides, fill = member_casual,
                    color = member_casual)) +
          geom_col(width = 0.6, position = position_dodge(0.7)) +
          theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
          theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
          scale_fill_manual("member_casual", values = c("casual" = "#104E8B", "member" = "#00BFFF")) +
          labs(title = "Number of Rides by Day") + 
          ylab("Number of rides") + 
          xlab("Day of Week")
```

-   Plot for average ride per month

``` r
         ggplot(summary_month, 
                aes(x = month, y = average_duration, fill = member_casual,
                    color = member_casual, group = member_casual)) + 
         ylim(0,50) +
         geom_line(linetype = "dashed") + 
         geom_point() +
         theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
         theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
         labs(title = "Average Ride Duration by Month") +
         ylab("Ride length (minutes)") + 
         xlab("Month")
```

-   Plot for average number of rides per month

``` r
                ggplot(summary_month, 
                aes(x = month, y = num_of_rides, fill = member_casual,
                    color = member_casual, group = member_casual)) + 
                geom_line(linetype = "dotted") + 
                geom_point() +
                theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
                theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
                labs(title = "Average Number of Rides by Month") +
                ylab("Number of Rides") + 
                xlab("Month")
```

-   Plot for average ride length by type of bike

``` r
  ggplot(summary_type, aes(x = rideable_type, y = average_duration, 
                                  fill = member_casual, color = member_casual)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
  theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
  scale_fill_manual("member_casual", values = c("casual" = "#00868B", "member" = "#5CACEE")) +
  labs(title = "Average Ride Length by type of bike") +
  ylab("Ride length (minutes)") + 
  xlab("Type of Bike")
```

-   Plot for average number of rides by type of bike

``` r
  ggplot(summary_type, aes(x = rideable_type, y = num_of_rides, 
                                  fill = member_casual, color = member_casual)) +
  geom_col(width = 0.6, position = position_dodge(0.7)) +
  theme(panel.grid.major.y = element_line(color = "black", size = 0.1, linetype = 1)) +
  theme(panel.grid.minor.y = element_line(color = "black", size = 0.1, linetype = 1)) +
  scale_fill_manual("member_casual", values = c("casual" = "#838B83", "member" = "#E0EEE0")) +
  labs(title = "Average Number of Rides by type of bike") +
  ylab("Number of Rides") + 
  xlab("Type of Bike")
```
