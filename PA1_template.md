Reproducible Research
=====================

<font color = "#112467" face = Times New Roman>Introduction</font>
------------------------------------------------------------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices

such as a [Fitbit](http://www.fitbit.com/), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone
Up](https://jawbone.com/up). These type of devices are part of the
"quantified self" movement

a group of enthusiasts who take measurements about themselves regularly
to improve their health, to find patterns

in their behavior, or because they are tech geeks. But these data remain
under-utilized both because the raw data

are hard to obtain and there is a lack of statistical methods and
software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data

at 5 minute intervals through out the day. The data consists of two
months of data from an anonymous

individual collected during the months of October and November, 2012 and
include the number of steps

taken in 5 minute intervals each day.

**The data for this assignment can be downloaded from the course web
site:**

-   **Dataset:** [Activity monitoring
    data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
    \[52k\]

**The variables included in this dataset are:**

-   **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as NA)  
-   **date**: The date on which the measurement was taken in YYYY-MM-DD
    format  
-   **interval**: Identifier for the 5-minute interval in which
    measurement was taken

<font color = "#112467" face = Times New Roman>loading required libraries</font>
--------------------------------------------------------------------------------

    library(readr)
    library(stringr)
    library(tidyr)
    library(dplyr)
    library(purrr)
    library(lubridate)
    library(ggplot2)

<font color = "#112467" face = Times New Roman>Importing data</font>
--------------------------------------------------------------------

    activity <- read_csv("activity.csv")

<font color = "#112467" face = Times New Roman>What is mean total number of steps taken per day?</font>
-------------------------------------------------------------------------------------------------------

1- Calculate the total number of steps taken per day

    activity1 <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(steps = sum(steps))

2- Make a histogram of the total number of steps taken each day

    activity1 %>%
    ggplot(aes(x = steps))+ 
        geom_histogram(fill = "#112367", color = "black", binwidth = 1000) + 
        labs(title = str_wrap("Total Number of Steps taken each day", indent = 45),
             x = "Steps per Day", 
             y = "Frequency")+
        scale_y_continuous(breaks = c(seq(0,10,by = .5)))+
        theme_bw()

![](steps%20by%20day-1.png)

3- Calculate and report the mean and median of the total number of steps
taken per day

    steps_mean <- as.integer(round(mean(activity1$steps)))
    steps_median <- as.integer(round(median(activity1$steps)))

The Mean of the steps equals 10766, while the Median equals 10765

<font color = "#112467" face = Times New Roman>What is the average daily activity pattern?</font>
-------------------------------------------------------------------------------------------------

1- Transforming data

    activity2 <- activity %>%
        filter(!is.na(steps), !is.na(interval)) %>%
        mutate(
            hrs = parse_character(interval %/% 100),
            mins =parse_character(interval %% 100),
            hrs = ifelse(str_length(hrs) == 1, str_c("0", hrs, sep = ""), hrs),
            mins = ifelse(str_length(mins) == 1, str_c("0", mins, sep = ""), mins)) %>%
        unite(time, hrs, mins, sep = ":") %>%
        mutate(
            time = parse_time(time, "%H:%M"))%>%
        group_by(time) %>%
        summarise(steps = mean(steps))

2- Plotting

    activity2 %>%
        ggplot(aes(time, steps))+
        geom_line(color = "#112367", size = 1)+
        labs(x = "Time(Hours)", y = "average steps")+
        theme_bw()

![](Avg%20daily%20activity-1.png)

3- Which 5-minute interval, on average across all the days, contains the
maximum number of steps?

    activity2 <- activity2 %>% 
        filter(steps == max(steps)) 
    stringr::str_c("The Maximum Number of Steps equals: ", 
                   round(activity2$steps),
                   " at Time equals: ",
                   activity2$time)

    ## [1] "The Maximum Number of Steps equals: 206 at Time equals: 08:35:00"

<font color = "#112467" face = Times New Roman>Imputing missing values</font>
-----------------------------------------------------------------------------

*Note that there are a number of days/intervals where there are missing
values (coded as NA)*

1- Calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with NAs)

    steps_missing <- sum(is.na(activity$steps))

The Total Number of Missing Values is 2304

2- Devise a strategy for filling in all of the missing values in the
dataset.

*I think the best strategy is to replace missing values with the most
recent Non-missing value*

    steps_non.missing <- activity %>%
        group_by(date) %>%
        filter(!every(steps,is.na)) %>%
        fill(steps)

3- Create a new dataset that is equal to the original dataset but with
the missing data filled in.

    activity3 <- steps_non.missing %>%
        left_join(activity, by = c("date", "interval")) %>%
        select(-steps.y) %>%
        rename(steps = steps.x)

4- Make a histogram of the total number of steps taken each day

    activity3 %>%
        group_by(date) %>%
        summarise(steps = sum(steps))%>%
        ggplot(aes(x = steps))+ 
        geom_histogram(fill = "#112367", color = "black", binwidth = 1000) + 
        scale_y_continuous(breaks = c(seq(0,10,by = .5)))+
        labs(title = str_wrap("Total Steps each day(NA replaced)",indent = 50),
             x = "Steps", 
             y = "Frequency")+
        theme_bw()

![](steps%20Non-missing-1.png)

And Calculate and report the mean and median total number of steps taken
per day

    steps_non.missing <- steps_non.missing %>%
        group_by(date) %>%
        summarise(steps = sum(steps)) %>%
        summarise (mean = as.integer(mean(steps)), median = as.integer(median(steps)))

Average Total Number of Steps is 10766, and Median is 10765

<font color = "#112467" face = Times New Roman>Are there differences in activity patterns between weekdays and weekends?</font>
-------------------------------------------------------------------------------------------------------------------------------

1- ransforming data

    activity4 <- activity %>%
        filter(!is.na(steps), !is.na(interval)) %>%
        mutate(
            hrs = parse_character(interval %/% 100),
            mins =parse_character(interval %% 100),
            hrs = ifelse(str_length(hrs) == 1, str_c("0", hrs, sep = ""), hrs),
            mins = ifelse(str_length(mins) == 1, str_c("0", mins, sep = ""), mins)) %>%
        unite(time, hrs, mins, sep = ":") %>%
        mutate(
            time = parse_time(time, "%H:%M"),
            week_type = ifelse(wday(date, label = TRUE) %in% c("Sun","Sat"), "Weekend", "Weekday")) %>%
        group_by(time, week_type) %>%
        summarise(steps = mean(steps))

2- Plotting

    activity4 %>%
        ggplot(aes(x = time, y = steps, color = week_type))+
        geom_line(size = 1)+
        scale_y_continuous(breaks = seq(0, 240, by = 10))+
        theme_bw() +
        labs(color = "Week type", title = str_wrap("Average daily steps", indent = 55), x = "Time(Hours)")

![](activity%20by%20weektype-1.png)
