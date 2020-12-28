---
Title: "Reproducible Research: Course Project 1"
Author: "Kenisha Jn Baptiste"
Date: "December 27th, 2020"
Output: 
  html_document:
    keep_md: true
---


## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

* **Steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA) </br>
* **Date:** The date on which the measurement was taken in YYYY-MM-DD format </br>
* **Interval:** Identifier for the 5-minute interval in which measurement was taken </br>

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
</br>

### Loading and preprocessing the data

1.1. Downloading and reading in the dataset.

```{r}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
rawData <- read.csv("activity.csv", stringsAsFactors = FALSE)
dim(rawData)
```

Total number of observations in this dataset:  17568 3
</br>

1.2. Preprocessing the data (Removing NAs)

```{r}
noNAData <- rawData[complete.cases(rawData), ]
dim(noNAData)
```

Total number of observations minus NAs in this dataset: 15264 3
</br>

1.3. Reading csv Data into Data.Table. 

```{r}
library("data.table")
activityDT <- data.table(noNAData)
```

### What is mean total number of steps taken per day?

1.1. Initiate package to create Histogram of the total number of steps taken each day.

```{r}
library(ggplot2)
```

1.2. Calculate the total number of steps taken per day

```{r}
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
```

1.3. Printing total number of steps for the first 10 days in Activity Data.

```{r}
head(Total_Steps, 10)
```

Ans:

Date | Steps
--- | --- 
 1: 2012-10-02 | 126 
 2: 2012-10-03 | 11352
 3: 2012-10-04 | 12116
 4: 2012-10-05 | 13294
 5: 2012-10-06 | 15420
 6: 2012-10-07 | 11015
 7: 2012-10-09 | 12811
 8: 2012-10-10 | 9900
 9: 2012-10-11 | 10304
10: 2012-10-12 | 17382
</br>

2.0. Create histogram of the total number of steps taken each day. 

```{r}
Plot1 <- ggplot(Total_Steps, aes(as.factor(date), steps)) +
  geom_histogram(color = "darkblue", fill ="lightblue", stat = "identity") + 
  xlab("Date") + 
  ylab("Step Frequency") +
  ggtitle("Histogram of Total Number of Steps Taken (per Day)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

Ans:

```{r}
Plot1
```

3.0. Calculate and report the mean and median of the total number of steps taken per day.

```{r}
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

Ans: <br>

Mean Steps  |  Median Steps
--- | --- 
10766.19  | 10765
</br>

### What is the average daily activity pattern?


1.1. Computing 5-minute interval, on average across all the days in the dataset.

```{r}
IntervalDT <- activityDT[, c(lapply(.SD, mean)), .SDcols = c("steps"), by = .(interval)] 
```

1.2. A time series plot (i.e. type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r}
Plot2 <- ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avgerage Daily Step Activity Pattern", x = "5-Minute Interval", y = "Average Across all Days")
```

Ans:

```{r}
Plot2
```

2.0. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
IntervalDT[steps == max(steps), .(max_interval = interval)]
```
Ans:  <br>
**Max Interval**
      835
</br>

### Imputing missing values


1.0. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```{r}
numNA <- nrow(rawData) - nrow(noNAData)
```
Ans: <br>

**2304**

</br>

2.0. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Accidentally overwrote data. Starting from scratch.*

Reading data into new code

```{r}
activityCDT <- data.table::fread(input = "activity.csv")
```

Calculating the total number of missing values in the dataset.

```{r}
activityCDT[is.na(steps), .N ]
```

Calculation is the correct number: 2304
<br>
</br>

2.1. Filling in missing values with median of dataset. 

```{r}
activityCDT[is.na(steps), "steps"] <- activityCDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3.0. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
data.table::fwrite(x = activityCDT, file = "compData.csv", quote = FALSE)
```

Ans:
<br>
New file name "compData.csv" created in folder.
</br>

<br>
</br>

4.0. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


4.1. Total number of steps taken per day.

```{r}
TotalN_Steps <- activityCDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)]
```

4.2. Mean and Median total number of steps taken per day

```{r}
TotalN_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

Ans: <br>

Mean Steps  |  Median Steps
--- | --- 
9354.23    |    10395

</br>

4.3. A histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```{r}
Plot3 <- ggplot(TotalN_Steps, aes(as.factor(date), steps)) +
     geom_histogram(color = "darkblue", fill ="lightblue", stat = "identity") + 
     xlab("Date") + 
     ylab("Step Frequency") +
     ggtitle("Total Number of Steps Taken (per Day)(W/ Missing Values") + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

Ans:

```{r}
Plot3
```

Ans:

4.4. See table below for comparison of the difference between estimates from the first and second dataset.


Estimate | Mean Steps | Median Steps
--- | --- | ---
First Dataset (Without NA) | 10765 | 10765
Second Dataset (W/ Missing Values) | 9354.23 | 10395
</br>

### Are there differences in activity patterns between weekdays and weekends?


1.1. Create a new factor variable in the dataset with two levels – “Weekday” and “Weekend” indicating whether a given date is a Weekday or Weekend day.

```{r}
activityCDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityCDT[, `Day of Week`:= weekdays(x = date)]
activityCDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "Weekday or Weekend"] <- "Weekday"
activityCDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "Weekday or Weekend"] <- "Weekend"
activityCDT[, `Weekday or Weekend` := as.factor(`Weekday or Weekend`)]
```

1.2. Average number of steps taken, averaged across all weekday days or weekend days. 

```{r}
activityCDT[is.na(steps), "steps"] <- activityCDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalCDT <- activityCDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `Weekday or Weekend`)]
```

2.0. Panel Plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
Plot4 <- ggplot(IntervalCDT , aes(x = interval , y = steps, color=`Weekday or Weekend`)) + geom_line() + labs(title = "Average Daily Steps by Weektype", x = "5-Minute Interval", y = "Number of Steps Taken") + facet_wrap(~`Weekday or Weekend` , ncol = 1, nrow=2)
```

Ans:

```{r}
Plot4
```

<p style="text-align:center;"> **End of Project** </p>


<p style="text-align:center;"> **Thank you for reviewing!** </p>
