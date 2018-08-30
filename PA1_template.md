**Loading and preprocessing the data**
--------------------------------------

    # Needed libraries
    library(readr)
    library(fansi)
    library(ggplot2)

    # English system language
    Sys.setenv("LANGUAGE"="En")
    Sys.setlocale("LC_ALL", "English")

    ## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"

    # Loading the data
    activity <- read_csv("activity.csv")

    ## Parsed with column specification:
    ## cols(
    ##   steps = col_integer(),
    ##   date = col_date(format = ""),
    ##   interval = col_integer()
    ## )

    # Let's view some of our data
    head(activity)

    ## # A tibble: 6 x 3
    ##   steps date       interval
    ##   <int> <date>        <int>
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

**What is mean total number of steps taken per day?**
-----------------------------------------------------

    # Calculation of the total number of steps taken per day
    t_steps <- aggregate(activity$steps, by = list(activity$date), sum)

    # Setting column names
    colnames(t_steps) <- c("Date", "Total.Steps")

    # Histogram of Total Steps per day
    hist(t_steps$Total.Steps, main = "Histogram of Total Steps per day", breaks = 20, xlab = "Total Steps", xlim = c(0,25000))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # Mean & median of total steps per day, excluding NA's
    mean(t_steps$Total.Steps, na.rm = TRUE)

    ## [1] 10766.19

    median(t_steps$Total.Steps, na.rm = TRUE)

    ## [1] 10765

**What is the average daily activity pattern?**
-----------------------------------------------

    # Áverage number of steps taken each interval
    avg_steps <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm = TRUE)

    # Setting column names
    colnames(avg_steps) <- c("Interval", "Average.Steps")

    #  Ôime series plot of the 5-minute interval and the average number of steps taken, averaged across all days 
    plot(avg_steps$Average.Steps, type="l", main = "Time Series plot of Average Steps per day", xlab = "5-minute Interval", ylab = "Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # 5-minute interval with the maximum number of steps
    activity[which.max(avg_steps$Average.Steps),3]

    ## # A tibble: 1 x 1
    ##   interval
    ##      <int>
    ## 1      835

**Imputing missing values**
---------------------------

    # Calculation and reporting of the total number of missing values in the dataset
    print(t_NA <- sum(is.na(activity)))

    ## [1] 2304

    head(index <- as.numeric(is.na(activity$steps)))

    ## [1] 1 1 1 1 1 1

    n_activity <- activity

    # total number of NA's
    sum(index) 

    ## [1] 2304

    # filling in all of the missing values in the dataset
    for (i in 1:17568){
        
        if(index[i]==1) {
            
            n_activity$steps[i] <- avg_steps[avg_steps$Interval==activity$interval[i], 2]
        }
        
    }

    # Calculation of the total number of steps taken per day
    n_t_steps <- aggregate(n_activity$steps, by = list(n_activity$date), sum)

    # Setting column names
    colnames(n_t_steps) <- c("Date", "Imp.Total.Steps")

    # Histogram of Total Steps per day with imputed Na's
    hist(n_t_steps$Imp.Total.Steps, main = "Histogram of Total Steps per day", breaks = 10, xlab = "Total Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Mean & median of total steps per day with imputed NA's
    mean(n_t_steps$Imp.Total.Steps)

    ## [1] 10766.19

    median(n_t_steps$Imp.Total.Steps)

    ## [1] 10766.19

    # The impact of imputing missing data on the estimates of the total daily number of steps
    z <- rbind(t_steps$Total.Steps, n_t_steps$Imp.Total.Steps)
    p1 <- hist(t_steps$Total.Steps, main = "Histogram of Total Steps per day", breaks = 10, xlab = "Total Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    p2 <- hist(n_t_steps$Imp.Total.Steps, main = "Histogram of Total Steps per day", breaks = 10, xlab = "Total Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-3.png)

    plot(p1, col=rgb(1/4,0,0,1/4), ylim = c(0,25), xlim = c(0,25000))  
    plot(p2, col=rgb(1,1,0,1/4), add=T)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-4.png)

**Are there differences in activity patterns between weekdays and weekends?**
-----------------------------------------------------------------------------

    par(mfrow = c(2,1))

    n_activity$day <- ifelse(weekdays(n_activity$date) == "Saturday" | weekdays(n_activity$date) == "Sunday", "weekend", "weekday")

    s_weekend <- tapply(n_activity[n_activity$day == "weekend" ,]$steps, n_activity[n_activity$day == "weekend" ,]$interval, mean, na.rm = TRUE)

    s_weekday <- tapply(n_activity[n_activity$day == "weekday" ,]$steps, n_activity[n_activity$day == "weekday" ,]$interval, mean, na.rm = TRUE)

    d_activity <- aggregate(steps ~ interval + day, n_activity, mean)

    # Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
    print(plot <- ggplot(d_activity, aes(x = interval , y = steps, color = day)) + geom_line() +
      labs(title = "Comparison in average number of steps in each interval", 
           x = "5-minute Interval", y = "Average number of steps") +
        facet_wrap(~day, ncol = 1, nrow=2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
