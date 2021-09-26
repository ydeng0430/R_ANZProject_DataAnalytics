---
title: 'Task 1: Data preparation and customer analytics'
author: "Yixi Deng"
date: "7/23/2021"
output:
  html_document:
    df_print: paged
    toc: true
    toc_float: true
  pdf_document: default
  word_document: default
header-includes: \usepackage{float}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.pos = 'H')
```

## Load required libraries and datasets
```{r include=FALSE}
#### Load the required libraries
library(data.table)
library(tidyverse)
library(knitr)
library(rpart)
library(dplyr)
library(ozmaps)
#### Load the dataset
filePath <- "C:/Users/User/Desktop/JobSeeking/Project/Forage/ANZ/"
transactionData <- fread(paste0(filePath,"ANZ synthesised transaction dataset.csv"))
```

## Exploratory data analysis

### Examining transaction data
We can use `str()` to look at the format of each column and see a sample of data.
```{r Examining transaction data}
#### Examine transaction data
str(transactionData)
```

### Find the missing values
Next, we need to see if there is any null or blank cell in each variable.
```{r}
# Count both NA and blank cells except extraction 
sapply(transactionData[,-"extraction"], function(x) sum(is.na(x)|x == "")) 
```
```{r}
# Count both NA and blank cells for extraction 
sum(sapply(as.numeric(transactionData$extraction), function(x) sum(is.na(x)|x == ""))) 
```

There are 4326 missing values in `card_present_flag` column, 11158 missing values in `bpay_biller_code` column, 4326 missing values in `merchant_id` column, 11160 missing values in `merchant_code` column, 4326 missing values in `merchant_suburb` column, 4326 missing values in `merchant_state` column, and 4326 missing values in `merchant_long_lat` column.

### Change the formats for some variables
```{r Convert variables to proper formats}
#### Convert status column to a factor format
transactionData$status <- as.factor(transactionData$status)
#### Convert date column to a date format
transactionData$date <- as.Date(transactionData$date, format = "%m/%d/%Y")
#### Convert gender column to a factor format
transactionData$gender <- as.factor(transactionData$gender)
#### Convert merchant_state column to a factor format
transactionData$merchant_state <- as.factor(transactionData$merchant_state)
#### Convert movement column to a factor format
transactionData$movement <- as.factor(transactionData$movement)
str(transactionData)
```

```{r}
# Show them in percentage for non-character variables
sapply(transactionData[,c("merchant_code","balance","gender","age","merchant_state","amount","movement","long_lat")],
function(x) sum(is.na(x)|x == "")/length(x)*100) 
```

We found 92.67% of data in `merchant_code` column and 35.92% of data in `merchant_state` column. We'll leave the missing values in `merchant_code` and `merchant_state` right now, and if necessary, we'll try to fill those missing values for future analysis.


### Find the missing date
```{r}
#### Find the missing date 
DateRange <- seq(range(transactionData$date)[1],range(transactionData$date)[2],by=1)
DateRange[!DateRange %in% transactionData$date]
```

We found one date was not in the dataset. We would go back to this, if this date is important for the analysis. 

### Create new variable
```{r}
#### Create month variable
transactionData$month <- 
  ifelse(month(transactionData$date) == 8, "Aug", 
         ifelse(month(transactionData$date) == 9, "Sep", "Oct"))
table(transactionData$month)
sum(table(transactionData$month))
```

We created new variable `month` for comparing the transactions on those three months.

### Find the outliers 
```{r}
#### Find the outliers in amount column
boxplot(transactionData$amount)
#### Check the stats of the normal amount data
boxplot.stats(transactionData$amount)$stats
#### Check the proportion of the outliers
sum(transactionData$amount > 110)/length(transactionData$amount)
#### Check whether the outlier amounts belong to specific customers
table(transactionData$customer_id[transactionData$amount > 110])
uniqueN(transactionData$customer_id[transactionData$amount > 110])
uniqueN(transactionData$customer_id)
```

From the plot above, we can see that there are over 15% of transactions which exceeded 110 dollars. However, we can't remove those transaction data as outliers, for every customer has multiple transactions over 110 dollars.

## Insights Exploration

### Average transaction amount
```{r}
# Histogram
x <- transactionData$amount
h <- hist(x,breaks = 20, col = "grey",
          xlab = "Transaction amount (AUD$)",
          ylab = "The number of transactions")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit, mean = mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit,col="orange",lwd=2)
# Main statistics
mean(x)
median(x)
max(x)
```

The distribution of `amount` has a positive skew. Half of `amount` are less than 29 dollars; however, the mean of `amount` is 187.9336 dollars, which indicates the other half of `amount` is much larger than the half which are less than 29 dollars.

### The average number of transactions per customer each month
```{r}
### The average number of transaction per customer on Aug 2018
total_txn_n_Aug <- uniqueN(transactionData$transaction_id[transactionData$month == "Aug"])
total_cust_n_Aug <- uniqueN(transactionData$customer_id[transactionData$month == "Aug"])
(avg_txn_n_per_customer_Aug <- total_txn_n_Aug/total_cust_n_Aug)
### The average number of transaction per customer on Sep 2018
total_txn_n_Sep <- uniqueN(transactionData$transaction_id[transactionData$month == "Sep"])
total_cust_n_Sep <- uniqueN(transactionData$customer_id[transactionData$month == "Sep"])
(avg_txn_n_per_customer_Sep <- total_txn_n_Sep/total_cust_n_Sep)
### The average number of transaction per customer on Oct 2018
total_txn_n_Oct <- uniqueN(transactionData$transaction_id[transactionData$month == "Oct"])
total_cust_n_Oct <- uniqueN(transactionData$customer_id[transactionData$month == "Oct"])
(avg_txn_n_per_customer_Oct <- total_txn_n_Oct/total_cust_n_Oct)
```

On average, every customer had about 40 transactions in each month. Now, let's see whether we can find any interesting behaviours for different lengths of time.

### Segment the dataset by transaction date and time
```{r}
#### Split the time out of the extraction column into the time column
transactionData$time <- substr(transactionData$extraction,12,19)
```

```{r}
#### Split the date into two columns, weekday, and weekend
transactionData$day <- weekdays(transactionData$date)
```

### Visualise transaction spending over the course of an average day or week
```{r}
total_txn_n <- uniqueN(transactionData$transaction_id)
total_day_n <- uniqueN(transactionData$date)
avg_txn_n_per_day <- total_txn_n/total_day_n
txn_n_df <- transactionData %>% group_by(date) %>% summarise_at(vars(transaction_id),list(n = uniqueN))
txn_n_df$day <- weekdays(txn_n_df$date)
head(txn_n_df)
```

```{r}
#### Plot the transaction volume over the course of an average day
barplot(txn_n_df$n/100, names.arg = txn_n_df$date,
        cex.names = 0.6,
        xlab = "Date",
        ylab = "The Average Number of Transactions",
        main = "The Average Number of Daily Transactions per Customer")
```

We can see a clear pattern in the plot above. Peaks and troughs frequently appeared in the plot. Let's take a closer look.

```{r}
txn_n_df1 <- txn_n_df %>% group_by(day) %>% summarise(n = mean(n))
txn_n_df1.0 <- txn_n_df1[c(2,6,7,5,1,3,4),]


#### Plot the transaction volume over days
barplot(txn_n_df1.0$n/100, names.arg = txn_n_df1.0$day,
        cex.names = 0.6,
        xlab = "Days",
        ylab = "The Average Number of Transactions",
        main = "The Average Number of Daily Transactions per Customer")
```

It seems like customers tend to have more transactions between Wednesday and Friday. How about the number of transactions within a day?

```{r}
transactionData$hour <- as.integer(substr(transactionData$time,1,2))
txn_n_df2 <- transactionData %>% group_by(hour) %>% summarise_at(vars(transaction_id),list(n = uniqueN))

#### Plot the transaction volume over the course of an average day
barplot(txn_n_df2$n/(100*uniqueN(transactionData$date)), names.arg = txn_n_df2$hour,
        cex.names = 0.6,
        xlab = "Hours",
        ylab = "The Number of Transactions",
        main = "The Number of Hourly Transactions per Customer")
```

From the plot above, we can see that most of custmoers purchase something around 9:00 am, 11:00 am, and 5:00 pm. Not surprisely, those times are meal times.

After exploring some insights of the number of transactions, let's have a look at the transaction spending for different lengths of time.

### Visualise transaction spending over the course of an average day or week
```{r}
spending_df <- transactionData %>% group_by(date) %>%  summarise_at(vars(amount),list(spending = sum))
avg_spending_df <- merge(spending_df,txn_n_df)
avg_spending_df$avg_spending <- avg_spending_df$spending/avg_spending_df$n
head(avg_spending_df)
```


```{r}
#### Plot the transaction volume over the course of an average day
barplot(avg_spending_df$avg_spending/100, names.arg = avg_spending_df$date,
        cex.names = 0.6,
        xlab = "Date",
        ylab = "Average Spending (AUD$)",
        main = "The Average Daily Spending per Customer")
```

The pattern of average daily spending per customer is similar to that in the average number of daily transactions per customer. Peaks and troughs occurred periodically. Now, let's have a closer look.

```{r}
spending_df1 <- transactionData %>% group_by(day) %>%  summarise_at(vars(amount),list(spending = sum))
avg_spending_df1 <- merge(spending_df1,txn_n_df2)
avg_spending_df1$avg_spending <- avg_spending_df1$spending/avg_spending_df1$n
avg_spending_df1$avg_spending_per_customer <- avg_spending_df1$avg_spending/100
head(avg_spending_df1)
```


```{r}
avg_spending_df1.0 <- avg_spending_df1[c(2,6,7,5,1,3,4),]

#### Plot the transaction volume over weekdays
barplot(avg_spending_df1.0$avg_spending_per_customer, names.arg = avg_spending_df1.0$day,
        cex.names = 0.6,
        xlab = "Days",
        ylab = "Avearge Spending (AUD$)",
        main = "The Average Daily Spending per Customer")
```

It seems like customers tend to spend more on Monday and Friday and spend least on weekend. Next, we take a look at the hourly transaction spending.

```{r}
avg_spending_df2 <- transactionData %>% group_by(hour) %>% summarise_at(vars(amount),list(avg_spending = mean))
avg_spending_df2$avg_spending_per_customer <- avg_spending_df2$avg_spending/100


#### Plot the transaction volume over the course of an average day
barplot(avg_spending_df2$avg_spending_per_customer, names.arg = avg_spending_df2$hour,
        cex.names = 0.6,
        xlab = "Hours",
        ylab = "The Averge Spending (AUD$)",
        main = "The Average Hourly Spending per Customer")
```

Customers tend to spend the most at 1:00 pm and 5:00 pm.

### Location information

Let's have a look where the customers are from based on the information in `long_lat` column.

#### Create a new dataframe with information with only account number and coordinates
```{r}
account_loc_inf <- transactionData[match(unique(transactionData$account),transactionData$account), c("account","long_lat")]
account_loc_inf$long <- as.numeric(substr(account_loc_inf$long_lat,1,6))
account_loc_inf$lat <- as.numeric(substr(account_loc_inf$long_lat,8,13))
head(account_loc_inf)
```

#### Create a function to find the state given the coordinates
```{r}
coords_state <- function(coords_state, loc_inf,state_name){
        loc_inf$states <- ifelse(loc_inf$long <= coords_state[[1]][3] & 
                                 loc_inf$long >= coords_state[[1]][1] &
                                 loc_inf$lat <=  coords_state[[1]][4] & 
                                 loc_inf$lat >= coords_state[[1]][2], 
                                 state_name,loc_inf$states)
}
```

#### Define the coordinates for each state
```{r}
coords_NSW <- as.data.frame(c(140.9993, -37.50503, 153.6299, -28.15703 ),c("xmin","ymin","xmax","ymax"))
coords_VIC <- as.data.frame(c(140.9617, -39.13396, 149.9763, -33.99605 ),c("xmin","ymin","xmax","ymax"))
coords_QLD <- as.data.frame(c(137.9943, -29.17719, 153.5522, -9.229287 ),c("xmin","ymin","xmax","ymax"))
coords_SA <- as.data.frame(c(129.0013, -38.06, 141.003, -25.99638 ),c("xmin","ymin","xmax","ymax"))
coords_WA <- as.data.frame(c(112.9211, -35.12228, 129.0019, -13.74142 ),c("xmin","ymin","xmax","ymax"))
coords_TAS <- as.data.frame(c(143.8353, -43.63203, 148.4472, -39.45196 ),c("xmin","ymin","xmax","ymax"))
coords_NT <- as.data.frame(c(129.0005, -25.99862, 137.9991, -10.97392 ),c("xmin","ymin","xmax","ymax"))
coords_ACT <- as.data.frame(c(148.7628, -148.7628, 149.3972, -35.12442 ),c("xmin","ymin","xmax","ymax"))
```

#### Update the location information dataframe with states
```{r}
account_loc_inf$states <- NA

account_loc_inf$states <- coords_state(coords_NSW, account_loc_inf, "NSW")
account_loc_inf$states <- coords_state(coords_VIC, account_loc_inf, "VIC")
account_loc_inf$states <- coords_state(coords_QLD, account_loc_inf, "QLD")
account_loc_inf$states <- coords_state(coords_SA, account_loc_inf, "SA")
account_loc_inf$states <- coords_state(coords_WA, account_loc_inf, "WA")
account_loc_inf$states <- coords_state(coords_TAS, account_loc_inf, "TAS")
account_loc_inf$states <- coords_state(coords_NT, account_loc_inf, "NT")
account_loc_inf$states <- coords_state(coords_ACT, account_loc_inf, "ACT")

head(account_loc_inf)
```

#### Make a bar plot to display the location distribution of customers
```{r}
#### Create a dataframe with the number of cusomters in each state
state_freq_df <- as.data.frame(table(account_loc_inf$states))

#### Make a bar plot 
barplot(state_freq_df$Freq, names.arg = state_freq_df$Var1,
        cex.names = 0.6,
        xlab = "States", ylab = "The number of customers",
        main = "The Number of Cusotmers in Each State"
        )
```

As the plot shown above, most of customers are from Victoria and New South Wales states.

