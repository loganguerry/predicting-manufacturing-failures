# ============================================== #
# ============================================== #

# rm(list = ls())

# potentially useful packages
library(readr)
library(dplyr)
library(randomForest)
library(rpart)
library(caret)

# import data
setwd('~/Desktop/Spring 2019/Internship Class/Hunter-Douglas Project')
alldata <- read.csv('HD_data.csv', na.strings = c("","NULL","NA", "<NA>"))
alldata <- alldata[is.na(alldata$ORIGINAL_ORDER) == FALSE,]

# converting date strings to datetime format
alldata$DATE <- as.Date(as.character(alldata$SO_CREATED_DATE),format = "%Y%m%d")
head(alldata)

# creating dummy for failed products (rep/rem/crr)
alldata$fail <- 0
alldata[alldata$ORDER_REASON_ID == 'REP' | alldata$ORDER_REASON_ID == 'REM' | alldata$ORDER_REASON_ID == 'CRR',]$fail <- 1

# only necessary reason codes
alldata <- alldata[alldata$ORDER_REASON_ID == 'REP' | alldata$ORDER_REASON_ID == 'REM' | alldata$ORDER_REASON_ID == 'STD'| alldata$ORDER_REASON_ID == 'CON' | alldata$ORDER_REASON_ID == 'CRR',]
summary(alldata$ORDER_REASON_ID)

# creating line variable - captures Origina Orders and Failures differently
alldata$line <- ifelse(alldata$ORIGINAL_ORDER_LINE == 0, alldata$SALES_ORDER_LINE, alldata$ORIGINAL_ORDER_LINE)
head(alldata)
# only original orders
originalorders <- alldata[alldata$ORIGINAL_ORDER == alldata$SALES_ORDER & alldata$ORIGINAL_ORDER_LINE == 0 | alldata$ORDER_REASON_ID == "STD" | alldata$ORDER_REASON_ID == "CON",]
head(originalorders)
nrow(alldata)-nrow(originalorders)

# creating original date column
originalorderlines <- distinct(alldata[alldata$ORIGINAL_ORDER == alldata$SALES_ORDER | alldata$ORDER_REASON_ID == "STD" | alldata$ORDER_REASON_ID == "CON", c("ORIGINAL_ORDER", "DATE")])
clean <- left_join(alldata,originalorderlines,by="ORIGINAL_ORDER")
names(clean)[names(clean) == 'DATE.x'] <- "LINE_ORDER_DATE"
names(clean)[names(clean) == 'DATE.y'] <- "ORIGINAL_ORDER_DATE"

# creating 90 warranty variable
clean$days <- NA
clean$days <- difftime(clean$LINE_ORDER_DATE, clean$ORIGINAL_ORDER_DATE, unit = "days")

# removing orders with fails outside of warranty period
clean <- clean[clean$fail == 0 | clean$fail == 1 & clean$days <= 90,]
clean <- clean[is.na(clean$line) == FALSE,]

# all fails
allfails <- clean[clean$fail == 1,]
head(allfails)

# creating final dataframe of non-repeating order lines - FINAL
OG_orders_failed <- semi_join(originalorders,allfails, by = c("ORIGINAL_ORDER", "line"))
OG_orders_no_fail <- anti_join(originalorders,allfails, by = c("ORIGINAL_ORDER", "line"))
head(OG_orders_failed)
OG_orders_failed$fail <- 1
final <- rbind(OG_orders_failed,OG_orders_no_fail)

# checking successful creation of "final"
head(final)
final[final$ORIGINAL_ORDER == 230053700182,]
final[final$ORIGINAL_ORDER == 230053700182 & final$SALES_ORDER_LINE == 1,]
summary(final)

# write CSV of cleaned data
write_csv(final, "~/Desktop/HD_data_clean.csv")

# ============================================== #
# ============================================== #

# subset of data - Factory A / Roller Shades (~5% failure rate)
subset <- final[final$ORIGINAL_PLANT == "A" & final$PRODUCT_CATEGORY == "07 Roller Shades",]

# remove variables
subset_trim <- subset[,-c(1,2,3,4,5,6,25,26,28)]
head(subset_trim)

# write subset CSV
write_csv(subset_trim, "~/Desktop/HD_clean_subset.csv")

# ============================================== #
# ============================================== #

