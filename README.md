---
title: "Intuit Presentation"
author: "Jonathan Mizel"
output: html_document
---

```{r setup, include=FALSE}

setwd("C:/Users/jmize/Documents/R/Intuit")

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(flextable)
library(plotly)

```



```{r load data, echo = FALSE}

#load data (coverted file from xlsx to csv in Excel as dates were not importing properly using readxl, changed currency to numeric)

dat = read.csv("Intuit Data Set.csv", na.strings=c("NA","<NA>", "(null)"))

str(dat)

dq = dat %>% filter(AMOUNT <= 0)

summary(dq)

```

```{r clean data, echo = FALSE}

dat$START.DATE = date(mdy(dat$START.DATE))
dat$FINAL.DATE = as.character(dat$FINAL.DATE)
dat$FINAL.DATE.TS = mdy_hm(dat$FINAL.DATE)
dat$FINAL.DATE = date(dat$FINAL.DATE.TS)
dat$time_diff = dat$FINAL.DATE - dat$START.DATE
dat$START.YEAR = year(dat$START.DATE)
dat$START.QTR = quarter(dat$START.DATE, with_year = TRUE) 
dat$START.YR.MNTH = floor_date(dat$START.DATE, unit = "month")
dat$CODE = as.factor(dat$CODE)

#glimpse(dat)

summary(dat)

theme_vader(flextable(head(dat)))

```

#Overall Fraud


```{r echo=FALSE}

tot_frd = dat %>% select(ID, FRAUD) %>% group_by(FRAUD) %>% tally()
#tot_frd

p=ggplot(data=tot_frd, aes(x=FRAUD, y=n)) +
  geom_bar(stat="identity") +
  theme_bw()
#p

colnames(tot_frd)[2] = "Fraud Percent"

x = paste(round(100*(tot_frd[2,2] / sum(tot_frd$`Fraud Percent`)),2), "%", sep = "")

x = as.data.frame(x)
colnames(x)[1] = "fraud_pct"

x = theme_vader(flextable(x))

dat %>% group_by(FRAUD) %>% tally()
x
#p

```

# Summary by Year


```{r echo=FALSE}


frd_yr1 = dat %>% filter(FRAUD == "N") %>% group_by(START.YEAR, FRAUD) %>% tally()
colnames(frd_yr1)[3] = "good_txn"
frd_yr2 = dat %>% filter(FRAUD == "Y") %>% group_by(START.YEAR, FRAUD) %>% tally()
colnames(frd_yr2)[3] = "frd_txn"
frd_sum = dat %>% filter(FRAUD == "Y") %>% group_by(START.YEAR) %>% summarise(fraud_amt = sum(AMOUNT))

yr_summary = merge(frd_yr1, frd_yr2, by = "START.YEAR", all = TRUE)
yr_summary = na.omit(yr_summary[,c(1,3,5)])
smy = merge(yr_summary,frd_sum,by="START.YEAR")
smy$frd_pct = smy$frd_txn/(smy$frd_txn+smy$good_txn)
smy = smy %>% arrange((START.YEAR)) 

smy$cost_per_frd = smy$fraud_amt/smy$frd_txn

smy$good_txn = format(smy$good_txn, big.mark = ",")
smy$frd_txn = format(smy$frd_txn, big.mark = ",")
smy$fraud_amt = paste("$", format(smy$fraud_amt, big.mark = ","), sep = "")
smy$frd_pct = paste(round(100*(smy$frd_pct),2), "%", sep="")
smy$cost_per_frd = paste("$", format(round(smy$cost_per_frd), big.mark = ","), sep = "")

theme_vader(flextable(smy))


```

# Summary by Quarter


```{r echo=FALSE}


frd_yr1 = dat %>% filter(FRAUD == "N") %>% group_by(START.QTR, FRAUD) %>% tally()
colnames(frd_yr1)[3] = "good_txn"
frd_yr2 = dat %>% filter(FRAUD == "Y") %>% group_by(START.QTR, FRAUD) %>% tally()
colnames(frd_yr2)[3] = "frd_txn"
frd_sum = dat %>% filter(FRAUD == "Y") %>% group_by(START.QTR) %>% summarise(fraud_amt = sum(AMOUNT))

yr_summary = merge(frd_yr1, frd_yr2, by = "START.QTR", all = TRUE)
yr_summary = na.omit(yr_summary[,c(1,3,5)])
smy = merge(yr_summary,frd_sum,by="START.QTR")
smy$frd_pct = smy$frd_txn/(smy$frd_txn+smy$good_txn)
smy = smy %>% arrange((START.QTR)) 

smy1 = smy

smy$cost_per_frd = smy$fraud_amt/smy$frd_txn

smy$good_txn = format(smy$good_txn, big.mark = ",")
smy$frd_txn = format(smy$frd_txn, big.mark = ",")
smy$fraud_amt = paste("$", format(smy$fraud_amt, big.mark = ","), sep = "")
smy$frd_pct = paste(round(100*(smy$frd_pct),2), "%", sep="")
smy$cost_per_frd = paste("$", format(round(smy$cost_per_frd), big.mark = ","), sep = "")

theme_vader(flextable(smy))



a = ggplot(data=smy1, aes(x=START.QTR, y=frd_pct)) +
  geom_line(color="red", size = 1) +
   xlab("Date") + ylab("Fraud Percent") +
  #geom_smooth(method='lm', formula= y~x)+
  theme_dark()

a


ggplotly(a)

```

# Summary by Month


```{r echo=FALSE}


frd_yr1 = dat %>% filter(FRAUD == "N") %>% group_by(START.YR.MNTH, FRAUD) %>% tally()
colnames(frd_yr1)[3] = "good_txn"
frd_yr2 = dat %>% filter(FRAUD == "Y") %>% group_by(START.YR.MNTH, FRAUD) %>% tally()
colnames(frd_yr2)[3] = "frd_txn"
frd_sum = dat %>% filter(FRAUD == "Y") %>% group_by(START.YR.MNTH) %>% summarise(fraud_amt = sum(AMOUNT))

yr_summary = merge(frd_yr1, frd_yr2, by = "START.YR.MNTH", all = TRUE)
yr_summary = na.omit(yr_summary[,c(1,3,5)])
smy = merge(yr_summary,frd_sum,by="START.YR.MNTH")
smy$frd_pct = smy$frd_txn/(smy$frd_txn+smy$good_txn)
smy = smy %>% arrange((START.YR.MNTH)) 

smy$cost_per_frd = smy$fraud_amt/smy$frd_txn

smy1 = smy
smy$good_txn = format(smy$good_txn, big.mark = ",")
smy$frd_txn = format(smy$frd_txn, big.mark = ",")
smy$fraud_amt = paste("$", format(round(smy$fraud_amt), big.mark = ","), sep = "")
smy$frd_pct = paste(round(100*(smy$frd_pct),2), "%", sep="")
smy$cost_per_frd = paste("$", format(round(smy$cost_per_frd), big.mark = ","), sep = "")

theme_vader(flextable(smy))

ggplot(data=smy1, aes(x=START.YR.MNTH, y=frd_pct)) +
  geom_line()


```

# Summary by Day


```{r echo=FALSE}


frd_yr1 = dat %>% filter(FRAUD == "N") %>% group_by(START.DATE, FRAUD) %>% tally()
colnames(frd_yr1)[3] = "good_txn"
frd_yr2 = dat %>% filter(FRAUD == "Y") %>% group_by(START.DATE, FRAUD) %>% tally()
colnames(frd_yr2)[3] = "frd_txn"
frd_sum = dat %>% filter(FRAUD == "Y") %>% group_by(START.DATE) %>% summarise(fraud_amt = sum(AMOUNT))

yr_summary = merge(frd_yr1, frd_yr2, by = "START.DATE", all = TRUE)
yr_summary = na.omit(yr_summary[,c(1,3,5)])
smy = merge(yr_summary,frd_sum,by="START.DATE")
smy$frd_pct = smy$frd_txn/(smy$frd_txn+smy$good_txn)
smy = smy %>% arrange((START.DATE)) 

smy1 = smy

smy$cost_per_frd = smy$fraud_amt/smy$frd_txn

smy$good_txn = format(smy$good_txn, big.mark = ",")
smy$frd_txn = format(smy$frd_txn, big.mark = ",")
smy$fraud_amt = paste("$", format(round(smy$fraud_amt), big.mark = ","), sep = "")
smy$frd_pct = paste(round(100*(smy$frd_pct),2), "%", sep="")
smy$cost_per_frd = paste("$", format(round(smy$cost_per_frd), big.mark = ","), sep = "")

theme_vader(flextable(smy))

#smy_ss = filter(smy, frd_txn >= 30)
#smy_ss

```



```{r}
#smy1 = smy
smy1 = smy1 %>% filter(START.DATE > '2019-01-01')
a = ggplot(data=smy1, aes(x=START.DATE, y=frd_pct)) +
  geom_point(color="red", size = 1.6) +
  geom_line(color = "yellow", size = .4) +
   xlab("Date") + ylab("Fraud Percent") +
  #stat_smooth(method='lm', formula= y~x)+
  theme_dark()
#ggplotly(a)
a

```


# Logistic regression

```{r}

dat$frd = ifelse(dat$FRAUD == "Y", 1, 0)
dat$FRAUD = NULL


```


```{r}



test = dat

#colnames(test)

test1 = test[,c(3,12)]
int_log = glm(frd ~., data = test1, family = 'binomial')

#head(dat)

summary(int_log)




```




```{r}



test = dat

colnames(test)

test1 = test[,c(4,12)]
int_log = glm(frd ~., data = test1, family = 'binomial')

#head(dat)

summary(int_log)




```
```{r}



test = dat

colnames(test)

test1 = test[,c(5,12)]
int_log = glm(frd ~., data = test1, family = 'binomial')

#head(dat)

summary(int_log)




```





```{r}

library(regclass)



test = dat %>% filter(AMOUNT < 7000)

test$OUTCOME  = sapply(test$OUTCOME, toupper)

test$code_flag = ifelse(test$CODE %in% c(57.8,15.78), "T", "F")
test$code_flag = as.factor(test$code_flag)
test$outcome_flag = ifelse(test$OUTCOME %in% c("ARD", "ARK"), "T", "F")
test$outcome_flag = as.factor(test$outcome_flag)
test$frd = as.factor(test$frd)
test1 = test[,c(1,3,12:14)]

test_full = test1

test1$ID <- 1:nrow(test1)
train <- test1 %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(test1, train, by = 'ID')
train$ID = NULL
test$ID = NULL



int_log = glm(frd ~ AMOUNT + code_flag + outcome_flag, data = train, family = 'binomial')
int_log2 = glm(frd ~ AMOUNT + code_flag + outcome_flag, data = test_full, family = 'binomial')

pr = predict(int_log, test, type="response") 

y_pred_num <- ifelse(pr > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$frd

mean(y_pred == y_act)

confusion_matrix(int_log2)

8189/31637

32246/32929

4781/7492
```


# Fraud by Code

```{r}

#sum(table(unique(dat$CODE)))

code_frd_no = dat %>% filter(frd == 0) %>% group_by(CODE) %>% tally()
colnames(code_frd_no)[2] = "is_good"
code_frd = dat %>% filter(frd == 1) %>% group_by(CODE) %>% tally()
colnames(code_frd)[2] = "is_fraud"

code_frd_summ = merge(code_frd, code_frd_no, by = "CODE")

code_frd_summ$fraud_pct = code_frd_summ$is_fraud/(code_frd_summ$is_fraud+code_frd_summ$is_good)
code_frd_summ$fraud_pct = paste(round(100*(code_frd_summ$fraud_pct),2), "%", sep="")


code_frd_summ = code_frd_summ %>% filter((is_fraud+is_good) > 300) %>% arrange(desc(is_fraud))
  
theme_vader(flextable(code_frd_summ))

```



# Fraud by Outcome

```{r}


dat = mutate_all(dat, .funs=toupper)

sum(table(unique(dat$OUTCOME)))

code_frd_no = dat %>% filter(frd == 0) %>% group_by(OUTCOME) %>% tally()
colnames(code_frd_no)[2] = "is_good"
code_frd = dat %>% filter(frd == 1) %>% group_by(OUTCOME) %>% tally()
colnames(code_frd)[2] = "is_fraud"

code_frd_summ = merge(code_frd, code_frd_no, by = "OUTCOME")

code_frd_summ$fraud_pct = code_frd_summ$is_fraud/(code_frd_summ$is_fraud+code_frd_summ$is_good)
code_frd_summ$fraud_pct = paste(round(100*(code_frd_summ$fraud_pct),2), "%", sep="")
code_frd_summ = code_frd_summ %>% arrange(desc(is_fraud))


theme_vader(flextable(code_frd_summ))

```



# Fraud by Amount ???
Disty plot


```{r}

dist = dat %>% filter(AMOUNT > 30000 & AMOUNT < 100000) 

#hist(dat$AMOUNT, breaks = 6000, col = "blue", main = "Simple Histogram", xlab = "Amount", ylab = "count")

p <- dist %>% ggplot( aes(x=AMOUNT)) +
    #geom_histogram( binwidth=50, fill="orange", color="red", alpha=0.9) +
    geom_histogram(aes(color = FRAUD, fill = FRAUD), 
                position = "identity", bins = 50, alpha = 0.4)+
    scale_fill_manual(values = c("#00AFBB", "red"))+
  theme_dark()
    
    
p

#?hist

```


```{r}


sug = dat %>% filter(AMOUNT > 0) 
sug$amt_flag = ifelse(sug$AMOUNT > 7000, "high", "low")

sug = sug %>% group_by(FRAUD, amt_flag) %>% summarize(total_amount = sum(AMOUNT))


#select(ID, AMOUNT, FRAUD)

head(sug)


```

dist1 = dat %>% filter(AMOUNT > 1000 & AMOUNT < 10000)

p <- dist1 %>% ggplot( aes(x=AMOUNT)) +
    geom_histogram( binwidth=250, fill="orange", color="red", alpha=0.9) +
    ggtitle("Transactions $1k-10k") +
    theme_dark() +
    theme(
      plot.title = element_text(size=15)
    )

p

#hist(dist1$AMOUNT, breaks = 6)

dist2 = dat %>% filter(AMOUNT > 10000 & AMOUNT < 30000) 
p <- dist2 %>% ggplot( aes(x=AMOUNT)) +
    geom_histogram( binwidth=2500, fill="orange", color="#e9ecef", alpha=0.9) +
    ggtitle("Transactions $10k-30k") +
    theme_dark() +
    theme(
      plot.title = element_text(size=15)
    )

p
#hist(dist2$AMOUNT, breaks = 6)


dist2 = dat %>% filter(AMOUNT > 30000 & AMOUNT < 100000) 

p <- dist2 %>% ggplot( aes(x=AMOUNT)) +
    geom_histogram( binwidth=2500, fill="orange", color="#e9ecef", alpha=0.9) +
    ggtitle("Transactions $30k-100000k") +
    theme_dark() +
    theme(
      plot.title = element_text(size=15)
    )

p

#hist(dist2$AMOUNT, breaks = 6)


```




```{r}

dat_cl = dat %>% filter(FRAUD == "Y")

dist = dat_cl %>% filter(AMOUNT > 0 & AMOUNT < 1000) 

hist(dist$AMOUNT, breaks = 6)



dist1 = dat_cl%>% filter(AMOUNT > 1000 & AMOUNT < 10000) 

hist(dist1$AMOUNT, breaks = 6)

dist2 = dat_cl%>% filter(AMOUNT > 10000 & AMOUNT < 30000) 

hist(dist2$AMOUNT, breaks = 6)


dist2 = dat_cl%>% filter(AMOUNT > 30000 & AMOUNT < 100000) 

hist(dist2$AMOUNT, breaks = 6)


```



```{r}

tdiff = dat

tdiff$time_diff = as.numeric(tdiff$time_diff)

tdiff1 = tdiff %>% filter(time_diff < 100)

hist(tdiff1$time_diff, breaks = 6)


```
