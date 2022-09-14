---
title: "Session 6: Homework 3"
author: "Your name goes here"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(knitr)                             #kable
library(scales)                            #percentage 
library(data.table)                        #table
library(infer)
```

# Youth Risk Behavior Surveillance

Every two years, the Centers for Disease Control and Prevention conduct the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

## Load the data

This data is part of the `openintro` textbook and we can load and inspect it. There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:

?yrbss

```{r}
data(yrbss)
glimpse(yrbss)
```

Before you carry on with your analysis, it's is always a good idea to check with `skimr::skim()` to get a feel for missing values, summary statistics of numerical variables, and a very rough histogram.

## Exploratory Data Analysis

You will first start with analyzing the `weight` of participants in kilograms. Using visualization and summary statistics, describe the distribution of weights. How many observations are we missing weights from?

##The minimum value of weight is 30, the maximum value is 181, the median is 64, the 1/4 quantile is 56, the 3/4 quantile is 76, and the total number of missing values is 1004.

```{r, eda_on_weight}

skimr::skim(yrbss)

x <- yrbss$weight 
h<-hist(x,  
        xlab="weight", main="Histogram with Normal Curve") 


xfit<-seq(min(x,na.rm = TRUE),max(x,na.rm = TRUE),length=40)               #  order the data 
yfit<-dnorm(xfit,mean=mean(x,na.rm = TRUE),sd=sd(x,na.rm = TRUE))          #  density of normal dis
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="red", lwd=2) 


kable(yrbss%>%
  filter(is.na(weight))%>%
  summarise(Toatal_missing_data=n()),"simple")

summary(yrbss$weight)
```

Next, consider the possible relationship between a high schooler's weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

Let's create a new variable in the dataframe `yrbss`, called `physical_3plus` , which will be `yes` if they are physically active for at least 3 days a week, and `no` otherwise. You may also want to calculate the number and % of those who are and are not active for more than 3 days. RUse the `count()` function and see if you get the same results as `group_by()... summarise()`

```{r, mutate_and_count}

yrbss$physical_3plus<-ifelse(yrbss$physically_active_7d>=3,"yes","no")


#group_by()... summarise()
yrbss%>%
 #filter(is.na(physical_3plus)==FALSE)%>%
  #filter(physical_3plus=='no')%>%
  group_by(physical_3plus)%>%
  summarise(n=n())%>%
  mutate(prop=label_percent()(n/nrow(yrbss)))

#count()
yrbss%>%
count(physical_3plus)

yrbss%>%
  filter(is.na(physical_3plus)==FALSE)%>%
  group_by(physical_3plus)%>%
  summarise(number=n())%>%
ggplot(aes(x=physical_3plus,y=number))+
  geom_bar(stat = "identity",width =0.5)+
  labs(x="physical_3plus",y="number")+
  ggtitle("The bar graph of the number of physical_3plus",
          subtitle = "")+
  theme_bw()
  

```

Can you provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week?

#95% confidence interval is \[0.317,0.333\]

```{r, confidence_interval}

test<-yrbss %>% 
  filter(is.na(physical_3plus)==FALSE)%>%
  mutate(physical_3_p=ifelse(physical_3plus=="yes",1,0), physical_3_l=ifelse(physical_3plus=="no",1,0))
  
set.seed(1234)
boot_3lower <- test %>% 
# Select
  filter(is.na(physical_3plus)==FALSE)%>%                  
# Specify the variable of interest
  specify(response = physical_3_l) %>%
# Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") 
 

percentile_ci<- boot_3lower %>% 
   get_confidence_interval(level=0.95, type="percentile")
percentile_ci




```

Make a boxplot of `physical_3plus` vs. `weight`. Is there a relationship between these two variables? What did you expect and why?

> There is a positive relationship between being physically active for at least 3 days a week and weight. The result is somehow counter-intuitive, but might be explained by the reason that people who stay highly active physically have more muscles.

```{r, boxplot}

yrbss%>%
  filter(is.na(physical_3plus)==FALSE)%>%
ggplot(aes(x=physical_3plus,y=weight))+
  geom_boxplot()+
  ggtitle("The boxplot of the weight of physical_3plus",
          subtitle = "")+
  theme_bw()




```

## Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test. Note that when we calculate the mean, SD, etc. weight in these groups using the mean function, we must ignore any missing values by setting the `na.rm = TRUE`.

```{r, ci_using_formulas}

  
set.seed(1234)
boot_weight_no <- test %>% 
# Select Animation films
  filter(is.na(physical_3plus)==FALSE)%>%        
  filter(physical_3plus=="no") %>% 
# Specify the variable of interest
  specify(response = weight) %>%
# Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>% 
  get_confidence_interval(level=0.95, type="percentile")
 



  
set.seed(1234)
boot_weight_yes <- test %>% 
# Select Animation films
  filter(is.na(physical_3plus)==FALSE) %>% 
  filter(physical_3plus=="yes")%>%                  
# Specify the variable of interest
  specify(response = weight) %>%
# Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>% 
  get_confidence_interval(level=0.95, type="percentile")

Confidence_Interval_table <- rbind(boot_weight_no, boot_weight_yes)
 


Confidence_Interval_table

```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. Let us also conduct a hypothesis test.

## Hypothesis test with formula

Write the null and alternative hypotheses for testing whether mean weights are different for those who exercise at least times a week and those who don't.

Null Hypothesis:mean weights are same for those who exercise at least times a week and those who don't. alternative hypotheses:mean weights are not same for those who exercise at least times a week and those who don't. From the output of t.test, p-value = 9e-08\<0.05, Null Hypothesis can be rejected and alternative hypotheses accepted.

> Null Hypothesis:mean weights are same for those who exercise at least times a week and those who don't.
>
> Alternative hypotheses:mean weights are not same for those who exercise at least times a week and those who don't. From the output of t.test, p-value = 9e-08\<0.05, Null Hypothesis can be rejected and alternative hypotheses accepted.

```{r, t_test_using_R}

t.test(weight ~ physical_3plus, data = yrbss)


```

## Hypothesis test with `infer`

Next, we will introduce a new function, `hypothesize`, that falls into the infer workflow. You will use this method for conducting hypothesis tests.

But first, we need to initialize the test, which we will save as `obs_diff`.

```{r, calc_obs_difference}
yrbss<-yrbss%>%
  filter(is.na(physical_3plus)==FALSE,is.na(weight)==FALSE)
obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))


```

Notice how you can use the functions specify and calculate again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being yes - no != 0.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as null.

```{r, hypothesis_testing_using_infer_package}

null_dist <- yrbss %>%
  # specify variables
  specify(weight ~ physical_3plus) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("yes", "no"))

```

Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within generate is set to permute, which is the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:

```{r}

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

```

Now that the test is initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`?

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```

This the standard workflow for performing hypothesis tests.

# IMDB ratings: Differences between directors

Recall the IMBD ratings data. I would like you to explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. I have already calculated the confidence intervals for the mean ratings of these two directors and as you can see they overlap.

```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```

First, I would like you to reproduce this graph. You may find `geom_errorbar()` and `geom_rect()` useful.

In addition, you will run a hpothesis test. You should use both the `t.test` command and the `infer` package to simulate from a null distribution, where you assume zero difference between the two.

> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

You can load the data and examine its structure

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I`

null hypotheses:the mean IMDB rating for Steven Spielberg and Tim Burton are the same . alternative hypotheses:the mean IMDB rating for Steven Spielberg and Tim Burton are not the same .

```{r}


movies<-movies%>%
  filter(director %in% c("Steven Spielberg", "Tim Burton"))

#director=="Steven Spielberg",mean=7.57 ,the 95% confidence interval is [7.27,7.87]
a<-movies%>%
  filter(director=="Steven Spielberg")
t.test(a$rating)

#director=="Tim Burton",mean=6.93 ,the 95% confidence interval is [6.53,7.33]
b<-movies%>%
  filter(director=="Tim Burton")
t.test(b$rating)


df <- tibble(
  director = c("Steven Spielberg","Tim Burton"),
  mean = c(7.57, 6.93),
  lower = c( 7.27,6.53),
  upper = c( 7.87, 7.33)
) %>% 
  map_df(rev)


options(ggplot2.discrete.colour= c("#F8766D", "#00BFC4"))

p <- ggplot(data = df,aes(color = director))
p+geom_errorbarh(aes(y=forcats::fct_inorder(director),xmin=lower,xmax=upper),
                 height=0.2,
                 size=1.5)+
  #geom_text(aes(label=VAL),size=4,Vjust=-0.5)+
  ggrepel::geom_text_repel(aes(x = mean, y = director, label = mean, size=1.5, position="indentity"))+
  ggrepel::geom_text_repel(aes(x = lower, y = director, label = lower, size=1.5 , position="indentity"))+
  ggrepel::geom_text_repel(aes(x = upper, y = director, label = upper,size=1.5, position="indentity"))+
  geom_point(aes(x=mean,y=director),size=5)+
  theme_bw()+
  ggtitle("Do Spielberg and Burton have the same mean IMDB ratings",
          subtitle = "95% confidence intercals overlap")+
  theme(legend.position = "none")+
  xlab("Mean IMBD Rating")+
  ylab("")+
  geom_rect(aes(xmin=7.271, xmax=7.311, ymin=-Inf, ymax=Inf),color="white",alpha = .1)





t.test(rating ~ director, data = movies)


obs_diff <- movies %>%
  specify(rating ~ director) %>%
  calculate(stat = "diff in means", order = c("Steven Spielberg","Tim Burton"))


```

```{r, hypothesis_testing_using_infer_package}

null_dist <- movies %>%
  # specify variables
  specify(rating ~ director) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("Steven Spielberg","Tim Burton"))

```

We can visualize this null distribution with the following code:

```{r}

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()+theme_bw()

```

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")+
  theme_bw()

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```
