#  _____          _                                   _         _    _            
# |  __ \        | |             /\                  | |       | |  (_)           
# | |  | |  __ _ | |_  __ _     /  \    _ __    __ _ | | _   _ | |_  _   ___  ___ 
# | |  | | / _` || __|/ _` |   / /\ \  | '_ \  / _` || || | | || __|| | / __|/ __|
# | |__| || (_| || |_| (_| |  / ____ \ | | | || (_| || || |_| || |_ | || (__ \__ \
# |_____/  \__,_| \__|\__,_| /_/    \_\|_| |_| \__,_||_| \__, | \__||_| \___||___/
#                                                         __/ |                   
#                                                         |___/                    
#                      _                                        _     __ 
#     /\              (_)                                      | |   /_ |
#    /  \    ___  ___  _   __ _  _ __   _ __ ___    ___  _ __  | |_   | |
#   / /\ \  / __|/ __|| | / _` || '_ \ | '_ ` _ \  / _ \| '_ \ | __|  | |
#  / ____ \ \__ \\__ \| || (_| || | | || | | | | ||  __/| | | || |_   | |
# /_/    \_\|___/|___/|_| \__, ||_| |_||_| |_| |_| \___||_| |_| \__|  |_|
#                          __/ |                                         
#                         |___/          
#
#------------------------------------------------------------------------------------------------------------------------
# Clear the environment
rm(list = ls())

## Clear the output console (same with Ctrl + L)
cat("\014")

#------------------------------------------------------------------------------------------------------------------------
# Load the library
library(dplyr)
library(gtools)
library(igraph)
library(igraphdata)
library(plyr)
library(zoo)

#------------------------------------------------------------------------------------------------------------------------
# Load the dataset
setwd("~/Downloads/Monash/SEM6/Data\ Analytics/Assignment/01/")
raw_wf = read.csv('webforum.csv')

#------------------------------------------------------------------------------------------------------------------------
# Introduction
## Say what we are going do for this report
## Some keywords that are frequently used

#------------------------------------------------------------------------------------------------------------------------
# Preliminary
## Overview of dataset
str(raw_wf)

## Statistics of each field 
summary(raw_wf)

## Posts with 0 word count
print(paste('The number of posts with 0 word count:', with(raw_wf, length(WC[WC == 0]))))

## Proportion of question marks that exceed 100%
print(paste('The number of posts with the proportion of question marks that exceeds 100%:', with(raw_wf, length(QMark[QMark > 100]))))

## Number of unique threads
print(paste('The number of unique threads:', length(unique(raw_wf$ThreadID))))

## Number of unique authors
print(paste('The number of unique authors (excluding anonymous authors):', with(raw_wf, length(unique(AuthorID[AuthorID != -1])))))

## Number of anonymous authors
print(paste('The number of anonymous authors:', with(raw_wf, length(AuthorID[AuthorID == -1]))))

## Date range of these posts
class(raw_wf$Date)
raw_wf$Date = as.Date(raw_wf$Date)
class(raw_wf$Date)
start_date = with(raw_wf, Date[which.min(Date)])
end_date = with(raw_wf, Date[which.max(Date)])
print(paste('Date range:', start_date, 'to', end_date))

## Interaction among PostID, ThreadID and AuthorID
### A thread might be used in different posts
# interaction1 = with(raw_wf, by(raw_wf, ThreadID, function(df) length(df$PostID)))
interaction1 = ddply(raw_wf, .(ThreadID), function(df) length(df$PostID))
interaction1

### A thread might be used by different authors
interaction2 = ddply(raw_wf, .(ThreadID), function(df) length(unique(df$AuthorID)))
interaction2

### An author might have different posts
interaction3 = ddply(raw_wf, .(AuthorID), function(df) length(df$PostID))
interaction3

### An author might have different threads
interaction4 = ddply(raw_wf, .(AuthorID), function(df) length(unique(df$ThreadID)))
interaction4

### Duplicate rows
print(paste('The number of duplicate rows:', nrow(raw_wf[duplicated(raw_wf),])))

#------------------------------------------------------------------------------------------------------------------------
# Data Preprocessing
clean_wf = raw_wf
rm(raw_wf)

## Format Date into Year and Month
clean_wf$Date = as.yearmon(clean_wf$Date)
clean_wf

## Remove the post with 0 word count
### 0 word count = data with no value 
clean_wf[clean_wf$WC == 0,]
clean_wf = clean_wf[clean_wf$WC != 0,]
nrow(clean_wf)

## Remove posts with question mark proportion > 100%
with(clean_wf, length(QMark[QMark > 100]))
clean_wf = clean_wf[clean_wf$QMark <= 100,]
nrow(clean_wf)

## Remove unused column
### Why WC?
#### Reason 1: based on the forum
#### Reason 2: linear regression
##### roughly 5% of Multiple R-squared value
fitted_WC = lm(clean_wf$WC ~ ., data = clean_wf[7:ncol(clean_wf)])
summary(fitted_WC)

#### Reason 3: correlation matrix
##### The values of correlation between WC and other columns are mostly low (except social)
##### Therefore, we can remove it as it did not associate much with them
round(cor(clean_wf[7:ncol(clean_wf)], clean_wf[,6]), digits = 4)

### Why Time?
#### Reason: group by month, no need to be so specific
clean_wf$WC = NULL
clean_wf$Time = NULL
clean_wf

## Combine posemo and negem using the formula
### polarity = posemo - negemo
#### if < 0: neg | else: pos
clean_wf$polarity = clean_wf$posemo - clean_wf$negemo
clean_wf$posemo = NULL
clean_wf$negemo = NULL
clean_wf

#------------------------------------------------------------------------------------------------------------------------
# Data Analysis
## Question 1
### We define the groups as: 
#### Approach 1.1 :the group of threads (active vs inactive)
#### Approach 1.2: the group of authors (more posts vs less posts)
### According to the breakdown provided in LIWC official website, we decided to use variable "affect" as our focus 
### It is because its value summarise the expressing sentiment
fitted_senti = lm(clean_wf$affect ~ .,data=clean_wf[,c(9:15,17:ncol(clean_wf))])
summary(fitted_senti)
step_fitted_senti = step(fitted_senti)
summary(step_fitted_senti)

### Extract the variables from step model
var_list_after_step = names(step_fitted_senti$coefficients)[-1]

### Correlation between affect and other variables
senti_cor_table = round(cor(clean_wf[,var_list_after_step], clean_wf[,16]), digits=4)
colnames(senti_cor_table) = "SentiCorr"
senti_cor_table[order(-senti_cor_table),]

### polarity, anger, swear and anx are selected (top 4)

### Approach 1.1: Group of threads (active vs inactive)
#### Summary for thread activity
##### We calculate the number of activities based on the number of months that have comments 
activity_freq_for_thread = ddply(clean_wf, .(ThreadID), function(df) length(unique(df$Date)))
colnames(activity_freq_for_thread)[2] = "ActivityFreq"
summary(activity_freq_for_thread)

#### Separate into active and inactive threads
##### Active thread = above 75% 
##### Inactive thread = 25% - 75% (Q1 to Q3)
##### Any thread that has greater than 4 activities is considered as an active thread
##### For every thread that has 1 - 4 activities, they are inactive threads
#<Active Thread Group>
active_thread_ids= activity_freq_for_thread[activity_freq_for_thread$ActivityFreq > 4,]$ThreadID
active_thread_ids

#<Inactive Thread Group>
inactive_thread_ids = activity_freq_for_thread[between(activity_freq_for_thread$ActivityFreq, 1, 4),]$ThreadID
inactive_thread_ids

#### Calculate the mean of 4 summary variables of active and inactive thread groups by month
#<Active Thread Group>
wf_by_active_threads = clean_wf[clean_wf$ThreadID %in% active_thread_ids,] %>% 
                dplyr::group_by(Date) %>%
                dplyr:: summarise(
                    polarity = mean(polarity),
                    anger = mean(anger),
                    swear = mean(swear),
                    anx = mean(anx)
                )

#<Inactive Thread Group>
wf_by_inactive_threads = clean_wf[clean_wf$ThreadID %in% inactive_thread_ids,] %>% 
                dplyr::group_by(Date) %>%
                dplyr:: summarise(
                    polarity = mean(polarity),
                    anger = mean(anger),
                    swear = mean(swear),
                    anx = mean(anx)
                )

#************************************************************************************************************************
#************************************************************************************************************************
# The variable and function below would be used for the rest of the code
## Variable "months_from_2002to12": stores the list of months from year 2002 - 2012
## Function "fill_missing_month_rows": fill in rows with null value if there is a missing month exists in the table
months_from_2002to12 = as.yearmon(seq(start_date, end_date, by="1 month"))

fill_missing_month_rows = function(df){
    return(full_join(data.frame(Date=months_from_2002to12), df))    
}
#************************************************************************************************************************
#************************************************************************************************************************

#### Missing months exist
#<Active Thread Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_active_threads$Date))

#<Inactive Thread Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_inactive_threads$Date))

#### Plot the time series graphs
#<Active Thread Group>
wf_by_active_threads = fill_missing_month_rows(wf_by_active_threads)
time_series_for_active_threads = ts(wf_by_active_threads[wf_by_active_threads$Date >= c(2005),], frequency=12, start=c(2005,1), end=c(2011,12))
plot(time_series_for_active_threads[,2:5], main="Time Series for Active Thread Group")
ts.plot(time_series_for_active_threads[,2:5], main="Time Series for Active Thread Group", ylim=c(-4,10), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_active_threads)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.65)

#<Inactive Thread Group>
wf_by_inactive_threads = fill_missing_month_rows(wf_by_inactive_threads)
time_series_for_inactive_threads = ts(wf_by_inactive_threads[wf_by_inactive_threads$Date >= c(2005),], frequency=12, start=c(2005,1), end=c(2011,12))
plot(time_series_for_inactive_threads[,2:5], main="Time Series for Inactive Thread Group")
ts.plot(time_series_for_inactive_threads[,2:5], main="Time Series for Inactive Thread Group", ylim=c(-4,10), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_inactive_threads)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.65)

#************************************************************************************************************************
#************************************************************************************************************************
# Remove anonymous authors
## As the rest of the analysis would be done without anonymous authors
clean_wf = clean_wf[clean_wf$AuthorID != -1,]
#************************************************************************************************************************
#************************************************************************************************************************

### Approach 1.2: Group of authors (more posts vs less posts)
#### Summary for number of posts by authors
##### We calculate the number of posts possessed by each author
##### Our assumption: members who only have one post are not considered.
post_freq_for_author = ddply(clean_wf, .(AuthorID), function(df) length(df$Post))
colnames(post_freq_for_author)[2] = "PostFreq"
post_freq_for_author = post_freq_for_author[post_freq_for_author$PostFreq != 1,]
summary(post_freq_for_author)

#### Separate into authors with more posts and less posts
##### authors with more posts: members who have the number of posts above 50% (> 4 posts)
##### authors with less posts: members who have the number of posts below 50% (<= 4 posts)
#<More Post Author Group>
more_post_author_ids = post_freq_for_author[post_freq_for_author$PostFreq > 4,]$AuthorID
more_post_author_ids

#<Less Post Author Group>
less_post_author_ids = post_freq_for_author[post_freq_for_author$PostFreq <= 4,]$AuthorID
less_post_author_ids

#### Calculate the mean of 4 variables of more and less post_num author groups by month
#<More Post Author Group>
wf_by_more_post_authors = clean_wf[clean_wf$AuthorID %in% more_post_author_ids,] %>% 
                      dplyr::group_by(Date) %>%
                      dplyr:: summarise(
                           polarity = mean(polarity),
                           anger = mean(anger),
                           swear = mean(swear),
                           anx = mean(anx)
                      )

#<Less Post Author Group>
wf_by_less_post_authors = clean_wf[clean_wf$AuthorID %in% less_post_author_ids,] %>% 
                         dplyr::group_by(Date) %>%
                         dplyr:: summarise(
                              polarity = mean(polarity),
                              anger = mean(anger),
                              swear = mean(swear),
                              anx = mean(anx)
                         )


#### Missing months exist
#<More Post Author Group>
as.yearmon(setdiff(as.yearmon(seq(start_date, end_date, by="1 month")), wf_by_more_post_authors$Date))

#<Less Post Author Group>
as.yearmon(setdiff(as.yearmon(seq(start_date, end_date, by="1 month")), wf_by_less_post_authors$Date))

#### Plot the time series graphs
#<More Post Author Group>
wf_by_more_post_authors = fill_missing_month_rows(wf_by_more_post_authors)
time_series_for_more_post_authors = ts(wf_by_more_post_authors[wf_by_more_post_authors$Date >= c(2005),], frequency=12, start=c(2005,1), end=c(2011,12))
plot(time_series_for_more_post_authors[,2:5], main="Time Series for Author Group with More Posts")
ts.plot(time_series_for_more_post_authors[,2:5], main="Time Series for Author Group with More Posts", ylim=c(-5,15), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_more_post_authors)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.65)

#<Less Post Author Group>
wf_by_less_post_authors = fill_missing_month_rows(wf_by_less_post_authors)
time_series_for_less_post_authors = ts(wf_by_less_post_authors[wf_by_less_post_authors$Date >= c(2005),], frequency=12, start=c(2005,1), end=c(2011,12))
plot(time_series_for_less_post_authors[,2:5], main="Time Series for Author Group with Less Posts")
ts.plot(time_series_for_less_post_authors[,2:5], main="Time Series for Author Group with Less Posts", ylim=c(-5,15), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_less_post_authors)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.65)

#### Ring graph for members in the biggest thread (most number of posts)
##### Calculate the number of posts contributed by each member to a certain thread
post_freq_by_author_thread = count(clean_wf, c("ThreadID","AuthorID"))

##### Find the thread that has the most number of posts
biggest_thread = count(post_freq_by_author_thread, c("ThreadID"))
biggest_thread_id = biggest_thread$ThreadID[which.max(biggest_thread$freq)]

##### Select those members in the biggest thread
post_freq_by_author_thread = with(post_freq_by_author_thread, post_freq_by_author_thread[ThreadID == biggest_thread_id,])

##### Construct ring topology of graph
construct_ring_graph = function(table){
    ring_graph = graph_from_data_frame(table)
    E(ring_graph)$width = table$freq/5
    E(ring_graph)$arrow.size = 0.2
    plot(ring_graph,layout=layout.circle, vertex.shape="none")
}
construct_ring_graph(post_freq_by_author_thread)

##### Remove the members who have less than 4 posts (below the average)
###### Reason of doing this is to have clearer representation of the ring graph
summary(post_freq_by_author_thread)
post_freq_by_author_thread = with(post_freq_by_author_thread, post_freq_by_author_thread[freq > 3,])
construct_ring_graph(post_freq_by_author_thread)

## Question 2
### We plan to use 2 approaches for classifying the active and/or socially-connected members
#### Approach 2.1: Activity
#### Approach 2.2: Network
### Why we choose these 4 summary variables? (Analytic, Clout, Tone, Authentic)
#### Reason 1: based on the website
#### Reason 2: linear regression
##### moderately high R-squared value
##### It indicates that the change of these 4 variables is said to be quite easily affected by other variables

##### Model for Analytic
fitted_Analytic = lm(clean_wf$Analytic ~ ., data = clean_wf[9:ncol(clean_wf)])
summary(fitted_Analytic)

##### Model for Clout
fitted_Clout = lm(clean_wf$Clout ~ ., data = clean_wf[9:ncol(clean_wf)])
summary(fitted_Clout)

##### Model for Authentic
fitted_Authentic = lm(clean_wf$Authentic ~ ., data = clean_wf[9:ncol(clean_wf)])
summary(fitted_Authentic)

##### Model for Tone
fitted_Tone = lm(clean_wf$Tone ~ ., data = clean_wf[11:ncol(clean_wf)])
summary(fitted_Tone)

### Approach 2.1: Activity
#### Summary for member activity
##### We calculate the number of activities based on the number of months that a member has comments
##### Our assumption: members who only posted within one month are not considered.
###### For example: they might ask questions regarding their assignment only within one month
activity_freq_for_member = ddply(clean_wf, .(AuthorID), function(df) length(unique(df$Date)))
colnames(activity_freq_for_member)[2] = "ActivityFreq"
activity_freq_for_member = activity_freq_for_member[activity_freq_for_member$ActivityFreq != 1,]
summary(activity_freq_for_member)
      
#### Separate into active and inactive members
##### Active members = above 75% 
##### Inactive members = 25% - 75% (Q1 to Q3)
##### Any member that has greater than 6 activities is considered as an active member
##### For every member that has 2 - 6 activities, they are inactive members
#<Active Member Group>
active_member_ids = activity_freq_for_member[activity_freq_for_member$ActivityFreq > 6,]$AuthorID
active_member_ids

#<Inactive Member Group>
inactive_member_ids = activity_freq_for_member[between(activity_freq_for_member$ActivityFreq, 2, 6),]$AuthorID
inactive_member_ids

#### Calculate the mean of 4 summary variables of active and inactive author groups by month
#<Active Member Group>
wf_by_active_members = clean_wf[clean_wf$AuthorID %in% active_member_ids,] %>% 
                          dplyr::group_by(Date) %>%
                          dplyr:: summarise(
                            Analytic = mean(Analytic),
                            Clout = mean(Clout),
                            Authentic = mean(Authentic),
                            Tone = mean(Tone)
                          )

#<Inactive Member Group>
wf_by_inactive_members = clean_wf[clean_wf$AuthorID %in% inactive_member_ids,] %>% 
                          dplyr::group_by(Date) %>%
                          dplyr:: summarise(
                            Analytic = mean(Analytic),
                            Clout = mean(Clout),
                            Authentic = mean(Authentic),
                            Tone = mean(Tone)
                          )

#### Select data from year 2005 onwards
##### Reason: missing months from year 2002 - 2004
#<Active Member Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_active_members$Date))

#<Inactive Member Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_inactive_members$Date))

#### Plot the time series graphs
#<Active Member Group>
time_series_for_active_members = ts(wf_by_active_members[wf_by_active_members$Date >= c(2005),], frequency=12, start=c(2005))
plot(time_series_for_active_members[,2:5], main="Time Series for Active Member Group")
ts.plot(time_series_for_active_members[,2:5], main="Time Series for Active Member Group", ylim=c(0,100), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_active_members)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.5)

#<Inactive Member Group>
time_series_for_inactive_members = ts(wf_by_inactive_members[wf_by_inactive_members$Date >= c(2005),], frequency=12, start=c(2005))
plot(time_series_for_inactive_members[,2:5], main="Time Series for Inactive Member Group")
ts.plot(time_series_for_inactive_members[,2:5], main="Time Series for Inactive Member Group", ylim=c(0,100), col=c("red","orange","purple","green"))
legend("topright", colnames(time_series_for_inactive_members)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.5)

### Approach 2.2: Network
#### Summary of number of threads by members
##### We calculate the number of threads that each member has
##### Our assumption: (1) if the member has posted in a thread, he/she is said to be connected with the people in the thread.
#####                 (2) members with only one thread are not considered. (decision from summary)
thread_freq_by_authors = ddply(clean_wf, .(AuthorID), function(df) length(unique(df$ThreadID)))
colnames(thread_freq_by_authors)[2] = c("NumThread")
thread_freq_by_authors = with(thread_freq_by_authors, thread_freq_by_authors[NumThread > 1,])
summary(thread_freq_by_authors)

#### Number of authors that involve in more than one thread
length(thread_freq_by_authors$AuthorID)

#### Extract the author ids with more than one post
authors_with_more_than_one_post = thread_freq_by_authors$AuthorID
  
#### Create a table (data frame) that contains list of authors by the group of threads  
author_list_by_threads = clean_wf[clean_wf$AuthorID %in% authors_with_more_than_one_post,] %>%
                     dplyr::group_by(ThreadID) %>%
                     dplyr::summarise(
                       AuthorID = list(unique(AuthorID))
                     )

##### Create a function that takes a list of values and generates all the combinations of connection among them
my_rel_comb = function(list){
  return(combinations(n=length(list), r=2, v=list))
}

##### Apply combination function to each list of authors and combine them by rows
connection_table = do.call(rbind, lapply(author_list_by_threads$AuthorID, my_rel_comb))
connection_table = unique(connection_table) # Remove those duplicate connections between two authors

#### Convert table (data frame) into graph
connection_graph = graph_from_data_frame(connection_table,directed=F)

#### Create a function to tabulate and print vertex characteristics based on code provided by Dilpreet (from tutorial week 5)
describe_vertices <- function(net, order_col) {
      if (missing(order_col)) {
        order_col = 1
      }
      degree = as.table(degree(net))
      betweenness = as.table(betweenness(net))
      closeness = as.table(closeness(net))
      eigenvector = as.table(evcent(net)$vector)
      vertex_tab = as.data.frame(rbind(degree, betweenness, closeness, eigenvector))
      vertex_tab = t(vertex_tab)
      vertex_tab = round(vertex_tab, 4)
      cat("VERTEX CHARACTERISTICS: ", "\n")
      return(vertex_tab[order(-vertex_tab[,order_col]), ])
}

#### Analyze the connection graph created (summary for centrality information)
vertex_characteristic_table = as.data.frame(describe_vertices(connection_graph))
vertex_characteristic_table
summary(vertex_characteristic_table)

# Take top and bottom 25% socially-connected author group
# Top 25% Socially-Connected Author : >= 198 
# Bottom 25% Socially-Connected Author : <= 70 = btm 25%
top25 = as.numeric(unlist(rownames(vertex_characteristic_table[vertex_characteristic_table$degree >= 198,])))
btm25 = as.numeric(unlist(rownames(vertex_characteristic_table[vertex_characteristic_table$degree <= 70,])))

#### Calculate the mean of 4 summary variables of top and bottom 25% author groups by month
#<Top 25% Socially-Connected Author Group>
wf_by_top25 = clean_wf[clean_wf$AuthorID %in% top25,] %>% 
              dplyr::group_by(Date) %>%
              dplyr:: summarise(
                Analytic = mean(Analytic),
                Clout = mean(Clout),
                Authentic = mean(Authentic),
                Tone = mean(Tone)
              )

#<Bottom 25% Socially-Connected Author Group>
wf_by_btm25 = clean_wf[clean_wf$AuthorID %in% btm25,] %>% 
              dplyr::group_by(Date) %>%
              dplyr:: summarise(
                Analytic = mean(Analytic),
                Clout = mean(Clout),
                Authentic = mean(Authentic),
                Tone = mean(Tone)
              )

#### Select data from year 2005 onwards
##### Reason: missing months from year 2002 - 2004
#<Top 25% Socially-Connected Author Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_top25$Date))

#<Bottom 25% Socially-Connected Author Group>
as.yearmon(setdiff(months_from_2002to12, wf_by_btm25$Date))

#### Plot the time series graphs
#<Top 25% Socially-Connected Author Group>
wf_by_top25 = fill_missing_month_rows(wf_by_top25)
time_series_for_top25 = ts(wf_by_top25[wf_by_top25$Date >= c(2005),], frequency=12, start=c(2002,1), end=c(2011,12))
plot(time_series_for_top25[,2:5], main="Time Top 25% Socially-Connected Author Group")
ts.plot(time_series_for_top25[,2:5], main="Time Series for Top 25% Socially-Connected Author Group", ylim=c(0,100), col=c("red","orange","purple","green"))
legend("bottomleft",inset=c(0.01,0.01), colnames(time_series_for_top25)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.5)

#<Bottom 25% Socially-Connected Author Group>
wf_by_btm25 = fill_missing_month_rows(wf_by_btm25)
time_series_for_btm25 = ts(wf_by_btm25[wf_by_btm25$Date >= c(2005),], frequency=12, start=c(2002,1), end=c(2011,12))
plot(time_series_for_btm25[,2:5], main="Time Series for Bottom 25% Socially-Connected Author Group")
ts.plot(time_series_for_btm25[,2:5], main="Time Series for Bottom 25% Socially-Connected Author Group", ylim=c(0,100), col=c("red","orange","purple","green"))
legend("bottomleft",inset=c(0.01,0.01), colnames(time_series_for_btm25)[2:5], col=c("red","orange","purple","green"), lty=1, cex=.5)

#************************************************************************************************************************
#************************************************************************************************************************
# Small Testing Example
## Uncomment the block of code below if needed for testing
# x = c(1,2,3)
# y = c(1,5,3)
# z = do.call(rbind, lapply(list(x,y), my_rel_comb))
# a = graph_from_data_frame(z, directed=F)
# plot(a)
# describe_vertices(a)
#************************************************************************************************************************
#************************************************************************************************************************