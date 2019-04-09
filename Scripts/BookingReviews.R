remove(list = ls()) #clear environment

#load rome.RDA first
data_rome <- x[!duplicated(x[c(1,2,3,5)]),]
unique(data_rome$hotelid)
data_rome$city <- "Rome"

#Counting random reviews to verify correct number
filterDataOctober <- mydata[mydata$hotelid == "/hotel/it/hotelbledrome.html" & mydata$stay_date == "Stayed in October 2018",]
filterDataSeptember <- mydata[mydata$hotelid == "/hotel/it/hotelbledrome.html" & mydata$stay_date == "Stayed in September 2018",]
filterDataJanuary2017 <- mydata[mydata$hotelid == "/hotel/it/hotelbledrome.html" & mydata$stay_date == "Stayed in January 2017",]

#relais-dei-cinquecento
filterRelaisJanuary2017 <- mydata[mydata$hotelid == "/hotel/it/relais-dei-cinquecento.html" & mydata$stay_date == "Stayed in January 2017",]

x[x$hotelid == "/hotel/it/residence-relais-cassia.it.html",]
mydata[mydata$hotelid == "hotel/it/residence-centro-benigni.html",]
mydata[mydata$hotelid == "/hotel/it/hotelbledrome.html",]
#966 - 957 = 9 hotel senza review;

#Taipei
taipei <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/taipei_htonly.csv", header = TRUE)
data_taipei <- taipei[!duplicated(taipei[c(1,2,3,5)]),]
data_taipei$city <-"Taipei"

#Milan
milan <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/milan.csv", header = TRUE)
data_milan <- milan[!duplicated(milan[c(1,2,3,5)]),]
data_milan$city <-"Milan"


#Venice
venice <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/venice.csv", header = TRUE)
data_venice <- venice[!duplicated(venice[c(1,2,3,5)]),]
data_venice$city <-"Venice"

#Florence
florence <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/florence.csv", header = TRUE)
data_florence <- florence[!duplicated(florence[c(1,2,3,5)]),]
data_florence$city <-"Florence"

#Verona
verona <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/DataInput/verona.csv", na = "NA", header = TRUE)
data_verona <- verona[!duplicated(verona[c(1,2,3,5)]),]
data_verona$city <-"Verona"

#merging dataframes
data_all <- rbind(data_florence,
                  data_milan,
                  data_rome,
                  data_taipei,
                  data_venice,
                  data_verona)
#is.na(data_all) <- data_all==' ' #consider missing values as NAs
#data_all[is.na(data_all)] <- " " #consider NAs as blanks

#export as excel file
install.packages("xlsx")
library(xlsx)
write.csv(data_all, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/dataAll.csv", row.names = TRUE ,na = " " )

#Read data_all
df <- read.csv("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/Progetto/Data/DataOutput/dataAll.csv", na = "NA", header = TRUE)
install.packages("devtools")
devtools::install_github("jimhester/vroom", force = T)
library(vroom)
data_all <- vroom("/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/Progetto/Data/DataOutput/dataAll.csv", delim = ',')
#data_all <- data_all %>% top_n(1000, hotelid)
is.na(data_all) <- data_all==' '

#Dataset with only positive
Positive <- data_all %>% select(positive)
Positive <- as.data.frame(Positive[!is.na(Positive$positive),])
colnames(Positive) [1] <- "Positive Reviews"
write.csv(dataPositive, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/Positive.csv", row.names = TRUE ,na = " " )

#Dataset with only negative
Negative <- data_all %>% select(negative)
Negative <- as.data.frame(Negative[!is.na(Negative$negative),])
colnames(Negative) [1] <- "Negative Reviews"
write.csv(dataNegative, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/Negative.csv", row.names = TRUE ,na = " " )

#create a dataset with only positive, negative and full review
library(tidyverse)
data_all_full <- data_all %>% unite(fullText, positive, negative, sep = " " )
is.na(data_all_full) <- data_all_full == ' ' #consider missing values as NAs
#data_all_full[is.na(data_all_full)] <- " " #consider NAs as blanks

#Create a review ID column
data_all_full$reviewID <- seq.int(nrow(data_all_full))

#write.csv(data_all_full, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/ProjectR/dataFullText.csv", row.names = TRUE ,na = " " )

table(is.na(data_all_full$response)) #number of responses

#Select only italian reviews 
library(cld2)
it_reviewResponse <- data_all_full %>% 
  filter(detect_language(data_all_full$fullText) == 'it') #detect italian reviews only
table(is.na(it_reviewResponse$response)) #number of responses

it_reviewResponse <- it_reviewResponse[!is.na(it_reviewResponse$response),]

#Take a sample of 500 random full reviews and responses
Sample_ReviewResponse <- sample_n(reviewResponse1, size = 500)
write.csv(Sample_ReviewResponse, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/BookingReviews/ProjectR/DataOutput/SampleFullResponse.csv", row.names = TRUE ,na = " " )

#Take a sample of 500 random full reviews+responses
library(readr)
Sample <- sample_n(reviewResponse2, size = 500)
readr::write_csv(Sample, "/users/elisacangialosi/Desktop/LouisianaStateUniversity/IISemester/BookingReviews/ProjectR/DataOutput/SampleFinal2.csv" ,na = " ")

#Importing libraries
library(tidyverse)#general utility & workflow functions
install.packages("tidytext")
library(tidytext) #tidy implimentation of NLP methods
install.packages("topicmodels")
library(topicmodels) # for LDA topic modelling 
install.packages("NLP")
library(NLP)
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
