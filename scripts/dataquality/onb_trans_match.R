### Data Quality of Auslandsastriaca translations in the ONB###
##Author: Lisa Teichmann 
##Date: 19 January 2024
##Data extraction: The following links have been used to query the catalogue for Auslandsaustriaca before and after 2009
##after 2009: https://obv-at-oenb.alma.exlibrisgroup.com/view/sru/43ACC_ONB?version=1.2&query=alma.national_bibliography_number=OeBC%20and%20alma.local_field_970=56%20and%20alma.%20main_pub_date%3E2009&operation=searchRetrieve
##before 2009: https://obv-at-oenb.alma.exlibrisgroup.com/view/sru/43ACC_ONB?version=1.2&query=alma.national_bibliography_number=OeBC%20and%20alma.local_field_970=56%20and%20alma.%20main_pub_date%3C2009&operation=searchRetrieve
##Data filtering: Data has been filtered for records that include the MARC field 41/h (original language)

#Load packages
#install.packages(c("ggplot2", "dplyr", "gridExtra", "stringr", "readxl"))
library(dplyr)
library(gridExtra)
library(stringr)
library(readxl)
library(ggplot2)

#Import master tables extracted from ONB from Python script
onbtrans_before2009<-read.csv("scripts/290424_onb_trans_before2009.csv", header=T,row.names=NULL,sep=",")
onbtrans_after2009<-read.csv("scripts/290424_onb_trans_after2009.csv", header=T,row.names=NULL,sep=",")

#Clean columns
onbtrans_before2009$IDN <-as.factor(onbtrans_before2009$IDN)
onbtrans_after2009$IDN <-as.factor(onbtrans_after2009$IDN)
onbtrans_before2009[1] <- NULL 
onbtrans_after2009[1] <- NULL

#append to one table
onbtrans <- rbind(onbtrans_before2009, onbtrans_after2009)

#Check for duplicates
sum(duplicated(onbtrans)=="TRUE")

#Check missing values
creator_perc <- (sum(onbtrans$author=="fail")/nrow(onbtrans))*100
publisher_perc<- (sum(onbtrans$publisher=="fail")/nrow(onbtrans))*100
country_perc<- (sum(onbtrans$country=="fail")/nrow(onbtrans))*100
isbn_perc<- (sum(onbtrans$ISBN=="fail")/nrow(onbtrans))*100
unititle_perc<- (sum(onbtrans$uniform.title=="fail")/nrow(onbtrans))*100
year_perc<- (sum(onbtrans$year=="fail")/nrow(onbtrans))*100
idn_perc<- (sum(onbtrans$IDN=="fail")/nrow(onbtrans))*100

### Create new DF with percentages of missing values per variable
na_perc <- data.frame(category = c("author", "publisher", "country", "ISBN", "uniform title", "year", "IDN"),
                      percentage_nas = c(creator_perc, publisher_perc, country_perc, isbn_perc, unititle_perc, year_perc, idn_perc)
)

###Plot percentages of NA
ggplot(na_perc,aes(x= reorder(category,-percentage_nas),percentage_nas))+geom_bar(stat ="identity")+ xlab("Category") + ylab("Percentage of Missing Values")+ labs(title = "Completeness in the DNB translation dataset", subtitle = "Percentages across categories") +coord_flip()+theme_bw()
ggsave("figures/onb_trans_dataquality_completeness.png")

### Clean author column of special characters etc.
View(table(onbtrans$author))
torem <- c("<<von>>", "<<de>>", " ", "-","[0-9]")
onbtrans$author <- gsub(torem, "", onbtrans$author)
onbtrans$author <- gsub("-[0-9]*","", onbtrans$author)
onbtrans$author <- gsub("[0-9]","", onbtrans$author)
str_trim(onbtrans$author)

#check for author name ambiguity
View(table(onbtrans$author))

## count unique author names
length(unique(onbtrans$author))
#finding: returns 747 unique author names, which appear to be coherent

## clean other columns
onbtrans$title <- gsub("<<","", onbtrans$title)
onbtrans$title <- gsub(">>","", onbtrans$title)

onbtrans$year <- gsub("\\[|\\]", "", onbtrans$year)
onbtrans$year <- gsub("\\D+", " ", onbtrans$year)
onbtrans$year <- str_remove(onbtrans$year, "(?<=\\d{4}).*")
onbtrans$year <- str_remove(onbtrans$year, ".*(?=\\d{4})")

View(table(onbtrans$year))

write.csv(onbtrans, "data/020524_onb_trans_clean.csv")

###Match onb with dnb by author name

###Match onb with dnb by isbn






