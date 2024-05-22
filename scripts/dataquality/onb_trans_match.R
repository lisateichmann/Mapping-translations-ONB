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
library(stringr)
library(tidyr)

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

##remove columns that do not match with dnb: original.language
onbtrans$original.language <- NULL

write.csv(onbtrans, "data/020524_onb_trans_clean.csv")

###Match onb with dnb by isbn

##pre-processing of dnb data
dnb_all <- read.csv("~/DNB-German-Fiction-Translations-Catalogue-Data/Data/dnb_transdata_220523/alldnb_2023_220523.csv")
dnbtrans <- dnb_all
##remove dashes in isbn's
dnbtrans$ISBN <- gsub("-","", dnbtrans$ISBN)

##move IDN to new column
dnbtrans$author <- sub("\\s*\\[.*", "", dnbtrans$creator)

##move author to new column
dnbtrans <- separate(dnbtrans, identifier ,into = c("identifier", "IDN"), sep= "IDN:")

##only keep colnames in dnbtrans that are in onbtrans
onb_colnames <- c(colnames(onbtrans))
dnbtrans <- dnbtrans[, (colnames(dnbtrans) %in% onb_colnames)]

##add new column for data source and add unique identifier
dnbtrans$id <- 1:nrow(dnbtrans) 
dnbtrans$id  <- paste0('dnb', dnbtrans$IDN)
onbtrans$id <- 1:nrow(onbtrans) 
onbtrans$id  <- paste0('onb', onbtrans$IDN)

##Match
#dnb_onb_match <- dnbtrans[match(dnbtrans$ISBN, onbtrans$ISBN)]
##check true/false
sum(onbtrans$ISBN %in% dnbtrans$ISBN, na.rm = TRUE)
sum(dnbtrans$ISBN %in% onbtrans$ISBN, na.rm = TRUE)

##remove titles with no ISBN to prevent false matches
(sum(onbtrans$ISBN=="fail")/nrow(onbtrans))*100
#3.7% no ISBN
(sum(dnbtrans$ISBN=="")/nrow(dnbtrans))*100
#6.7% no ISBN

dnbtrans <- dnbtrans[!(is.na(dnbtrans$ISBN) | dnbtrans$ISBN==""), ]
onbtrans <- onbtrans[!(is.na(onbtrans$ISBN) | onbtrans$ISBN=="fail"), ]

##remove titles without author
dnbtrans <- dnbtrans[!(is.na(dnbtrans$author) | dnbtrans$author==""), ]
onbtrans <- onbtrans[!(is.na(onbtrans$author) | onbtrans$author=="fail"| onbtrans$author==""), ]

##only keep true matches
##titles that are in the ONB which are also in the DNB
onb_match <- subset(onbtrans, (ISBN %in% dnbtrans$ISBN))
##titles that are in the DNB which are also in the ONB (there might be titles in the DNB that are not in the ONB)
dnb_match <- subset(dnbtrans, (ISBN %in% onbtrans$ISBN))

##distinct???
which(duplicated(trans_match$ISBN))

##match funktion probieren

##combine both
trans_match <- rbind(onb_match, dnb_match)

write.csv(trans_match, "data/210524_dnb_onb_trans_match.csv")

##How much matches?
nrow(onb_dnb_match)/nrow(onbtrans)
#30.6% of onb matches with dnb

##all
nrow(trans_match)/(nrow(onbtrans)+nrow(dnbtrans))
#16.0% matches!

##add new column with TRUE/FALSE (does not work)
# dnbtrans_match <- dnbtrans
# dnbtrans_match$match <-  ifelse(onbtrans$ISBN==dnbtrans$ISBN, TRUE, FALSE)
# dnbtrans_match$match <- ifelse(grepl(onbtrans$ISBN,dnbtrans$ISBN),'TRUE','FALSE')

###Data exploration

##most frequent authors
View(table(trans_match$author))

##which titles from ONB are NOT DNB
onb_nomatch <- subset(onbtrans, !(ISBN %in% dnbtrans$ISBN))

#check if it is correct
nrow(onb_nomatch)/nrow(onbtrans)
#47.3 % no match
nrow(onb_nomatch)+nrow(onb_match)
##correct!

write.csv(trans_match, "data/210524_onb_trans_nomatch.csv")

###The authors both libraries have most in common
View(table(trans_match$author))
trans_match_author_freq <- as.data.frame(table(trans_match$author))

trans_match_author_freq %>%
  top_n(20) %>% ggplot(aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Authors") + ylab("Titles")+ labs(title = "Matched authors in the ONB and DNB", subtitle = "Top 20 by title frequency") +coord_flip()+theme_bw()

ggsave("figures/onb_dnb_matched_authortitle_freq_hist.png")

##compare top author's titles in DNB vs. ONB
onbtrans %>%  
  group_by(author) %>%
  summarise(onb_freq = n_distinct(ISBN)) %>%
  top_n(20) %>% ggplot(aes(x= reorder(author,-onb_freq),onb_freq))+geom_bar(stat ="identity")+ xlab("Authors") + ylab("Titles")+ labs(title = "Most frequent authors in ONB", subtitle = "Top 20 by title frequency") +coord_flip()+theme_bw()
  ggsave(filename="figures/onb_authortitle_freq_hist.png")

###Modeling the absence of ONB authors in the DNB

##total of each author’s translated titles and target languages in the ONB, ranking them in descending order

##which author is the most translated for languages
onb_match_author_title <- onb_match %>%  
  group_by(author) %>%
  summarise(onb_title_freq = n_distinct(ISBN))
  
onb_match_author_lang <- onb_match %>%  
  group_by(author) %>%
  summarise(onb_lang_freq = n_distinct(language))

onb_match_freqs <- onb_match_author_title
onb_match_freqs$onb_lang_freq <- onb_match_author_lang$onb_lang_freq

onb_match_freqs %>% 
  arrange(desc(onb_title_freq)) %>%
  slice(1:20) %>%
  pivot_longer(!author, names_to = "type", values_to = "freqs") %>% 
  ggplot(aes(x = reorder(author, -freqs), y = freqs, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("figures/onb_authortitle_lang_freq_barchart.png", width = 6, height = 4, dpi=300)

##To identify (under-) represented authors, I will then compare the author’s positions in the ranking with the author records in the German National Library. 

dnb_match_author_title <- dnb_match %>%  
  group_by(author) %>%
  summarise(dnb_title_freq = n_distinct(ISBN))

dnb_match_author_lang <- dnb_match %>%  
  group_by(author) %>%
  summarise(dnb_lang_freq = n_distinct(language))

dnb_match_freqs <- dnb_match_author_title
dnb_match_freqs$dnb_lang_freq <- dnb_match_author_lang$dnb_lang_freq

dnb_match_freqs %>% 
  arrange(desc(dnb_title_freq)) %>%
  slice(1:20) %>%
  pivot_longer(!author, names_to = "type", values_to = "freqs") %>% 
  ggplot(aes(x = reorder(author, -freqs), y = freqs, fill=type)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("figures/dnb_authortitle_lang_freq_barchart.png", width = 6, height = 4, dpi=300)

##Scatterplot with authors colored if they are in onb, in dnb, or in both

ggplot(dnb_match_freqs, aes(dnb_lang_freq, dnb_title_freq))+ ggtitle("Correlation of language and title counts per author")+
  geom_text(label=dnb_match_freqs$author) + labs(x = "Languages", y = "Titles") + geom_smooth(method="lm")+theme_bw()

##same for top authors in each library (onbtrans and dnbtrans)
##needs normalization!!!

dnb_author_title <- dnbtrans %>%  
  group_by(author) %>%
  summarise(dnb_title_freq = n_distinct(ISBN))

dnb_author_title$dnb_title_scaled <- scale(dnb_author_title$dnb_title_freq)

dnb_author_lang <- dnbtrans %>%  
  group_by(author) %>%
  summarise(dnb_lang_freq = n_distinct(language))

dnb_author_lang$dnb_lang_scaled <- scale(dnb_author_lang$dnb_lang_freq)

dnb_freqs <- dnb_author_title
dnb_freqs$dnb_lang_freq <- dnb_author_lang$dnb_lang_freq

onb_author_title <- onbtrans %>%  
  group_by(author) %>%
  summarise(onb_title_freq = n_distinct(ISBN))

onb_author_title$onb_title_scaled <- scale(onb_author_title$onb_title_freq)

onb_author_lang <- onbtrans %>%  
  group_by(author) %>%
  summarise(onb_lang_freq = n_distinct(language))

onb_author_lang$onb_lang_scaled <- scale(onb_author_lang$onb_lang_freq)

onb_freqs <- onb_author_title
onb_freqs$onb_lang_freq <- onb_author_lang$onb_lang_freq

ggplot() + 
  geom_point(data=dnb_freqs %>% dplyr::top_n(20, dnb_title_freq), aes(x=dnb_lang_freq, y=dnb_title_freq), color='green') + 
  geom_text_repel(aes(x = dnb_lang_freq, y = dnb_title_freq, label = author), data = dnb_freqs %>% dplyr::top_n(20, dnb_title_freq))  +
  geom_point(data=onb_freqs %>% dplyr::top_n(20, onb_title_freq), aes(x=onb_lang_freq, y=onb_title_freq), color='red') + 
  geom_text_repel(aes(x = onb_lang_freq, y = onb_title_freq, label = author), data = onb_freqs %>% dplyr::top_n(20, onb_title_freq)) + labs(x = "Languages", y = "Titles", color="Legend")+ ggtitle("Correlation of language and title counts per author (top 20)\n for the ONB (red) and the DNB (green)")

ggsave("figures/dnb_onb_title_lang_corrplot.png")


##I will apply a Chi-squared test of independence to identify authors that are underrepresented in the ONB. 


##To highlight authors that appear across these library collections, I will create an interactive network graph in which authors represent connections or bridges between libraries








####ARCHIVE

##useful code

##color geom_point
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, col = Species)) +
  geom_point() +
  geom_point(aes(x = Sepal.Width, y = Sepal.Length, colour = 'Sepal.Width'),
             data = df) +
  scale_color_manual(
    values = c(
      "Sepal.Width" = "blue",
      "setosa" = "red",
      "versicolor" = "darkgreen",
      "virginica" = "orange"
    ),
    labels = c('New Point', "setosa", "versicolor", "virginica")
  )




