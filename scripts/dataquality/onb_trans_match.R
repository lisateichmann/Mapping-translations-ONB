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
library(ggrepel)
library(data.table)

#Import master tables extracted from ONB from Python script
onbtrans_before2009<-read.csv("scripts/290424_onb_trans_before2009.csv", header=T,row.names=NULL,sep=",")
onbtrans_after2009<-read.csv("scripts/290424_onb_trans_after2009.csv", header=T,row.names=NULL,sep=",")

#Clean columns
onbtrans_before2009$IDN <-as.factor(onbtrans_before2009$IDN)
onbtrans_after2009$IDN <-as.factor(onbtrans_after2009$IDN)
onbtrans_before2009[1] <- NULL 
onbtrans_after2009[1] <- NULL

#append to one table
onbtrans_oj <- rbind(onbtrans_before2009, onbtrans_after2009)

#Check for duplicates
sum(duplicated(onbtrans_oj)=="TRUE")
##Investigate duplicates
onbtrans_dup <- onbtrans_oj[duplicated(onbtrans_oj$ISBN),]

##remove duplicates
onbtrans_oj <- unique(onbtrans_oj)

#Check missing values
creator_perc <- (sum(onbtrans_oj$author=="fail")/nrow(onbtrans_oj))*100
publisher_perc<- (sum(onbtrans_oj$publisher=="fail")/nrow(onbtrans_oj))*100
country_perc<- (sum(onbtrans_oj$country=="fail")/nrow(onbtrans_oj))*100
isbn_perc<- (sum(onbtrans_oj$ISBN=="fail")/nrow(onbtrans_oj))*100
unititle_perc<- (sum(onbtrans_oj$uniform.title=="fail")/nrow(onbtrans_oj))*100
year_perc<- (sum(onbtrans_oj$year=="fail")/nrow(onbtrans_oj))*100
idn_perc<- (sum(onbtrans_oj$IDN=="fail")/nrow(onbtrans_oj))*100

### Create new DF with percentages of missing values per variable
na_perc <- data.frame(category = c("author", "publisher", "country", "ISBN", "uniform title", "year", "IDN"),
                      percentage_nas = c(creator_perc, publisher_perc, country_perc, isbn_perc, unititle_perc, year_perc, idn_perc)
)

###Plot percentages of NA
ggplot(na_perc,aes(x= reorder(category,-percentage_nas),percentage_nas))+geom_bar(stat ="identity")+ xlab("Category") + ylab("Percentage of Missing Values")+ labs(title = "Completeness in the ONB translation dataset", subtitle = "Percentages across categories") +coord_flip()+theme_bw()
ggsave("figures/onb_trans_dataquality_completeness.png")

onbtrans <- onbtrans_oj

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

##remove duplicates
dnbtrans <- unique(dnbtrans)

##Match
#dnb_onb_match <- dnbtrans[match(dnbtrans$ISBN, onbtrans$ISBN)]
##check true/false
sum(onbtrans$ISBN %in% dnbtrans$ISBN, na.rm = FALSE)
#sum(dnbtrans$ISBN %in% onbtrans$ISBN, na.rm = FALSE)
#i dont know why this one has 2 titles less
#check which two titles have less??

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
nrow(subset(onbtrans, (!ISBN %in% dnbtrans$ISBN)))

write.csv(onb_match, "data/210524_dnb_onb_trans_match.csv")

##How much matches?
nrow(onb_match)/nrow(onbtrans)
#52.4% of onb matches with dnb

nrow(onb_match)/nrow(dnbtrans)
#7.4% of DNB matches with ONB

##all
nrow(trans_match)/(nrow(onbtrans)+nrow(dnbtrans))
#17.5% matches!

# Load library
library(VennDiagram)

# Chart
venn.diagram(
  x = list(
    onbtrans %>% select(ISBN) %>% unlist() , 
    dnbtrans %>% select(ISBN) %>% unlist() 
  ),
  category.names = c("ONB" , "DNB"),
  filename = 'figures/dnb_onb_overlap_venn.png',
  output=TRUE,
  fill=c("tomato3","palegreen4"),
  cat.pos = c(-45, 60),
  width=4000,
  height=4000
)

###Data exploration

##which titles from ONB are NOT DNB
onb_nomatch <- subset(onbtrans, !(ISBN %in% dnbtrans$ISBN))

dnb_nomatch <-subset(dnbtrans, !(ISBN %in% onbtrans$ISBN))

#check if it is correct
nrow(onb_nomatch)/nrow(onbtrans)
#47.5 % no match
(nrow(onb_nomatch)+nrow(onb_match))/nrow(onbtrans)
##correct!

write.csv(onb_nomatch, "data/210524_onb_trans_nomatch.csv")

###Modeling the presence and absence of ONB authors in the DNB

###1. The most present authors both libraries have most in common and the ONB has

##top author in the ONB
onbtrans %>%  
  group_by(author) %>%
  summarise(onb_freq = n_distinct(ISBN)) %>%
  top_n(20) %>% ggplot(aes(x= reorder(author,-onb_freq),onb_freq))+geom_bar(stat ="identity")+ xlab("Authors") + ylab("Titles")+ labs(title = "Most frequent authors in ONB", subtitle = "Top 20 by title frequency") +coord_flip()+theme_bw()
  ggsave(filename="figures/onb_authortitle_freq_hist.png")
  
###The authors both libraries have most in common
  onb_match %>%  
    group_by(author) %>%
    summarise(onb_freq = n_distinct(ISBN)) %>%
    top_n(20) %>% ggplot(aes(x= reorder(author,-onb_freq),onb_freq))+geom_bar(stat ="identity")+ xlab("Authors") + ylab("Titles")+ labs(title = "Most frequent matched authors in ONB and DNB", subtitle = "Top 20 by title frequency") +coord_flip()+theme_bw()
  ggsave(filename="figures/onb_dnb_match_authortitle_freq_hist.png")
  
##2. total of each author’s translated titles and target languages in the ONB, ranking them in descending order

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

##Scatterplot

ggplot(onb_match_freqs, aes(onb_lang_freq, onb_title_freq))+ ggtitle("Correlation of language and title counts per author")+
  geom_text(label=onb_match_freqs$author) + labs(x = "Languages", y = "Titles") + geom_smooth(method="lm")+theme_bw()

##lonb tail!

##To identify (under-) represented authors, I will then compare the author’s positions in the ranking with the author records in the German National Library. 
##needs normalization!!! scaling does not work

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
dnb_freqs$dnb_lang_scaled <- dnb_author_lang$dnb_lang_scaled

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
onb_freqs$onb_lang_scaled <- onb_author_lang$onb_lang_scaled


ggplot() + 
  geom_point(data=dnb_freqs %>% dplyr::top_n(20, dnb_title_scaled), aes(x=dnb_lang_scaled, y=dnb_title_scaled), color='green') + 
  geom_text_repel(aes(x = dnb_lang_scaled, y = dnb_title_scaled, label = author), data = dnb_freqs %>% dplyr::top_n(20, dnb_title_scaled))  +
  geom_point(data=onb_freqs %>% dplyr::top_n(20, onb_title_scaled), aes(x=onb_lang_scaled, y=onb_title_scaled), color='red') + 
  geom_text_repel(aes(x = onb_lang_scaled, y = onb_title_scaled, label = author), data = onb_freqs %>% dplyr::top_n(20, onb_title_scaled)) + labs(x = "Languages", y = "Titles", color="Legend")+ ggtitle("Correlation of language and title counts per author (top 20)\n for the ONB (red) and the DNB (green)")

ggsave("figures/dnb_onb_title_lang_corrplot.png")

##Hypothesis 1: authors are underrepresented if they are in the tail (have less than mean translations)
mean(onb_freqs$onb_title_freq)
#6.9

#visualize the tail

#only ONB
library(ggpubr)
gghistogram(onb_freqs, x = "onb_title_freq", bins = 20, xlab="Titles", ylab="Authors",
            title="Distribution of title sums per author (mean=6.9)\nin the ONB",
            add = "mean")

ggsave("figures/dnb_onb_title_dist_tail.png", width=5.5, height=5.5)

#matched
mean(onb_match_freqs$match_title_freq)
#9.383212

gghistogram(onb_match_freqs, x = "match_title_freq", bins = 20, xlab="Titles", ylab="Authors",
            title="Distribution of title sums per author (mean=9.3)\nin ONB and DNB",
            add = "mean")

ggsave("figures/dnb_onb_matched_title_dist_tail.png", width=5.5, height=5.5)

##other tail measures
#plot(density(onb_freqs$onb_title_freq), main="Empirical cumulative distribution function(ECDF)\nTitle Counts by Author in ONB")
#qqnorm(onb_freqs$onb_title_freq);qqline(onb_freqs$onb_title_freq, col = 2)
#pnorm(20, mean=(mean(onb_freqs$onb_title_freq, na.rm = TRUE)),sd=(sd(onb_freqs$onb_title_freq, na.rm = TRUE))) - pnorm(0, mean=(mean(onb_freqs$onb_title_freq, na.rm = TRUE)),sd=(sd(onb_freqs$onb_title_freq, na.rm = TRUE)))

# % of authors with <5 titles
onb_freqs_tail <- onb_freqs %>% filter(onb_title_freq %in% 0:5) %>% filter(onb_lang_freq %in% 0:5)
(nrow(onb_freqs_tail)/nrow(onb_freqs))*100
#85.45198% of authors

onb_match_tail <- onb_match_freqs %>% filter(onb_title_freq %in% 0:5) %>% filter(onb_lang_freq %in% 0:5)
(nrow(onb_match_tail)/nrow(onb_match_freqs))*100
#74.08759% of authors

##Need table with onb_freq and dnb_freq and match_freq to measure difference
onb_dnb_freqs <- onb_match_freqs
setnames(onb_dnb_freqs, "onb_title_freq", "match_title_freq")
setnames(onb_dnb_freqs, "onb_lang_freq", "match_lang_freq")
onb_dnb_freqs$dnb_title_freq <- dnb_freqs$dnb_title_freq[match(onb_dnb_freqs$author,dnb_freqs$author)];
onb_dnb_freqs$dnb_lang_freq <- dnb_freqs$dnb_lang_freq[match(onb_dnb_freqs$author,dnb_freqs$author)];
onb_dnb_freqs$onb_title_freq <- onb_freqs$onb_title_freq[match(onb_dnb_freqs$author,onb_freqs$author)];
onb_dnb_freqs$onb_lang_freq <- onb_freqs$onb_lang_freq[match(onb_dnb_freqs$author,onb_freqs$author)];

write.csv(onb_dnb_freqs, "results/210527_onb_dnb_freqs.csv")

##Chi-squared test of independence see distribution
library(tibble)
chi_df <- onb_dnb_freqs[, c('match_title_freq','onb_title_freq', 'author')]
chi_df <- column_to_rownames(chi_df, 'author')

chisq <- chisq.test(chi_df)
chisq

# Expected counts
round(chisq$residuals,2)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE, mar=c(0,0,0,0))

chisq_res <- as.data.frame(chisq$residuals)

write.csv(chisq_res, "results/210527_onb_dnb_freqs_chisquare_residuals.csv")


ggplot(chisq_res, aes(x = match_title_freq, y = onb_title_freq, label=rownames(chisq_res))) +  # Set up canvas with outcome variable on y-axis
  ggrepel::geom_text_repel(size = 2, box.padding = 0.05, max.overlaps = 100 )+
  ggtitle("Chi Square Test Residuals for title counts for matched authors \nand ONB authors")+
  labs(x = "Residuals for matched authors", y = "Residuals for ONB authors")+
  theme_bw()

ggsave("figures/210527_onb_dnb_freqs_chisquare_residuals.png")

##Which authors are underrepresented? 
#1. LM model and calculate residuals between onb_match and onbtrans
#fit model
author_freq_model <- lm(match_title_freq ~ onb_title_freq, data=onb_dnb_freqs)

#view model summary
summary(author_freq_model) 

#calculate the standardized residuals
standard_res <- rstandard(author_freq_model)

#view the standardized residuals
standard_res

#column bind standardized residuals back to original data frame
author_freq_res <- cbind(onb_dnb_freqs, standard_res)

#filter unusual authors that have residuals of less than -2 and more than 2
author_freq_res_outliers <- author_freq_res  %>% filter(standard_res < -1  | standard_res > 1)

#plot predictor variable vs. standardized residuals
author_freq_res_outliers %>% ggplot(aes(x = reorder(author, -standard_res), y = standard_res)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + geom_point()

ggsave("figures/270524_onb_dnb_match_freqs_LM_outliers.png", width = 6, height = 4, dpi=300)
write.csv(author_freq_res, file="results/270524_onb_dnb_match_freqs_LM_residuals.csv")

##Plot title and language frequencies for these authors to see which ones stand out
ggplot(author_freq_res_outliers, aes(match_title_freq, onb_title_freq)) + geom_point()+ ggtitle("Title sums in ONB and DNB and only in ONB with residuals of >2 and <-2") +
  labs(x = "Title sums in DNB and ONB", y = "Title sums in ONB") + geom_smooth(method="lm") + theme_bw() + geom_text_repel(aes(label=author), max.overlaps=20)

ggsave("figures/270524_onb_dnb_match_freqs_LM_outliers_corplot.png", width = 10, height = 4, dpi=300)

#2. LM model and residuals between dnbtrans and onbtrans

#replace na's in DNB with 0-values
onb_dnb_freqs_nona <- onb_dnb_freqs 
onb_dnb_freqs_nona <- onb_dnb_freqs_nona %>% replace(is.na(.), 0)

#fit model
author_freq_model_2 <- lm(dnb_title_freq ~ onb_title_freq, data=onb_dnb_freqs_nona)

#view model summary
summary(author_freq_model_2) 

#calculate the standardized residuals
standard_res_2 <- rstandard(author_freq_model_2)

#view the standardized residuals
standard_res_2

#column bind standardized residuals back to original data frame
author_freq_res_2 <- cbind(onb_dnb_freqs, standard_res_2)

#filter unusual authors that have residuals of less than -2 and more than 2
author_freq_res_outliers_2 <- author_freq_res_2  %>% filter(standard_res_2 < -1  | standard_res_2 > 1)

#plot predictor variable vs. standardized residuals
author_freq_res_outliers_2 %>% ggplot(aes(x = reorder(author, -standard_res_2), y = standard_res_2)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + geom_point()

ggsave("figures/270524_onb_dnb_freqs_LM_outliers.png", width = 6, height = 4, dpi=300)
write.csv(author_freq_res_2, file="results/270524_onb_dnb_freqs_LM_residuals.csv")

##Plot title and language frequencies for these authors to see which ones stand out
ggplot(author_freq_res_outliers_2, aes(match_title_freq, onb_title_freq)) + geom_point()+ ggtitle("Title sums in ONB and DNB and only in ONB with residuals of >2 and <-2") +
  labs(x = "Title sums in DNB", y = "Title sums in ONB") + geom_smooth(method="lm") + theme_bw() + geom_text_repel(aes(label=author), max.overlaps=20)

ggsave("figures/270524_onb_dnb_freqs_LM_outliers_corplot.png", width = 10, height = 4, dpi=300)


##Cut off the top 20 and look at the tail of matched (e.g. by language communities) OR random sample the tail


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




