
#########################
##### QuantLunch4 ######
## Demo Text Analysis ##
## Seraphine F. Maerz ##
########################


# This mini tutorial on text analysis in R consists of three parts:
# 1) An example of how to scrape texts such as political speeches
# 2) An illustration of how to apply a simple dictionary analysis
# 3) An illustration of how to use AI / LLMs to classify political speeches

# For a more comprehensive introduction to "Using AI for text analysis in R"
# see my upcoming online workshop here: https://instats.org/seminar/using-ai-for-text-analysis-in-r4
# (sign up for free with you UniMelb email address)

# CAUTION! If you are on a slow machine, I recommend to NOT code along during the tutorial
# Some packages might need further preparations before they run on your (older?Windows?) machine  

# CAUTION! Step 3 will require an API key from openai which is like a personal password
# and which requires payment set up for each run of the code (~ 3$, without guarantee)


#########################
##### 1) Scraping texts #
#########################

# GOAL: get all speeches from the Australian PM Anthony Albanese
# provided on gov.com.au, and score them on an illiberal-liberal scale
# based on a dictionary approach 


# make sure you have installed all packages
# via Packages/bottom right window or via github (see below)
# before you load them with library()

#library(devtools)
#install_github("SeraphineM/audacola")
library(audacola)
library(rvest)
library(purrr)
library(httr)
library(RCurl)
library(XML)
library(stringr)
library(xml2)
library(tidyverse)
library(ellipse)
library(extrafont)
library(plotrix)
library(ggplot2)


# "old-school" dynamic webscraping with Selenium and the audacola package
# get all links to speeches on gov.com.au, overall 22 pages with 12 speeches each
# for more information about how get_links works and what input is required: https://github.com/SeraphineM/audacola
links <- audacola::get_links("https://www.pm.gov.au/media?search_api_fulltext=&field_media_type=703&field_date%5Bmin%5D=&field_date%5Bmax%5D=",
                             "https://www.pm.gov.au/media?search_api_fulltext=&field_media_type=703&field_date%5Bmin%5D=&field_date%5Bmax%5D=",
                             "&page=",
                             "//h3//a",
                             "0", "21")

# complete links
all_links <- str_c("https://www.pm.gov.au", links)
all_links
# get page source for each speech link
sources <- audacola::get_pagesource(all_links)
# download speeches as htmls
audacola::get_html(sources, case = "html_speeches")

# now, extract the text of the speeches and build a quanteda speech corpus
# check parameters with first html in folder and get_first function
first <- audacola::get_first(loc_html = "html_speeches/",
                             x_text = "//p",
                             x_title = "//h1/span",
                             x_date = "//time",
                             speaker = "Anthony Albanese",
                             country = "Australia",
                             country_id = "aus",
                             regime = "Democracy",
                             source = "pm.gov.au")
# based on this, get them all
speech_corpus <- audacola::get_text(loc_html = "html_speeches/",
                                    x_text = "//p",
                                    x_title = "//h1/span",
                                    x_date = "//time",
                                    speaker = "Anthony Albanese",
                                    country = "Australia",
                                    country_id = "aus",
                                    regime = "Democracy",
                                    source = "pm.gov.au")

#save(speech_corpus, file = "speech_corpus.RData")
load("speech_corpus.RData")

library(quanteda)
# inspect speech_corpus
summary(speech_corpus)
# check date range
date <- docvars(speech_corpus, "date")
date
# check title
title <- docvars(speech_corpus, "title")
title
# check size of texts
ntoken(speech_corpus)
# check single texts in corpus
as.character(speech_corpus)[100]
# rename texts of corpus
names <- docnames(speech_corpus)
# compile character vector of length of docs with identifier ahead
indiv_names <- sprintf("aus[%s]",seq(1:length(names)))




#########################
##### 2) Dictionary  ####
#########################

# GOAL: scale each speech of Albanese based on a
# dictionary and scaling model on illiberal vs liberal rhetoric


# load dictionary of Maerz and Schneider (2019)
# to measure illiberal vs liberal speeches
# more info here: https://link.springer.com/article/10.1007/s11135-019-00885-7
dictionary <- dictionary(file = "Dic_valid.ykd")

# turn our speech_corpus into a quanteda dfm (see https://tutorials.quanteda.io/ for more info)
toks <- tokens(speech_corpus, remove_punct = TRUE)
dfm <- dfm(toks)
dfm <- dfm_group(dfm, groups = docvars(toks, "date")) # summarize speeches per month
print(dfm)
# apply dictionary on dfm_months
dicdfm <- dfm_lookup(dfm, dictionary = dictionary)
print(dicdfm)

# implement the Maerz and Schneider (2019) scaling model 
# adjust number of documents here
model <- matrix(dicdfm, nrow = 211, ncol = 6)
model <- as.data.frame(model)
model[,7] <- rownames(dfm)
colnames(model)[7] <- "ID"
# make ID rownames
row.names(model) <- model[,7]
model[,7] <- NULL
# add up categories
model$illiberal <- model$V1 + model$V2 + model$V3
model$liberal <- model$V4 + model$V5 + model$V6
# erase other colums
model[,c(1:6)] <- NULL
# q is the estimate of the position on the illiberal-liberal scale
q <- log((model$illiberal[1]+0.5)/(model$liberal[1]+0.5))
# sigi are sigma values for the first dimension (i =ideological orientation)
# sqrt is the function which computes the square root of a numeric vector
sigi <- sqrt((model$illiberal[1]+0.5)^-1 + (model$liberal[1]+0.5)^-1)
# now for all cases as loop
n <- nrow(model)
for (i in 1:n){
  Illib <- model$illiberal[i]
  Lib <- model$liberal[i]
  q[i] <- log((Lib+0.5)/(Illib+0.5))
  sigi[i] <- sqrt((model$liberal[i]+0.5)^-1 + (model$illiberal[i]+0.5)^-1)
}
# add scale for first dimension and sigma values as columns to model
model$scaleideo <- q
model$sigideo <- sigi
# now, calculate confidence intervals on the x-axis 
# based on the interval formula as suggested by Lowe (2011) 
# [q - 1.96xsigma, q + 1.96xsigma]
model$xneg <- c(model$scaleideo-(1.96*model$sigideo))
model$xpos <- c(model$scaleideo+(1.96*model$sigideo))
# reformate date 
model$date <- rownames(model)
model$date <- gsub("^[A-Za-z]+\\s", "", model$date)
model$date <- format(as.Date(model$date, format='%d %B %Y'))

# now make nice plot with ggplot2 to illustrate scores on scale
ggplot2::theme_set(theme_bw())
plot <- ggplot(model) +
  aes(x = date, y = scaleideo, ymin = xneg, ymax = xpos, group = 1) +  
  geom_point(size = 1, color = "grey27") +  
  geom_line(size = 0.5, color = "grey27") +  
  geom_ribbon(fill = "grey50", alpha = 0.3) +  
  labs(y = "Illiberal vs Liberal Rhetoric", 
       x = "Date of Anthony Albanese's Speeches") +
  ylim(-4, 4) +  # Adjust y-axis range based on your data
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))
plot



#########################
##### 3) LLM example ####
#########################

# GOAL: use the LLM "gpt4o" to have all speeches
# scaled as illiberal vs liberal rhetoric


# load packages 
library(httr)
library(tidyverse)

# transform speech_corpus into dataframe
speeches <- convert(speech_corpus, to = c("data.frame"),)

# Note: code used here was adapted by this blog post: https://rpubs.com/nirmal/setting_chat_gpt_R
# get a ChatGPT API key from here: https://platform.openai.com/overview and provide billing information 
# (otherwise it will not work, but CAUTION, running the code will cost you something!!)

# put your API key here: 
my_API <- ""

# use this "hey_chatGPT" function to access the API and give the task to GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

# make a simple test run first
hey_chatGPT("What is the capital of Australia")

# create a "gpt" column in our speeches data
speeches$gpt <- NA

# use a loop function to ask GPT for classifying each single speech as instructed below
for (i in 1:nrow(speeches)) {
  print(i)
  question <- "TASK:
You are a political scientist. Score paragraphs from a political leaders' speech on an 'illiberal-liberal values' scale, 
a continuous scale ranging from 0 to 6. Illiberal refers to any values which are overly traditionalist, nationalist or paternalist.
Liberal refers to any values which are liberal, in favour of equity between men and women, inclusive and respectful of minorities and their rights.

SCORING METRIC:
6 : extremely illiberal
5 : mostly illiberal
4 : slightly illiberal
3 : neither illiberal nor liberal
2 : slightly liberal
1 : mostly liberal
0 : extremely liberal 

RESPONSE GUIDELINE:
Think carefully about balancing illiberal and liberal criteria for an accurate score."
  
  text <- speeches[i,2]  # in column 2 are all speeches stored     
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  speeches$gpt[i] <- result
}


# Use sapply to iterate over the vector and extract the last number, including decimals
speeches$gpt_score <- sapply(speeches$gpt, function(row_str) {
  # Find all numbers (including decimals) in the string
  numbers <- gregexpr("[0-9]+(\\.[0-9]+)?", row_str)
  last_number <- regmatches(row_str, numbers)[[1]]
  
  # Return the last number found, convert to numeric (if no number is found, return NA)
  if (length(last_number) > 0) {
    as.numeric(tail(last_number, 1))
  } else {
    NA
  }
})

#save(speeches, file = "speeches.RData")
load("speeches.RData")

# check single responses
speeches[185,]

# compare the results to the dictinoray analysis
# for this, we need to flip and rescale so it compares to the dict. model scale
speeches$gpt_score_flipped <- 6 - speeches$gpt_score
speeches$gpt_re <- speeches$gpt_score_flipped - 3
# reformate date 
speeches$date <- gsub("^[A-Za-z]+\\s", "", speeches$date)
speeches$date <- format(as.Date(speeches$date, format='%d %B %Y'))

# now make a similar plot to illustrate scores on scale
ggplot2::theme_set(theme_bw())
plot_gpt <- ggplot(speeches) +
  aes(x = date, y = gpt_re, group = 1) +  
  geom_point(size = 1, color = "grey27") +  
  geom_line(size = 0.5, color = "grey27") +  
  #geom_ribbon(fill = "grey50", alpha = 0.3) +  
  labs(y = "Illiberal vs Liberal Rhetoric (gpt-4o)", 
       x = "Date of Anthony Albanese's Speeches") +
  ylim(-4, 4) +  # Adjust y-axis range based on your data
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))
plot_gpt


# let's discuss implications, ethical concerns, precautions (VALIDATE, VALIDATE, VALIDATE), and potential next steps!
