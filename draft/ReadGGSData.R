
# loading libraries
library(googlesheets)
library(dplyr)
library(SnowballC)
library(NLP)
library(openNLP)

# list
spreadsheets<- gs_ls("SS Expenditure*")

# identify file
filename_search<- 'SS Expenditure (2017) test'
spreadsheet<- spreadsheets %>% filter(sheet_title==filename_search)

# register
ssspend_wb<- gs_title(filename_search)
# to browse: ssspend_wb %>% gs_browse()
sheets<- gs_ws_ls(ssspend_wb)
# to read sheet: ssspend_wb %>% gs_read(ws="<name>")

# get spending data
get.spend.data<- function(wb, month) {
  temp_ws<- wb %>% gs_read(ws = month);   max.rowid<- nrow(temp_ws);   rm(temp_ws)
  wb %>% 
    gs_read(ws = month, range = cell_limits(c(2, 2), c(max.rowid, 9))) %>% 
    data.frame(stringsAsFactors=FALSE) %>% 
    mutate(Date=as.Date(Date, "%m/%d/%Y"),
           Debit=as.numeric(gsub('\\$', '', Debit)),
           Comment=ifelse(is.na(Comment), '', Comment))
}

get.spend.data.months<- function(wb, months) {
  if (length(months)>=1) ws<- get.spend.data(wb, months[1])
  if (length(months)>1) {
    for (month in months[-1]) ws<- ws %>% bind_rows(get.spend.data(wb, month))
  }
  ws
}

# natural language processing
# ref: https://stackoverflow.com/questions/30995232/how-to-use-opennlp-to-get-pos-tags-in-r
word_token_annotator<- Maxent_Word_Token_Annotator()
postag_annotator<- Maxent_POS_Tag_Annotator()
tokenize.int<- function(string) {
  s<- as.String(string)
  annobj<- Annotation(1L, 'sentence', 1L, nchar(s))
  annotate(s, word_token_annotator, annobj)
}
tokenize<- function(string) {
  tokentbl<- tokenize.int(string)
  mapply(function(start, end) substr(string, start, end), 
         tokentbl$start[ tokentbl$type=='word'], 
         tokentbl$end[ tokentbl$type=='word'])
}
tag.POS<- function(string) {
  tbl<- annotate(as.String(string), postag_annotator, tokenize.int(as.String(string)))
  unlist(lapply(tbl$features, '[[', 'POS'))
}
stem.all.tokens<- function(string) {
  tokentbl<- tokenize.int(as.String(string))
  mapply(function(start, end) wordStem(substr(string, start, end)), 
         tokentbl$start[ tokentbl$type=='word'], 
         tokentbl$end[ tokentbl$type=='word'])
}

# category normalization
xwalk<- read.csv('crosswalk.csv', stringsAsFactors = FALSE, header=FALSE)
xwalk.map<- as.list(xwalk$V2);    names(xwalk.map)<- xwalk$V1;    rm(xwalk)
categories.tohash<- function(categories) {
  hash<- list()
  for (category in categories) {
    stemmed.category<- tolower(stem.all.tokens(category))
    hash[[stemmed.category]]<- append(hash[[stemmed.category]], category)
  }
  hash
}
score.word<- function(word) {
  poss<- tag.POS(word)
  capitalized<- Reduce(function(a, b) a & b, mapply(function(token) grepl('^[A-Z]', token), tokenize(word)))
  
  # scoring
  score<- 0
  # Rule 1: prefer '-ing' ending
  if ('VBG' %in% poss) score<- score+1
  # Rule 2: prefer capitalized start
  if (capitalized) score<- score+1
  # Rule 3: prefer singular
  if (('NN' %in% poss) | ('NNP' %in% poss)) score<- score+1
  
  score
}
choose.bestword<- function(words) names(which.max(mapply(score.word, words)))
normalize.category<- function(category, all.hash, xwalk.map) {
  stemmed.category<- tolower(stem.all.tokens(category))
  if (stemmed.category %in% names(xwalk.map)) stemmed.category<- xwalk.map[[stemmed.category]]
  if (stemmed.category %in% names(all.hash)) {
    choose.bestword(all.hash[[stemmed.category]])
  } else {
    category
  }
}

# edit cells
