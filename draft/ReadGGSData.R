
# loading libraries
library(googlesheets)
library(dplyr)
library(SnowballC)

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
normalize.categories<- function() {
  
}

# edit cells
