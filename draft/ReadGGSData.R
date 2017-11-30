
library(RSSSpending)

# list
spreadsheets<- gs_ls("SS Expenditure*")

# identify file
filename_search<- 'SS Expenditure (2017) test'
spreadsheet<- spreadsheets %>% filter(sheet_title==filename_search)

# register
ssspend_wb<- access.google.spreadsheet(spreadsheet$sheet_title)
# to browse: ssspend_wb %>% gs_browse()
sheets<- gs_ws_ls(ssspend_wb)
# to read sheet: ssspend_wb %>% gs_read(ws="<name>")

# category normalization
#xwalk<- read.csv('crosswalk.csv', stringsAsFactors = FALSE, header=FALSE)
#xwalk.map<- as.list(xwalk$V2);    names(xwalk.map)<- xwalk$V1;    rm(xwalk)

# get data
jan.data<- get.spend.data(ssspend_wb, 'January')
halfyear.data<- get.spend.data.months(ssspend_wb, month.name[1:6])

# edit summary cells
# https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html
allspend.data<- get.all.spend.data(ssspend_wb, pausing.time = 6)
summary.spreadsheet<- generate.summary(allspend.data)
online.update.gs.summary(ssspend_wb, summary.spreadsheet)

