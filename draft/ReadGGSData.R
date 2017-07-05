
# loading libraries
library(googlesheets)
library(dplyr)

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

# edit cells
