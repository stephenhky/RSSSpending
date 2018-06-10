
#' Retrieve spending data of a given month from the given workbook.
#' 
#' @param wb Google Spreadsheet workbook
#' @param month Month ('January', 'Feburary' ...)
#' @return data frame of the spending of the given month
#' @importFrom dplyr %>% mutate
#' @importFrom googlesheets gs_read
#' @export
get.spend.data<- function(wb, month) {
  temp_ws<- wb %>% gs_read(ws = month);   max.rowid<- nrow(temp_ws);   rm(temp_ws)
  if (max.rowid>=2) {
    ws<- wb %>% gs_read(ws = month, range = cell_limits(c(2, 2), c(max.rowid+1, 9)))
    max.rowid<- nrow(ws)
  } else {
    max.rowid<- 0
  }
  
  if (max.rowid>2) {
    ws %>% 
      data.frame(stringsAsFactors=FALSE) %>% 
      mutate(Date=as.Date(Date, "%m/%d/%Y"),
             Debit=as.numeric(gsub('\\$|\\,', '', Debit)),
             Comment=ifelse(is.na(Comment), '', Comment))
  } else {
    data.frame(Date=c(), Place=c(), Category=c(), City=c(), Debit=c(), Comment=c(), Individual=c(), Payment.Method=c(),
               stringsAsFactors=FALSE)
  }
}

#' Retrieve spending data of a few given months from the given workbook. Dataframe will be concatenated.
#' 
#' @param wb Google Spreadsheet workbook
#' @param months a vector of months
#' @return concatenated data frame of the spending of the given months
#' @importFrom dplyr %>% bind_rows
#' @export
get.spend.data.months<- function(wb, months) {
  if (length(months)>=1) ws<- get.spend.data(wb, months[1])
  if (length(months)>1) {
    for (month in months[-1]) ws<- ws %>% bind_rows(get.spend.data(wb, month))
  }
  ws
}

#' Retrieve all spend data
#' 
#'@param wb Google Spreadsheet workbook
#'@param pausing.time pausing time in retrieving data, in seconds (default: 6)
#'@return all spending data frame
#'@importFrom dplyr %>% bind_rows
#'@export
get.all.spend.data<- function(wb, pausing.time = 6) {
  # retrieving data
  allspend.data<- data.frame(stringsAsFactors = FALSE)
  for (monthid in 1:12) {
    thismonth.spenddata<- get.spend.data(wb, month.name[monthid])
    Sys.sleep(pausing.time)
    allspend.data<- allspend.data %>% bind_rows(thismonth.spenddata)
  }
  
  allspend.data
}