
#' Retrieve spending data of a given month from the given workbook.
#' 
#' @param wb Google Spreadsheet workbook
#' @param month Month ('January', 'Feburary' ...)
#' @return data frame of the spending of the given month
#' @export
get.spend.data<- function(wb, month) {
  temp_ws<- wb %>% gs_read(ws = month);   max.rowid<- nrow(temp_ws);   rm(temp_ws)
  wb %>% 
    gs_read(ws = month, range = cell_limits(c(2, 2), c(max.rowid, 9))) %>% 
    data.frame(stringsAsFactors=FALSE) %>% 
    mutate(Date=as.Date(Date, "%m/%d/%Y"),
           Debit=as.numeric(gsub('\\$', '', Debit)),
           Comment=ifelse(is.na(Comment), '', Comment))
}

#' Retrieve spending data of a few given months from the given workbook. Dataframe will be concatenated.
#' 
#' @param wb Google Spreadsheet workbook
#' @param months a vector of months
#' @return concatenated data frame of the spending of the given months
#' @export
get.spend.data.months<- function(wb, months) {
  if (length(months)>=1) ws<- get.spend.data(wb, months[1])
  if (length(months)>1) {
    for (month in months[-1]) ws<- ws %>% bind_rows(get.spend.data(wb, month))
  }
  ws
}