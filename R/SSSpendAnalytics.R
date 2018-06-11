



#' Calculate the summary and output a dataframe
#'
#'@param allspend.data data frame of spend data
#'@return summary worksheet
#'@importFrom dplyr %>% group_by summarise filter rename_ left_join select_ rename arrange mutate_all
#'@importFrom stats setNames
#'@export
generate.summary<- function(allspend.data) {
  # calculation
  allmonth.sum<- allspend.data %>% group_by(Category) %>% summarise(Spending=sum(Debit, na.rm = TRUE))
  for (monthid in 1:12) {
    thismonth.data<- allspend.data %>% 
      filter(months(Date)==month.name[monthid]) %>% 
      group_by(Category) %>% 
      summarize(Spending=sum(Debit, na.rm=TRUE)) %>% 
      rename_(.dots=setNames('Spending', month.name[monthid]))
    allmonth.sum<- allmonth.sum %>% left_join(thismonth.data, by='Category')
  }
  allmonth.sum<- allmonth.sum %>% 
    select_(.dots=as.list(c('Category', month.name, 'Spending'))) %>%   # rearranging columns
    rename(Total=Spending) %>%                                          # rename total
    arrange(-Total) %>%                                                 # sort descendingly
    mutate_all(funs(replace(., is.na(.), 0)))                           # fill all NAs with zero
  
  allmonth.sum
}

#' Update summary
#'
#'@param wb Google Spreadsheet workbook  
#'@param summary.df summary data frame
#'@importFrom dplyr %>%
#'@importFrom googlesheets gs_edit_cells
#'@export
online.update.gs.summary<- function(wb, summary.df) {
  wb %>% gs_edit_cells(ws = 'Summary', input = summary.df, anchor = 'A2')
}
