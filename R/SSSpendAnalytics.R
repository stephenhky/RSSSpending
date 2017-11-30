
#' Calculate the summary and output a dataframe
#' 
#'@export
generate.summary<- function(wb) {
  allspend.data<- get.spend.data.months(wb, c('January', 'February', 'March',
                                              'April', 'May', 'June', 
                                              'July', 'August', 'September',
                                              'October', 'November', 'December'))
}

