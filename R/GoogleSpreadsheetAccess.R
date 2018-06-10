
#' Retrieve Google Spreadsheet Workbook.
#' 
#' @param docname name of the spreadsheet
#' @return Google spreadsheet workbook
#' @importFrom googlesheets gs_title
#' @export
access.google.spreadsheet<- function(docname) {
  gs_title(docname)  
}
