library(XML)
##
# http://www.omegahat.org/RSXML/Tour.pdf
#

#' readODS
#' @export
readODS=function(file=NULL, sheet=NULL){
  con=unz(file,filename="content.xml")
  open(con)
  XML=xmlTreeParse((readLines(con)))
  close(con)
  root=xmlRoot(XML)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=xmlSize(sheets)-1 # <table:named-expressions/> is useless
  
  returnValue=list()
  for(i in 1:nrOfSheets){
    sheet=sheets[[i]]
    sheet[[1]]
    temp=list()
    sheets$table
  }
  
  
  sheets
  
  xmlName(r)
  xmlSize(r)
  r
  xmlChildren(r$body)
  xmlChildren
  
}

