library(XML)
##
# http://www.omegahat.org/RSXML/Tour.pdf
# TODO add dependencies
# TODO add cheats
# TODO check images and formats and stufff....
# if formula... take value...



#' readODS
#' 
#' returns a list of data.frames 1 data.frame per sheet
#' 
#' TODO: check if emtpy rows are the only ones with "number-rows-repeated"...
#' ALSO number-columns-repeated
#' 
#' @export
readODS=function(file=NULL, sheet=NULL){
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  
  returnValue=list()
  sheetIndex=0
  for(sheet in sheets[names(sheets)=="table"]){
    sheetIndex=sheetIndex+1
    #sheet=sheets[[3]]

    # check if it's not an empty sheet 
    
    # if(!any(names(xmlAttrs(sheet[["table-column"]])) %in% "number-columns-repeated") ){# bugs out if only 1 col
    if(){
      print("empty sheet")
      returnValue[[sheetIndex]]=data.frame()
      next
    }
    
    # determine size
    #<table:table-column style-name="co1" number-columns-repeated="7" default-cell-style-name="Default"/>
    nrOfCols=as.integer(xmlAttrs(sheet[["table-column"]])[["number-columns-repeated"]])
    nrOfRows=sum(names(sheet)=="table-row") # nr of rows in XML
    for(row in sheet[names(sheet)=="table-row"]){
      #<table:table-row style-name="ro1" number-rows-repeated="3">
      if(!is.na(xmlAttrs(row)["number-rows-repeated"])){
        nrOfRows=nrOfRows+as.integer(xmlAttrs(row)[["number-rows-repeated"]])-1
      }
    }
    
    # create the data.frame
    d=data.frame(matrix(data="",nrow=nrOfRows ,ncol=nrOfCols), stringsAsFactors=F)
    
    # fill it
    rowIndex=0
    for(row in sheet[names(sheet)=="table-row"]){
      rowIndex=rowIndex+1
      if(!is.na(xmlAttrs(row)["number-rows-repeated"])){
        # only on empty rows???
        rowIndex=rowIndex+as.integer(xmlAttrs(row)[["number-rows-repeated"]])-1
        next
      }
      colIndex=0
      for(cell in row[names(row)=="table-cell"] ) {# <table:table-cell>
        colIndex=colIndex+1
        if(is.null(xmlAttrs(cell))){ # silly liblre office has: <table:table-cell/>
#           print("STUPID LIBRE OFFICE!!!")
          next
        }
        #<table:table-cell table:number-columns-repeated="3"/>
        if(!is.na(xmlAttrs(cell)["number-columns-repeated"])){
#           print(as.integer(xmlAttrs(cell)[["number-columns-repeated"]]))
          # repeat empty columns
          colIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
          next
        }
        d[rowIndex,colIndex]=xmlValue(cell[["p"]]) # <text:p>
      }# col/cell
    }# row
#     print(d)
    returnValue[[sheetIndex]]=d
  }# sheet
  return (returnValue)
}


#' getNrOfSheets
#'
#'  @export
getNrOfSheetsInODS = function(file=NULL){ 
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  return(nrOfSheets)
} 


#' getODSRoot
#' 
#' internal only!
getODSRoot = function(file=NULL){
  if(is.null(file)) stop("no filename given")
  if(!file.exists(file)) stop("file does not exist")
  
  con=unz(file,filename="content.xml")
  open(con)
  #incomplete final line found on
#   XML=xmlTreeParse((readLines(con)))
  XML=xmlTreeParse((suppressWarnings(readLines(con))))
  close(con)
  return(xmlRoot(XML))
}

