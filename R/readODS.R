library(XML)
##
# http://www.omegahat.org/RSXML/Tour.pdf
# TODO add dependencies
# TODO add cheats
# TODO check images and formats and stufff....
# if formula... take value...
# todo: â€â€ turns into ""   libre office version.
# probably an encoding thingy... 
# 


#' readODS
#' 
#' returns a list of data.frames 1 data.frame per sheet
#' 
#' TODO: check if emtpy rows are the only ones with "number-rows-repeated"...
#' ALSO number-columns-repeated
#' 
#' @export
readODS=function(file=NULL, sheet=NULL, includeFormulaValues=TRUE){
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  
  returnValue=list()
  sheetIndex=0
  for(sheet in sheets[names(sheets)=="table"]){
    sheetIndex=sheetIndex+1
    #sheet=sheets[[3]]

    
    
    
#     # determine size
#     #<table:table-column style-name="co1" number-columns-repeated="7" default-cell-style-name="Default"/>
#     nrOfCols=0
#     if(!any(names(xmlAttrs(sheet[["table-column"]])) %in% "number-columns-repeated") ){# bugs out if only 1 col
#       # if that attribute does not exist it could be an empty sheet, or a 1 column sheet.
# #       print("empty sheet")
# #       returnValue[[sheetIndex]]=data.frame()
# #       next
#       nrOfCols=1
#     } else {
#       nrOfCols=as.integer(xmlAttrs(sheet[["table-column"]])[["number-columns-repeated"]])
#     }
#     
#     nrOfRows=sum(names(sheet)=="table-row") # nr of rows in XML
#     for(row in sheet[names(sheet)=="table-row"]){
#       #<table:table-row style-name="ro1" number-rows-repeated="3">
#       if(!is.na(xmlAttrs(row)["number-rows-repeated"])){
#         nrOfRows=nrOfRows+as.integer(xmlAttrs(row)[["number-rows-repeated"]])-1
#       }
#     }
    
    # storage object
    

    d=list()
    # fill it
    rowIndex=0
    for(row in sheet[names(sheet)=="table-row"]){
      rowIndex=rowIndex+1
      if(!is.na(xmlAttrs(row)["number-rows-repeated"])){
        # only on empty rows???
        rowIndex=rowIndex+as.integer(xmlAttrs(row)[["number-rows-repeated"]])-1
        next
      }
#       d[[rowIndex]]=list()
      d[[rowIndex]]=""
      
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
        d[[rowIndex]][[colIndex]]=xmlValue(cell[["p"]])# <text:p>
#         d[rowIndex,colIndex]=xmlValue(cell[["p"]]) # <text:p>
      }# col/cell
#       if (d[[rowIndex]]==list()){
#         print("HERE!!!!!!!!!!!!")
#       }
    }# row
#     print(d)
    # convert it into a data.frame
    nrOfRows=length(d)
    nrOfCols=max(sapply(X=d,FUN=length))
  
#     print(d)

    l=data.frame(matrix(data="",nrow=nrOfRows ,ncol=nrOfCols), stringsAsFactors=F)
    for(i in 1:nrOfRows){
#       print("row")
#       print(i)
#       print(length(d[[i]]))
#       print(d[[i]])
      for(j in 1:length(d[[i]])){
#         print(j)
#         print(d[[i]][[j]])
        if(!is.null(d[[i]][[j]]) && !is.na(d[[i]][[j]])) {
          l[i,j]=d[[i]][[j]]
        }
#         print("here?")
      }
    }    
#     print(l)

    returnValue[[sheetIndex]]=l
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
#   con=unz(file,filename="content.xml", encoding="ANSI") # encoding is ingnored...
#   con=unz(file,filename="content.xml", encoding="LALALALALA") # encoding is ingnored...
  open(con)
  #incomplete final line found on
#   XML=xmlTreeParse((readLines(con)))
  XML=xmlTreeParse((suppressWarnings(readLines(con))))
  close(con)
  return(xmlRoot(XML))
}

