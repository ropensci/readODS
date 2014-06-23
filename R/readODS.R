library(XML)
##
# http://www.omegahat.org/RSXML/Tour.pdf
# todo: â€â€ turns into ""  libre office -- windows only....
# probably an encoding thingy... 
# 
#TODO: test gnummeric and koffice generated ODS files
#TODO: check if emtpy rows are the only ones with "number-rows-repeated"...
# ALSO number-columns-repeated




#' read.ods
#' 
#' @description 
#' returns a list of data.frames 1 data.frame per sheet
#' 
#' @param file filepath to the ods file
#' @param sheet select the sheet(s) you want, if left empty it will return all sheets in a list, if only 1 number is given it will return the sheet as a data.frame (not as a list of data.frames)
#' @param formulaAsFormula a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. 
#' 
#' @details 
#' the data.frame contains all strings (not factors)
#' 
#' @import XML
#' @export
read.ods=function(file=NULL, sheet=NULL, formulaAsFormula=F){
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  
  returnValue=list()
  sheetIndex=0
  for(sheeti in sheets[names(sheets)=="table"]){
    sheetIndex=sheetIndex+1
    #sheeti=sheets[[3]]

    d=list()
    d[1]="" # avoid bug later on if no rows
    # fill it
    rowIndex=0
    for(row in sheeti[names(sheeti)=="table-row"]){
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
          next
        }
        #<table:table-cell table:number-columns-repeated="3"/>
        if(!is.na(xmlAttrs(cell)["number-columns-repeated"])){
#           print(as.integer(xmlAttrs(cell)[["number-columns-repeated"]]))
          # repeat empty columns
          colIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
          next
        }
        # display the formula instead of its result
        # so SUM(A1:A3) instead of something like 7... or 11...
        if (formulaAsFormula){
          if(!is.na(xmlAttrs(cell)["formula"])){ #office:formula ... but parser is weird...
            if(length(d)<=rowIndex) d[[rowIndex]]=""
            d[[rowIndex]][[colIndex]]=xmlAttrs(cell)[["formula"]]
            next
          }
        }
        
#         print(xmlValue(cell[["p"]]))

        # show numbers instead of formula
        # so SUM(A1:A3) become something like 18... or 5.. 
        if(length(xmlValue(cell[["p"]]))>0){
          # <text:p> anything <text:p/>
          if(length(d)<=rowIndex) d[[rowIndex]]=""
          d[[rowIndex]][[colIndex]]=xmlValue(cell[["p"]])
        } else {
          # <text:p/>
          # libre office can have a value as an attribute
#           print(xmlAttrs(cell))
          if(!is.na(xmlAttrs(cell)["value"])){ #office:value ... but parser is weird...
            if(length(d)<=rowIndex) d[[rowIndex]]=""
            d[[rowIndex]][[colIndex]]=xmlAttrs(cell)[["value"]]
          } else {
            # no value... weird... do nothing!
            print(paste("maybe make me a warning... but found no value for defined cell at sheet:",sheetIndex, "row:",rowIndex,"col:",colIndex ,sep=" "))
          }
        }
      }# col/cell
    }# row
    nrOfRows=length(d)
    nrOfCols=max(sapply(X=d,FUN=length))
    l=data.frame(matrix(data="",nrow=nrOfRows ,ncol=nrOfCols), stringsAsFactors=F)
    for(i in 1:nrOfRows){
      for(j in 1:length(d[[i]])){
        if(!is.null(d[[i]][[j]]) && !is.na(d[[i]][[j]])) 
          l[i,j]=d[[i]][[j]]
      }#col
    }#row
    colnames(l)=numberToLetters(1:nrOfCols)
    rownames(l)=1:nrOfRows
    
    returnValue[[sheetIndex]]=l
  }# sheet
  
  # give back what was asked for!
  if (!is.null(sheet)){
    if(length(sheet)>1){
      return(returnValue[sheet])
    } else {
      return(returnValue[[sheet]])
    }
  }
  return (returnValue)
}


#' getNrOfSheets
#' @description
#' returns the number of sheets in the .ods file
#' @param file path to the .ods file
#' @details
#' use readODS() to actualy get the sheets
#' @export
getNrOfSheetsInODS = function(file=NULL){ 
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  return(nrOfSheets)
} 


#' getODSRoot
#' @description
#' returns the XML root for the given .ods file
#' @param file path to the .ods file
#' @details
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



#' numberToLetters
#' 
#' @description
#' converts numbers to microplate row names and Excel & ODS column names
#' 
#' @param listOfNumbers the numbers you want to convert to chars
#' @details
#' 1=A
#' 26=Z
#' 27=ZA
#' 702=ZZ
#' 703=AAA
#' 
#' supports lists of numbers!
#' 
#' numberToLetters(1:1000)
#' 
numberToLetters=function(listOfNumbers=NULL){
  returnValue=NULL
  for(i in 1:length(listOfNumbers)){
    remainder=listOfNumbers[[i]]
    returnLetters=""
    while(T){
      if(remainder==0){
        break
      }
      if(remainder%%26!=0){
        returnLetters=paste(LETTERS[remainder%%26],returnLetters,sep = "")
        remainder=remainder%/%26  
      }else{
        returnLetters=paste("Z",returnLetters,sep = "")
        remainder=(remainder%/%26)-1
      }
    }
    returnValue[[i]]=returnLetters
  }
  return(returnValue)
}

#' lettersToNumber
#' 
#' @description
#' converts microplate row names and Excel & ODS column names into numbers
#' 
#' @param listOfStrings the strings you want to convert to numbers
#' 
#' @details
#' supports lists of chars!
#' ignores case
#' 
#' A=1
#' Z=26
#' ZA=27
#' ZZ=702
#' AAA=703 
#' 
#' lettersToNumber("AAa")
lettersToNumber=function( listOfStrings=NULL){
  total=NULL
  for(i in 1:length(listOfStrings)){ # for each string
    total[i]=0
    for (j in 1:nchar(listOfStrings[i])){ # for each char in the string
      current=match(casefold(substring(listOfStrings[i],j,j)),letters)
      total[i]=total[i]+(current*26^(nchar(listOfStrings[i])-j))
    }
  }
  return(total)
}

