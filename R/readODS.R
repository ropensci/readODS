library(XML)
##
# http://www.omegahat.org/RSXML/Tour.pdf
# todo: â€â€ turns into ""  libre office -- windows only....
# probably an encoding thingy... 
# 
#TODO: test gnummeric and koffice generated ODS files
#TODO: check if emtpy rows are the only ones with "number-rows-repeated"...
# ALSO number-columns-repeated -- FIXED
#
# the thing i should have probably read (AKA the ODS standard :P): 
# http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1420324_253892949






#' read.ods
#' 
#' @description 
#' returns a list of data.frames 1 data.frame per sheet
#' 
#' @param file filepath to the ods file
#' @param sheet select the sheet(s) you want, if left empty it will return all sheets in a list, if only 1 number is given it will return the sheet as a data.frame (not as a list of data.frames)
#' @param formulaAsFormula a switch to display formulas as formulas "SUM(A1:A3)" or as the resulting value "3"... or "8".. 
####param usePreParser libre office delivers invalid XML files: <text:p>></text:p>,  this function first fixes the xml before parsing: <text:p>&gt</text:p> this is ofcourse a lot slower..
#' 
#' @details 
#' the data.frame contains all strings (not factors)
#' 
#' @import XML
#' @export
read.ods=function(file=NULL, sheet=NULL, formulaAsFormula=F){
#   read.ods=function(file=NULL, sheet=NULL, formulaAsFormula=F, usePreParser=T){
  root=NULL
#   if(usePreParser){
#     root=odsPreParser(file)
#   }else{
#     root=getODSRoot(file)
#   }
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
#         print(paste("row:",rowIndex," col:",colIndex,sep=""))
        #<table:table-cell table:number-columns-repeated="3"/>
        if(!is.na(xmlAttrs(cell)["number-columns-repeated"]) && length((names(cell)))==0  ){
#           print(as.integer(xmlAttrs(cell)[["number-columns-repeated"]]))
          # repeat empty columns
          colIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
          next
        }
        # display the formula instead of its result
        # so SUM(A1:A3) instead of something like 7... or 11...
        if (formulaAsFormula){
          if(!is.na(xmlAttrs(cell)["formula"])){ #office:formula ... but parser is weird...
            if(length(d)<rowIndex) d[[rowIndex]]=""
            d[[rowIndex]][[colIndex]]=xmlAttrs(cell)[["formula"]]
            next
          }
        }
        
#         print(xmlValue(cell[["p"]]))

        # show numbers instead of formula
        # so SUM(A1:A3) become something like 18... or 5.. 
        if(length(xmlValue(cell[["p"]]))>0){
          # <text:p> anything <text:p/>
          if(length(d)<rowIndex) d[[rowIndex]]="" # create empty row
          if(!is.na(xmlAttrs(cell)["number-columns-repeated"])){
            nextIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
            d[[rowIndex]][colIndex:nextIndex]=xmlValue(cell[["p"]])
            colIndex=nextIndex
          }else{
            d[[rowIndex]][[colIndex]]=xmlValue(cell[["p"]])
          }
        } else {
          # <text:p/>
          # libre office can have a value as an attribute
#           print(xmlAttrs(cell))
          if(!is.na(xmlAttrs(cell)["value"])){ #office:value ... but parser is weird...
            if(length(d)<rowIndex) d[[rowIndex]]=""
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
#' 
#' @description
#' returns the number of sheets in the .ods file
#' @param file path to the .ods file
#' @details
#' use read.ods() to actualy get the sheets
#' @export
getNrOfSheetsInODS = function(file=NULL){ 
  root=getODSRoot(file)
  body=root[["body"]]
  sheets=body[["spreadsheet"]]
  nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  return(nrOfSheets)
} 


#' getODSRoot
#' 
#' @keywords internal 
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
#' @keywords internal
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
#' @keywords internal
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



#' odsPreParser
#' 
#' @keywords internal
#' 
#' WTF!!! XML suddenly started being able to handle these files... or maybe just on windows?
#' 
#' 
#' @description
#' libre office can do crap like this:
#' <text:p>></text:p>
#' which is not valid xml... and the XML package doesn't like that..
#' OK NM NOW IT DOES! leaving the code here in case of changes...
#' 
#' also to not make the code fulgy as all hell, the content.xml file is first scanned for these XML violations, and then fixed!
#' <text:p>></text:p> --> <text:p>&gt</text:p>
#' @param file - the xml file to be parsed...
#' @details
#' also returns an .ods root..
#' 
#' 
odsPreParser = function(file=NULL){
  if(is.null(file)) stop("no filename given")
  if(!file.exists(file)) stop("file does not exist")
  con=unz(file,filename="content.xml")

  open(con)
  lines=suppressWarnings(readLines(con=con))
  close(con)
#   print(lines)
  #
  # pre parse context.XML
  line=lines[1] # first line: <?xml version=\"1.0\" encoding=\"UTF-8\"?>
  correctedXML=line
  correctedXML=paste(correctedXML,"\n",sep="")
  line=lines[2] # 2nd line is the rest:
  sizeOfXML=nchar(line)
  listOfElements=NULL
  
  continue=TRUE
  index=0 # character position in the content.xml file
  while(continue){
    index=index+1
#     print("mainloop")
#     print(index)
    if(substring(line,index,index)=="<"){# for each element
#       print("start reading element")
      # determine element name
      elementName=""
      gotElementName=F
      stopElement=F
      emptyElement=F
      correctedXML=paste(correctedXML,substring(line,index,index),sep="")
      
      while(T){ # inside the element
        index=index+1
#         print(substring(line,index,index))
        if(substring(line,index,index)==" "){
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
          gotElementName=T
        }else if( substring(line,index,index)=='"' || substring(line,index,index)=="'" ){# attribute value
          # <elementName attributeName="attributeValue">
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
          index=index+1
#           while( substring(line,index,index)!='"' || substring(line,index,index)!="'" ){ # does not work...
          while(T){ # loop the attribute value
            correctedXML=paste(correctedXML,substring(line,index,index),sep="")
#             print(substring(line,index,index))
            index=index+1
            
            if(substring(line,index,index)=='"' || substring(line,index,index)=="'"){
#               print("1029384756")
              correctedXML=paste(correctedXML,substring(line,index,index),sep="")
              break
            }
          }
        }else if(substring(line,index,index)=="/"){ # either </elementName> or </elementName>
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
          if(elementName==""){ #</elementName>
            stopElement=T
#             print("stopElement")
          }else{ #<elementName/>
            emptyElement=T
            gotElementName=T
#             print("emptyElement")
          }
        }else if(substring(line,index,index)==">"){
          # end of element 
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
          if(stopElement){
            # remove last element from element list
            if(listOfElements[[length(listOfElements)]]!=elementName){
#               print(length(listOfElements))
              stop(paste("this .ods file is broken beyond repair..., it's element are inconsistent expected: '",listOfElements[[length(listOfElements)]], "' but given: '", elementName, "'", sep=""))
            }
            listOfElements=listOfElements[1:(length(listOfElements)-1)]
#             print(paste("removed ",elementName,sep=""))
          } else {
            # new element
            # check if empty
            if(emptyElement==F){
              # only add an element to the list if it is not
              # <elementName/>
              listOfElements=append(listOfElements,elementName)
#               print(paste("added ", elementName,sep=""))
            } else {
#               print(paste("empty ", elementName,sep=""))
            } 
          }
          #check if it is a cell element
          if(elementName=="text:p" && emptyElement==F){
            #check if </text:p>
            while(T){ # look inside the cell content
              index=index+1
              # 
              # check if end
              if(substring(line,index,index+8)=="</text:p>"){
                index=index+8
                correctedXML=paste(correctedXML,"</text:p>",sep="")
                listOfElements=listOfElements[1:(length(listOfElements)-1)]
                break
              }else if(substring(line,index,index+6)=="</text:"){
                # things like span uper case and other formating thingies
                index=index+6
                while(T){
                  index=index+1
                  if(substring(line,index,index)==">"){
                    # ignore these elements
                    break
                  }
                }
                  
              }else{ 
                # else check if things need to be replaced..
                char=substring(line,index,index)
                if(char=="<"){
                  correctedXML=paste(correctedXML,"&lt;",sep="")
                }else if(char==">"){
                  correctedXML=paste(correctedXML,"&gt;",sep="")
                }else if(char=="&"){
                  correctedXML=paste(correctedXML,"&amp;",sep="")
                }else{ # i could add ' and " but i don't think they will cause trouble
                  # normal char
                  correctedXML=paste(correctedXML,char,sep="")
                }
              }#check for </text:p>
            }#loop over the cell content
          }#/<text:p>
          
          break
        }else if(gotElementName==F){
          elementName=paste(elementName,substring(line,index,index),sep="")
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
        }else{
          # attributeName
          # <elementName, attributeName="attributeValue">
          correctedXML=paste(correctedXML,substring(line,index,index),sep="")
        }
        
      } #/ element contents
        
    }#/element
    else {
      print("!!!!!!!!!!this should not happen!!!!!!!!!!!!")
      print(substring(line,index,index))
    }
    
    # stop conditions
    if(index>=sizeOfXML)continue=FALSE
  }
  
  if(!is.null(listOfElements)){
    print("parsing went wrong, as it reached end of the document but not all elements were closed")
    print("list of unclosed elements")
    print(listOfElements)
  }
    
  # normal parse part
  print("normal parse part!!!")
#   print(correctedXML)
 
#   tempFile=tempfile()
#   print(tempFile)
#   write(correctedXML,file=tempFile)

  XML=xmlTreeParse(file=correctedXML, asText=TRUE)



  return(xmlRoot(XML))
}



