## library(XML)
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
## read.ods=function(file=NULL, sheet=NULL, formulaAsFormula=F){
## #   read.ods=function(file=NULL, sheet=NULL, formulaAsFormula=F, usePreParser=T){
##   root=NULL
## #   if(usePreParser){
## #     root=odsPreParser(file)
## #   }else{
## #     root=getODSRoot(file)
## #   }
##   root=getODSRoot(file)
##   body=root[["body"]]
##   sheets=body[["spreadsheet"]]
##   nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
  
##   returnValue=list()
##   sheetIndex=0
##   tempSheets=sheets[names(sheets)=="table"]
##   if(!is.null(sheet)){
##     tempSheets[sheet]
##   }
##   for(sheeti in tempSheets){
##     sheetIndex=sheetIndex+1
##     #sheeti=sheets[[3]]
    
##     d=list()
##     d[1]="" # avoid bug later on if no rows
##     # fill it
##     rowIndex=0
##     for(row in sheeti[names(sheeti)=="table-row"]){
##       rowIndex=rowIndex+1
##       if(!is.na(xmlAttrs(row)["number-rows-repeated"])){
##         # only on empty rows???
##         rowIndex=rowIndex+as.integer(xmlAttrs(row)[["number-rows-repeated"]])-1
##         next
##       }
      
##       colIndex=0
##       for(cell in row[names(row)=="table-cell"] ) {# <table:table-cell>
##         colIndex=colIndex+1
##         if(is.null(xmlAttrs(cell))){ # silly liblre office has: <table:table-cell/>
##           next
##         }
## #         print(paste("row:",rowIndex," col:",colIndex,sep=""))
##         #<table:table-cell table:number-columns-repeated="3"/>
##         if(!is.na(xmlAttrs(cell)["number-columns-repeated"]) && length((names(cell)))==0  ){
## #           print(as.integer(xmlAttrs(cell)[["number-columns-repeated"]]))
##           # repeat empty columns
##           colIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
##           next
##         }
##         # display the formula instead of its result
##         # so SUM(A1:A3) instead of something like 7... or 11...
##         if (formulaAsFormula){
##           if(!is.na(xmlAttrs(cell)["formula"])){ #office:formula ... but parser is weird...
##             if(length(d)<rowIndex) d[[rowIndex]]=""
##             d[[rowIndex]][[colIndex]]=xmlAttrs(cell)[["formula"]]
##             next
##           }
##         }
        
## #         print(xmlValue(cell[["p"]]))

##         # show numbers instead of formula
##         # so SUM(A1:A3) become something like 18... or 5.. 
##         if(length(xmlValue(cell[["p"]]))>0){
##           # <text:p> anything <text:p/>
##           if(length(d)<rowIndex) d[[rowIndex]]="" # create empty row
##           if(!is.na(xmlAttrs(cell)["number-columns-repeated"])){
##             nextIndex=colIndex+as.integer(xmlAttrs(cell)[["number-columns-repeated"]])-1
##             d[[rowIndex]][colIndex:nextIndex]=xmlValue(cell[["p"]])
##             colIndex=nextIndex
##           }else{
##             d[[rowIndex]][[colIndex]]=xmlValue(cell[["p"]])
##           }
##         } else {
##           # <text:p/>
##           # libre office can have a value as an attribute
## #           print(xmlAttrs(cell))
##           if(!is.na(xmlAttrs(cell)["value"])){ #office:value ... but parser is weird...
##             if(length(d)<rowIndex) d[[rowIndex]]=""
##             d[[rowIndex]][[colIndex]]=xmlAttrs(cell)[["value"]]
##           } else {
##             # no value... weird... do nothing!
##             # this can happen if you have code like this: =IF(E13="","",Q13)
## #             print(paste("maybe make me a warning... but found no value for defined cell at sheet:",sheetIndex, "row:",rowIndex,"col:",colIndex ,sep=" "))
##           }
##         }
##       }# col/cell
##     }# row
##     nrOfRows=length(d)
##     nrOfCols=max(sapply(X=d,FUN=length))
##     l=data.frame(matrix(data="",nrow=nrOfRows ,ncol=nrOfCols), stringsAsFactors=F)
##     for(i in 1:nrOfRows){
##       for(j in 1:length(d[[i]])){
##         if(!is.null(d[[i]][[j]]) && !is.na(d[[i]][[j]])) 
##           l[i,j]=d[[i]][[j]]
##       }#col
##     }#row
##     colnames(l)=numberToLetters(1:nrOfCols)
##     rownames(l)=1:nrOfRows
    
##     returnValue[[sheetIndex]]=l
##   }# sheet
  
##   # convert to data.frame if a single sheet was asked for.
##   if (!is.null(sheet) && length(sheet)==1){
##     return(returnValue[[sheet]])
##   }
##   return (returnValue)
## }


## #' getNrOfSheets
## #' 
## #' @description
## #' returns the number of sheets in the .ods file
## #' @param file path to the .ods file
## #' @details
## #' use read.ods() to actualy get the sheets
## #' @export
## getNrOfSheetsInODS = function(file=NULL){ 
##   root=getODSRoot(file)
##   body=root[["body"]]
##   sheets=body[["spreadsheet"]]
##   nrOfSheets=sum(names(sheets)=="table") # <table:named-expressions/> is useless
##   return(nrOfSheets)
## } 


## #' getODSRoot
## #' 
## #' @keywords internal 
## #' @description
## #' returns the XML root for the given .ods file
## #' @param file path to the .ods file
## #' @details
## #' internal only!
## getODSRoot = function(file=NULL){
##   if(is.null(file)) stop("no filename given")
##   if(!file.exists(file)) stop("file does not exist")
##   con=unz(file,filename="content.xml")
## #   con=unz(file,filename="content.xml", encoding="ANSI") # encoding is ingnored...
## #   con=unz(file,filename="content.xml", encoding="LALALALALA") # encoding is ingnored...
##   open(con)
##   #incomplete final line found on
## #   XML=xmlTreeParse((readLines(con)))
##   XML=xmlTreeParse((suppressWarnings(readLines(con))))
##   close(con)
##   return(xmlRoot(XML))
## }


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
numbers_to_letters=function(listOfNumbers=NULL){
  returnValue <- NULL
  for(i in 1:length(listOfNumbers)){
      remainder <- listOfNumbers[[i]]
      returnLetters=""
      while(TRUE){
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


## #' lettersToNumber
## #' 
## #' @keywords internal
## #' @description
## #' converts microplate row names and Excel & ODS column names into numbers
## #' 
## #' @param listOfStrings the strings you want to convert to numbers
## #' 
## #' @details
## #' supports lists of chars!
## #' ignores case
## #' 
## #' A=1
## #' Z=26
## #' ZA=27
## #' ZZ=702
## #' AAA=703 
## #' 
## #' lettersToNumber("AAa")
## lettersToNumber=function( listOfStrings=NULL){
##   total=NULL
##   for(i in 1:length(listOfStrings)){ # for each string
##     total[i]=0
##     for (j in 1:nchar(listOfStrings[i])){ # for each char in the string
##       current=match(casefold(substring(listOfStrings[i],j,j)),letters)
##       total[i]=total[i]+(current*26^(nchar(listOfStrings[i])-j))
##     }
##   }
##   return(total)
## }


require(xml2)

### return a parsed XML tree from an ODS file
parse_ods_file= function(file = NULL){
    if(is.null(file)) {
        stop("no filename given")
    }
    if(!file.exists(file)) {
        stop("file does not exist")
    }
    con <- unz(file,filename="content.xml")
    parsed_ods<- read_xml(con)
    return(parsed_ods)
}

extract_namespace <- function(parsed_ods) {
    ods_ns <- xml_ns(parsed_ods)
    return(ods_ns)
}

parse_sheets <- function(parsed_ods, ods_ns) {
    parsed_sheets <- xml_find_all(parsed_ods, ".//office:body/office:spreadsheet/table:table", ods_ns)
    return(parsed_sheets)
}


check_cell_repeat <- function(cell, ods_ns) {
    if (xml_has_attr(cell, "table:number-columns-repeated", ods_ns)) {
        return(as.numeric(xml_attr(cell, "table:number-columns-repeated", ods_ns)))
    }
    return(1)
}

check_cell_with_textp <- function(cell, ods_ns) {
    return(length(xml_find_all(cell, ".//text:p", ods_ns)) != 0)
}

parse_single_cell <- function(cell, ods_ns, formula_as_formula = FALSE, use_office_value = TRUE) {
    cell_value <- xml_text(xml_find_all(cell, ".//text:p", ods_ns))
    if (cell_value == "" & use_office_value & xml_has_attr(cell, "office:value", ods_ns)) {
        cell_value <- xml_attr(cell, "office:value", ods_ns)
    }
    if (formula_as_formula & xml_has_attr(cell, "table:formula", ods_ns)) {
        cell_value <- xml_attr(cell, "table:formula", ods_ns)
    }
    return(cell_value)
}

parse_rows <- function(parsed_sheet, ods_ns, formula_as_formula) {
    rows <- xml_find_all(parsed_sheet, ".//table:table-row", ods_ns)
    cell_values <- data.frame()
    current_row <- 0
    for (row in rows) {
        current_row <- current_row + 1
        if (xml_has_attr(row, "table:number-rows-repeated", ods_ns)) {
            ## empty row, just bump the current_row
            current_row <- current_row + as.numeric(xml_attr(row, "table:number-rows-repeated", ods_ns)) - 1
        } else {
            ##parse the value in each column
            current_col <- 0
            for (cell in xml_find_all(row, ".//table:table-cell", ods_ns)) {
                bump_cell <- check_cell_repeat(cell, ods_ns)
                cell_with_textp <- check_cell_with_textp(cell, ods_ns)
                current_col <- current_col + 1
                if (cell_with_textp) {
                    ## non_empty cell, get the value
                    cell_value <- parse_single_cell(cell, ods_ns, formula_as_formula = formula_as_formula)
                    cell_values <- rbind(cell_values, data.frame(row_id = current_row, col_id = current_col, cell_value = cell_value, stringsAsFactors = FALSE))
                }
                if (bump_cell > 1 & !cell_with_textp) {
                    current_col <- current_col + bump_cell - 1
                }
                if (bump_cell > 1 & cell_with_textp) {
                    for (bump in 1:(bump_cell-1)) {
                        current_col <- current_col + 1
                        cell_values <- rbind(cell_values, data.frame(row_id = current_row, col_id = current_col, cell_value = cell_value ,stringsAsFactors = FALSE))
                    }
                }
            }
        }
    }
    return(cell_values)

}


### steal from rio
change_df_with_header <- function(x) {
    colnames(x) <- x[1,]
    g <- x[2:nrow(x),]
    rownames(g) <- seq(from = 1, to = nrow(g))
    return(g)
}

### ugly version
to_data_frame <- function(cell_values, header = FALSE) {
    res <- data.frame(matrix(data = "", nrow = max(cell_values[,1]) ,ncol= max(cell_values[,2])), stringsAsFactors = FALSE)
    for(i in 1:nrow(cell_values)){
        res[cell_values[i, 1], cell_values[i, 2]] <- cell_values[i, 3]
    }
    if (header) {
        res <- change_df_with_header(res)
    } else {
        colnames(res) <- numbers_to_letters(1:ncol(res))
    }
    return(res)
}

parse_ods_to_sheets <- function(file) {
    parsed_ods <- parse_ods_file(file)
    ods_ns <- extract_namespace(parsed_ods)
    sheets <- parse_sheets(parsed_ods, ods_ns)
    return(list(sheets, ods_ns))
}

read_ods <- function(file = NULL, sheet = 1, header = FALSE, formula_as_formula = FALSE) {
    res <- parse_ods_to_sheets(file)
    ods_ns <- res[[2]]
    sheets <- res[[1]]
    if (!is.null(sheet)) {
        cell_values <- parse_rows(sheets[sheet], ods_ns, formula_as_formula = formula_as_formula)
        return(to_data_frame(cell_values, header))
    } else {
        return(lapply(sheets, function(x) to_data_frame(parse_rows(x, ods_ns, formula_as_formula = formula_as_formula), header)))
    }
}

read.ods <- function(file=NULL, sheet=NULL, formulaAsFormula=F) {
    return(read_ods(file = file, sheet = sheet, header = FALSE, formula_as_formula = formulaAsFormula))
}

get_num_sheet_in_ods <- function(file) {
    sheets <- parse_ods_to_sheets(file)[[1]]
    return(length(sheets))
}

getNrOfSheetsInODS <- function(file) {
    return(get_num_sheet_in_ods(file))
}
