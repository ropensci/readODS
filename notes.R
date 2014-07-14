## http://stackoverflow.com/questions/20671814/non-zero-exit-status-r-3-0-1-xml-and-rcurl
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libxml2-dev
#
## http://www.omegahat.org/RSXML/Tour.pdf
#
#
# 

library(XML)
library(testthat)



file=paste(getwd(),"/tests/testdata/smartSheetTest.ods",sep="")
smartSheet(read.ods(file ,sheet=1))


# this actually works!
testPath=paste(getwd(),"/tests/R/",sep="")
codePath=paste(getwd(),"/R/",sep="")
auto_test(test_path=testPath, code_path=codePath)
test_dir(testPath)# does not work..

file=paste(getwd(),"/tests/testdata/test.ods",sep="")
file=paste(getwd(),"/tests/testdata/multisheet.ods",sep="")
file=paste(getwd(),"/tests/testdata/sum.ods",sep="")
file=paste(getwd(),"/tests/testdata/readODS test file google docs created.ods",sep="")
file=paste(getwd(),"/tests/testdata/lotsofnothing_test.ods",sep="")
file=paste(getwd(),"/tests/testdata/layout_test.ods",sep="")
file=paste(getwd(),"/tests/testdata/table.ods",sep="")
file=paste(getwd(),"/tests/testdata/1996-2000.ods",sep="")


expect_true(all(dim(read.ods(file)[2])==c(36,212)))


read.ods(file)
read.ods(file)
read.ods(file)
read.ods(file,formulaAsFormula = T)
read.ods(file ,sheet=1)



read.ods(file,usePreParser = F)

odsPreParser(file)




df=data.frame(a=1:10,b=1:10,c=1:10)
df[3,]=NULL #does not work
df[3]=NULL #works
df[,3]=NULL #same as the one above... so rows are 
df[3,1:3]=NULL

df


tryCatch(warning("gjstest"),warning=print("gottaloveme!"))# gives error
tryCatch(warning("gjstest"),warning=function(e)print("gottaloveme!")) #works!!
tryCatch(warning("gjstest"),warning=function(e)T)

if(tryCatch(warning("jackpot!"),warning=function(e)T))print("jackpot!")#jackpot!


df=data.frame(a=1:10,b=letters[1:10],c=1:10, stringsAsFactors = F)
tryCatch(as.double(df[,1]),warning=function(e)T)
tryCatch(as.double(df[,2]),warning=function(e)T)


elementName="GJSTTEST"
listOfElements=""
listOfElements=append(listOfElements,elementName)
listOfElements
listOfElements[length(listOfElements)]=NULL
length(listOfElements)


test=read.ods(file ,sheet=1)



#           print(correctedXML[(length(correctedXML)-20):length(correctedXML)])



d=list()
d[10]=list()
d[10][2]="a"
d


d=list()
d[[10]]=list()
d[[10]][[2]]="a"
d

e=data.frame(d)


for(i in 1:10){
  if(i==4)next
  print(i)
}




file=paste(getwd(),"/tests/testdata/test.ods",sep="")
con=unz(file,filename="content.xml")
open(con)
XML=xmlTreeParse(suppressWarnings(readLines(con)))
close(con)

XML




# file=paste(dir,"/content.xml",sep="")
XML=xmlTreeParse(file=paste(getwd(),"/tests/testdata/test.ods:content.xml",sep=""))
file.remove(file)
XML






















# works
file=paste(getwd(),"/tests/testdata/content.xml",sep="")
xmlTreeParse(file)

#does not work!
file=paste(getwd(),"/tests/testdata/test.ods",sep="")
test=unz(file,filename="content.xml")
# xml=readLines(test)
xml=readLines(test)
xml=paste(xml,readLines(test),sep="")
# xml=substr(xml,1,nchar(xml)-38) # remove <?xml version="1.0" encoding="UTF-8"??> # makes it work!

# xml=readLines(test)
# xmlParse(xml,fullNamespaceInfo=TRUE,asText=TRUE)
xmlTreeParse(file=xml, asText=TRUE) # does not work!
# xmlTreeParse(file=xml, asText=TRUE, encoding='<?xml version="1.0" encoding="UTF-8"??>') # does not work!

# tree=htmlTreeParse(file=xml,asText=TRUE) # works !
# xmlInternalTreeParse(file=xml,asText=TRUE) # does not work!
# xmlNativeTreeParse(file=xml,asText=TRUE) # does not work!
tree$children$html




# gsub(pattern='<?xml version =\\\', replace="gjs", x=xml)

# gsub(pattern='<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>', replace="gjs", x=xml)


close(test)
# substring(text=xml,)


# "<?xml version=\"1.0\" encoding=\"UTF-8\"?>""
xmlParse(xml)

tree=xmlTreeParse(file=xmlParse(xml),asText=TRUE)


file=paste(getwd(),"/tests/testdata/test.ods",sep="")
test=unz(file,filename="content.xml")
xmlTreeParse(readLines(test),asText=TRUE)



txt <- "<doc>
          <el> aa </el>
       </doc>" # works
txt="<doc><el>aa</el></doc>"

res <- xmlParse(txt,asText=TRUE)
res.tree <- xmlTreeParse(txt,asText=TRUE)



## xmlTreeParse
file=paste(getwd(),"/tests/testdata/content.xml",sep="")
grep("^(http|ftp|file)://", file, useBytes = TRUE, perl = TRUE)


