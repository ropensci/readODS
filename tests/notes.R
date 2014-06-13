## http://stackoverflow.com/questions/20671814/non-zero-exit-status-r-3-0-1-xml-and-rcurl
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libxml2-dev
#
## http://www.omegahat.org/RSXML/Tour.pdf
#

library(XML)


file=paste(getwd(),"/tests/testdata/test.ods",sep="")
file=paste(getwd(),"/tests/testdata/multisheet.ods",sep="")
file=paste(getwd(),"/tests/testdata/sum.ods",sep="")
readODS(file)
readODS(file)
readODS(file)







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


