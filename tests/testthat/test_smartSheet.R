## # tests for smartSheet.R
## library(testthat)



## test_that("smartSheet", {
##   file="../testdata/smartSheetTest.ods"
##   s=smartSheet(read.ods(file ,sheet=1))
##   df1=data.frame(Test=1:7)
##   df1$test2=c("A","1","2","3","B","nextmissing","")
##   df1$"5"=rep(5,7)
##   df1$moreheader=c("string","me","up","with","a","rope","please")
##   rownames(df1)=c("cookies","123","1234","lalalala","rowname5","row6","row7")

##   expect_true(all(s==df1))
##   expect_true(all(rownames(s)==rownames(df1)))
##   expect_true(all(colnames(s)==colnames(df1)))
  
##   dfs=smartSheet(read.ods(file))
##   expect_true(length(dfs)==7)
##   expect_true(all(dfs[[5]]==letters[1:3]))
##   expect_true(all(dfs[[6]]==letters[1:3]))
##   expect_true(all(dfs[[7]]==letters[1:4]))
  
  
  
##   file="../testdata/1996-2000.ods"
##   dfs=read.ods(file)
##   dfs[[1]]
##   s=smartSheet(dfs[[1]],rowNames = T)
##   expect_true(all(s==c(80,5,3,15)))# check that they are numbers, and correct 
##   s=smartSheet(dfs[[1]],rowNames = T,convertToNumbers = F)
##   expect_true(all(s==c("80","5","3","15")))# check that they are numbers, and correct 
  
##   s=smartSheet(dfs[[2]])
##   expect_true(colnames(s)[[1]]=="Team")
##   expect_true(all(dim(s)==c(35,20)))
##   expect_true(class(s[35,20])=="numeric")
##   s=smartSheet(dfs[[2]],colNames = F)
##   expect_true(all(dim(s)==c(36,20)))
##   expect_true(s[36,20]==0)#EEEEUGH this should be false cause its a "0"!!! FUCK R!
##   expect_true(class(s[36,20])=="character")
  
##   s=smartSheet(dfs[[2]],rowNames = T)
##   expect_true(rownames(s)[1]=="AH")
  
##   s=smartSheet(dfs[[2]],rowNames = T, colNames=F)
##   expect_true(rownames(s)[1]=="Team")  
## })
