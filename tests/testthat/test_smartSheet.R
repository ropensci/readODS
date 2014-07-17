# tests for smartSheet.R
library(testthat)



test_that("smartSheet", {
  file="../testdata/smartSheetTest.ods"
  s=smartSheet(read.ods(file ,sheet=1))
  df1=data.frame(Test=1:7)
  df1$test2=c("A","1","2","3","B","nextmissing","")
  df1$"5"=rep(5,7)
  df1$moreheader=c("string","me","up","with","a","rope","please")
  rownames(df1)=c("cookies","123","1234","lalalala","rowname5","row6","row7")

  expect_true(all(s==df1))
  expect_true(all(rownames(s)==rownames(df1)))
  expect_true(all(colnames(s)==colnames(df1)))
  
  dfs=smartSheet(read.ods(file))
  expect_true(length(dfs)==7)
  expect_true(all(dfs[[5]]==letters[1:3]))
  expect_true(all(dfs[[6]]==letters[1:3]))
  expect_true(all(dfs[[7]]==letters[1:4]))
  
  
  
  
})
