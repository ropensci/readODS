# readODS_test.R
library(testthat)

print(getwd())

# work dir needs to be testthat



test_that("getNrOfSheetsInODS", {
  # getNrOfSheetsInODS
  file=paste("../testdata/test.ods",sep="")
  expect_equal(getNrOfSheetsInODS(file),1)
  file=paste("../testdata/multisheet.ods",sep="")
  expect_equal(getNrOfSheetsInODS(file),4)
  file=paste("../testdata/sum.ods",sep="")
  expect_equal(getNrOfSheetsInODS(file),1)
  file=paste("../testdata/readODStestfilegoogledocscreated.ods",sep="")
  expect_equal(getNrOfSheetsInODS(file),4)
#   expect_equal(getNrOfSheetsInODS(file),3) # CRASH!!!
})


test_that("read.ods", {
  # read.ods
  file=paste("../testdata/sum.ods",sep="") 
  expect_equal(read.ods(file, sheet=1, formulaAsFormula=TRUE)[3,1],"of:=SUM([.A1:.A2])")
  expect_equal(read.ods(file, sheet=1, formulaAsFormula=FALSE)[3,1],"3")
  
  df=data.frame(A=as.character(1:3),stringsAsFactors = F)
  rODS=read.ods(file, sheet=1, formulaAsFormula=FALSE)
  expect_equal(rODS,df)
  
  file=paste("../testdata/lotsofnothing_test.ods",sep="")
  print(dim(read.ods(file, sheet=1)))
  expect_equal(dim(read.ods(file, sheet=1)),c(21,13)) # test if empty rows at the end are ignored
  
  # small file 
  file=paste("../testdata/table.ods",sep="")
  df=data.frame(A=c("gender","m","f","m"), 
                B=c("visit1","4","8","8"),
                C=c("visit2","6","9","2"),
                D=c("visit3","8","4","1"),
                stringsAsFactors = F)
  expect_equal(read.ods(file, sheet=1),df)
  
  file=paste("../testdata/layout_test.ods",sep="")
  sheet1=read.ods(file, sheet=1)
  expect_equal(sheet1[8,"F"],"empty") # this is a repeated element
  
  sheet2=read.ods(file, sheet=2)
  expect_equal(dim(sheet2),c(22,13))
  expect_true(all(sheet1[21,]==sheet2[22,]))
  
  file=paste("../testdata/multisheet.ods",sep="")
  df=data.frame(matrix("",14,7),stringsAsFactors = F)
  df[1,1]="COOKIES"
  df[4,2]="1"
  df[6,3]="2"
  df[8,3]="3"
  df[14,4]="3"
  df[7,5]="3"
  df[9,5]="1"
  df[10,7]="1"
  sheet2=read.ods(file, sheet=2)
  expect_true(all(sheet2==df))
 
  
  
  # this test will fail on windows!!!
#   file=paste("../testdata/test.ods",sep="")
#   sheet1=read.ods(file, sheet=1)
#   expect_equal(sheet1[3,"E"],'lalala lalala,,””,,') 
  
})

print("")
print("test_readODS.R tested!")
