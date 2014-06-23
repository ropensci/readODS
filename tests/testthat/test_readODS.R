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
  
  
})

print("")
print("test_readODS.R tested!")
