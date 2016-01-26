# tests for readODS.R
test_that("getNrOfSheetsInODS", {
    file="../testdata/test.ods"
    expect_equal(getNrOfSheetsInODS(file),1)
    file="../testdata/multisheet.ods"
    expect_equal(getNrOfSheetsInODS(file),4)
    file="../testdata/sum.ods"
    expect_equal(getNrOfSheetsInODS(file),1)
    file="../testdata/readODStestfilegoogledocscreated.ods"
    expect_equal(getNrOfSheetsInODS(file),4)
})


test_that("read_ods", {
    file="../testdata/sum.ods"
    expect_equal(read.ods(file, sheet=1, formulaAsFormula=TRUE)[3,1],"of:=SUM([.A1:.A2])")
    expect_equal(read.ods(file, sheet=1, formulaAsFormula=FALSE)[3,1],"3")
  
    df=data.frame(A=as.character(1:3),stringsAsFactors = F)
    rODS=read.ods(file, sheet=1, formulaAsFormula=FALSE)
    expect_equal(rODS,df)
  
    file="../testdata/lotsofnothing_test.ods"
    expect_equal(dim(read.ods(file, sheet=1)),c(21,13)) # test if empty rows at the end are ignored
    expect_equal(class(read.ods(file, sheet=1)),"data.frame")
    ## small file 
    file="../testdata/table.ods"
    df=data.frame(A=c("gender","m","f","m"), 
        B=c("visit1","4","8","8"),
        C=c("visit2","6","9","2"),
        D=c("visit3","8","4","1"),
        stringsAsFactors = F)
    expect_equal(read.ods(file, sheet=1),df)
    
    file="../testdata/layout_test.ods"
    sheet1=read.ods(file, sheet=1)
    expect_equal(sheet1[8,"F"],"empty") # this is a repeated element
    
    sheet2=read.ods(file, sheet=2)
    expect_equal(dim(sheet2),c(22,13))
    expect_true(all(sheet1[21,]==sheet2[22,]))
    
    file=paste("../testdata/multisheet.ods",sep="")
    df=data.frame(matrix(as.character(NA),14,7),stringsAsFactors = F)
    df[1,1]="COOKIES"
    df[4,2]="1"
    df[6,3]="2"
    df[8,3]="3"
    df[14,4]="3"
    df[7,5]="3"
    df[9,5]="1"
    df[10,7]="1"
    sheet2=read.ods(file, sheet=2)
    expect_true(all(sheet2==df, na.rm = TRUE))
 
    file="../testdata/1996-2000.ods"
    expect_true(all(dim(read.ods(file)[[2]])==c(36,21)))
})
