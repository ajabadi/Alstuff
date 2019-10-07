#' Write R outputs to Excel file
#'
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title Data frame to excel file converter
#' @param dataf data.frame, with rownames and column names
#' @param file character, relative path to the output file name
#' @param sheetName character, sheet name to create
#' @param sheetName logical, whether to replace possible existing sheet
#' @return Creates a new sheet in the workbook
#' @import xlsx
#' @export
#' @references \url{http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r}
#' @keywords excel
#' @family file
#' @example ./examples/R2Excel-example.R

R2Excel <- function(dataf, file, sheetName, title, subtitle, overwriteSheet=TRUE){

  if(!file.exists(file)){
    ## create a new workbook for outputs
    ## ++++++++++++++++++++++++++++++++++++
    ## possible values for type are : "xls" and "xlsx"
    wb<-createWorkbook(type="xlsx")
  } else {
    wb <- loadWorkbook(file)
  }
  ## Define some cell styles
  ## ++++++++++++++++++++++++++++++++++++
  ## Title and sub title styles
  TITLE_STYLE <- CellStyle(wb)+
    Font(wb,  heightInPoints=16, color="blue", isBold=TRUE, underline=1)
  # +  Fill(backgroundColor="orange", foregroundColor="orange", pattern="SOLID_FOREGROUND")

  SUB_TITLE_STYLE <- CellStyle(wb) +
    Font(wb,  heightInPoints=14,
         isItalic=TRUE, isBold=FALSE)

  ## Styles for the data table row/column names
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Fill(backgroundColor="yellow",  pattern="SOLID_FOREGROUND")

  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK")) +
    Fill(backgroundColor="orange", foregroundColor="orange", pattern="SOLID_FOREGROUND")

  # ----- Failed attempt to add style to body in alternating rows
  TABLE_CONTENT_STYLE_1 <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Fill(backgroundColor="grey70", foregroundColor="grey70", pattern="SOLID_FOREGROUND")

  TABLE_CONTENT_STYLE_2 <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Fill(backgroundColor="grey40", foregroundColor="grey40", pattern="SOLID_FOREGROUND")

  ## Create a new sheet in the workbook
  ## ++++++++++++++++++++++++++++++++++++
  if(overwriteSheet & (sheetName %in% names(getSheets(wb)))) removeSheet(wb, sheetName = sheetName)

  sheet <- createSheet(wb, sheetName = sheetName)
  ## ++++++++++++++++++++++++
  ## Helper function to add titles
  ## ++++++++++++++++++++++++
  ## - sheet : sheet object to contain the title
  ## - rowIndex : numeric value indicating the row to
  #contain the title
  ## - title : the text to use as title
  ## - titleStyle : style object to use for title
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  ## Add title and sub title into a worksheet
  ## ++++++++++++++++++++++++++++++++++++
  ## Add title
  xlsx.addTitle(sheet, rowIndex=1, title=title,
                titleStyle = TITLE_STYLE)
  ## Add sub title
  xlsx.addTitle(sheet, rowIndex=2,
                title=subtitle,
                titleStyle = SUB_TITLE_STYLE)
  ## Add the summary from notebook into the worksheet
  ## ++++++++++++++++++++++++++++++++++++

  # CONT_STYLE <- as.list(rep(TABLE_CONTENT_STYLE_1, dim(dataf)[1]))
  addDataFrame(dataf, sheet, startRow=4, startColumn=1,
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  ## Change column width
  setColumnWidth(sheet, colIndex=c(1:ncol(dataf)), colWidth=16)
  ## Add a plot into a worksheet
  ## ++++++++++++++++++++++++++++++++++++
  ## create a png plot
  # png("boxplot.png", height=800, width=800, res=250, pointsize=8)
  # boxplot(count ~ spray, data = InsectSprays,
  #         col = "blue")
  # dev.off()
  # ## Create a new sheet to contain the plot
  # sheet <-createSheet(wb, sheetName = "boxplot")
  # ## Add title
  # xlsx.addTitle(sheet, rowIndex=1, title="Box plot using InsectSprays data",
  #               titleStyle = TITLE_STYLE)
  # ## Add the plot created previously
  # addPicture("boxplot.png", sheet, scale = 1, startRow = 4,
  #            startColumn = 1)
  # ## remove the plot from the disk
  # res<-file.remove("boxplot.png")
  ## Save the workbook to a file...
  ## ++++++++++++++++++++++++++++++++++++
  saveWorkbook(wb, file = file)
}
