R2Excel(mtcars, file='mtcars.xlsx', sheetName = 'mtcars', title = "Main title", subtitle = "More detailed description")
if(!file.exists('mtcars.xlsx'))
   stop('file was not created')
## remove
unlink('mtcars.xlsx')
