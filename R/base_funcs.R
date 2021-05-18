#' Enhanced base saveRDs
#'
#' Handles auto-renaming and overwrite checks. Useful when multiple files are being generated using different parameters. No need to do a lot of sprints's.
#' Also prevents unwanted overwrite. If suffix='auto', it'll number them.
#' @title cutom saveRDS
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#'
#' @param file full/relative path to the file name
#' @param object object to save
#' @param force force replace?
#' @param log a message for log file in __log directory
#' @param ... args passed to saveRDS
#' @param suffix descriptive suffix to add
#'
#' @return file name modified by suffix
#' @family file
#' @rdname basefuncs
#' @export
saveRDS2 <- function(object, file, force=FALSE, suffix=NULL, log=NULL, ...){
  file_sfx <- file
  ## if file already exists and no forcing asked  ---------------------------------------
  if(file.exists(file) & !force){
    ## ensure suffix is valid
    if(!is(try(suffix), 'character')){
      stop(sprintf('You sure you want to overwrite %s?. Use force=TRUE to overwrite.', sQuote(file)), call. = FALSE)
    } else {
      if(suffix=='auto'){ ## if auto suffix asked, number files
        pl <- parent_base_ext(file)
        nf <- length(list.files(path =pl$parent, pattern = pl$base))
        file_sfx <- suffixer(file=file, suffix = sprintf('(%i)', nf))
      } else{
        file_sfx <- suffixer(file=file, suffix = suffix)
      }
    }
  }
  ## if logging asked  ---------------------------------------
  if(try(is(log,'character'))){
    ## check that __log dir exists in parent
    logfile <- sprintf('%s/__log/%s.log', dirname(file), basename(file))
    ## if logfile does not exist
    if(!file.exists(logfile)){
      ## check directory exists first
      if(!dir.exists(dirname(logfile))){
        dir.create(dirname(logfile))
      }
      file.create(logfile)
      cat('initialise log on', date(),'\n\n', file = logfile)
    }
    cat(sprintf('%s: %s \n\n', basename(file_sfx), log), file = logfile, append = TRUE)
  }

  saveRDS(object=object, file=file_sfx,...)
}


#' @title custom png in figure folder
#' @importFrom R.utils filePath
#' @param figdir figure directory
#' @rdname basefuncs
#' @export
png2 <- function(file, fig_dir='figure/',...) png(filePath(fig_dir, paste0(file,'.png')), ...)

#' @title custom pdf in figure folder
#' @importFrom R.utils filePath
#' @param figdir figure directory
#' @rdname basefuncs
#' @export
pdf2 <- function(file, fig_dir='figure/', ...) pdf(filePath(fig_dir, paste0(file,'.pdf')), ...)
