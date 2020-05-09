#' GO enrichment
#'
#'GO terms enrichment for mixOmics output
#'
#' @param spls.obj spls or diablo
#' @param dat markers <- selectVar(spls.obj, comp = comp)[[dat]]$value
#' @param comp PLS component
#' @param algorithm passed to 'runTest``
#' @param trms terms to highlight
#' @param ID "symbol" or "ensembl"
#' @param minEnrich min enrichment
#' @param nodeSize 5-10 is good
#' @param topNodes # of top nodes in GO plot to keep
#' @param mapping "org.Mm.eg.db" or "org.Hs.eg.db" package must be loaded
#' @param subset subset of genes, if NULL signature used
#' @param feas_genes feasible genes, if NULL all matrix colnames used
#' @param col colors for non-matching and matching terms
#' @import topGO
#' @rdname goEnrich
#' @return a GO term enrichment plot
#' @export
GOget <- function(spls.obj, dat, comps=1:2, ID="symbol", nodeSize=10,
                      mapping="org.Mm.eg.db", subset=NULL, feas_genes= NULL){



  ## if feasible genes not supplied
  if(is.null(feas_genes)){
    ## a named vector of markers loadings
    # if(!dat %in% names(spls.obj)){
    #   stop('invalid omic name')
    # }
    feas_genes <- colnames(spls.obj[[dat]])
    feas_genes <- rep(0, length(feas_genes)) %>% set_names(feas_genes)

    for (comp in comps){
      feas_genes[selectVar(spls.obj, comp = comp)$Y$name] <- 1 ## is a named vector
    }

  }

  if(!is.null(subset)){ ## if we want to enrich against a subset only
    if(is.null(spls.obj)) stop('spls object needed when subset is not NULL')
    feas_genes <- feas_genes[subset]
  }

  selection <- function(allScore){ return(allScore > 0)} # function that returns TRUE/FALSE

  allGO2genes <- annFUN.org(whichOnto="BP", feasibleGenes=names(feas_genes), mapping=mapping, ID=ID)

  GOdata <- new("topGOdata",
                ontology="BP",
                allGenes=feas_genes,
                annot=annFUN.GO2genes,
                GO2genes=allGO2genes,
                geneSel=selection,
                nodeSize=nodeSize)

  return(GOdata)
}


#' Enrich from GOdata
#'
#' @param GOdata  GOdata
#' @param algorithm algorithm
#' @param statistic statistic
#' @param topNodes top nodes
#'
#' @return data.frame of terms
#' @import topGO
#' @rdname goEnrich
#' @export
goEnrich<- function(GOdata, algorithm="classic", statistic="ks", topNodes=25){
  ## use rank info, irrelevant here

  results.ks <- runTest(GOdata, algorithm, statistic)
  goEnrichment <- GenTable(GOdata, KS=results.ks, orderBy="KS", topNodes=topNodes)
  goEnrichment <- goEnrichment[,c("GO.ID","Term","KS")]
  # goEnrichment$Term <- gsub(" [a-z]*\\.\\.\\.$", "", goEnrichment$Term)
  # goEnrichment$Term <- gsub("\\.\\.\\.$", "", goEnrichment$Term)
  # goEnrichment$Term <- paste(goEnrichment$GO.ID, goEnrichment$Term, sep=", ")
  goEnrichment <- goEnrichment[!duplicated(goEnrichment$Term),]
  goEnrichment$Term <- factor(goEnrichment$Term, levels=rev(goEnrichment$Term))
  goEnrichment$KS <- as.numeric(goEnrichment$KS)
  return(goEnrichment)
}



#' plot the goEnrich output
#'
#' @param goEnrichment matrix
#' @param trms list of terms
#' @param col colors
#' @param ymax max of y in the plot
#' @param minor minor grid
#' @rdname goEnrich
#' @export
GOplot <- function(goEnrichment, minEnrich=0.5, trms=list(regul='regul', tumor='tum'),
                   col=NULL, ymax=max(-log10(goEnrichment$KS)), minor=0.5, maxEnrich=NULL){


  if(is.null(col)){
    ## different colors for terms list
    cm <-  color.mixo(c(2,1,4,5))
    col <-c('grey40', cm[seq_along(trms)])
  }
  ## first color for no match
  if(length(col)!=(1+length(trms)))
    stop("col of not correct length")


  goEnrichment <- goEnrichment[!is.na(goEnrichment[,3]),]

  if(!is.null(maxEnrich)){
    minKS <- 10^(-maxEnrich)
    goEnrichment <- goEnrichment[goEnrichment$KS>=minKS,]
  }

  ## plot
  ## function to create a vector of colors based on matching to a list of terms
  col_match <- function(goEnrichment, trms,col){
    trm_col <- rep(col[1],  dim(goEnrichment)[1])
    for (i in seq_along(trms)){
      trm <- trms[[i]]
      trm_match <- vector(length = dim(goEnrichment)[1])
      for (tm in trm){
        trm_match <- trm_match| grepl(tm, goEnrichment$Term)
      }
      trm_match <- rev(trm_match)
      trm_col[trm_match] <- col[i+1]
    }
    trm_col
  }

  trm_col <- col_match(goEnrichment, trms, col)
  # highlight <- rev(highlight)

  p <- ggplot(goEnrichment, aes(x=Term, y=-log10(KS))) +
    stat_summary(geom = "bar", fun.y = mean, position = "dodge", fill=trm_col) +
    xlab("") +
    ylab("GO Term Enrichment") +
    scale_y_continuous(breaks = seq(0, ymax, by = minor)) +
    theme_bw() +
    theme(

      legend.position='none',
      legend.background=element_rect(),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      plot.title=element_text(angle=0, size=24, face="bold", vjust=1),
      axis.text.x=element_text(angle=0, size=12, face="bold", hjust=0.8),
      axis.text.y=element_text(angle=0, size=12, face="bold", vjust=0.5, color = trm_col),
      axis.title=element_text(size=12, face="bold", colour = "grey20"),
      legend.key=element_blank(),     #removes the border
      legend.key.size=unit(1, "cm"),      #Sets overall area/size of the legend
      legend.text=element_text(size=18),  #Text size
      title=element_text(size=18)) +
    guides(colour=guide_legend(override.aes=list(size=2.5))) +
    coord_flip()

  return(p)
}
