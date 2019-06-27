net_stacked <- function(x=test, YLIM=c(-0.8, 0.2), positives0=c(3, 4), negatives0=1, neutral0=2) {
  
  ## x: a data.frame or list, where each column is a ordered factor with the same levels
  ## lower levels are presumed to be "negative" responses; middle value presumed to be neutral
  ## returns a ggplot2 object of a net stacked distribution plot
  
  ## Test that all elements of x have the same levels, are ordered, etc.
  all_levels <- levels(x[[1]])
  n <- length(all_levels)
  levelscheck <- all(sapply(x, function(y)
    all(c(is.ordered(y), levels(y) == all_levels))
  ))
  if(!levelscheck)
    stop("All levels of x must be ordered factors with the same levels")
  
  ## Reverse order of columns (to make ggplot2 output look right after coord_flip)
  x <- x[length(x):1]
  
  ## Identify middle and "negative" levels
  if(is.null(neutral0)){
  if(n %% 2 == 1){
    neutral <- all_levels[ceiling(n/2)]
  }else{
    neutral <- NULL
  }
  } else {neutral<- all_levels[neutral0]}
  
  if(is.null(negatives0)){
    negatives <- all_levels[1:floor(n/2)]
  } else {negatives <- all_levels[negatives0]}
  
  if(is.null(positives0)){
    positives <- setdiff(all_levels, c(negatives, neutral))
  } else {positives <- all_levels[positives0]}
  
  
  
  ## remove neutral, summarize as proportion
  listall <- lapply(names(x), function(y) {
    column <- (na.omit(x[[y]]))
    out <- data.frame(Question = y, prop.table(table(column)))
    names(out) <- c("Question", "Response", "Freq")
    
    if(!is.null(neutral0)){
      #out <- out[out$Response != neutral,]
    } else {if(is.null(neutral0)){
      out <- out[out$Response != neutral,]
    }
    }
    out
  })
  
  dfall <- do.call(rbind, listall)
  
  ## split by positive/negative
  pos <- dfall[dfall$Response %in% positives,]
  neg <- dfall[dfall$Response %in% negatives,]
  neu <- dfall[dfall$Response %in% neutral,]
  
  
  ## Negate the frequencies of negative responses, reverse order
  neg$Freq <- -neg$Freq
  #neg$Response <- ordered(neg$Response, levels = rev(levels(neg$Response)))
  pos$Response <- ordered(pos$Response, levels = rev(levels(pos$Response)))
  #
  (round(max(neu$Freq), 1))->limneu
  neu.blank<-neu
  neu.blank$Freq<- -1
  #neu$Freq0<-neu$Freq
  #neu$Freq1<- -1
  #neu$Freq1+neu$Freq->neu$Freq
  

  
  stackedchart <- ggplot() +
    aes(Question, Freq, fill = Response, order = Response) + 
    
    #geom_bar(data = neu.blank, stat = "identity", width=, col=gray(level = 0.2)) +
    #geom_bar(data = neu, stat = "identity", width=, col="whith") +
    geom_bar(data = neg, stat = "identity", width=) +
    geom_bar(data = pos, stat = "identity") + geom_hline(yintercept=0) +
    scale_y_continuous(name = "",
                       labels = paste0(abs(seq(-100, 100, 20)), "%"),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, .2)) +
    scale_fill_discrete(limits = c(negatives, positives, neutral)) +
    coord_flip(ylim = YLIM)
  neuchart<- ggplot() +
    aes(Question, Freq) + 
    geom_bar(data = neu, stat = "identity", show.legend = FALSE) +
    #scale_y_continuous(name = "",
    #                   paste0(abs(seq(-100, 100, 20)), "%"),
    #                   limits = c(0, limneu),
    #                   breaks = seq(0, limneu, .1))+
    coord_flip(ylim = c(0, limneu))+
    theme( axis.title.y=element_blank(),
           axis.text.y=element_blank()
           #axis.ticks.y=element_blank()
           )
  
  library("gridExtra")
  library(cowplot)
  #grid.arrange(stackedchart, neuchart, ncol = 2, nrow = 1)
  (max(YLIM)-min(YLIM))/limneu->rapo
  plot_grid(get_legend(stackedchart), ggplot(),
    stackedchart+theme(legend.position="none"), neuchart, nrow = 2, ncol=2, 
    rel_widths = c(rapo+1, 1), rel_heights = c(1, 5)#, align="v")
  )
}