#Source https://stackoverflow.com/questions/14817006/ggplot-version-of-charts-performancesummary

# advanced charts.PerforanceSummary based on ggplot
gg.charts.PerformanceSummary <- function(rtn.obj, main = "", plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  # clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
  #   univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
  #   univ.rtn.xts.obj
  # }
  
  # # Create cumulative return function
  # cum.rtn <- function(clean.xts.obj, g = TRUE)
  # {
  #   x <- clean.xts.obj
  #   if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
  #   y
  # }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj)
  {
    #x <- clean.rtn.xts(xts.obj)
    x <- xts.obj
    series.name <- colnames(xts.obj)[1]
    tmp <- x
    tmp$rtn <- x
    colnames(tmp) <- c("Timeseries","Pointwise") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$year <- round(as.POSIXct(index(tmp), tz="UTC"), "months")
    tmp.df.long <- melt(tmp.df,id.var="year")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "year", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Timeseries")) +
      geom_line(data = subset(df, variable == "Pointwise")) +
      geom_line(data = subset(df, variable == "Cumulative")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("10 years"), labels = date_format("%Y")) +
      ylab("") +
      xlab("")
    
  } 
  else 
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x])}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "year", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                   , labeller = label_value) + # label_value is default

      # display points
      geom_point(data = subset(df, variable == c("Timeseries"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) +
      
      geom_point(data = subset(df, variable == c("Pointwise"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) +
      
      geom_point(data = subset(df, variable == c("Cumulative"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) +
      
      # manually select shape of geom_point
      scale_shape_manual(values = c(1:ncol(df))) + 
      
      # line colours
      geom_line(data = subset(df, variable == "Timeseries"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      geom_line(data = subset(df, variable == "Pointwise"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      geom_line(data = subset(df, variable == "Cumulative"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # vertocal line to indicate zero values
      geom_vline(xintercept=c(as.numeric(as.POSIXct("1910-12-31 UTC"))), linetype=4) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(limits=c(as.POSIXct("1873-12-31 06:00:00",tz="UTC"), as.POSIXct("1943-12-31 18:00:00",tz="UTC")),
                       date_breaks="10 years",labels=date_format("%Y"),
                       time_trans(tz="UTC"))+
  
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend 
    
    gglegend <- guide_legend(override.aes = list(size = ncol(df)))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect()
             , legend.box = "horizontal" # not working?
             , axis.text.x = element_text(angle = 0, hjust = 1)
      )
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}
