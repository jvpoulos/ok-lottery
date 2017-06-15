TsPlot <- function(df, main = "") {
  library(ggplot2)
  library(scales)
  
  gg.xts <- ggplot(df, aes(x = date)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
  
  # display points
  # geom_point(data = subset(df, variable == c("Observed"))
  #            , aes(colour = factor(cat), shape = factor(variable)), size = 1.2, show.legend = TRUE) +
  # 
  # geom_point(data = subset(df, variable == c("Predicted"))
  #            , aes(colour = factor(cat), shape = factor(variable)), size = 1.2, show.legend = TRUE) +
  

  # manually select shape of geom_point
  #scale_shape_manual(values = c(1:ncol(df))) + 
  
  # line colours
   geom_line(data = subset(df, variable == "Observed"), aes(y = value, colour = factor(cat), linetype="Observed"), show.legend = TRUE) +
   
   geom_line(data = subset(df, variable == "Predicted"), aes(y = value, colour = factor(cat), linetype="Predicted"), show.legend = TRUE) +
   
   geom_line(data = subset(df, variable == "Pointwise"), aes(y = value, colour = factor(cat), linetype="Predicted"), show.legend = TRUE) +
   
   geom_line(data = subset(df, variable == "Cumulative"), aes(y = value ,colour = factor(cat), linetype="Predicted"), show.legend = TRUE) +
    
  # intervals
   
    #geom_ribbon(data = subset(df, variable == "Predicted"), aes(ymin = pred.min, ymax=pred.max, fill = factor(cat), color=factor(cat)), alpha = .1) +
    
  # geom_ribbon(data = subset(df, variable == "Pointwise"), aes(ymin = pointwise.min, ymax=pointwise.max, fill = factor(cat), color=factor(cat)), alpha = .1) +
   
   geom_ribbon(data = subset(df, variable == "Cumulative"), aes(ymin = cumulative.min, ymax=cumulative.max, fill = factor(cat), color=factor(cat)), alpha = .1) +   
  
    # vertical line to indicate intervention
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1889-04-22 UTC"))), linetype=4) + #begin val
  
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1906-12-31 UTC"))), linetype=4) + #end val
    
  # horizontal line to indicate zero values
  geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
  #horizontal ticks
    scale_x_datetime(limits=c(as.POSIXct("1850-12-01 06:00:00",tz="UTC"), as.POSIXct("1950-12-01 18:00:00",tz="UTC")),
                      date_breaks="10 years",labels=date_format("%Y"),
                      time_trans(tz="UTC"))+
    
  # horizontal ticks
  # scale_x_datetime(limits=c(as.POSIXct("1873-12-01 06:00:00",tz="UTC"), as.POSIXct("1950-12-01 18:00:00",tz="UTC")),
  #                  date_breaks="10 years",labels=date_format("%Y"),
  #                  time_trans(tz="UTC"))+

  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)

# annotation text
  ann_text <- data.frame(date = c(as.POSIXlt("1875-01-01 EST"),as.POSIXlt("1900-01-01 EST"),as.POSIXlt("1925-01-01 EST")), value=125, 
                         series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-reform \n (training)", "reform \n (validation)","post-reform \n (test)" ))

# legend 

gglegend <- guide_legend(override.aes = list(size = ncol(df)))

gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) +
  
  # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
  # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
  
  theme( legend.title = element_blank()
         , legend.position = c(0.35,0.05)
         , legend.justification = c(1,0)
         , legend.background = element_rect()
         , axis.title.x=element_blank()
         , axis.ticks.x=element_blank()
         , axis.ticks.y=element_blank()
         , legend.box = "horizontal" # not working?)
  ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=4)
return(gg.xts)
}