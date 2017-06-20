TsPlotCensus <- function(df, main = "") {
  library(ggplot2)
  library(scales)
  
  gg.xts <- ggplot(df, aes(x = date)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
   geom_line(data = subset(df, variable == "Observed gini"), aes(y = value, colour = "Observed gini", linetype="Observed gini"), show.legend = TRUE) +
   
   geom_line(data = subset(df, variable == "Predicted gini"), aes(y = value, colour = "Predicted gini", linetype="Predicted gini"), show.legend = TRUE) +
   
   geom_line(data = subset(df, variable == "Pointwise gini"), aes(y = value, colour = "Predicted gini"), show.legend = FALSE) +
   
   geom_line(data = subset(df, variable == "Cumulative gini"), aes(y = value ,colour = "Predicted gini"), show.legend = FALSE) +
    
    geom_line(data = subset(df, variable == "Observed tenancy"), aes(y = value, colour = "Observed tenancy", linetype="Observed tenancy"), show.legend = FALSE) +
    
    geom_line(data = subset(df, variable == "Predicted tenancy"), aes(y = value, colour = "Predicted tenancy", linetype="Predicted tenancy"), show.legend = FALSE) +
    
    geom_line(data = subset(df, variable == "Pointwise tenancy"), aes(y = value, colour = "Predicted tenancy"), show.legend = FALSE) +
    
    geom_line(data = subset(df, variable == "Cumulative tenancy"), aes(y = value ,colour = "Predicted tenancy"), show.legend = FALSE) +
    
  # intervals
   
  # geom_ribbon(data = subset(df, variable == "Predicted"), aes(ymin = pred.min, ymax=pred.max, fill = "Predicted", color="Predicted"), alpha = .1) +
    
  #  geom_ribbon(data = subset(df, variable == "Pointwise"), aes(ymin = pointwise.min, ymax=pointwise.max, color="Predicted"), alpha = .1) +
   
    geom_ribbon(data = subset(df, variable == "Cumulative gini"), aes(ymin = cumulative.gini.min, ymax=cumulative.gini.max, colour="Predicted gini"), alpha=.2, show.legend = FALSE) +   
    
    geom_ribbon(data = subset(df, variable == "Cumulative tenancy"), aes(ymin = cumulative.tenancy.min, ymax=cumulative.tenancy.max, colour="Predicted tenancy"), alpha=.2, show.legend = FALSE) +   
  
    # vertical line to indicate intervention
  
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1901-01-01 06:00:00",tz="UTC"))), linetype=4) + 
    
  # horizontal line to indicate zero values
  geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
 # horizontal ticks
  # scale_x_datetime(limits=c(as.POSIXct("1891-08-01 06:00:00",tz="UTC"), as.POSIXct("1931-08-01 18:00:00",tz="UTC")),
  #                   date_breaks="3 years",labels=date_format("%Y"),
  #                   time_trans(tz="UTC"))+
  #   
  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)

# annotation text
  ann_text <- data.frame(date = c(as.POSIXlt("1896-01-01 EST"), as.POSIXlt("1911-01-01 EST")), value=0.9, 
                         series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-lottery \n (training)", "post-lottery \n (test)"))

# legend 

  gg.xts <- gg.xts +
  
  theme( legend.title = element_blank()
         , legend.position = c(0.90,0.8)
         , legend.justification = c(1,0)
         , legend.background = element_rect()
         , axis.text=element_text(size=12)
         , axis.title.x=element_blank()
         , axis.ticks.x=element_blank()
         , axis.ticks.y=element_blank()
         , legend.text=element_text(size=12, family = "serif")
         , legend.box = "horizontal" # not working?)
  ) + geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
   scale_colour_manual(name="Legend", values = c("Observed gini" = "#E69F00","Predicted gini" = "#E69F00", "Observed tenancy" = "#56B4E9", "Predicted tenancy" = "#56B4E9")) +
   scale_linetype_manual(name="Legend", values = c("Predicted gini" = "dashed","Predicted tenancy" = "dashed", "Observed gini" = "solid", "Observed tenancy" = "solid"))  + 
  theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}