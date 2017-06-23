TsPlotCensus <- function(df, main = "") {
  library(ggplot2)
  library(scales)
  
  gg.xts <- ggplot(df, aes(x = date)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
   geom_line(data = subset(df, variable == "Observed gini"), aes(y = value, colour = "Observed gini", linetype="Observed gini"), show.legend = TRUE, size=0.5) +
   
   geom_line(data = subset(df, variable == "Predicted gini"), aes(y = value, colour = "Predicted gini", linetype="Predicted gini"), show.legend = TRUE, size=0.5) +
   
   geom_line(data = subset(df, variable == "Pointwise gini"), aes(y = value, colour = "Predicted gini", linetype="Predicted gini"), show.legend = FALSE, size=1) +
   
   geom_line(data = subset(df, variable == "Cumulative gini"), aes(y = value ,colour = "Predicted gini", linetype="Predicted gini"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Observed tenancy"), aes(y = value, colour = "Observed tenancy", linetype="Observed tenancy"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Predicted tenancy"), aes(y = value, colour = "Predicted tenancy", linetype="Predicted tenancy"), show.legend = FALSE, size=0.5) +
    
    geom_line(data = subset(df, variable == "Pointwise tenancy"), aes(y = value, colour = "Predicted tenancy" , linetype="Predicted tenancy"), show.legend = FALSE, size=1) +
    
    geom_line(data = subset(df, variable == "Cumulative tenancy"), aes(y = value ,colour = "Predicted tenancy" , linetype="Predicted tenancy"), show.legend = FALSE, size=1) +
    
  # intervals
   
    geom_ribbon(data = subset(df, variable == "Pointwise gini"), aes(ymin = pointwise.gini.min, ymax=pointwise.gini.max, colour="Predicted gini"), alpha=.2, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Pointwise tenancy"), aes(ymin = pointwise.tenancy.min, ymax=pointwise.tenancy.max, colour="Predicted tenancy"), alpha=.2, show.legend = FALSE) +
   
    geom_ribbon(data = subset(df, variable == "Cumulative gini"), aes(ymin = cumulative.gini.min, ymax=cumulative.gini.max, colour="Predicted gini"), alpha=.2, show.legend = FALSE) +   
    
    geom_ribbon(data = subset(df, variable == "Cumulative tenancy"), aes(ymin = cumulative.tenancy.min, ymax=cumulative.tenancy.max, colour="Predicted tenancy"), alpha=.2, show.legend = FALSE) +   
  
    # vertical line to indicate intervention
  
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1901-01-01 06:00:00",tz="UTC"))), linetype=4) + 
    
  # horizontal line to indicate zero values
    geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
  # y axis percentage labels
    scale_y_continuous(labels = scales::percent) +

  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)

# annotation text
  ann_text <- data.frame(date = c(as.POSIXlt("1896-01-01 EST"), as.POSIXlt("1911-01-01 EST")), value=0.6, 
                         series = factor("Time-series", levels = c("Time-series","Pointwise impact","Cumulative impact")),
                         lab = c("pre-lottery \n (training)", "post-lottery \n (test)"))

# legend 

  gg.xts <- gg.xts +
  
  theme( legend.title = element_blank()
         , legend.background = element_rect()
         , axis.text=element_text(size=12)
         , axis.title.x=element_blank()
         , axis.ticks.x=element_blank()
         , axis.ticks.y=element_blank()
         , legend.text=element_text(size=12, family = "serif")
         , legend.box = "horizontal" # not working?
         , legend.position = "top"
  ) + 
    geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
   scale_colour_manual(name="", values = c("Observed gini" = "#E69F00","Predicted gini" = "#E69F00", "Observed tenancy" = "#56B4E9", "Predicted tenancy" = "#56B4E9"),
                       labels=c("Observed Gini", "Observed tenancy", "Predicted Gini", "Predicted tenancy")) +
   scale_linetype_manual(name="", values = c("Predicted gini" = "dashed","Predicted tenancy" = "dashed", "Observed gini" = "solid", "Observed tenancy" = "solid"),
                         labels=c("Observed Gini", "Observed tenancy", "Predicted Gini", "Predicted tenancy"))  + 
  theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}