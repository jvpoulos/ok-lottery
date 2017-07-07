TsPlotCensus <- function(df, main = "") {
  library(ggplot2)
  library(scales)
  library(wesanderson)
  
  gg.xts <- ggplot(df, aes(x = date)) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, labeller = label_value) + # label_value is default
    
  theme(strip.text= element_text(size = 12, family = "serif", face='bold')) +
  
  # line colours
    
    # geom_line(data = subset(df, variable == "Observed adjusted gini"), aes(y = value, colour = "Observed adjusted gini", linetype="Observed adjusted gini"), show.legend = TRUE, size=0.3) +
    # 
    # geom_line(data = subset(df, variable == "Predicted adjusted gini"), aes(y = value, colour = "Predicted adjusted gini", linetype="Predicted adjusted gini"), show.legend = TRUE, size=0.3) +
    # 
    # geom_line(data = subset(df, variable == "Pointwise adjusted gini"), aes(y = value, colour = "Predicted adjusted gini", linetype="Predicted adjusted gini"), show.legend = FALSE, size=1) +
    # 
    # geom_line(data = subset(df, variable == "Cumulative adjusted gini"), aes(y = value ,colour = "Predicted adjusted gini", linetype="Predicted adjusted gini"), show.legend = FALSE, size=1) +
    
    
   # geom_line(data = subset(df, variable == "Observed gini"), aes(y = value, colour = "Observed gini", linetype="Observed gini"), show.legend = TRUE, size=0.3) +
   # 
   # geom_line(data = subset(df, variable == "Predicted gini"), aes(y = value, colour = "Predicted gini", linetype="Predicted gini"), show.legend = TRUE, size=0.3) +
   # 
   # geom_line(data = subset(df, variable == "Pointwise gini"), aes(y = value, colour = "Predicted gini", linetype="Predicted gini"), show.legend = FALSE, size=1) +
   # 
   # geom_line(data = subset(df, variable == "Cumulative gini"), aes(y = value ,colour = "Predicted gini", linetype="Predicted gini"), show.legend = FALSE, size=1) +
    
     geom_line(data = subset(df, variable == "Observed tenancy"), aes(y = value, colour = "Observed tenancy", linetype="Observed tenancy"), show.legend = FALSE, size=0.3) +
   
     geom_line(data = subset(df, variable == "Predicted tenancy"), aes(y = value, colour = "Predicted tenancy", linetype="Predicted tenancy"), show.legend = FALSE, size=0.3) +
     
     geom_line(data = subset(df, variable == "Pointwise tenancy"), aes(y = value, colour = "Predicted tenancy" , linetype="Predicted tenancy"), show.legend = FALSE, size=1) +
     
     geom_line(data = subset(df, variable == "Cumulative tenancy"), aes(y = value ,colour = "Predicted tenancy" , linetype="Predicted tenancy"), show.legend = FALSE, size=1) +
    
  # intervals
   
    # geom_ribbon(data = subset(df, variable == "Pointwise adjusted gini"), aes(ymin = pointwise.agini.min, ymax=pointwise.agini.max, colour="Predicted adjusted gini"), alpha=.2, show.legend = FALSE) +
    # 
    # geom_ribbon(data = subset(df, variable == "Cumulative adjusted gini"), aes(ymin = cumulative.agini.min, ymax=cumulative.agini.max, colour="Predicted adjusted gini"), alpha=.2, show.legend = FALSE) +   
    
    # geom_ribbon(data = subset(df, variable == "Pointwise gini"), aes(ymin = pointwise.gini.min, ymax=pointwise.gini.max, colour="Predicted gini"), alpha=.2, show.legend = FALSE) +
    # 
    # geom_ribbon(data = subset(df, variable == "Cumulative gini"), aes(ymin = cumulative.gini.min, ymax=cumulative.gini.max, colour="Predicted gini"), alpha=.2, show.legend = FALSE) +   
    
      geom_ribbon(data = subset(df, variable == "Pointwise tenancy"), aes(ymin = pointwise.tenancy.min, ymax=pointwise.tenancy.max, colour="Predicted tenancy"), alpha=.2, show.legend = FALSE) +
    
    geom_ribbon(data = subset(df, variable == "Cumulative tenancy"), aes(ymin = cumulative.tenancy.min, ymax=cumulative.tenancy.max, colour="Predicted tenancy"), alpha=.2, show.legend = FALSE) +   
  
    # vertical line to indicate intervention
  
    geom_vline(xintercept=c(as.numeric(as.POSIXct("1901-07-31 06:00:00",tz="UTC"))), linetype=4) + 
    
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
  ann_text <- data.frame(date = c(as.POSIXlt("1896-01-01 EST"), as.POSIXlt("1911-01-01 EST")), value=0.52, 
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
     scale_colour_manual(name="", values = c("Observed tenancy" = wes_palette("Darjeeling")[5],"Predicted tenancy" = wes_palette("Darjeeling")[5]),
                         labels=c("Observed tenancy", "Predicted tenancy")) +
     scale_linetype_manual(name="", values = c("Predicted tenancy" = "dashed", "Observed tenancy" = "solid"),
                           labels=c("Observed tenancy", "Predicted tenancy"))  + 
    
    # geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
    # scale_colour_manual(name="", values = c("Observed gini" = wes_palette("Darjeeling")[3],"Predicted gini" = wes_palette("Darjeeling")[3]),
    #                     labels=c("Observed Gini", "Predicted Gini")) +
    # scale_linetype_manual(name="", values = c("Predicted gini" = "dashed", "Observed gini" = "solid"),
    #                       labels=c("Observed Gini", "Predicted Gini"))  +
    # 
    # geom_text(data = ann_text,aes(y = value, label =lab), family="serif", fontface="italic",  size=5) +
    # scale_colour_manual(name="", values = c("Observed adjusted gini" = wes_palette("Darjeeling")[4],"Predicted adjusted gini" = wes_palette("Darjeeling")[4]),
    #                     labels=c("Observed adjusted Gini", "Predicted adjusted Gini")) +
    # scale_linetype_manual(name="", values = c("Predicted adjusted gini" = "dashed", "Observed adjusted gini" = "solid"),
    #                       labels=c("Observed adjusted Gini", "Predicted adjusted Gini"))  + 
  theme(legend.key.width=unit(3,"line")) 
return(gg.xts)
}