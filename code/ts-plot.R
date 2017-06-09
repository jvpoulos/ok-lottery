TsPlot <- function(df, main = "") {
  gg.xts <- ggplot(df, aes_string(x = "year", y = "value" )) +
  
  # panel layout
  facet_grid(series~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
               , labeller = label_value) + # label_value is default
  
  # display points
  geom_point(data = subset(df, variable == c("Observed"))
             , aes(colour = factor(cat), shape = factor(cat)), size = 1.2, show.legend = TRUE) +
  
  geom_point(data = subset(df, variable == c("Predicted"))
             , aes(colour = factor(cat), shape = factor(cat)), size = 1.2, show.legend = TRUE) +
  

  # manually select shape of geom_point
  scale_shape_manual(values = c(1:ncol(df))) + 
  
  # line colours
  geom_line(data = subset(df, variable == "Observed"), aes(colour = factor(cat), linetype=factor(variable)), show.legend = TRUE) +
  
  geom_line(data = subset(df, variable == "Predicted"), aes(colour = factor(cat), linetype=factor(variable)), show.legend = TRUE) +
  
  geom_line(data = subset(df, variable == "Pointwise"), aes(colour = factor(cat)), show.legend = FALSE) +
  
  geom_line(data = subset(df, variable == "Cumulative"), aes(colour = factor(cat)), show.legend = FALSE) +
  
  # vertocal line to indicate intervention
  geom_vline(xintercept=c(as.numeric(as.POSIXct("1910-01-01 UTC"))), linetype=4) +
  
  # horizontal line to indicate zero values
  geom_hline(yintercept = 0, size = 0.5, colour = "black") +
  
  # horizontal ticks
  scale_x_datetime(limits=c(as.POSIXct("1873-12-01 06:00:00",tz="UTC"), as.POSIXct("1950-12-01 18:00:00",tz="UTC")),
                   date_breaks="10 years",labels=date_format("%Y"),
                   time_trans(tz="UTC"))+
  
  # main y-axis title
  ylab("") +
  
  # main x-axis title
  xlab("") +
  
  # main chart title
  ggtitle(main)

# legend 

gglegend <- guide_legend(override.aes = list(size = ncol(df)))

gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
  
  # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
  # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
  
  theme( legend.title = element_blank()
         , legend.position = c(0.2,0.7)
         , legend.justification = c(1,0)
         , legend.background = element_rect()
         , axis.title.x=element_blank()
         , axis.ticks.x=element_blank()
         , axis.ticks.y=element_blank()
         , legend.box = "horizontal" # not working?)
  )
return(gg.xts)
}