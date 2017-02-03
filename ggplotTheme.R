library(ggplot2)
library(gridExtra)
library(grid)

# Example usage
ggplot(data=mtcars, aes(x=am, y=cyl, colour = cyl, group = cyl)) +
  ylab("Cylinder") +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_point(size = 2) +
  xlab("AM") +
  theme_ubicomp()

theme_ubicomp <- function(base_size = 20) {
  t_bw <- theme_bw(base_size = base_size) #reset to theme_bw + font size
  t_e <- theme(
        text = element_text(size = base_size, family = "Helvetica", colour = "#515151", face = "plain", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = 1, debug = FALSE), #font and font colour
    
        panel.background = element_blank(), panel.border = element_blank(), #remove background and border
        panel.grid.major = element_line(colour = '#bbbbbb', linetype = 'dashed'), #customize look of major gridlines
        panel.grid.minor = element_blank(), #remove minor grid lines
        
        axis.ticks=element_blank(), #remove axis ticks
        axis.text = element_text(size= base_size - 2), #axis text size
        axis.line.x = element_line(colour = "#515151", size = 0.5), #add x axis line
        axis.line.y = element_line(colour = "#515151", size = 0.5), #add y axis line
        axis.title.y = element_text(size = base_size - 2, face = "bold",angle = 90, margin = margin(1,15,1,1)),
        axis.title.x = element_text(size = base_size - 2, face = "bold", margin = margin(15,1,1,1)),
        
        legend.key = element_blank(), #remove legend background
        legend.position	= "bottom", #position legend at bottom
        legend.box = "horizontal", #legend items positioned horizontally
        legend.text = element_text(size = base_size + 1), #legend text size
        legend.title = element_blank(), #remove legend title
        legend.key.size = unit(1.2, "cm"), #increase legend size
        
        strip.background = element_blank() #removes background for facet 
  )
  return(`%+replace%`(t_bw, t_e))
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
  return(combined)
}
