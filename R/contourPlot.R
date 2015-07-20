# (c) Kevin Dunn, 2014 - 2015.

# DELETE THESE LINES BEFORE SUBMITTING
rm(list = ls())   
library(ggplot2)
# Run this line if you don't have the "ggplot2" package installed
# install.packages("ggPlot2", dependencies = TRUE)


# For example:  contourPlot(myModel, A, B, main="Contours in factor A and B"))
# For example:  contourPlot(myNextModel, P, Q, main="Contours in factor P and Q"))

contourPlot <- function(lsmodel, horiz, vert, main="Contour plot", 
                        N=25, bound=c(3.2, 3.2)){

  # N <- 25: resolution of surface  (higher values give smoother plots)
  # bound <- 3.2: range of the coded variables to plot on the axes
  H.grid <- seq(-bound[1], bound[2], length=N)
  V.grid <- seq(-bound[1], bound[2], length=N)
  grd <- expand.grid(H.grid, V.grid)
  n <- dim(grd)[1]
  
  # Valid column names are only those that are not the response variable.
  # The response variable is model.frame(lsmodel)[, 1], so ignore that first 
  # column.
  valid.names <- colnames(model.frame(lsmodel))[dim(model.frame(lsmodel))[2]:2]

  if (!(horiz %in% valid.names)){
    stop(paste('The variable "', toString(horiz), '" was not a variable name in the linear model.\n  Valid variable names are: ', toString(valid.names), sep=''))
  }
  if (!(vert %in% valid.names)){
    stop(paste('The variable "', toString(vert), '" was not a variable name in the linear model.\n  Valid variable names are: ', toString(valid.names), sep=''))
  }
  valid.names <- valid.names[valid.names != horiz]
  valid.names <- valid.names[valid.names != vert]
  
  h.points = model.frame(lsmodel)[ ,horiz]
  v.points = model.frame(lsmodel)[ ,vert]
  expt_points <- data.frame(horiz=h.points, vert=v.points)
  colnames(grd) <- c(horiz, vert)
  colnames(expt_points) <- c(horiz, vert)
  grd <- rbind(grd, expt_points)
  n.points.grid <- dim(grd)[1]
  
  # Set any unspecified variables to zero.
  # TODO: improve by allowing these variables to be specified with the ... input 
  # to this function
  for(elem in valid.names){
    grd[[elem]] = 0
  }
  
  # Predict directly from least squares model
  grd$y <- predict(lsmodel, grd)
  binwidth = (max(grd$y) - min(grd$y))/20
  
  p <- ggplot(data=grd[1:n,], aes_string(x=horiz, y=vert, z="y")) + 
    stat_contour(aes(color=..level..), binwidth=binwidth) +
    scale_colour_gradientn(colours=terrain.colors(10)) +
    theme(panel.background=element_rect(fill="white")) +
    theme(panel.grid=element_blank()) +
    theme_bw() +
    theme(plot.title = element_text(size = rel(2))) +
    theme(axis.title = element_text(face="bold", size = rel(1.5))) +
    labs(title=main) + 
    geom_point(data=grd[(n+1):n.points.grid,], aes_string(x=horiz, y=vert), size=5) + 
    scale_x_continuous(breaks = seq(-round(bound[1]), round(bound[2]), by = 1)) + 
    scale_y_continuous(breaks = seq(-round(bound[1]), round(bound[2]), by = 1))
  
  p          # Execute the plot (i.e. draw it!)
  return(p)  # Return the plot, so user can continue to modify it
}
y <- c( 4, 2, 6, 8, 9)
P <- c(  0,  -1,  +1,  -1,  +1)
T <- c(  0,  -1,  -1,  +1,  +1)
mod.base.0 <- lm(y ~ P*T)
#contourPlot(mod.base.0, "P", "T")


Q <- P*T
y <- c(407, 193, 468, 310, 571)
mod.base.1 <- lm(y ~ P*T*Q)
summary(mod.base.1)

#contourPlot(mod.base.1, "A")
#contourPlot(mod.base.1, "P", "B")
contourPlot(mod.base.1, "P", "Q")

