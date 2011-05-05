# source("~/Documents/cranvas/demos/scatterplot.r", chdir=T)
source("slider.r")
library(qtpaint)

# creates a dataframe with 100,000 bivariate normal points
n <- 1e5
df <- data.frame(X = rnorm(n, 50, 25), Y = rnorm(n, 50, 25))

# a slider that goes from 0.5 to infinity in 0.5 increments. The 
# starting position is 3
size_slider <- new_slider(0.5, Inf, 0.5, 3)

# a slider that goes from 0.01 to 1 in increments of 0.05. Its 
# starting value is 1. Since we're using it for alpha, we want to 
# start with full, opague points.
alpha_slider <- new_slider(0.01, 1, 0.05, 1)

# This will be a paintFun; it'll tell a layer what to draw.
render_plot <- function(item, painter, exposed) {
  size <- size_slider$val() # current size of the size slider
  alpha <- alpha_slider$val() # current size of the alpha slider
  col <- ggplot2::alpha("grey20", alpha) # defining grey color with alpha
  qantialias(painter) <- FALSE # ?
  
  if (size < 0.5) { # shouldn't be possible: for very small sizes
    qstrokeColor(painter) <- col # strokes will be grey
    
    # will draw points with the painter mapped to locations in the data frame
    qdrawPoint(painter, df[, 1], df[,2]) 
  } else { # for normal sizes
    circle <- qglyphCircle(size) # creates a circle glyph of the current size
    qfillColor(painter) <- col # fills with grey at correct alpha
    qstrokeColor(painter) <- NA # no strokes; saves time
    
    # will draw circle glyphs mapped to locations in the data frame
    qdrawGlyph(painter, circle, df[, 1], df[,2]) 
  }
}

# function to be run when the user presses a key. Seems like its missing the required layer argument. This allows users to use the arrows to adjust, size and alpha.
handle_keys <- function(layer, event, ...) {
  if (event$key() == 16777235) {
    size_slider$inc() # up arrow increases size by one inc
  } else if (event$key() == 16777237) {
    size_slider$dec() # down arrow decreases size by one inc
  } else if (event$key() == 16777236) {
    alpha_slider$dec() # left arrow decreases alpha by one inc
  } else if (event$key() == 16777234) {
    alpha_slider$inc() # right arrow increases alpha by one inc
  }
  
  # redraws the points layer to show the changes
  qupdate(points)
}

# Closing any previously defined views?
if (exists("view")) view$close()

# Creatign a plot. First we make a scene to host the plot, then we add a root layer to the scene as a foundation for the other layers. Finally, we create a view of our plot to display.
scene <- Qt$QGraphicsScene()
root <- qlayer(scene)
view <- qplotView(scene = scene)

# Adding a layer of points to the plot. This layer will draw itself using the render_plot function above, and it has built in keypress interaction. It will be a child layer of the root layer
points <- qlayer(root, render_plot, keyPressFun = handle_keys)

# Setting the limits of the points layer to span the range of the data set
points$setLimits(qrect(range(df$X), range(df$Y)))

# printing our plot
print(view)
