# source("~/Documents/cranvas/demos/map-zoomer.r", chdir= T)
library(qtpaint)
library(qtbase) # Need for qrect and others
library(ggplot2)

if (!exists("geo")) {
  load("geo.rdata")  
}

df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
df <- df[complete.cases(df), ] 

scatterplot <- function(layer, painter, exposed) {
  qstrokeColor(painter) <- "black" # note: faster to remove stroke
  qfillColor(painter) <- "black"
  if (zoom_level > 10) { 
    circle <- qglyphCircle(2) # couldn't find qpathCircle
    qdrawGlyph(painter, circle, df$x, df$y)    
  } else {
    qdrawPoint(painter, df$x, df$y)    
  }
}

midmean <- function(x) mean(range(x, na.rm = TRUE))
half_range <- function(x) diff(range(x, na.rm = TRUE)) / 2

pos <- c(midmean(df$x), midmean(df$y))
rng <- c(half_range(df$x), half_range(df$y))
zoom_level <- 0


mouse_zoom <- function(level, event) { # added level
  # have to use environment type subsetting with event$pos
  pos <<- c(event$pos()$x(), event$pos()$y())
    if (event$modifiers() == 268435456) { # again the output is integers
    zoom_out()
  } else {
    zoom_in()
  }

}

zoom_in <- function(event, ...) {
  zoom_level <<- zoom_level + 1
  zoom_update()
}

zoom_out <- function() {
  zoom_level <<- zoom_level - 1
  zoom_update()  
}

zoom_update <- function() {
  # couldn't find qlimit(), so accessed points$setLimits
  points$setLimits(qrect(
    c(pos[1] - rng[1] / 1.4 ^ zoom_level, pos[1] + rng[1] / 1.4 ^ zoom_level), 
    c(pos[2] - rng[2] / 1.4 ^ zoom_level, pos[2] + rng[2] / 1.4 ^ zoom_level)
  ))
}


handle_keys <- function(level, event) { # added level
  if (event$key() == 16777235) { # changed to the function and integer output
    pos[2] <<- pos[2] + rng[2] / zoom_level / 8
  } else if (event$key() == 16777237) { # and here
    pos[2] <<- pos[2] - rng[2] / zoom_level / 8
  } else if (event$key() == 16777234) { # and here
    pos[1] <<- pos[1] - rng[1] / zoom_level / 8
  } else if (event$key() == 16777236) { # and here
    pos[1] <<- pos[1] + rng[1] / zoom_level / 8
  }
  zoom_update()
  qupdate(scene)
}  

scene <- Qt$QGraphicsScene() # have to call QGraphicsScene from the Qt object
points <- qlayer(scene, scatterplot, 
  mouseDoubleClickFun = mouse_zoom, keyPressFun = handle_keys) 
zoom_update()

view <- qplotView(scene = scene)
print(view)