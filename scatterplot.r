# source("~/Documents/cranvas/demos/scatterplot.r")
library(qtpaint)
source("-util.r")

n <- 50000
x <- rnorm(n, 50, 25)
y <- rnorm(n, 50, 25)
df <- data.frame(X = x, Y = y)

size <- 3
alpha <- 1

render_plot <- function(layer, canvas, exposed) {
  circle <- qvPathCircle(0, 0, size)

  qvFillColor(canvas) <- ggplot2::alpha("blue", alpha)
  qvStrokeColor(canvas) <- NA
  qvGlyph(canvas, circle, df[,1], df[,2])
}

handle_keys <- function(event) {
  if (event$key == "up") {
    size <<- size + 1
  } else if (event$key == "down") {
    size <<- max(size - 1, 1)
  } else if (event$key == "left") {
    alpha <<- max(alpha - 0.05, 0.05)
  } else if (event$key == "right") {
    alpha <<- min(alpha + 0.05, 1)
  }
  qvUpdate(scene)
}

scene <- qvScene()
view <- qvView(scene = scene)

points <- qvLayer(scene, render_plot, keyPressFun = handle_keys)
qvSetLimits(points, range(df$X), range(df$X))

print(view)
