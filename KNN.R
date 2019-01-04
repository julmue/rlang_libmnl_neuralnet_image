library(pixmap)
library(scatterplot3d)
library(plot3D)
library(neuralnet)

set.seed(42)

# Helper functions
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

rotate <- function(x) t(apply(x, 2, rev))

crop <- function(i) {
  v <- 1:300
  i <- i[v + 100, v]
}
  
# image aquisition
marylin <- read.pnm("C:\\Users\\jmueller\\Desktop\\PresentationANN\\pic\\Marylin.pgm")

img <- marylin@grey
img <- normalize(img)
img <- rotate(img)

# -------------------------------------------------------------------------------------------------
# Crop Image

img <- crop(img) #img[v + 100, v]

image(img)
str(img)


# m <- matrix(fixProcesses$Time, ncol = 100, nrow = 50, byrow = FALSE) # old value for complete frame
# m <- matrix(fixProcesses$Time, nrow = 10, ncol =5 ,byrow = TRUE)

# -------------------------------------------------------------------------------------------------
# 3D Landscape Plot

persp3D(
  z = img,
  phi = 75, #15, 
  theta = 20, 
#  bty = "bl2",
   r = 1000
#  main = "Function of System Scan Runtime",
#  sub = "Number of Processes: 2",
#  xlab = "Number of Datasets",
#  ylab = "Hardware Factor",
#  zlab = "Time",
#  border = "black",
#  clab = "Runtime in Hours"
)

# and from above
persp3D(
  z = img,
  phi = 90, #15, 
  theta = 0, 
  #  bty = "bl2",
  r = 1000
  #  main = "Function of System Scan Runtime",
  #  sub = "Number of Processes: 2",
  #  xlab = "Number of Datasets",
  #  ylab = "Hardware Factor",
  #  zlab = "Time",
  #  border = "black",
  #  clab = "Runtime in Hours"
)



# -------------------------------------------------------------------------------------------------
# Generate Sample Data

x <- round(runif(10000, min = 0, max = 300), digits = 0)
y <- round(runif(10000, min = 0, max = 300), digits = 0)
smp <- data.frame(x, y)

smp$z <- apply(smp, 1, function(v) img[v[1],v[2]])
smp$z <- as.numeric(smp$z)


str(smp)

#
smpImg <- matrix(0, nrow = 300, ncol = 300)
apply(smp, 1, function(v) smpImg[v[[1]],v[[2]]] <<- v[[3]] )

## comparison
image(img)
image(smpImg)

# and from above
persp3D(
  z = smpImg,
  phi = 90, #15, 
  theta = 0, 
  #  bty = "bl2",
  r = 1000
  #  main = "Function of System Scan Runtime",
  #  sub = "Number of Processes: 2",
  #  xlab = "Number of Datasets",
  #  ylab = "Hardware Factor",
  #  zlab = "Time",
  #  border = "black",
  #  clab = "Runtime in Hours"
)


# -------------------------------------------------------------------------------------------------
# Split Sample Data

smpImgTrainSize <- floor(0.75 * nrow(smp))
trainIndex <- sample(seq_len(nrow(smp)), size = smpImgTrainSize)

train <- smp[trainIndex, ]
test <- smp[-trainIndex, ]



# -------------------------------------------------------------------------------------------------
# Simple Neural Network 

marylinModel <- neuralnet(
  z ~ x + y, 
  data = train,
  hidden = 50,  # number of hidden layers and neurons per layer c(3,5)
  stepmax = 1e+05)


# -------------------------------------------------------------------------------------------------
# Generate Base Matrix
