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
marylin <- read.pnm("C:\\Users\\jmueller\\Desktop\\PresentationANN\\pic\\landscape.pgm")

img <- marylin@grey
img <- normalize(img)
img <- rotate(img)

image(img)
str(img)


# m <- matrix(fixProcesses$Time, ncol = 100, nrow = 50, byrow = FALSE) # old value for complete frame
# m <- matrix(fixProcesses$Time, nrow = 10, ncol =5 ,byrow = TRUE)

# -------------------------------------------------------------------------------------------------
# 3D Landscape Plot

persp3D(
  z = img,
  phi = 50, #15, 
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

x <- round(runif(2000, min = 1, max = 200), digits = 0)   # 2000
y <- round(runif(2000, min = 1, max = 200), digits = 0)   # 2000
smp <- data.frame(x, y)

smp$z <- apply(smp, 1, function(v) img[v[1],v[2]])
smp$z <- as.numeric(smp$z)


str(smp)

#
smpImg <- matrix(0, nrow = 200, ncol = 200)
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

smpImgTrainSize <- floor(1 * nrow(smp))
trainIndex <- sample(seq_len(nrow(smp)), size = smpImgTrainSize)

train <- smp[trainIndex, ]
test <- smp[-trainIndex, ]



# -------------------------------------------------------------------------------------------------
# Simple Neural Network 

set.seed(1234)
marylinModel <- neuralnet(
  z ~ x + y, 
  data = train,
# startweights = c(0.5, length = 35),
#  hidden = 5,  # number of hidden layers and neurons per layer c(3,5), c(5,13,7)
  hidden = c(7,7),
#  algorithm = "rprop+",
#  stepmax = 1e+07,
  stepmax = 10000000,
  rep = 1,
  threshold = 0.1, # 0.02,
  lifesign = "full",
  err.fct = 'ce'
  )

plot(marylinModel)

# -------------------------------------------------------------------------------------------------
# Generate Base Matrix

modelFN <- data.frame( x = rep(1:100, each = 100), y = rep(1:100, 100))

# compute
modelResults <- compute(marylinModel, modelFN)
## vector of the predition results
modelFN$z <- modelResults$net.result

imgANN <- matrix(modelFN$z, nrow = 100, byrow = TRUE) 

image(imgANN)
image(img)



# -----------------------------------------------------------------------------------------
# visualization
persp3D(
  z = imgANN,
  phi = 90, #15, 
  theta = 0, 
  r = 1000
)

persp3D(
  z = img,
  phi = 90, #15, 
  theta = 0, 
  r = 1000
)
