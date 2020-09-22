##### Week 2 exercises solutions, R portion

#### First mini-exercise
gammafactor <- function(vfrac){ 
  # This function returns the gamma factor in special relativity as a function of speed v that is given in units of light speed
  1/sqrt(1-vfrac^2)
}

Lcontraction <- function(L0, vfrac){
  # This function takes in a real number L_0 in meters for e.g., length, and computes the length L if the object is travelling at v in units of light speed
  L0/gammafactor(vfrac)
}

# Another way is:
Lcontract <- function(L0, vfrac){
  # This function takes in a real number L_0 in meters for e.g., length, and computes the length L if the object is travelling at v in units of light speed
  L0*sqrt(1-vfrac^2) 
}


#### Last exercise
### read in text file B that was saved in the python part

fileB = "noisy_data.csv"

noisydata <- read.csv(file = paste0("../sandbox/", fileB), header=FALSE, col.names = c("xnoisy", "ynoisy"))

head(noisydata)

## plot the data
plot(noisydata)

# make a prettier one
plot(noisydata, xlab="x", ylab="y", col="darkblue", pch=19, cex.lab=1.5)
grid()

## compute the summary statistics
summary(noisydata)

# make a boxplot
boxplot(noisydata)

### data file A
fileA = "data.csv"

# read in the data
mydata <- read.csv(file = paste0("../problems/",fileA), header=FALSE, col.names = c("x", "y"))

# compute the summary statistics
summary(mydata)

# or make it fancier
plot(mydata, xlab="x", ylab="y")
grid()

# make a boxplot of the summary statistics
boxplot(mydata)

# compare the summary statistics
boxplot( cbind(mydata, noisydata))
