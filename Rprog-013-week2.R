## Part 1 - PollutantMean

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filesFull <- list.files(directory, full.names = TRUE)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(filesFull[i]))
  }
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  datPol <- dat[, pollutant]
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  mean(datPol, na.rm = TRUE)
}

## Part 2 - Completely Observed Cases

complete <- function(directory, id) {
  ## compile list of all files in a directory
  filesFull <- list.files(directory, full.names = TRUE)
  ## create a data frame with columns for ID and nobs
  dat <- data.frame()
  Vnobs <- vector('numeric')
  Vid <- vector('numeric')
  for (i in id) {
    dat <- read.csv(filesFull[i])
    Vnobs <- c(Vnobs, sum(complete.cases(dat)))
    Vid <- c(Vid, i)
  }
  ## for each file add a value in the id column corresponding to 'id', 
  ## and a value in the 'nobs' column corresponding to the number of rows (nrow) with
  ## complete cases (complete.cases)
  nobsDat <- data.frame(id = Vid, nobs = Vnobs)
  ## Return the data frame
  return(nobsDat)
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}

## Part 3 - Threshold Cutoff Assignment


corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filesFull <- list.files(directory, full.names = TRUE)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  Vcors <- vector('numeric')
  for (f in filesFull) {
    if (threshold < sum(complete.cases(read.csv(f)))) {
      dat <- na.omit(read.csv(f))
      Vcors <- c(Vcors, cor(dat$sulfate, dat$nitrate))
    }
  }
  ## Return a numeric vector of correlations
  return(Vcors)
}
