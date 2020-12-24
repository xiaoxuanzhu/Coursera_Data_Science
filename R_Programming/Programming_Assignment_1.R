pollutantmean <- function(directory, pollutant, id = 1:332){
     thefiles <- list.files(directory, full.names = TRUE)[id]
     theData <- lapply(thefiles, function(x) read.csv(x)[[pollutant]])
     themean <- mean(unlist(theData), na.rm = TRUE)
     print(themean)
}

complete <- function(directory, id = 1:332){
     for (i in id) {
          theData_i <- read.csv(list.files(directory, full.names = TRUE)[i])
          nobs_i <- nrow(theData_i[complete.cases(theData_i),])
          output <- data.frame(id = i, nobs = nobs_i)
          print(output)
     }
}

complete_v2 <- function(directory, id = 1:332){
     thefiles <- list.files(directory, full.names = TRUE)[id]
     theNOBS <- lapply(thefiles, function(x) nrow(read.csv(x)[complete.cases(read.csv(x)),]))
     output <- data.frame(id, nobs = unlist(theNOBS))
     print(output)
}

corr <- function(directory, threshold = 0){
     thefiles <- list.files(directory, full.names = TRUE)
     theGOOD <- lapply(thefiles, function(x) read.csv(x)[complete.cases(read.csv(x)),])
     theNOBS <- lapply(thefiles, function(x) nrow(read.csv(x)[complete.cases(read.csv(x)),]))
     theGOOD_selected <- theGOOD[which(unlist(theNOBS) > threshold)]
     theCORR <- lapply(theGOOD_selected, function(x) cor(x$sulfate, x$nitrate))
     print(unlist(theCORR))
}

