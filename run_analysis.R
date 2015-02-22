## @author Salvador Suarez Bojorquez

createTidyData <- function() {
    
    dataDir <- "data-raw/"
    outFileName <- ""
    
    colWidths <- rep(16, 561)
    colWidths[1] <- 17
    
    ## 1. Merges the training and the test sets to create one data set.
    
    x1 <- read.fwf(file=paste(dataDir, "test/X_test.txt", sep=""), widths=colWidths)
    x2 <- read.fwf(file=paste(dataDir, "train/X_train.txt", sep=""), widths=colWidths)
    
    x0 <- rbind(x1, x2)
    
    y1 <- read.fwf(file=paste(dataDir, "test/y_test.txt", sep=""), widths=c(1))
    y2 <- read.fwf(file=paste(dataDir, "train/y_train.txt", sep=""), widths=c(1))
    
    y0 <- rbind(y1, y2)
    
    s1 <- read.table(file=paste(dataDir, "test/subject_test.txt", sep=""), sep=" ")
    s2 <- read.table(file=paste(dataDir, "train/subject_train.txt", sep=""), sep=" ")
    
    s0 <- rbind(s1, s2)
    
    ## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    
    featureNames <- read.table(file=paste(dataDir, "features.txt", sep=""), sep=" ")
    goodNames <- grep("(mean|std)\\(", featureNames[, 2])
    
    xMeasures <- x0[, goodNames]
    
    ## 3. Uses descriptive activity names to name the activities in the data set
    
    activityNames <- read.table(file=paste(dataDir, "activity_labels.txt", sep=""), sep=" ")
    
    yDescriptive <- data.frame(activityNames[y0[[1]], 2])
    
    ## 4. Appropriately labels the data set with descriptive variable names. 
    
    xNames = as.character(featureNames[goodNames, 2])
    colnames(xMeasures) <- xNames
    
    colnames(yDescriptive) <- c("activity")
    
    colnames(s0) <- c("subject")
    
    data0 <- cbind(yDescriptive, s0, xMeasures)
    
    ## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
    ##    of each variable for each activity and each subject.
    
    library(stats)
    dataSummary <- aggregate(data0[, 3:ncol(data0)], list(data0$activity, data0$subject), mean)
    
    dataSummNames <- names(dataSummary)
    dataSummNames <- sapply(dataSummNames, function(name) paste("avg-",as.character(name),sep=""))
    dataSummNames[[1]] <- "activity"
    dataSummNames[[2]] <- "subject"
    colnames(dataSummary) <- dataSummNames
    
    dataSummary
}

tidyData <- createTidyData()