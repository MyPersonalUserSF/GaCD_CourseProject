rm(list = ls())
### Coursera Getting and Cleaning Data -- Course Project
#
# Read the training datasets: Order them together into 1
train.c<-file("UCI HAR Dataset/train/X_train.txt")
open(train.c)
train.X <- read.table(train.c)
close(train.c)
train.c<-file("UCI HAR Dataset/train/y_train.txt")
open(train.c)
train.y <- read.table(train.c)
close(train.c)
train.c<-file("UCI HAR Dataset/train/subject_train.txt")
open(train.c)
train.subject <- read.table(train.c)
close(train.c)
train.A <- cbind(train.subject, train.y, train.X)
 # Conserve space
rm("train.subject", "train.y", "train.X", "train.c")

# Do the same for the test datasets: Order them together into 1
test.c<-file("UCI HAR Dataset/test/X_test.txt")
open(test.c)
test.X <- read.table(test.c)
close(test.c)
test.c<-file("UCI HAR Dataset/test/y_test.txt")
open(test.c)
test.y <- read.table(test.c)
close(test.c)
test.c<-file("UCI HAR Dataset/test/subject_test.txt")
open(test.c)
test.subject <- read.table(test.c)
close(test.c)
test.A <- cbind(test.subject, test.y, test.X)
# Conserve space
rm("test.subject", "test.y", "test.X", "test.c")

# Bring the training and test data together
AllData <- rbind(train.A, test.A)
# Conserve space
rm("train.A", "test.A")

# Change colunmn names
#  First set Subject and Activity, then loop through the Feature values 
#  and match to columns 3:N, setting names to lowercase (tidy)
colnames(AllData)[1:2] <- c("subject", "activity")
f <- read.table("UCI HAR Dataset/features.txt", colClasses = c("integer", "character"))
for (i in 1:nrow(f)) {
	colnames(AllData)[i+2] <- tolower(f[i, 2])
	}

# Use actual activity names rather than codes
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
for (i in 1:nrow(activities)) {
	aI <- AllData$activity == activities[i, 1]
	# Prevent error if there are no rows with a particular activity code
	if (length(AllData[aI,]$activity) > 1) {
		AllData[aI,]$activity <- as.character(activities[i, 2])
		}
	}

AllmsData <- AllData[,grepl("subject|activity|mean|std", colnames(AllData))]
# Conserve space
rm("AllData")

# Create the tidy dataset as average of all variables grouped by subject and activity
library(sqldf)
tidy <- sqldf('select subject, activity, avg("tbodyacc-mean()-x"), avg("tbodyacc-mean()-y"), avg("tbodyacc-mean()-z"), avg("tbodyacc-std()-x"), avg("tbodyacc-std()-y"), avg("tbodyacc-std()-z"), avg("tgravityacc-mean()-x"), avg("tgravityacc-mean()-y"), avg("tgravityacc-mean()-z"), avg("tgravityacc-std()-x"), avg("tgravityacc-std()-y"), avg("tgravityacc-std()-z"), avg("tbodyaccjerk-mean()-x"), avg("tbodyaccjerk-mean()-y"), avg("tbodyaccjerk-mean()-z"), avg("tbodyaccjerk-std()-x"), avg("tbodyaccjerk-std()-y"), avg("tbodyaccjerk-std()-z"), avg("tbodygyro-mean()-x"), avg("tbodygyro-mean()-y"), avg("tbodygyro-mean()-z"), avg("tbodygyro-std()-x"), avg("tbodygyro-std()-y"), avg("tbodygyro-std()-z"), avg("tbodygyrojerk-mean()-x"), avg("tbodygyrojerk-mean()-y"), avg("tbodygyrojerk-mean()-z"), avg("tbodygyrojerk-std()-x"), avg("tbodygyrojerk-std()-y"), avg("tbodygyrojerk-std()-z"), avg("tbodyaccmag-mean()"), avg("tbodyaccmag-std()"), avg("tgravityaccmag-mean()"), avg("tgravityaccmag-std()"), avg("tbodyaccjerkmag-mean()"), avg("tbodyaccjerkmag-std()"), avg("tbodygyromag-mean()"), avg("tbodygyromag-std()"), avg("tbodygyrojerkmag-mean()"), avg("tbodygyrojerkmag-std()"), avg("fbodyacc-mean()-x"), avg("fbodyacc-mean()-y"), avg("fbodyacc-mean()-z"), avg("fbodyacc-std()-x"), avg("fbodyacc-std()-y"), avg("fbodyacc-std()-z"), avg("fbodyacc-meanfreq()-x"), avg("fbodyacc-meanfreq()-y"), avg("fbodyacc-meanfreq()-z"), avg("fbodyaccjerk-mean()-x"), avg("fbodyaccjerk-mean()-y"), avg("fbodyaccjerk-mean()-z"), avg("fbodyaccjerk-std()-x"), avg("fbodyaccjerk-std()-y"), avg("fbodyaccjerk-std()-z"), avg("fbodyaccjerk-meanfreq()-x"), avg("fbodyaccjerk-meanfreq()-y"), avg("fbodyaccjerk-meanfreq()-z"), avg("fbodygyro-mean()-x"), avg("fbodygyro-mean()-y"), avg("fbodygyro-mean()-z"), avg("fbodygyro-std()-x"), avg("fbodygyro-std()-y"), avg("fbodygyro-std()-z"), avg("fbodygyro-meanfreq()-x"), avg("fbodygyro-meanfreq()-y"), avg("fbodygyro-meanfreq()-z"), avg("fbodyaccmag-mean()"), avg("fbodyaccmag-std()"), avg("fbodyaccmag-meanfreq()"), avg("fbodybodyaccjerkmag-mean()"), avg("fbodybodyaccjerkmag-std()"), avg("fbodybodyaccjerkmag-meanfreq()"), avg("fbodybodygyromag-mean()"), avg("fbodybodygyromag-std()"), avg("fbodybodygyromag-meanfreq()"), avg("fbodybodygyrojerkmag-mean()"), avg("fbodybodygyrojerkmag-std()"), avg("fbodybodygyrojerkmag-meanfreq()"), avg("angle(tbodyaccmean,gravity)"), avg("angle(tbodyaccjerkmean),gravitymean)"), avg("angle(tbodygyromean,gravitymean)"), avg("angle(tbodygyrojerkmean,gravitymean)"), avg("angle(x,gravitymean)"), avg("angle(y,gravitymean)"), avg("angle(z,gravitymean)") from AllmsData group by subject, activity')

# Product tidy dataset as test file row.name=FALSE
write.table(tidy, file='UCI HAR Dataset/tidy.txt' , sep='\t', row.names=FALSE)

