# download a file
download.file(url = 'https://ndownloader.figshare.com/files/2292169',
              destfile = 'data_raw/portal_data_joined.csv')

# load the data
surveys <- read.csv('data_raw/portal_data_joined.csv')

# inspect the data
head(surveys)
View(surveys)
str(surveys)

# size
dim(surveys)
nrow(surveys)
ncol(surveys)
# content
head(surveys)
tail(surveys)
# names
names(surveys) # equivalent of colnames() for data.frame object
# rownames(surveys)
# summary
str(surveys)
summary(surveys)

# indexing and subsetting data frames
surveys[1,1] # row 1 column 1
surveys[1,6] # row 1 column 6
surveys[, 1] # first column, a vector
class(surveys[, 1])
surveys[1] # first column, a data frame
class(surveys[1])
surveys[3, ] # first row
class(surveys[3, ]) # third row but it's a data frame
surveys[1:3, 7] # fist three elements of column 7, as a vector
head_surveys <- surveys[1:6, ] # equivalent to head(surveys)
surveys[, -1]
surveys[-(7:34786),]
# data frames can be subset by indices or column names
surveys['species_id'] # result is a data frame
surveys$species_id # result is a vector
surveys[, 'species_id'] # result is a vector
surveys[['species_id']] # result is a vector
# challenge
surveys[200, ]
class(surveys[200,])
surveys[nrow(surveys),]
tail(surveys)
surveysLast <- surveys[nrow(surveys),]
typeof(surveysLast)
class(surveysLast)
surveysMiddle <- surveys[nrow(surveys)/2,]
surveys[-(7:nrow(surveys)),]

# Factors represent categorical data, that are associated with INTEGER values
# Factors can be ordered or unordered
sex <- factor(c('male', 'female', 'female', 'male'))
levels(sex)
nlevels(sex)
# order levels in a factor
sex <- factor(sex, levels = c('male', 'female'))

# Convert factors into character or numeric
as.character(sex)

yearFct <- factor(c(1990, 1983, 1977, 1998, 1990))
# this will only give indices of levels rather than values of year
as.numeric(yearFct)
# to obtain years as numerical values, convert them into characters then to numeric
as.numeric(as.character(yearFct))
as.numeric(levels(yearFct))
# usually, we do this:
# We obtain all the factor levels using levels(year_fct)
# We convert these levels to numeric values using as.numeric(levels(year_fct))
# We then access these numeric values using the underlying integers of the vector 
# year_fct inside the square brackets
as.numeric(levels(yearFct))[yearFct]
# convert to factors, factors can be directly plotted
plot(as.factor(surveys$sex))

# Rename factors
# create new object for modification rather than modify the original data
sex <- surveys$sex
head(sex)
levels(sex)
levels(sex)[1:3] <- c('female', 'male', 'undetermined')
levels(sex)
plot(sex)
plot(surveys$sex)

# stringsAsFactors = FALSE
surveys <- read.csv('data_raw/portal_data_joined.csv', stringsAsFactors = TRUE)
str(surveys)

surveys <- read.csv('data_raw/portal_data_joined.csv', stringsAsFactors = FALSE)
str(surveys)

surveys$plot_type <- factor(surveys$plot_type)
str(surveys)

# create a data frame
animalData <- data.frame(
  animal = c('dog', 'cat', 'sea cucumber', 'sea urchin'),
  feel = c('furry', 'furry', 'squishy', 'spiny'),
  weight = c(45, 8, 1.1, 0.8)
)

View(animalData)

# Formatting dates
str(surveys)

library('lubridate')

myDate <- ymd('2020-07-11')
str(myDate)
myDate

paste('2020', '7', '11', sep = '.') # concatenate after converting to strings
myDate <- ymd(paste('2020', '7', '11', sep = '-'))
str(myDate)

paste(surveys$year, surveys$month, surveys$day, sep = '-')
ymd(paste(surveys$year, surveys$month, surveys$day, sep = '-'))

# create a new column called 'Date'
surveys$Date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = '-'))
str(surveys)

# inspect the new column
summary(surveys$Date)
missingDates <- surveys[is.na(surveys$Date), c('year', 'month', 'day')]
head(missingDates)
