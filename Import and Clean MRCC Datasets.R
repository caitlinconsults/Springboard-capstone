## Cleaning Data from The Midwestern Regional Climate Center

#### The Midwestern Regional Climate Center (MRCC) provides free access to 
#### Midwestern U.S. climate data via their online cli-MATE portal:

#### http://mrcc.isws.illinois.edu/CLIMATE/index.jsp

## The code below will convert a raw “Daily-Observed / Between Two Dates” 
## dataset into an R-ready dataset.






## BEFORE RUNNING : If not already done, import and apply tidyr;






install.packages("tidyr")
library("tidyr")






## BEFORE RUNNING : Set the import_file variable. IMPORT FILE MUST BE IN WORKING DIRECTORY;






import_file <- "TWO HARBORS SD_b2dates.csv"






## The remaining code can be run with no manual inputs.

## Strip unformatted header data + import data as characters;

dirty <- read.csv(import_file, skip = 7, colClasses = c(rep("character",7)), header = TRUE)

## Strip unformatted tail data + extra column;

clean <- head(dirty, -14)
clean <- clean[,-8]
rm(dirty)

## Convert "missing" data to "NA";

clean[clean == "M"] <- NA

## Convert to date OR numeric; 

clean[,1] <- as.Date(clean[,1], "%Y-%m-%d")
clean[,c(2:7)] <- as.data.frame(sapply(clean[,c(2:7)], as.numeric))
clean[,7] <- NULL

## Remove rows where observations are NULL;

clean$Count.NA <- rowSums(is.na(clean))
clean <- clean[!clean$Count.NA == "5", ]

## Separate date column for future analysis

clean <- separate(clean, Date, c("Year", "Month", "Day"), sep = "-", remove = FALSE)
clean$Year <- as.numeric(clean$Year)
clean$Month <- as.numeric(clean$Month)
clean$Day <- as.numeric(clean$Day)

## Rename file + remove supportive data frames; 

new_name <- sub(" SD_b2dates.csv", "", import_file)
new_name <- gsub(" ", "_", new_name)
assign(new_name, clean)
rm(new_name)
rm(clean)
rm(import_file)

