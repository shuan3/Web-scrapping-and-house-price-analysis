#--------------------------------------------------------------------------------------
#################### House Data Clean #################################################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
## Install and load the required packages
#--------------------------------------------------------------------------------------
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(tidyr)) 
  install.packages("tidyr")

#--------------------------------------------------------------------------------------
## Data Loading
#--------------------------------------------------------------------------------------
#house_data <- read.csv("housedata.csv")
house_data  <- read.csv("ontario_property_listings_ORIG.csv")

dim(house_data)

#colnames(house_data)
#str(house_data)

#--------------------------------------------------------------------------------------
## Data Cleanup
#--------------------------------------------------------------------------------------

clean_house_data <- house_data

# Remove URL column because it contains the address
#clean_house_data = select(clean_house_data, -URL)
#dim(clean_house_data)

#--------------------------------------------------------------------------------------
## Data Cleanup for Status
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Status <- trimws(clean_house_data$Status)
dim(house_data)

# Summary by Status
clean_house_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

clean_house_data <- clean_house_data %>% filter(Status != "Lease")
dim(house_data)

# Change Status "Sale" to "Sold"
clean_house_data$Status[ clean_house_data$Status == "Sale"] <- "Sold"

# Summary by Status
clean_house_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
# Re-group Type values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Type   <- trimws(clean_house_data$Type)

# Check NULL and non-NULL counts
sum( is.na(clean_house_data$Type) ) #--2
sum( !is.na(clean_house_data$Type) ) #--39206

# Save OLD Type values in a new column
clean_house_data$TypeOld <- clean_house_data$Type
dim(clean_house_data)

# Set Type = Other
clean_house_data$Type <- "Other"

# Set New Type values based on OLD Type values
clean_house_data$Type[ grepl("Detached",      clean_house_data$TypeOld) == TRUE ] <- "Detached"
clean_house_data$Type[ grepl("Single Family", clean_house_data$TypeOld) == TRUE ] <- "Detached"

clean_house_data$Type[ grepl("Semi-Detached", clean_house_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_house_data$Type[ grepl("Semi Detached", clean_house_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_house_data$Type[ grepl("SEMI-DETACHED", clean_house_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_house_data$Type[ grepl("Link",          clean_house_data$TypeOld) == TRUE ] <- "Semi-Detached"

clean_house_data$Type[ grepl("Condo",         clean_house_data$TypeOld) == TRUE ] <- "Condo"
clean_house_data$Type[ grepl("Apartment",     clean_house_data$TypeOld) == TRUE ] <- "Condo"
clean_house_data$Type[ grepl("Apt",           clean_house_data$TypeOld) == TRUE ] <- "Condo"

clean_house_data$Type[ grepl("Townhouse",     clean_house_data$TypeOld) == TRUE ] <- "Townhouse"
clean_house_data$Type[ grepl("Twnhouse",      clean_house_data$TypeOld) == TRUE ] <- "Townhouse"

clean_house_data$Type[ grepl("Multiplex",     clean_house_data$TypeOld) == TRUE ] <- "Multiplex"
clean_house_data$Type[ grepl("Duplex",        clean_house_data$TypeOld) == TRUE ] <- "Multiplex"
clean_house_data$Type[ grepl("Triplex",       clean_house_data$TypeOld) == TRUE ] <- "Multiplex"
clean_house_data$Type[ grepl("Fourplex",      clean_house_data$TypeOld) == TRUE ] <- "Multiplex"

clean_house_data$Type[ grepl("Comm",          clean_house_data$TypeOld) == TRUE ] <- "Commercial"
clean_house_data$Type[ grepl("Business",      clean_house_data$TypeOld) == TRUE ] <- "Commercial"
clean_house_data$Type[ grepl("Industrial",    clean_house_data$TypeOld) == TRUE ] <- "Commercial"
clean_house_data$Type[ grepl("Investment",    clean_house_data$TypeOld) == TRUE ] <- "Commercial"
clean_house_data$Type[ grepl("Office",        clean_house_data$TypeOld) == TRUE ] <- "Commercial"
clean_house_data$Type[ grepl("Retail",        clean_house_data$TypeOld) == TRUE ] <- "Commercial"

clean_house_data$Type[ grepl("Land",          clean_house_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_house_data$Type[ grepl("Lots",          clean_house_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_house_data$Type[ grepl("No Building",   clean_house_data$TypeOld) == TRUE ] <- "Vacant-Land"

# Count Summary by Type
#house_types <- 
clean_house_data %>%
  group_by(Type) %>% summarise(Count = n())

# Remove OLD Type column
clean_house_data = select(clean_house_data, -TypeOld)
dim(clean_house_data)

#--------------------------------------------------------------------------------------
## Data Cleanup for Area
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Area <- trimws(clean_house_data$Area)

# Summary by Area
clean_house_data %>%
  group_by(Area) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Re-group Community / Neighbourhood values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Community <- trimws(clean_house_data$Community)

# Save OLD Community values in a new column
clean_house_data$CommunityOld <- clean_house_data$Community
dim(clean_house_data)

# Summary by Community
clean_house_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))

# Check NULL and non-NULL counts
sum( is.na(clean_house_data$CommunityOld) ) #--1179
sum( !is.na(clean_house_data$CommunityOld) ) #--40189

# Set New Community values based on OLD Community values
clean_house_data$Community[ grepl("Waterfront",            clean_house_data$CommunityOld) == TRUE ] <- "Waterfront"
clean_house_data$Community[ grepl("Willowdale",            clean_house_data$CommunityOld) == TRUE ] <- "Willowdale"
clean_house_data$Community[ grepl("Mount Pleasant",        clean_house_data$CommunityOld) == TRUE ] <- "Mount Pleasant"
clean_house_data$Community[ grepl("Islington-City Centre", clean_house_data$CommunityOld) == TRUE ] <- "Islington-City Centre"
clean_house_data$Community[ grepl("Newtonbrook",           clean_house_data$CommunityOld) == TRUE ] <- "Newtonbrook"
clean_house_data$Community[ grepl("Brampton",              clean_house_data$CommunityOld) == TRUE ] <- "Brampton"

# Summary by Community
clean_house_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

# Remove OLD Community column
clean_house_data = select(clean_house_data, -CommunityOld)
dim(clean_house_data) #--41110   100

# Rename column "Community" to "Neighbourhood"
colnames(clean_house_data)[colnames(clean_house_data) == "Community"] <- "Neighbourhood"

clean_house_data %>%
  group_by(Neighbourhood) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

#--------------------------------------------------------------------------------------
# Re-group Age values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Age <- trimws(clean_house_data$Age)

clean_house_data %>% filter(Age == "New")    %>% nrow() #--970
clean_house_data %>% filter(Age == "0to5")   %>% nrow() #--4338
clean_house_data %>% filter(Age == "6to10")  %>% nrow() #--1021
clean_house_data %>% filter(Age == "6to15")  %>% nrow() #--2791
clean_house_data %>% filter(Age == "11to15") %>% nrow() #--535
clean_house_data %>% filter(Age == "16to30") %>% nrow() #--3266
clean_house_data %>% filter(Age == "31to50") %>% nrow() #--2607
clean_house_data %>% filter(Age == "51to99") %>% nrow() #--1878
clean_house_data %>% filter(Age == "100+")   %>% nrow() #--605
clean_house_data %>% filter(Age == "999")    %>% nrow() #--1

clean_house_data$Age[ clean_house_data$Age == "New"]    <- "0"
clean_house_data$Age[ clean_house_data$Age == "0to5"]   <- "3"
clean_house_data$Age[ clean_house_data$Age == "6to10"]  <- "8"
clean_house_data$Age[ clean_house_data$Age == "6to15"]  <- "10"
clean_house_data$Age[ clean_house_data$Age == "11to15"] <- "13"
clean_house_data$Age[ clean_house_data$Age == "16to30"] <- "23"
clean_house_data$Age[ clean_house_data$Age == "31to50"] <- "40"
clean_house_data$Age[ clean_house_data$Age == "51to99"] <- "75"
clean_house_data$Age[ clean_house_data$Age == "100+"]   <- "101"
clean_house_data$Age[ clean_house_data$Age == "999"]    <- "101"

unique(clean_house_data$Age) %>% sort()

#--------------------------------------------------------------------------------------
# Re-group Basement values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_house_data$Basement <- trimws(clean_house_data$Basement)

# Check NULL and non-NULL counts
sum( is.na(clean_house_data$Basement) ) #--695
sum( !is.na(clean_house_data$Basement) ) #--40673

# Save OLD Basement values in a new column
clean_house_data$BasementOld <- clean_house_data$Basement
dim(clean_house_data)

# Set Basement = Unfinished
clean_house_data$Basement[!is.na(clean_house_data$BasementOld)]   <- "Unfinished"

# Set New Basement values based on OLD Basement values
clean_house_data$Basement[ grepl("Apartment",   clean_house_data$BasementOld) == TRUE ] <- "Finished"
clean_house_data$Basement[ grepl("Fin W/O",     clean_house_data$BasementOld) == TRUE ] <- "Finished"
clean_house_data$Basement[ grepl("Finished",    clean_house_data$BasementOld) == TRUE ] <- "Finished"
clean_house_data$Basement[ grepl("Full, Suite", clean_house_data$BasementOld) == TRUE ] <- "Finished"
clean_house_data$Basement[ grepl("Part Bsmt",   clean_house_data$BasementOld) == TRUE ] <- "Part Finished"
clean_house_data$Basement[ grepl("Part Fin",    clean_house_data$BasementOld) == TRUE ] <- "Part Finished"
clean_house_data$Basement[ grepl("Partial",     clean_house_data$BasementOld) == TRUE ] <- "Part Finished"

clean_house_data$Basement[clean_house_data$BasementOld == "None"]  <- "None"
clean_house_data$Basement[clean_house_data$BasementOld == "No"]    <- "None"

clean_house_data$Basement[clean_house_data$Type        == "Condo"] <- "None"

clean_house_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Remove OLD Basement column
clean_house_data = select(clean_house_data, -BasementOld)
dim(clean_house_data)

#-------------------------------------------------------------------------------
# Add a new column FinishedBasement
#-------------------------------------------------------------------------------

clean_house_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Add a new column FinishedBasement and default to O (false)
clean_house_data$FinishedBasement <- 0
dim(clean_house_data)

# Set FinishedBasement = 1 if the column Basement is "Finished"
clean_house_data$FinishedBasement[ clean_house_data$Basement == "Finished" ] <- 1

clean_house_data %>%
  group_by(FinishedBasement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

#-------------------------------------------------------------------------------
# Add a new column NearSchool
#-------------------------------------------------------------------------------

# Add a new column NearSchool and default to O (false)
clean_house_data$NearSchool <- 0
dim(clean_house_data)

# Set NearSchool = 1 if the column Feature contain key work "School"
clean_house_data$NearSchool[ grepl("School", clean_house_data$Feature, ignore.case=TRUE) == TRUE ] <- 1

clean_house_data %>%
  group_by(NearSchool) %>% summarise(Count = n())

#-------------------------------------------------------------------------------
# Add a new column NearPark
#-------------------------------------------------------------------------------

# Add a new column NearPark and default to O (false)
clean_house_data$NearPark <- 0
dim(clean_house_data)

# Set NearSchool = 1 if the column Feature contain key work "Park"
clean_house_data$NearPark[ grepl("Park", clean_house_data$Feature, ignore.case=TRUE) == TRUE ] <- 1

clean_house_data %>%
  group_by(NearPark) %>% summarise(Count = n())

#--------------------------------------------------------------------------------------
## Data Cleanup for Price
#--------------------------------------------------------------------------------------

dim(clean_house_data) #--41368

# Check NULL and non-NULL counts
sum( is.na(clean_house_data$ListPrice) ) #--1183
sum( !is.na(clean_house_data$ListPrice) ) #--40185

clean_house_data <- clean_house_data %>% filter( !is.na(clean_house_data$ListPrice) )
dim(clean_house_data) #--40185

# Price Summary
clean_house_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(ListPrice),    max  = max(ListPrice), 
            median = median(ListPrice), mean = mean(ListPrice) ) %>% arrange(desc(Count))

# Check and remove outliers
boxplot(clean_house_data$ListPrice)

clean_house_data %>% filter(ListPrice < 1000)     %>% nrow() #--3
clean_house_data %>% filter(ListPrice < 2000)     %>% nrow() #--233
clean_house_data %>% filter(ListPrice < 5000)     %>% nrow() #--1187
clean_house_data %>% filter(ListPrice < 10000)    %>% nrow() #--1217
clean_house_data %>% filter(ListPrice < 100000)   %>% nrow() #--1244

clean_house_data %>% filter(ListPrice > 2000000)  %>% nrow() #--1104
clean_house_data %>% filter(ListPrice > 3000000)  %>% nrow() #--328
clean_house_data %>% filter(ListPrice > 4000000)  %>% nrow() #--113
clean_house_data %>% filter(ListPrice > 5000000)  %>% nrow() #--57
clean_house_data %>% filter(ListPrice > 7500000)  %>% nrow() #--17
clean_house_data %>% filter(ListPrice > 10000000) %>% nrow() #--6
clean_house_data %>% filter(ListPrice > 12000000) %>% nrow() #--4
clean_house_data %>% filter(ListPrice > 14000000) %>% nrow() #--1

# Remove Price outliers
clean_house_data <- clean_house_data %>% filter(ListPrice >= 100000 & ListPrice <= 5000000)
dim(clean_house_data) #--38884

# Double Check Price outliers
boxplot(clean_house_data$ListPrice)

# Price Summary
clean_house_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(ListPrice),    max  = max(ListPrice), 
            median = median(ListPrice), mean = mean(ListPrice) )  %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Data Cleanup for Taxes
#--------------------------------------------------------------------------------------

dim(clean_house_data) #--38884

# Check NULL and non-NULL counts
sum( is.na(clean_house_data$Taxes) ) #--1280
sum( !is.na(clean_house_data$Taxes) ) #--37604

clean_house_data <- clean_house_data %>% filter( !is.na(clean_house_data$Taxes) )
dim(clean_house_data) #--37604

# Taxes Summary
clean_house_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(Taxes),    max  = max(Taxes), 
            median = median(Taxes), mean = mean(Taxes) ) %>% arrange(desc(Count))

# Check and remove outliers
boxplot(clean_house_data$Taxes)

clean_house_data %>% filter(Taxes < 100)    %>% nrow() #--72
clean_house_data %>% filter(Taxes < 500)    %>% nrow() #--112
clean_house_data %>% filter(Taxes < 1000)   %>% nrow() #--389

clean_house_data %>% filter(Taxes > 10000)  %>% nrow() #--812
clean_house_data %>% filter(Taxes > 15000)  %>% nrow() #--189
clean_house_data %>% filter(Taxes > 20000)  %>% nrow() #--59
clean_house_data %>% filter(Taxes > 50000)  %>% nrow() #--7
clean_house_data %>% filter(Taxes > 100000) %>% nrow() #--6

# Remove Taxes outliers
clean_house_data <- clean_house_data %>% filter(Taxes >= 1000 & Taxes <= 20000)
dim(clean_house_data) #--37156

# Check and remove Price outliers
boxplot(clean_house_data$Taxes)

# Taxes Summary
clean_house_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(Taxes),    max  = max(Taxes), 
            median = median(Taxes), mean = mean(Taxes) )  %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
# Save the new dataset
#--------------------------------------------------------------------------------------
dim(clean_house_data) #--37156   100

write.csv(clean_house_data,"ontario_property_listings_new.csv",  row.names = FALSE)

#--------------------------------------------------------------------------------------
