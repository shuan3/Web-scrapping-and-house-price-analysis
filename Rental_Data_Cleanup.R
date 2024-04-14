#--------------------------------------------------------------------------------------
#################### Rental Data CLeanup #############################################
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
#rental_data <- read.csv("finalrental.csv", header=TRUE)
rental_data  <- read.csv("ontario_rental_listings_ORIG.csv", header=TRUE)

dim(rental_data) #--11647   56

#colnames(rental_data)
#str(rental_data)

#--------------------------------------------------------------------------------------
## Data Cleanup
#--------------------------------------------------------------------------------------

clean_rental_data <- rental_data

# Remove Link column because it contains the address
clean_rental_data = select(clean_rental_data, -Link)
dim(clean_rental_data) #--11647   55

#--------------------------------------------------------------------------------------
## Data Cleanup for Status
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Status <- trimws(clean_rental_data$Status)

dim(clean_rental_data) #--11647

# Summary by Status
clean_rental_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

clean_rental_data <- clean_rental_data %>% filter(Status == "Lease")
dim(clean_rental_data) #--11063

# Summary by Status
clean_rental_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Re-group Type values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Type  <- trimws(clean_rental_data$Type)

# Summary by Type
clean_rental_data %>%
  group_by(Type) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

# Save OLD Type values in a new column
clean_rental_data$TypeOld <- clean_rental_data$Type
dim(clean_rental_data)

# Check NULL and non-NULL counts
sum( is.na(clean_rental_data$TypeOld) ) #--0
sum( !is.na(clean_rental_data$TypeOld) ) #--11063

# Set Type = Other
clean_rental_data$Type <- "Other"

# Set New Type values based on OLD Type values
clean_rental_data$Type[ grepl("Detached",      clean_rental_data$TypeOld) == TRUE ] <- "Detached"
clean_rental_data$Type[ grepl("Single Family", clean_rental_data$TypeOld) == TRUE ] <- "Detached"

clean_rental_data$Type[ grepl("Semi-Detached", clean_rental_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_rental_data$Type[ grepl("Semi Detached", clean_rental_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_rental_data$Type[ grepl("SEMI-DETACHED", clean_rental_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_rental_data$Type[ grepl("Link",          clean_rental_data$TypeOld) == TRUE ] <- "Semi-Detached"

clean_rental_data$Type[ grepl("Condo",         clean_rental_data$TypeOld) == TRUE ] <- "Condo"
clean_rental_data$Type[ grepl("Apartment",     clean_rental_data$TypeOld) == TRUE ] <- "Condo"
clean_rental_data$Type[ grepl("Apt",           clean_rental_data$TypeOld) == TRUE ] <- "Condo"

clean_rental_data$Type[ grepl("Townhouse",     clean_rental_data$TypeOld) == TRUE ] <- "Townhouse"
clean_rental_data$Type[ grepl("Twnhouse",      clean_rental_data$TypeOld) == TRUE ] <- "Townhouse"

clean_rental_data$Type[ grepl("Multiplex",     clean_rental_data$TypeOld) == TRUE ] <- "Multiplex"
clean_rental_data$Type[ grepl("Duplex",        clean_rental_data$TypeOld) == TRUE ] <- "Multiplex"
clean_rental_data$Type[ grepl("Triplex",       clean_rental_data$TypeOld) == TRUE ] <- "Multiplex"
clean_rental_data$Type[ grepl("Fourplex",      clean_rental_data$TypeOld) == TRUE ] <- "Multiplex"

clean_rental_data$Type[ grepl("Comm",          clean_rental_data$TypeOld) == TRUE ] <- "Commercial"
clean_rental_data$Type[ grepl("Business",      clean_rental_data$TypeOld) == TRUE ] <- "Commercial"
clean_rental_data$Type[ grepl("Industrial",    clean_rental_data$TypeOld) == TRUE ] <- "Commercial"
clean_rental_data$Type[ grepl("Investment",    clean_rental_data$TypeOld) == TRUE ] <- "Commercial"
clean_rental_data$Type[ grepl("Office",        clean_rental_data$TypeOld) == TRUE ] <- "Commercial"
clean_rental_data$Type[ grepl("Retail",        clean_rental_data$TypeOld) == TRUE ] <- "Commercial"

clean_rental_data$Type[ grepl("Land",          clean_rental_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_rental_data$Type[ grepl("Lots",          clean_rental_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_rental_data$Type[ grepl("No Building",   clean_rental_data$TypeOld) == TRUE ] <- "Vacant-Land"

# Summary by Type
clean_rental_data %>%
  group_by(Type) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))

# Remove OLD Type column
clean_rental_data = select(clean_rental_data, -TypeOld)
dim(clean_rental_data)

#--------------------------------------------------------------------------------------
## Data Cleanup for Area
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Area  <- trimws(clean_rental_data$Area)

dim(clean_rental_data)

# Summary by Area
clean_rental_data %>%
  group_by(Area) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))

clean_rental_data <- clean_rental_data %>% filter(Area == "Toronto")
dim(clean_rental_data) #--11621

#--------------------------------------------------------------------------------------
## Re-group Community values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Community  <- trimws(clean_rental_data$Community)

# Summary by Community
clean_rental_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))

# Save OLD Community values in a new column
clean_rental_data$CommunityOld <- clean_rental_data$Community
dim(clean_rental_data)

# Check NULL and non-NULL counts
sum( is.na(clean_rental_data$CommunityOld) ) #--0
sum( !is.na(clean_rental_data$CommunityOld) ) #--11063

# Set New Community values based on OLD Community values
clean_rental_data$Community[ grepl("Waterfront",            clean_rental_data$CommunityOld) == TRUE ] <- "Waterfront"
clean_rental_data$Community[ grepl("Willowdale",            clean_rental_data$CommunityOld) == TRUE ] <- "Willowdale"
clean_rental_data$Community[ grepl("Mount Pleasant",        clean_rental_data$CommunityOld) == TRUE ] <- "Mount Pleasant"
clean_rental_data$Community[ grepl("Islington-City Centre", clean_rental_data$CommunityOld) == TRUE ] <- "Islington-City Centre"
clean_rental_data$Community[ grepl("Newtonbrook",           clean_rental_data$CommunityOld) == TRUE ] <- "Newtonbrook"
clean_rental_data$Community[ grepl("Brampton",              clean_rental_data$CommunityOld) == TRUE ] <- "Brampton"

# Summary by Community
clean_rental_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

# Remove OLD Community column
clean_rental_data = select(clean_rental_data, -CommunityOld)
dim(clean_rental_data)

# Rename column "Community" to "Neighbourhood"
colnames(clean_rental_data)[colnames(clean_rental_data) == "Community"] <- "Neighbourhood"

clean_rental_data %>%
  group_by(Neighbourhood) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

#--------------------------------------------------------------------------------------
# Re-group Basement values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Basement <- trimws(clean_rental_data$Basement)

clean_rental_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Save OLD Basement values in a new column
clean_rental_data$BasementOld <- clean_rental_data$Basement
dim(clean_rental_data)

# Check NULL and non-NULL counts
sum( is.na(clean_rental_data$Basement) ) #--52
sum( !is.na(clean_rental_data$Basement) ) #--11011

# Set Basement = Unfinished
clean_rental_data$Basement[!is.na(clean_rental_data$BasementOld)]   <- "Unfinished"

# Set New Basement values based on OLD Basement values
clean_rental_data$Basement[ grepl("Apartment",   clean_rental_data$BasementOld) == TRUE ] <- "Finished"
clean_rental_data$Basement[ grepl("Fin W/O",     clean_rental_data$BasementOld) == TRUE ] <- "Finished"
clean_rental_data$Basement[ grepl("Finished",    clean_rental_data$BasementOld) == TRUE ] <- "Finished"
clean_rental_data$Basement[ grepl("Full, Suite", clean_rental_data$BasementOld) == TRUE ] <- "Finished"
clean_rental_data$Basement[ grepl("Part Bsmt",   clean_rental_data$BasementOld) == TRUE ] <- "Part Finished"
clean_rental_data$Basement[ grepl("Part Fin",    clean_rental_data$BasementOld) == TRUE ] <- "Part Finished"
clean_rental_data$Basement[ grepl("Partial",     clean_rental_data$BasementOld) == TRUE ] <- "Part Finished"

clean_rental_data$Basement[clean_rental_data$BasementOld == "None"]  <- "None"
clean_rental_data$Basement[clean_rental_data$BasementOld == "No"]    <- "None"

clean_rental_data$Basement[clean_rental_data$Type        == "Condo"] <- "None"

clean_rental_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Remove OLD Basement column
clean_rental_data = select(clean_rental_data, -BasementOld)
dim(clean_rental_data)

#-------------------------------------------------------------------------------
# Add a new column FinishedBasement
#-------------------------------------------------------------------------------

clean_rental_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Add a new column FinishedBasement and default to O (false)
clean_rental_data$FinishedBasement <- 0
dim(clean_rental_data)

# Set FinishedBasement = 1 if the column Basement is "Finished"
clean_rental_data$FinishedBasement[ clean_rental_data$Basement == "Finished" ] <- 1

clean_rental_data %>%
  group_by(FinishedBasement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Data Cleanup for Price
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_rental_data$Price <- trimws(clean_rental_data$Price)

dim(clean_rental_data) #--11063

clean_rental_data %>% filter(Price == "Sign up to See") %>% nrow() #--1078

clean_rental_data <- clean_rental_data %>% filter(Price != "Sign up to See")
dim(clean_rental_data) #--9984

clean_rental_data$Price <- gsub("\\$", "", gsub(",", "", clean_rental_data$Price) )
clean_rental_data$Price <- as.numeric(clean_rental_data$Price)

# Check and remove Price outliers
boxplot(clean_rental_data$Price)

clean_rental_data %>% filter(Price < 100)     %>% nrow() #--10
clean_rental_data %>% filter(Price < 500)     %>% nrow() #--22
clean_rental_data %>% filter(Price < 1000)    %>% nrow() #--42
clean_rental_data %>% filter(Price < 1500)    %>% nrow() #--206
clean_rental_data %>% filter(Price < 2000)    %>% nrow() #--2824

clean_rental_data %>% filter(Price > 3000)    %>% nrow() #--1399
clean_rental_data %>% filter(Price > 4000)    %>% nrow() #--471
clean_rental_data %>% filter(Price > 5000)    %>% nrow() #--228
clean_rental_data %>% filter(Price > 6000)    %>% nrow() #--140
clean_rental_data %>% filter(Price > 7000)    %>% nrow() #--75
clean_rental_data %>% filter(Price > 8000)    %>% nrow() #--25
clean_rental_data %>% filter(Price > 10000)   %>% nrow() #--4

clean_rental_data <- clean_rental_data %>% filter(Price >= 1000 & Price <= 6000)
dim(clean_rental_data) #--9789

# Double Check Price outliers
boxplot(clean_rental_data$Price)

# Price Summary
clean_rental_data %>% 
  group_by(Area) %>%
  summarise(Count = n(), 
            min   = min(Price), max = max(Price), 
            median = median(Price), mean = mean(Price) )

#--------------------------------------------------------------------------------------
# Save the new dataset
#--------------------------------------------------------------------------------------
dim(clean_rental_data) #--9789   57
write.csv(clean_rental_data,"ontario_rental_listings.csv",  row.names = FALSE)

#-------------------------------------------------------------------------------------