#--------------------------------------------------------------------------------------
#################### Housing Market - Data Analysis & Visualization ###################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
## Install and load the required packages
#--------------------------------------------------------------------------------------
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(tidyr)) 
  install.packages("tidyr")
if(!require(corrplot)) 
  install.packages("corrplot")
if(!require(ggplot2)) 
  install.packages("ggplot2")
if(!require(ggthemes)) 
  install.packages("ggthemes")

#--------------------------------------------------------------------------------------
## Data Loading
#--------------------------------------------------------------------------------------
house_data  <- read.csv("housedata.csv")

dim(house_data) #--41368    97

#--------------------------------------------------------------------------------------
#################### House Data Cleanup #################################################
#--------------------------------------------------------------------------------------

clean_data <- house_data

#--------------------------------------------------------------------------------------
## Data Cleanup for Status
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Status <- trimws(clean_data$Status)
dim(house_data) #--41368

# Summary by Status
clean_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

clean_data <- clean_data %>% filter(Status != "Lease")
dim(house_data) #--41368

# Change Status "Sale" to "Sold"
clean_data$Status[ clean_data$Status == "Sale"] <- "Sold"

# Summary by Status
clean_data %>%
  group_by(Status) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
# Re-group Type values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Type   <- trimws(clean_data$Type)

# Check NULL and non-NULL counts
sum( is.na(clean_data$Type) ) #--258
sum( !is.na(clean_data$Type) ) #--41110

# Save OLD Type values in a new column
clean_data$TypeOld <- clean_data$Type
dim(clean_data)

# Set Type = Other
clean_data$Type <- "Other"

# Set New Type values based on OLD Type values
clean_data$Type[ grepl("Detached",      clean_data$TypeOld) == TRUE ] <- "Detached"
clean_data$Type[ grepl("Single Family", clean_data$TypeOld) == TRUE ] <- "Detached"

clean_data$Type[ grepl("Semi-Detached", clean_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_data$Type[ grepl("Semi Detached", clean_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_data$Type[ grepl("SEMI-DETACHED", clean_data$TypeOld) == TRUE ] <- "Semi-Detached"
clean_data$Type[ grepl("Link",          clean_data$TypeOld) == TRUE ] <- "Semi-Detached"

clean_data$Type[ grepl("Condo",         clean_data$TypeOld) == TRUE ] <- "Condo"
clean_data$Type[ grepl("Apartment",     clean_data$TypeOld) == TRUE ] <- "Condo"
clean_data$Type[ grepl("Apt",           clean_data$TypeOld) == TRUE ] <- "Condo"

clean_data$Type[ grepl("Townhouse",     clean_data$TypeOld) == TRUE ] <- "Townhouse"
clean_data$Type[ grepl("Twnhouse",      clean_data$TypeOld) == TRUE ] <- "Townhouse"

clean_data$Type[ grepl("Multiplex",     clean_data$TypeOld) == TRUE ] <- "Multiplex"
clean_data$Type[ grepl("Duplex",        clean_data$TypeOld) == TRUE ] <- "Multiplex"
clean_data$Type[ grepl("Triplex",       clean_data$TypeOld) == TRUE ] <- "Multiplex"
clean_data$Type[ grepl("Fourplex",      clean_data$TypeOld) == TRUE ] <- "Multiplex"

clean_data$Type[ grepl("Comm",          clean_data$TypeOld) == TRUE ] <- "Commercial"
clean_data$Type[ grepl("Business",      clean_data$TypeOld) == TRUE ] <- "Commercial"
clean_data$Type[ grepl("Industrial",    clean_data$TypeOld) == TRUE ] <- "Commercial"
clean_data$Type[ grepl("Investment",    clean_data$TypeOld) == TRUE ] <- "Commercial"
clean_data$Type[ grepl("Office",        clean_data$TypeOld) == TRUE ] <- "Commercial"
clean_data$Type[ grepl("Retail",        clean_data$TypeOld) == TRUE ] <- "Commercial"

clean_data$Type[ grepl("Land",          clean_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_data$Type[ grepl("Lots",          clean_data$TypeOld) == TRUE ] <- "Vacant-Land"
clean_data$Type[ grepl("No Building",   clean_data$TypeOld) == TRUE ] <- "Vacant-Land"

# Count Summary by Type
#house_types <- 
clean_data %>%
  group_by(Type) %>% summarise(Count = n())

# Remove OLD Type column
clean_data = select(clean_data, -TypeOld)
dim(clean_data)

#--------------------------------------------------------------------------------------
## Data Cleanup for Area
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Area <- trimws(clean_data$Area)

# Summary by Area
clean_data %>%
  group_by(Area) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Re-group Community / Neighbourhood values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Community <- trimws(clean_data$Community)

# Save OLD Community values in a new column
clean_data$CommunityOld <- clean_data$Community
dim(clean_data)

# Summary by Community
clean_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))

# Check NULL and non-NULL counts
sum( is.na(clean_data$CommunityOld) ) #--1179
sum( !is.na(clean_data$CommunityOld) ) #--40189

# Set New Community values based on OLD Community values
clean_data$Community[ grepl("Waterfront",            clean_data$CommunityOld) == TRUE ] <- "Waterfront"
clean_data$Community[ grepl("Willowdale",            clean_data$CommunityOld) == TRUE ] <- "Willowdale"
clean_data$Community[ grepl("Mount Pleasant",        clean_data$CommunityOld) == TRUE ] <- "Mount Pleasant"
clean_data$Community[ grepl("Islington-City Centre", clean_data$CommunityOld) == TRUE ] <- "Islington-City Centre"
clean_data$Community[ grepl("Newtonbrook",           clean_data$CommunityOld) == TRUE ] <- "Newtonbrook"
clean_data$Community[ grepl("Brampton",              clean_data$CommunityOld) == TRUE ] <- "Brampton"

# Summary by Community
clean_data %>%
  group_by(Community) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

# Remove OLD Community column
clean_data = select(clean_data, -CommunityOld)
dim(clean_data) #--41110   100

# Rename column "Community" to "Neighbourhood"
colnames(clean_data)[colnames(clean_data) == "Community"] <- "Neighbourhood"

clean_data %>%
  group_by(Neighbourhood) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(20)

#--------------------------------------------------------------------------------------
# Re-group Age values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Age <- trimws(clean_data$Age)

clean_data %>% filter(Age == "New")    %>% nrow() #--970
clean_data %>% filter(Age == "0to5")   %>% nrow() #--4338
clean_data %>% filter(Age == "6to10")  %>% nrow() #--1021
clean_data %>% filter(Age == "6to15")  %>% nrow() #--2791
clean_data %>% filter(Age == "11to15") %>% nrow() #--535
clean_data %>% filter(Age == "16to30") %>% nrow() #--3266
clean_data %>% filter(Age == "31to50") %>% nrow() #--2607
clean_data %>% filter(Age == "51to99") %>% nrow() #--1878
clean_data %>% filter(Age == "100+")   %>% nrow() #--605
clean_data %>% filter(Age == "999")    %>% nrow() #--1

clean_data$Age[ clean_data$Age == "New"]    <- "0"
clean_data$Age[ clean_data$Age == "0to5"]   <- "3"
clean_data$Age[ clean_data$Age == "6to10"]  <- "8"
clean_data$Age[ clean_data$Age == "6to15"]  <- "10"
clean_data$Age[ clean_data$Age == "11to15"] <- "13"
clean_data$Age[ clean_data$Age == "16to30"] <- "23"
clean_data$Age[ clean_data$Age == "31to50"] <- "40"
clean_data$Age[ clean_data$Age == "51to99"] <- "75"
clean_data$Age[ clean_data$Age == "100+"]   <- "101"
clean_data$Age[ clean_data$Age == "999"]    <- "101"

clean_data$Age <- as.integer(clean_data$Age)

unique(clean_data$Age) %>% sort()

#--------------------------------------------------------------------------------------
# Re-group Basement values
#--------------------------------------------------------------------------------------

# Remove white spaces
clean_data$Basement <- trimws(clean_data$Basement)

# Check NULL and non-NULL counts
sum( is.na(clean_data$Basement) ) #--695
sum( !is.na(clean_data$Basement) ) #--40673

# Save OLD Basement values in a new column
clean_data$BasementOld <- clean_data$Basement
dim(clean_data)

# Set Basement = Unfinished
clean_data$Basement[!is.na(clean_data$BasementOld)]   <- "Unfinished"

# Set New Basement values based on OLD Basement values
clean_data$Basement[ grepl("Apartment",   clean_data$BasementOld) == TRUE ] <- "Finished"
clean_data$Basement[ grepl("Fin W/O",     clean_data$BasementOld) == TRUE ] <- "Finished"
clean_data$Basement[ grepl("Finished",    clean_data$BasementOld) == TRUE ] <- "Finished"
clean_data$Basement[ grepl("Full, Suite", clean_data$BasementOld) == TRUE ] <- "Finished"
clean_data$Basement[ grepl("Part Bsmt",   clean_data$BasementOld) == TRUE ] <- "Part Finished"
clean_data$Basement[ grepl("Part Fin",    clean_data$BasementOld) == TRUE ] <- "Part Finished"
clean_data$Basement[ grepl("Partial",     clean_data$BasementOld) == TRUE ] <- "Part Finished"

clean_data$Basement[clean_data$BasementOld == "None"]  <- "None"
clean_data$Basement[clean_data$BasementOld == "No"]    <- "None"

clean_data$Basement[clean_data$Type        == "Condo"] <- "None"

clean_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Remove OLD Basement column
clean_data = select(clean_data, -BasementOld)
dim(clean_data)

#--------------------------------------------------------------------------------------
# Add a new column FinishedBasement
#--------------------------------------------------------------------------------------

clean_data %>%
  group_by(Basement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

# Add a new column FinishedBasement and default to O (false)
clean_data$FinishedBasement <- 0
dim(clean_data)

# Set FinishedBasement = 1 if the column Basement is "Finished"
clean_data$FinishedBasement[ clean_data$Basement == "Finished" ] <- 1

clean_data %>%
  group_by(FinishedBasement) %>% 
  summarise(Count = n()) #--%>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
# Add a new column NearSchool
#--------------------------------------------------------------------------------------

# Add a new column NearSchool and default to O (false)
clean_data$NearSchool <- 0
dim(clean_data)

# Set NearSchool = 1 if the column Feature contain key work "School"
clean_data$NearSchool[ grepl("School", clean_data$Feature, ignore.case=TRUE) == TRUE ] <- 1

clean_data %>%
  group_by(NearSchool) %>% summarise(Count = n())

#--------------------------------------------------------------------------------------
# Add a new column NearPark
#--------------------------------------------------------------------------------------

# Add a new column NearPark and default to O (false)
clean_data$NearPark <- 0
dim(clean_data)

# Set NearSchool = 1 if the column Feature contain key work "Park"
clean_data$NearPark[ grepl("Park", clean_data$Feature, ignore.case=TRUE) == TRUE ] <- 1

clean_data %>%
  group_by(NearPark) %>% summarise(Count = n())

#--------------------------------------------------------------------------------------
## Data Cleanup for Price
#--------------------------------------------------------------------------------------

dim(clean_data) #--41368

# Check NULL and non-NULL counts
sum( is.na(clean_data$ListPrice) ) #--1183
sum( !is.na(clean_data$ListPrice) ) #--40185

clean_data <- clean_data %>% filter( !is.na(clean_data$ListPrice) )
dim(clean_data) #--40185

# Price Summary
clean_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(ListPrice),    max  = max(ListPrice), 
            median = median(ListPrice), mean = mean(ListPrice) ) %>% arrange(desc(Count))

# Check and remove outliers
boxplot(clean_data$ListPrice)

clean_data %>% filter(ListPrice < 1000)     %>% nrow() #--3
clean_data %>% filter(ListPrice < 2000)     %>% nrow() #--233
clean_data %>% filter(ListPrice < 5000)     %>% nrow() #--1187
clean_data %>% filter(ListPrice < 10000)    %>% nrow() #--1217
clean_data %>% filter(ListPrice < 100000)   %>% nrow() #--1244

clean_data %>% filter(ListPrice > 2000000)  %>% nrow() #--1104
clean_data %>% filter(ListPrice > 3000000)  %>% nrow() #--328
clean_data %>% filter(ListPrice > 4000000)  %>% nrow() #--113
clean_data %>% filter(ListPrice > 5000000)  %>% nrow() #--57
clean_data %>% filter(ListPrice > 7500000)  %>% nrow() #--17
clean_data %>% filter(ListPrice > 10000000) %>% nrow() #--6
clean_data %>% filter(ListPrice > 12000000) %>% nrow() #--4
clean_data %>% filter(ListPrice > 14000000) %>% nrow() #--1

# Remove Price outliers
clean_data <- clean_data %>% filter(ListPrice >= 100000 & ListPrice <= 5000000)
dim(clean_data) #--38884

# Double Check Price outliers
boxplot(clean_data$ListPrice)

# Price Summary
clean_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(ListPrice),    max  = max(ListPrice), 
            median = median(ListPrice), mean = mean(ListPrice) )  %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
## Data Cleanup for Taxes
#--------------------------------------------------------------------------------------

dim(clean_data) #--38884

# Check NULL and non-NULL counts
sum( is.na(clean_data$Taxes) ) #--1280
sum( !is.na(clean_data$Taxes) ) #--37604

clean_data <- clean_data %>% filter( !is.na(clean_data$Taxes) )
dim(clean_data) #--37604

# Taxes Summary
clean_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(Taxes),    max  = max(Taxes), 
            median = median(Taxes), mean = mean(Taxes) ) %>% arrange(desc(Count))

# Check and remove outliers
boxplot(clean_data$Taxes)

clean_data %>% filter(Taxes < 100)    %>% nrow() #--72
clean_data %>% filter(Taxes < 500)    %>% nrow() #--112
clean_data %>% filter(Taxes < 1000)   %>% nrow() #--389

clean_data %>% filter(Taxes > 10000)  %>% nrow() #--812
clean_data %>% filter(Taxes > 15000)  %>% nrow() #--189
clean_data %>% filter(Taxes > 20000)  %>% nrow() #--59
clean_data %>% filter(Taxes > 50000)  %>% nrow() #--7
clean_data %>% filter(Taxes > 100000) %>% nrow() #--6

# Remove Taxes outliers
clean_data <- clean_data %>% filter(Taxes >= 1000 & Taxes <= 20000)
dim(clean_data) #--37156

# Check and remove Price outliers
boxplot(clean_data$Taxes)

# Taxes Summary
clean_data %>% 
  group_by(Type) %>%
  summarise(Count  = n(), 
            min    = min(Taxes),    max  = max(Taxes), 
            median = median(Taxes), mean = mean(Taxes) )  %>% arrange(desc(Count))

#--------------------------------------------------------------------------------------
# Save the clean dataset
#--------------------------------------------------------------------------------------
#write.csv(clean_data,"housedata_clean.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------
#################### Data Analysis & Visualization ####################################
#--------------------------------------------------------------------------------------

#Get clean data
#--------------------------------------------------------------------------------------
fulldata <- clean_data 

#Ensure correct date format
#--------------------------------------------------------------------------------------
fulldata$ListDate   <-as.Date(fulldata$ListDate)
fulldata$SoldDate   <-as.Date(fulldata$SoldDate)
fulldata$ClosedDate <-as.Date(fulldata$ClosedDate)

#Selecting data for visualization
#--------------------------------------------------------------------------------------
house <- subset(fulldata,
                fulldata$Status=="Sold" & !is.na(fulldata$ListPrice) & !is.na(fulldata$SoldPrice),
                c(Status,Area,Type,Bathrooms,Bedrooms,ParkingTotal,ListPrice,SoldPrice,
                  Taxes,ListDate,SoldDate,ClosedDate,Basement,Garage,Feature,
                  Age,NearSchool,NearPark,FinishedBasement))
#View(house)
#check how many records were kept
NROW(house)
NROW(fulldata)
NROW(house)/NROW(fulldata)*100
#View(house)
#Grouping by
houseg<- house %>% select(Status,Area,Type,SoldPrice) %>% 
  group_by(Status,Area,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
#View(houseg)

#--------------------------------------------------------------------------------------
#1. Bubble plot region price and count + type
#--------------------------------------------------------------------------------------
ggplot(houseg, aes(x=Area, y=AveragePrice, size = count,color=Type)) +
   geom_point(alpha=2)+
 scale_size(range = c(.1, 24), name="Sales Count")+
ylim(200000,1500000)

#--------------------------------------------------------------------------------------
#2. Bubble plot - Price Difference vs region and house type
#--------------------------------------------------------------------------------------
houseg1<- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  group_by(Status,Area,Type) %>%  
  dplyr::summarise(AveragePriceDifference=mean(PriceDifference),count=n())
ggplot(houseg1, aes(x=Area, y=AveragePriceDifference, size = count,color=Type)) +
   geom_point(alpha=2)+
 scale_size(range = c(.1, 24), name="Sales Count")+
ylim(-25000,100000)

#--------------------------------------------------------------------------------------
#3. Boxplot - Price Difference vs region and house type
#--------------------------------------------------------------------------------------
houseg11<- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  filter(Type %in% c("Semi-Detached","Detached","Condo"))%>% 
  filter(Area %in% c("Toronto","York","Peel","Halton","Durham"))
ggplot(data=houseg11,aes(x=Area,y=PriceDifference/1000,fill=Type))+geom_boxplot()+ylim(-100,100)

#--------------------------------------------------------------------------------------
#4. Bar chart - Date Difference vs region and house type
#--------------------------------------------------------------------------------------
houseg2<- house %>% mutate(DateDiff=difftime(SoldDate, ListDate, units = "days")) %>% 
  filter(Type %in% c("Detached","Semi-Detached","Condo")) %>% 
  filter(Area %in% c("Toronto","York","Peel","Hamilton","Halton","Durham"))%>% 
  select(Status,Area,Type,DateDiff) %>% group_by(Status,Area,Type) %>%  
  dplyr::summarise(AverageDateDiff=mean(DateDiff),count=n())
#houseg2[order(houseg2$AverageDateDiff),]
#View(houseg2)
#ggplot(data=houseg2,aes(x=Area,y=AverageDateDiff))+stat_density2d(aes(color=Type))+geom_point()
#ggplot(data=houseg2,aes(x=Area,y=AverageDateDiff,shape=Type,size=count))+geom_point()
#https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
ggplot(houseg2,aes(fill=Type,x=Area,y=AverageDateDiff)) +geom_bar(position="dodge", stat="identity") + 
    ggtitle("Average Date Diff") + 
    xlab("Area") +
    ylab("Average Date Diff") +
    theme_bw()

#--------------------------------------------------------------------------------------
# Basement Finished / Unfinished Data Analysis
#--------------------------------------------------------------------------------------
sum(is.na(fulldata$Basement))/nrow(fulldata)
sum(is.na(fulldata$Feature))/nrow(fulldata)
sum(is.na(fulldata$Garage))/nrow(fulldata)

housea <- house %>% filter(Type %in% c("Detached","Semi-Detached","Townhouse"))
unique(housea$Basement)

houseb<- housea %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Basement,SoldPrice) %>% 
  group_by(Basement) %>% 
  dplyr::summarise(AverageSoldPrice=mean(SoldPrice),count=n())

housec<- houseb %>% 
  filter(Basement %in% c("Apartment","Finished","Part Finished","Unfinished"))
#check how many records were kept
housec

#--------------------------------------------------------------------------------------
#5. Basement Finished / Unfinished Pie Chart
#--------------------------------------------------------------------------------------
ggplot(housec, aes(x="",y=count, fill=Basement))+ 
  geom_bar(stat="identity",width = 1,color="white") +
  coord_polar("y",start=0)+theme_void()

#--------------------------------------------------------------------------------------
#6. Basement Finished / Unfinished Price Difference Chart
#--------------------------------------------------------------------------------------
ggplot(housec,aes(x=Basement,y=AverageSoldPrice,fill=Basement)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(breaks=seq(0,1000000,250000)) +
  xlab("Basement") +
  ylab("Average Sold Price") +
  ggtitle("Average Sold Price") + 
  theme_bw()

#--------------------------------------------------------------------------------------
#7. Age effect on Sales Volume and House Price
#--------------------------------------------------------------------------------------
houseage<- house %>% select(Age,Type,SoldPrice) %>% 
  group_by(Age,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
#View(houseage)

houseage1<- house %>% select(Age,SoldPrice) %>% 
  group_by(Age) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
#View(houseage1)
houseage2<-na.omit(houseage1)
#View(houseage2)

coeff<-1000
ggplot(houseage2, aes(Age)) +
  geom_line(aes(y=count), colour="blue") +
  geom_line(aes(y=AveragePrice/coeff), colour="red") +  
  scale_y_continuous(name = "Sales Count",
    sec.axis = sec_axis(~.*coeff, breaks =seq(0,4000000,1250000), name = "Average Price"))

#--------------------------------------------------------------------------------------
#8. School & Park effect on Sales Volume and House Price
#--------------------------------------------------------------------------------------
houseSchoolPark <- house %>% 
  select(Type, NearSchool, NearPark, SoldPrice) %>% 
  filter(Type %in% c("Detached", "Semi-Detached", "Condo")) %>%
  group_by(Type, NearSchool, NearPark) %>% 
  summarise(AveragePrice=mean(SoldPrice), count=n())
#View(houseSchoolPark)
head(houseSchoolPark)

#--------------------------------------------------------------------------------------