#--------------------------------------------------------------------------------------
#server.R
#--------------------------------------------------------------------------------------
#################### Server code for ShinyApps ########################################
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
if(!require(scales))
  install.packages("scales")

#--------------------------------------------------------------------------------------
## Data Preparation for Server for ShinyApps ##########################################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
# Read Data
#--------------------------------------------------------------------------------------
fullhousedata  <- read.csv("property_listings.csv")
fullrentaldata <- read.csv("rental_listings.csv")
rates_data     <- read.csv("mortgage_rates.csv")

#--------------------------------------------------------------------------------------
# Selecting data for visualization
#--------------------------------------------------------------------------------------
house <- subset(fullhousedata, fullhousedata$Status=="Sold" & 
		!is.na(fullhousedata$ListPrice) & 
		!is.na(fullhousedata$SoldPrice),
                c(Status,Area,Neighbourhood,Type,Rooms,Bathrooms,Bedrooms,ParkingTotal,ListPrice,SoldPrice,
                  Taxes,ListDate,SoldDate,ClosedDate,Basement,FinishedBasement,Garage,Feature,Age))
#View(house)
#check how many records were kept
NROW(house)
NROW(fullhousedata)
NROW(house)/NROW(fullhousedata)*100
#change date format
house$ClosedDate<-as.Date(house$ClosedDate)
house$SoldDate<-as.Date(house$SoldDate)
house$ListDate<-as.Date(house$ListDate)
#View(house)
#Grouping by
houseg<- house %>% select(Status,Area,Type,SoldPrice) %>% 
  group_by(Status,Area,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
#View(houseg)

#--------------------------------------------------------------------------------------
# Basement data for visualization
#--------------------------------------------------------------------------------------
sum(is.na(fullhousedata$Basement))/nrow(fullhousedata)
sum(is.na(fullhousedata$Feature))/nrow(fullhousedata)
sum(is.na(fullhousedata$Garage))/nrow(fullhousedata)

housea <- house %>% filter(Type %in% c("Detached","Semi-Detached","Townhouse"))
unique(housea$Basement)

houseb<- housea %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Basement,SoldPrice) %>% 
  group_by(Basement) %>% 
  dplyr::summarise(AverageSoldPrice=mean(SoldPrice),count=n())

housec<- houseb %>% 
  filter(Basement %in% c("Apartment","Finished","Part Finished","Unfinished"))
#check how many records were kept
#housec

#--------------------------------------------------------------------------------------
# House Data Preparation
#--------------------------------------------------------------------------------------
house_data <- house %>%
  subset(Type %in% c("Condo", "Detached") &
         !is.na(house$Taxes) &
         !is.na(house$SoldPrice) ) %>%
  mutate(ListPrice  <- as.integer(ListPrice) ) %>%
  mutate(SoldPrice  <- as.integer(SoldPrice) ) %>%
  select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Taxes, ListPrice, SoldPrice)

#--------------------------------------------------------------------------------------
# Rental Data Preparation
#--------------------------------------------------------------------------------------
rental_data <- fullrentaldata %>%
  subset(Type %in% c("Condo", "Detached") & 
         !is.na(fullrentaldata$Price) ) %>%
  select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Price)

#--------------------------------------------------------------------------------------
# Helpercode code for Rental Profit Calculator
#--------------------------------------------------------------------------------------
# Function: calculateMortgagepayment
#--------------------------------------------------------------------------------------
calculateMortgagepayment <- function(HomePrice, PercentDown, InterestRate, TermInYears)
{
  if (HomePrice >= 1000000 & PercentDown < 20) {
    PercentDown <- as.integer(20)
  }
  else if (HomePrice > 500000 & PercentDown < 10) {
    MinimumDownPayment <- (500000 * 0.05) + (HomePrice - 500000) * 0.10
    PercentDown     <- MinimumDownPayment / HomePrice * 100
  }

  DownPayment    <- HomePrice * PercentDown / 100
  MortgageAmount <- HomePrice - DownPayment

  if (PercentDown < 10) {
    MortgageInsurance <-   MortgageAmount * 4.00 / 100
  }
  else if (PercentDown < 15) {
    MortgageInsurance <-   MortgageAmount * 3.10 / 100
  }
  else if (PercentDown < 20) {
    MortgageInsurance <-   MortgageAmount * 2.80 / 100
  }
  else {
    MortgageInsurance <- 0.0
  }
  
  TermInYears <- as.integer(TermInYears)
  
  if (MortgageInsurance > 0.0 & TermInYears > 25) {
    TermInYears <- as.integer(25)
  }

  P <- MortgageAmount + MortgageInsurance
  i <- InterestRate / 100 / 12
  n <- TermInYears * 12

  M <- P * ( i * (1+i)^n ) / ( (1+i)^n - 1 )

  TotalLoanAmount <- P
  MortgagePayment  <- M

  MortgagePaymentTable <- data.frame(MortgagePayment, DownPayment, MortgageAmount,
                                     MortgageInsurance, TotalLoanAmount,
                                     PercentDown, InterestRate, TermInYears)  
  return(MortgagePaymentTable)
}

#--------------------------------------------------------------------------------------
#################### Define Server logic for ShinyApps ################################
#--------------------------------------------------------------------------------------

server <- function(input, output) {

#==================== HouseDataTable ====================
output$HouseDataTable <- DT::renderDataTable({

  houseDataTable <- house_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Taxes, ListPrice, SoldPrice)

  attach(houseDataTable)
  houseDataTable <- houseDataTable[order(ListPrice),]
  detach(houseDataTable)

  houseDataTable <- houseDataTable %>%
    mutate(Taxes       = formatC(Taxes,     format="d", big.mark=",") ) %>%
    mutate(ListPrice   = formatC(ListPrice, format="d", big.mark=",") ) %>%
    mutate(SoldPrice   = formatC(SoldPrice, format="d", big.mark=",") )

  DT::datatable(houseDataTable, options = list(pageLength = 5) )
})

#==================== HousePriceTable ====================
output$HousePriceTable <- renderTable({
  
  HousePriceSummary <- house_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Taxes, ListPrice, SoldPrice) %>%
    group_by(Area,Neighbourhood,Type) %>%
    dplyr::summarise( NumberOfHomes = n(),
                      MinimumHomePrice = as.integer( min(ListPrice) ),
                      AverageHomePrice = as.integer( mean(ListPrice) ),
                      MaximumHomePrice = as.integer( max(ListPrice) ) )
  
  attach(HousePriceSummary)
  HousePriceTable <- data.frame(AverageHomePrice, MinimumHomePrice, MaximumHomePrice, NumberOfHomes )
  detach(HousePriceSummary)

  HousePriceTable <- HousePriceTable %>%
    mutate(AverageHomePrice = formatC(AverageHomePrice, format="d", big.mark=",") ) %>%
    mutate(MinimumHomePrice = formatC(MinimumHomePrice, format="d", big.mark=",") ) %>%
    mutate(MaximumHomePrice = formatC(MaximumHomePrice, format="d", big.mark=",") )
})

#==================== PropertyTaxTable ====================
output$PropertyTaxTable <- renderTable({
  
  PropertyTaxSummary <- house_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Taxes, ListPrice, SoldPrice) %>%
    group_by(Area,Neighbourhood,Type) %>%
    dplyr::summarise( NumberOfHomes = n(),
                      MinimumPropertyTax = as.integer( min(Taxes) ),
                      AveragePropertyTax = as.integer( mean(Taxes) ),
                      MaximumPropertyTax = as.integer( max(Taxes) ) )
  
  attach(PropertyTaxSummary)
  PropertyTaxTable <- data.frame(AveragePropertyTax, MinimumPropertyTax, MaximumPropertyTax, NumberOfHomes )
  detach(PropertyTaxSummary)
  
  PropertyTaxTable <- PropertyTaxTable %>%
    mutate(AveragePropertyTax = formatC(AveragePropertyTax, format="d", big.mark=",") ) %>%
    mutate(MinimumPropertyTax = formatC(MinimumPropertyTax, format="d", big.mark=",") ) %>%
    mutate(MaximumPropertyTax = formatC(MaximumPropertyTax, format="d", big.mark=",") )
})

#==================== LandTransferTaxTable ====================
output$LandTransferTaxTable <- renderTable({
  
  HomePrice <- input$HomePrice

  if (HomePrice <= 55000) {
    MLTT <- HomePrice * 0.50 / 100
    PLTT <- MLTT
  }
  else if (HomePrice <= 250000) {
      MLTT <- HomePrice * 1.00 / 100
      PLTT <- MLTT - 275
  }
  else if (HomePrice <= 400000) {
    MLTT <- HomePrice * 1.50 / 100
    PLTT <- MLTT - 1525
  }
  else if (HomePrice <= 2000000) {
    MLTT <- HomePrice * 2.00 / 100
    PLTT <- MLTT - 3525
  }
  else {
    MLTT <- HomePrice * 2.50 / 100
    PLTT <- MLTT - 13525
  }
  LTT <- MLTT + PLTT

  HomePrice = formatC(HomePrice, format="d", big.mark=",")
  TorontoLandTransferTax = formatC(MLTT, format="d", big.mark=",")
  OntarioLandTransferTax = formatC(PLTT, format="d", big.mark=",")
  TotalLandTransferTax   = formatC(LTT,  format="d", big.mark=",")

  LandTransferTaxTable <- data.frame(TotalLandTransferTax, TorontoLandTransferTax,
                                     OntarioLandTransferTax, HomePrice)
})

#==================== MortgagePaymentTable ====================
output$MortgagePaymentTable <- renderTable({

  HomePrice    <- input$HomePrice
  PercentDown  <- input$PercentDown
  InterestRate <- input$Interest
  TermInYears <- as.integer(input$Term)

  # Calculate mortgage payment
  MortgagePaymentTable <- calculateMortgagepayment(HomePrice, PercentDown, InterestRate, TermInYears)

  MortgagePaymentTable <- MortgagePaymentTable %>%
    mutate(DownPayment        = formatC(DownPayment,        format="d", big.mark=",") ) %>%
    mutate(MortgageAmount    = formatC(MortgageAmount,    format="d", big.mark=",") ) %>%
    mutate(MortgageInsurance = formatC(MortgageInsurance, format="d", big.mark=",") ) %>%
    mutate(TotalLoanAmount  = formatC(TotalLoanAmount,  format="d", big.mark=",") ) %>%
    mutate(MortgagePayment   = formatC(MortgagePayment,   format="d", big.mark=",") )
})

#==================== InterestRateTable ====================
output$InterestRateTable <- DT::renderDataTable({

  InterestRateTable <- rates_data
  InterestRateTable <- select(InterestRateTable, -ID)

  attach(InterestRateTable)
  InterestRateTable <- InterestRateTable[order(Rate),]
  detach(InterestRateTable)

  InterestRateTable$Rate <- InterestRateTable$Rate * 100
  colnames(InterestRateTable)[colnames(InterestRateTable) == "Rate"] <- "RateInPercent"
  colnames(InterestRateTable)[colnames(InterestRateTable) == "Term"] <- "TermInYears"

  DT::datatable(InterestRateTable, options = list(pageLength = 10) )
})

#==================== RentalDataTable ====================
output$RentalDataTable <- DT::renderDataTable({
  
  rentalDataTable <- rental_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    mutate(Price = formatC(Price, format="d", big.mark=",") ) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Price)
  
  attach(rentalDataTable)
  rentalDataTable <- rentalDataTable[order(Price),]
  detach(rentalDataTable)
  
  DT::datatable(rentalDataTable, options = list(pageLength = 5) )
})

#==================== RentalPriceTable ====================
output$RentalPriceTable <- renderTable({

  RentalPriceSummary <- rental_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Price) %>%
    group_by(Area,Neighbourhood,Type) %>%
    dplyr::summarise( NumberOfRentals = n(),
                      MinimumRentPrice = as.integer( min(Price) ),
                      AverageRentPrice = as.integer( mean(Price) ),
                      MaximumRentPrice = as.integer( max(Price) ) )

  attach(RentalPriceSummary)
  RentalPriceTable <- data.frame(AverageRentPrice, MinimumRentPrice, MaximumRentPrice, NumberOfRentals)
  detach(RentalPriceSummary)

  RentalPriceTable <- RentalPriceTable %>%
    mutate(AverageRentPrice = formatC(AverageRentPrice, format="d", big.mark=",") ) %>%
    mutate(MinimumRentPrice = formatC(MinimumRentPrice, format="d", big.mark=",") ) %>%
    mutate(MaximumRentPrice = formatC(MaximumRentPrice, format="d", big.mark=",") )
})

#==================== RentalProfitTable ====================
output$RentalProfitTable <- renderTable({

  RentalPriceSummary <- rental_data %>%
    filter(Neighbourhood == input$Neighbourhood) %>%
    filter(Area      == input$Area) %>%
    filter(Type      == input$Type) %>%
    filter(Bedrooms  == input$Bedrooms) %>%
    filter(Bathrooms == input$Bathrooms) %>%
    select(Area, Neighbourhood, Type, Rooms, Bedrooms, Bathrooms, Basement, Price) %>%
    group_by(Area,Neighbourhood,Type) %>%
    dplyr::summarise( NumberOfRentals = n(),
                      MinimumRentPrice = as.integer( min(Price) ),
                      AverageRentPrice = as.integer( mean(Price) ),
                      MaximumRentPrice = as.integer( max(Price) ) )

  HomePrice    <- input$HomePrice
  PercentDown  <- input$PercentDown
  InterestRate <- input$Interest
  TermInYears <- as.integer(input$Term)

  # Calculate mortgage payment
  MortgagePaymentTable <- calculateMortgagepayment(HomePrice, PercentDown, InterestRate, TermInYears)

  MonthlyRent    <- input$MonthlyRent
  MonthlyProfit  <- MonthlyRent - MortgagePaymentTable$MortgagePayment

  MonthlyRent    = formatC(MonthlyRent,   format="d", big.mark=",")
  MonthlyProfit  = formatC(MonthlyProfit, format="d", big.mark=",")

  RentalProfitTable <- data.frame(MonthlyRent, MonthlyProfit)
})

#==================== Market Analysis Tab ====================
output$plot1 <- reactivePlot(function() {

#--------------------------------------------------------------------------------------
# Plotting chart based on input option
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
#1. Bubble plot - Region and House Type effect on House Price and Sales Volume
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "1.Region and House Type effect on Price and Sales Volume") {
plot1 <- ggplot(houseg, aes(x=Area, y=AveragePrice, size = count,color=Type)) +
   geom_point(alpha=2)+
   scale_size(range = c(.1, 24), name="Sales Count")+
   ylim(200000,1500000)
}

#--------------------------------------------------------------------------------------
#2. Bubble plot - Region and House Type effect on Sold Price Diff
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "2.Region and House Type effect on Sold Price Diff-Bubble") {
houseg1 <- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  group_by(Status,Area,Type) %>%  
  dplyr::summarise(AveragePriceDifference=mean(PriceDifference),count=n())
plot1 <- ggplot(houseg1, aes(x=Area, y=AveragePriceDifference, size = count,color=Type)) +
   geom_point(alpha=2)+
   scale_size(range = c(.1, 24), name="Sales Count")+
   ylim(-25000,100000)
}

#--------------------------------------------------------------------------------------
#3. Box plot - Region and House Type effect on Sold Price Diff
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "3.Region and House Type effect on Sold Price Diff-Boxplot") {
houseg11 <- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  filter(Type %in% c("Semi-Detached","Detached","Condo"))%>% 
  filter(Area %in% c("Toronto","York","Peel","Halton","Durham"))
plot1 <- ggplot(data=houseg11,aes(x=Area,y=PriceDifference/1000,fill=Type))+
  geom_boxplot()+ylim(-100,100)
}

#--------------------------------------------------------------------------------------
#4. Bar chart - Region and House Type effect on Time on Market
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "4.Region and House Type effect on Time on Market") {
houseg2 <- house %>% mutate(DateDiff=difftime(SoldDate, ListDate, units = "days")) %>% 
  filter(Type %in% c("Detached","Semi-Detached","Condo")) %>% 
  filter(Area %in% c("Toronto","York","Peel","Hamilton","Halton","Durham"))%>% 
  select(Status,Area,Type,DateDiff) %>% group_by(Status,Area,Type) %>%  
  dplyr::summarise(AverageDateDiff=mean(DateDiff),count=n())
#houseg2[order(houseg2$AverageDateDiff),]
#View(houseg2)
plot1 <- ggplot(houseg2,aes(fill=Type,x=Area,y=AverageDateDiff)) +
    geom_bar(position="dodge", stat="identity") + 
    ggtitle("Average Date Diff") + 
    xlab("Area") +
    ylab("Average Date Diff") +
    theme_bw()
}

#--------------------------------------------------------------------------------------
#5. Pie chart - Finished / Unfinished Basement effect on Market Share
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "5.Finished / Unfinished Basement effect on Market Share") {
plot1 <- ggplot(housec, aes(x="",y=count, fill=Basement))+ 
  geom_bar(stat="identity",width = 1,color="white") +
  coord_polar("y",start=0)+
  theme_void()
}

#--------------------------------------------------------------------------------------
#6. Bar chart - Finished / Unfinished Basement effect on Sold Price 
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "6.Finished / Unfinished Basement effect on Sold Price") {
plot1 <- ggplot(housec,aes(x=Basement,y=AverageSoldPrice,fill=Basement)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(breaks=seq(0,1000000,250000)) + 
  xlab("Basement") +
  ylab("Average Sold Price") +
  ggtitle("Average Sold Price") + 
  theme_bw()
}

#--------------------------------------------------------------------------------------
#7. Age effect on Sales Volume and House Price
#--------------------------------------------------------------------------------------
if (input$visualizeOption == "7.Age effect on Sales Volume and House Price") {
houseage<- house %>% select(Age,Type,SoldPrice) %>% 
  group_by(Age,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())

houseage1<- house %>% select(Age,SoldPrice) %>% 
  group_by(Age) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
houseage2<-na.omit(houseage1)

coeff <- 1000
plot1 <- ggplot(houseage2, aes(Age)) +
  geom_line(aes(y=count), colour="blue") +
  geom_line(aes(y=AveragePrice/coeff), colour="red") +  
  scale_y_continuous(name = "Sales Count",
	sec.axis = sec_axis(~.*coeff, breaks =seq(0,4000000,1250000), name = "Average Price"))
}
  
#Return the Plot chart
#--------------------------------------------------------------------------------------
print(plot1)
}

)}

#--------------------------------------------------------------------------------------