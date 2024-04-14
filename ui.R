#--------------------------------------------------------------------------------------
#ui.R
#--------------------------------------------------------------------------------------
#################### UI code for ShinyApps ############################################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
## Install and load the required packages
#--------------------------------------------------------------------------------------
if(!require(shiny))
  install.packages("shiny")
if(!require(shinythemes))
  install.packages("shinythemes")

if(!require(dplyr))
  install.packages("dplyr")
if(!require(tidyr)) 
  install.packages("tidyr")

#--------------------------------------------------------------------------------------
## Data Preparation for UI for ShinyApps ##############################################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
# Read Data
#--------------------------------------------------------------------------------------
fullhousedata  <- read.csv("property_listings.csv")
fullrentaldata <- read.csv("rental_listings.csv")
rates_data     <- read.csv("mortgage_rates.csv")

house_data  <- fullhousedata
rental_data <- fullrentaldata

#--------------------------------------------------------------------------------------
# Rental Data Preparation
#--------------------------------------------------------------------------------------
# Top Type
top_type <- rental_data %>% 
  group_by(Type) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% head(1)
top_type <- as.character(top_type$Type)
top_type

# Top Areas
top_areas <- rental_data %>% 
  group_by(Area) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% select(Area) %>% head(10)
top_area <- as.character(top_areas$Area %>% head(1))
top_area

# Top Neighbourhoods
top_neighbourhoods <- rental_data %>%
  group_by(Neighbourhood) %>% 
  summarise(Count = n()) %>% arrange(desc(Count)) %>% select(Neighbourhood) %>% head(20)
top_neighbourhood <- as.character(top_neighbourhoods$Neighbourhood %>% head(1))
top_neighbourhood

top_bedrooms <- 3
top_bathrooms <- 2

top_rent_data <- rental_data %>% filter(Area == top_area)
dim(top_rent_data) #--9844   57 #--42

rent_min  <- as.integer( min(top_rent_data$Price)  / 100 ) * 100
rent_max  <- as.integer( max(top_rent_data$Price)  / 100 + 1 ) * 100
rent_avg  <- as.integer( mean(top_rent_data$Price) / 100 + 1 ) * 100
rent_avg  <- as.integer( (rent_min + rent_max) / 2 / 100 + 1 ) * 100
rent_step <- 100
rent_min
rent_avg
rent_max

#--------------------------------------------------------------------------------------
# House Data Preparation
#--------------------------------------------------------------------------------------
top_house_data <- house_data %>% filter(Area == top_area)
dim(top_house_data) #--11191   100 #--12

hprice_min  <- as.integer( min(top_house_data$ListPrice)  / 1000   + 1 ) * 1000
hprice_avg  <- as.integer( mean(top_house_data$ListPrice) / 10000  + 1 ) * 10000
hprice_max  <- as.integer( max(top_house_data$ListPrice)  / 10000  + 1 ) * 10000
hprice_step <- 5000
hprice_min
hprice_avg
hprice_max

taxes_min  <- as.integer( min(top_house_data$Taxes)  / 100 ) * 100
taxes_max  <- as.integer( max(top_house_data$Taxes)  / 100 + 1 ) * 100
taxes_avg  <- as.integer( mean(top_house_data$Taxes) / 100 + 1 ) * 100
taxes_step <- 100
taxes_min
taxes_avg
taxes_max

#--------------------------------------------------------------------------------------
# Rates Data Preparation
#--------------------------------------------------------------------------------------
imin  <- min(rates_data$Rate) * 100
imax  <- max(rates_data$Rate) * 100
istep <- 0.01

#--------------------------------------------------------------------------------------
#################### Define UI for ShinyApps ##########################################
#--------------------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    "Toronto RealEstate Market",id = "inTabset",
    #==================== Rental Profit Calculator ====================
    tabPanel("Rental Profit Calculator", icon = icon("chart-line"),
      sidebarPanel(
        selectInput("Area", "Area:", top_area, ""),
        selectInput("Neighbourhood", "Neighbourhood:", top_neighbourhoods, ""),
        radioButtons("Type", "Property Type:", c("Condo", "Detached") ),
        sliderInput("Bedrooms",    "No. of Bedrooms:",  min=1,max=10,value=2,step=1),
        sliderInput("Bathrooms",   "No. of Bathrooms:", min=1,max=10,value=2,step=1)
      ),
      mainPanel(
        h4("Home Listing Data:"),
        DT::dataTableOutput("HouseDataTable")
      ),
      mainPanel(
        HTML('<hr style="height:50px; color:purple;">'),
        h4("Home Prices:"),
        tableOutput("HousePriceTable"),
        h4("Property Taxes:"),
        tableOutput("PropertyTaxTable")
      ),
      sidebarPanel(
        sliderInput("HomePrice",   "Home Price:", min=hprice_min,max=hprice_max,value=hprice_avg,step=hprice_step),
        sliderInput("PercentDown", "Down Payment (%):", min=5,max=100,value=20,step=5),
        sliderInput("Interest",    "Interest Rate:", min=imin,max=imax,value=imin,step=istep),
        selectInput("Term",        "Length of loan (years):", c(10,15,20,25,30),25),
      ),
      mainPanel(
        HTML('<hr style="height:50px; color:purple;">'),
        h4("Land Transfer Tax:"),
        tableOutput("LandTransferTaxTable"),
        h4("Monthly Mortgage Payment:"),
        tableOutput("MortgagePaymentTable")
      ),
      mainPanel(
        HTML('<hr style="height:50px; color:purple;">'),
        h4("Rental Data:"),
        DT::dataTableOutput("RentalDataTable")
      ),
      sidebarPanel(sliderInput("MonthlyRent", "Monthly Rent:",
                               min=rent_min,max=rent_max,value=rent_avg,step=rent_step)
      ),
      mainPanel(
        HTML('<hr style="height:50px; color:purple;">'),
        h4("Monthly Rental Prices:"),
        tableOutput("RentalPriceTable"),
        h4("Monthly Rental Profit:"),
        tableOutput("RentalProfitTable")
      )
    ),
    #==================== About Calculator ============================
    tabPanel("About Rental Profit Calculator", icon = icon("cloud"),
      includeHTML("About_Rental_Profit_Calculator.html")
    ),
    #==================== About Interest Rate =========================
    tabPanel("About Interest Rate", icon = icon("table"),
      mainPanel(
        DT::dataTableOutput("InterestRateTable")
      ),
    ),
    #==================== Market Analysis =============================
    tabPanel("Market Analysis", icon = icon("chart-bar"),
      sidebarPanel(
        selectInput(inputId = "visualizeOption",
          label = "Housing Market - Visualization:",
          choices = c("1.Region and House Type effect on Price and Sales Volume",
                      "2.Region and House Type effect on Sold Price Diff-Bubble",
                      "3.Region and House Type effect on Sold Price Diff-Boxplot",
                      "4.Region and House Type effect on Time on Market",
                      "5.Finished / Unfinished Basement effect on Market Share",
                      "6.Finished / Unfinished Basement effect on Sold Price",
                      "7.Age effect on Sales Volume and House Price"
                      ),
          selected =  ""),
      ), 
      mainPanel(
        plotOutput(outputId = "plot1", height = "600px")
      )
    )
))

#--------------------------------------------------------------------------------------