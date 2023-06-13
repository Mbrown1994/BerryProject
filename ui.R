# Packages used for this analysis
library(shinythemes)
library(tidyverse)
library(magrittr)
library(shiny)
library(caret)
library(tree)
library(dplyr)
library(DT)
library(ggplot2)
library(readr)
library(qcc)



# Ignore a few warnings
suppressWarnings(library(caret))


# Read in the Data
library(readr)
TICS <- read_csv("TICS.csv")

reason <- TICS %>% select(Reason)


# Define UI for application that draws a histogram
shinyUI(navbarPage(
  
  # Application title
  title = "TICs Project",
  
  # Add a theme
  theme = shinytheme("cerulean"),  
  
  # Add in the different tabs
  tabsetPanel(
    
    # Create the tab for the 'About' section of the app
    tabPanel(
      # Add the title
      title = "About",
      
      # Create a main panel for this specific tab
      mainPanel(
        
        # Add in the image
        img(
          src = "Berry.png",
          height = '300 px',
          width = '480 px'
          
        ),
        
        # Describe what this app will be used for
        h3("The Investigation"),
        "TIC dollars have increased significantly for the Charlotte plant in the year 2023.", br(),
        "This investigation is to identify and correct noticeable trends to mitigate the uptick", br(),
        "in customer complaints. Identification of a root cause is key to leading us to corrective", br(),
        "and preventive actions that are important to resolving performance problems. The use of", br(),
        "R programming and statistical approach within this investigation is necessary to pointing", br(),
        "us in the direction of which defect needs the most attention.",
        br(),
        br(),
        "Once critical defects have been identified, a plan will be put into place and tracked within", br(),
        "the CAPA portion of this file. Trend reports will be used to identify improvements as well as", br(),
        "identify the areas that still need improvement.", br(),
        
        
      ),
      sidebarPanel(
        
        # Section for discussing the data
        h3("The Data"),
       "The dataset used for this investigation includes all plant-related TICs entered for the CHAR",
       "location in 2023. Data was compiled based on product that was produced directly out of Charlotte",
       "and does not include defects related to other Berry facilities. All complaints are defined by customer,",
       "machine, mold, defect, and TIC amount.",
        br(),
        br(),
        "The following key terms will be used throughout the investigation:",
        br(),
        "TIC - Tracking Incident Credit System",
        br(),
        "Flash - Excess plastic that forms on the surface of injection molded parts",
        br(),
        "Short Shot - An incompletely filled mold cavity",
        br(),
        "Mixed Pallet - A pallet containing 1 or more boxes that are not the same item/lot as the rest",
        br(),
        "Light Color - Usually defined as start-up material",
        br(),
        "Contamination - any issue that's hazardous to human health",
        br(),
        "Voids - a visible air pocket, similar to a short",
        br(),
        "Blurred Print - smeared text, usually from pressure during the printing process",
        br(),
        "Mixed Product in Box - box contains one or more tub/lid that does not belong, although corretly labeled",
        br(),
        "Wrong Count - Not enough product was received, usually due to incorrect stack count",
        br(),
        "Out of Round - Tubs are not completely round but more of an oval shape",
        br(), 
        "Incorrect quantity shipped - not enough boxes on a pallet",
        br(),
        "Fit - Lid/Tub orientation issue",
        
      )
    ),
    
    # Create a tab for the data section of this app
    tabPanel(
      # Add the title
      title = "Data",
      
      # Create a side panel
      sidebarPanel(
        # Filter the data by plant
        selectInput(
          inputId = "specificDepartment",
          label = "Filter by Department",
          choices = unique(TICS$Department),
          selected = unique(TICS$Department),
          multiple = TRUE,
          selectize = TRUE
        ),
        
        # Filter the data by complaint
        selectInput(
          inputId = "specificComplaint",
          label = "Filter by Complaint",
          choices = c("Short Shot", "Light Color", "Mixed Pallet", "Contamination", "Voids", "Blurred Print", "Mixed Product in Box", "Wrong Count", "Out of Round", "Incorrect quantity shipped", "Flash", "Fit"),
          selected = c("Short Shot", "Light Color", "Mixed Pallet", "Contamination", "Voids", "Blurred Print", "Mixed Product in Box", "Wrong Count", "Out of Round", "Incorrect quantity shipped", "Flash", "Fit"),
          multiple = TRUE,
          selectize = TRUE
        ),
        
        # Filter by display of columns
        selectInput(
          inputId  = "selectedCols",
          label = "Filter Columns",
          choices = colnames(TICS),
          selected = colnames(TICS),
          multiple = TRUE,
          selectize = TRUE
        ),
        
        # Allow the data to be downloaded by the user
        downloadButton("Download", "Download the data here")
      
      ),
      
      mainPanel(
        dataTableOutput(outputId = "table")
      )
      
    ),
    
    # Create the drop-down tabs
    
    navbarMenu(
      # Create the title
      title = "The Investigation",
      
    tabPanel(
      title = "Data Exploration (Plant)",
      sidebarLayout(
        sidebarPanel(
          h3("Visual Summary (Plant)"),
          h4("Select data to view"),
          
    # Allows the user to decide what kind of data they want to view
      radioButtons(
        inputId = "DataChoice",
        label = "Plant TIC Data",
        choices = list("TIC reoccurance", "TIC Dollars")
      ),
    
    # Description for the user on the results
      h3("The Vital Few"),  
      "Currently, the 3 defects contributing the most to",
      "the increase in TIC dollars as well as TIC reoccurance",
      "are shorts, contamination, and flash. Shorts and flash",
      "are common defects in the injection molding process and",
      "they are the direct opposite of each other. Actions taken",
      "to eliminate shorts can cause flash and vice versa. Excess",
      "cavity pressure, mold damage, and increasing shot sizes are",
      "common actions that can result in flash. Poor venting and",
      "underpacking typically result in short shots. It's important",
      "to maintain process control so that we are not inadvertently",
      "creating both defects.",
      br(),
      br(),
      "The TICS relating to contamination have included a restacker",
      "belt, a screw, and a couple for grease contamination. For",
      "better understanding where potential contamination oppurtunities",
      "lie within the plant, we are performing walk-thrus of each department",
      "to better understand our risk associated."
    
    ),
    
    mainPanel(
      # Put the plots in the main panel
      h3("Visual Summary"),
      plotOutput("ParetoChart"),
      br(),
      br(),
  
       )
    )
    
      ),

    tabPanel(
      title = "Data Exploration (Molding)",
      sidebarLayout(
        sidebarPanel(
         h3("Visual Summary (Molding)"),
         h4("Select Data to View"),
         
      # Allow the user to decide what kind of data they want to view
        radioButtons(
          inputId = "machinechoice",
          label = "View TIC data by mold or machine",
          choices = list ("Mold", "Machine")
        ),
        
      h3("Description of TICS (Molding)"),
      "In reviewing the TICS by mold, the 14oz shows to",
      "be the biggest issue in CHAR. When filtered and",
      "and reviewed by machine, MC17 has produced the most",
      "non-conforming product resulting in TICS. Shorts and",
      "voids are the biggest issue shown with both the machine",
      "and the mold."
      
        ),
        
        mainPanel(
          # Put the plots in the main panel
          h3("Visual Summary"),
          plotOutput("MoldChart"),
          )  
      
        
        
        )),
    
    tabPanel(
      title = "Data Exploration (Print)",
      sidebarLayout(
        sidebarPanel(
          h3("Visual Summary (Print)"),
          h4("Select Data to View"),
          
        # View data
        radioButtons(
          inputId = "printchoice",
          label = "Machine TIC data by dollars or occurance",
          choices = list ("TIC Amount", "TIC Count", "TIC Reason")
          
        ),
        
        h3("Description of TICS (Print)"),
        "TICS for the print department seem to be more evenly",
        "spread across machines. In terms of TIC dollars, TPC09",
        "is responsible for two TICS - one of them costing 25,495.35",
        "TPC04 is the machine related to the restacker belt issue, ",
        "explaining the amount of TIC dollars off of that machine.",
        br(),
        br(),
        "In reviewing TIC reoccurances, light print is the most common",
        "defect. This is an area for improvement and will be the main",
        "focus for the department moving forward."

   ),
        
        mainPanel(
          h3("Visual Summary"),
          plotOutput("PrintChart")
          
        ))
      
    ),

      
    ))
    
    )
)

