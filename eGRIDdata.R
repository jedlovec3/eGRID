
library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)

plant_data_cnames <- read_excel("eGRID2021_data.xlsx", sheet = "PLNT21", skip = 1, n_max = 0) %>% 
  names()

plant_data <- read_excel("eGRID2021_data.xlsx", sheet = "PLNT21", skip = 2, col_names = plant_data_cnames)

plant_data <- plant_data %>% 
  mutate_if(is.numeric, ~replace_na(., 0))


operator_totals <- plant_data %>% 
    #filter(PSTATABB == "PA") %>% 
    group_by(OPRNAME, OPRCODE, ISORTO) %>% #, PLPRMFL, PLFUELCT) %>% 
    summarize(total_generation = sum(PLNGENAN), total_capacity = sum(NAMEPCAP), coal_generation = sum(PLGENACL), oil_generation = sum(PLGENAOL), gas_generation = sum(PLGENAGS), nuclear_generation = sum(PLGENANC), hydro_generation = sum(PLGENAHY), biomass_generation = sum(PLGENABM), wind_generation = sum(PLGENAWI), solar_generation = sum(PLGENASO), geothermal_generation = sum(PLGENAGT), other_fossil_generation = sum(PLGENAOF), other_generation = sum(PLGENAOP), renewables_generation = sum(PLGENATR), nox_emissions =  sum(PLNOXAN), so2_emissions = sum(PLSO2AN), co2_emissions = sum(PLCO2AN), ch4_emissions = sum(PLCH4AN), co2_eq_emissions = sum(PLCO2EQA)) %>% 
    mutate(renewables_pct = renewables_generation/total_generation, fossil_fuel_pct = (coal_generation + oil_generation + gas_generation + other_fossil_generation)/total_generation, co2_eq_rate = co2_eq_emissions/total_generation) %>% 
    arrange(desc(total_generation)) %>%
    mutate_if(is.numeric, round, 3) %>% 
    relocate(c(renewables_pct, fossil_fuel_pct, co2_eq_rate), .after = total_generation) 

state_totals <- plant_data %>%
    group_by(PSTATABB) %>% 
    summarize(total_generation = sum(PLNGENAN), total_capacity = sum(NAMEPCAP), coal_generation = sum(PLGENACL), oil_generation = sum(PLGENAOL), gas_generation = sum(PLGENAGS), nuclear_generation = sum(PLGENANC), hydro_generation = sum(PLGENAHY), biomass_generation = sum(PLGENABM), wind_generation = sum(PLGENAWI), solar_generation = sum(PLGENASO), geothermal_generation = sum(PLGENAGT), other_fossil_generation = sum(PLGENAOF), other_generation = sum(PLGENAOP), renewables_generation = sum(PLGENATR), nox_emissions =  sum(PLNOXAN), so2_emissions = sum(PLSO2AN), co2_emissions = sum(PLCO2AN), ch4_emissions = sum(PLCH4AN), co2_eq_emissions = sum(PLCO2EQA)) %>% 
    mutate(renewables_pct = renewables_generation/total_generation, fossil_fuel_pct = (coal_generation + oil_generation + gas_generation + other_fossil_generation)/total_generation, co2_eq_rate = co2_eq_emissions/total_generation) %>% 
    arrange(desc(co2_eq_rate)) %>%
    mutate_if(is.numeric, round, 3) %>% 
    relocate(c(renewables_pct, fossil_fuel_pct, co2_eq_rate), .after = total_generation)


iso_totals <- plant_data %>%
    group_by(ISORTO) %>% 
    summarize(total_generation = sum(PLNGENAN), total_capacity = sum(NAMEPCAP), coal_generation = sum(PLGENACL), oil_generation = sum(PLGENAOL), gas_generation = sum(PLGENAGS), nuclear_generation = sum(PLGENANC), hydro_generation = sum(PLGENAHY), biomass_generation = sum(PLGENABM), wind_generation = sum(PLGENAWI), solar_generation = sum(PLGENASO), geothermal_generation = sum(PLGENAGT), other_fossil_generation = sum(PLGENAOF), other_generation = sum(PLGENAOP), renewables_generation = sum(PLGENATR), nox_emissions =  sum(PLNOXAN), so2_emissions = sum(PLSO2AN), co2_emissions = sum(PLCO2AN), ch4_emissions = sum(PLCH4AN), co2_eq_emissions = sum(PLCO2EQA)) %>% 
    mutate(renewables_pct = renewables_generation/total_generation, fossil_fuel_pct = (coal_generation + oil_generation + gas_generation + other_fossil_generation)/total_generation, co2_eq_rate = co2_eq_emissions/total_generation) %>% 
    arrange(desc(co2_eq_rate)) %>%
    mutate_if(is.numeric, round, 3) %>% 
    relocate(c(renewables_pct, fossil_fuel_pct, co2_eq_rate), .after = total_generation)


ui <- fluidPage(
  title = "USA Electricity Generation in 2021",
      tabsetPanel(
        id = 'dataset',
        tabPanel("Operator", DT::dataTableOutput("mytable1")),
        tabPanel("State", DT::dataTableOutput("mytable2")),
        tabPanel("RTO/ISO", DT::dataTableOutput("mytable3"), 
                 img(src='Rto_map.gif'), 
                 uiOutput("tab"))
      )
    # )
  # )
)

server <- function(input, output) {
  
  url <- a("RTO/ISO Map", href="https://en.wikipedia.org/wiki/Regional_transmission_organization_%28North_America%29#/media/File:Rto_map.gif")
  output$tab <- renderUI({
    tagList(url)
  })
    
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(operator_totals, options = list(orderClasses = TRUE, options = list(lengthMenu = c(10, 25), pageLength = 10)))
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(state_totals, options = list(orderClasses = TRUE, pageLength = 52))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iso_totals, options = list(orderClasses = TRUE, pageLength = 10))
  })
  
}

shinyApp(ui, server)

#Create sidebar option to decide how many operators (largest X) to include using operator_totals %>% head(X)
#move image to third tab sidebar
#Add units
#Add explanation
#add source for data

