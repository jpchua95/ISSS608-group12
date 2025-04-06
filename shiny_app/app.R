pacman::p_load(shiny, shinycssloaders, tidyverse, plotly, sf, tmap, GGally, ggstatsplot, ggmosaic, bslib, RColorBrewer, scales, shinyWidgets, purrr, MASS, mgcv, Metrics, gt)

dengue_daily <- read_csv("data/dengue_daily_en.csv")
dengue_daily_sf <- st_as_sf(dengue_daily, 
                            coords = c("X_coord", "Y_coord"), 
                            crs = 3824)
dengue_daily_outbreak <- read_csv("data/dengue_daily_outbreak.csv")
wave1 <- read.csv("data/wave1.csv")
wave2 <- read.csv("data/wave2.csv")
wave3 <- read.csv("data/wave3.csv")

tmap_mode("view")


ui <- page_navbar(
    bg = "#70784d",
    
    title = "Exploring Dengue in Taiwan: Unraveling Taiwan's Outbreak Pattern",
    theme = bslib::bs_theme(
        bg = "white",
        fg = "#2D2F32",
        primary = "#70784d",
    ),
    
    nav_spacer(),
    
    # Overview Panel
    nav_panel("Overview",
              navset_tab(
                  
                  # Overview page, first tab for Overview of Dengue Cases in Taiwan
                  nav_panel("Overview of Dengue Cases in Taiwan", 
                  card(
                      layout_columns(
                          col_widths = c(4, 8), 
                          fill = TRUE,  
                          card(
                              card_header("Filters"),
                              card_body(
                                  style = "min-height: 300px;",
                                  selectInput("tw_map_age", "Select Age Group:",
                                              choices = c("All", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                                          "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                                          "60-64", "65-69", "70+"),
                                              selected = "All"),
                                  selectInput("tw_map_gender", "Select Gender:",
                                              choices = c("All", "Male" = "M", "Female" = "F"),
                                              selected = "All"),
                                  selectInput("tw_map_imported", "Select Imported or Local Case:",
                                              choices = c("All", "Imported" = "Yes", "Local" = "No"),
                                              selected = "All"),
                                  selectInput("tw_map_serotype", "Select Serotype",
                                              choices = c("All", "Type 1", "Type 2", "Type 3", "Type 4"),
                                              selected = "All"),
                                  sliderInput("tw_map_variable_year", "Select Year:",
                                              min = min(dengue_daily_sf$Onset_Year),
                                              max = max(dengue_daily_sf$Onset_Year),
                                              value = max(dengue_daily_sf$Onset_Year),
                                              step = 1, animate = TRUE, sep = ""),
                                  sliderInput("tw_map_variable_epiweek", "Select Week Range:",
                                              min = min(dengue_daily_sf$Onset_Epiweek),
                                              max = max(dengue_daily_sf$Onset_Epiweek),
                                              value = c(min(dengue_daily_sf$Onset_Epiweek), max(dengue_daily_sf$Onset_Epiweek)),
                                              step = 1, animate = TRUE, sep = "")
                              )
                          ),
                          
                          # Map Output (Middle)
                          card(
                              card_header("Distribution of Dengue Cases"),
                              card_body(
                                  tmapOutput("tw_map") %>% withSpinner(color = "#70784d"),
                                  fluidRow(
                                      column(12,
                                             h4("Selected Year:"),
                                             tags$h4(tags$strong(textOutput("selected_year"))),
                                             h4("Total Dengue Cases in Selected Year:"),
                                             tags$h4(tags$strong(textOutput("total_dengue_cases"))),
                                             h4("County with the Most Cases in Selected Year:"),
                                             tags$h4(tags$strong(textOutput("top_county")))
                                             )
                                      )
                                  )
                              ),
                          )
                      )
                  ),
                  
                  # Overview page, second tab for Dengue Cases Over the Years
                  nav_panel("Dengue Cases Over the Years", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            selectInput(inputId = "chart_variable",
                                                        label = "Select a variable:",
                                                        choices = c("Age Group" = "Age_Group",
                                                                    "Gender",
                                                                    "Imported Case" = "Imported_Case",
                                                                    "Serotype" = "Serotype",
                                                                    "County" = "Residential_County_City"),
                                                        selected = "Age Group"),
                                            sliderInput("chart_year", "Year(s) to Compare:",
                                                        min = min(dengue_daily_sf$Onset_Year),
                                                        max = max(dengue_daily_sf$Onset_Year),
                                                        value = c(min(dengue_daily_sf$Onset_Year), max(dengue_daily_sf$Onset_Year)),
                                                        step = 1, animate = TRUE, sep = "")
                                            )
                                        ),
                                    # Line & Bar Chart
                                    card(
                                        card_header("Distribution over the Years"),
                                        card_body(
                                            plotlyOutput("bar_chart") %>% withSpinner(color = "#70784d"),
                                            plotlyOutput("line_chart") %>% withSpinner(color = "#70784d")
                                            )
                                        )
                                    )
                                )
                            )
                  )
              ),
              
    
    # CDA
    nav_panel("Confirmatory Data Analysis",
              navset_tab(
                  
                  # CDA page, first tab
                  nav_panel("Years", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,  
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            selectInput("cda_variable_year", "Select a Variable",
                                                        choices = c("Age Group" = "Age_Group",
                                                                    "Gender",
                                                                    "Imported Case" = "Imported_Case",
                                                                    "Serotype",
                                                                    "County" = "Residential_County_City"),
                                                        selected = "Age Group"),
                                            selectInput("cda_test_type_year", "Type of Statistical Approach",
                                                        choices = c("Parametric" = "p", 
                                                                    "Non-parametric" = "np"),
                                                        selected = "Parametric"),
                                            sliderInput("cda_test_year", "Year(s) to Compare:",
                                                        min = min(dengue_daily_sf$Onset_Year),
                                                        max = max(dengue_daily_sf$Onset_Year),
                                                        value = c(min(dengue_daily_sf$Onset_Year), max(dengue_daily_sf$Onset_Year)),
                                                        step = 1, animate = TRUE, sep = ""),
                                            actionButton(inputId = "run_cda_year",
                                                         label = "Run Test")
                                            
                                        )
                                    ),
                                    
                                    # CDA results
                                    card(
                                        card_header("Results"),
                                        card_body(
                                            plotOutput("cda_plot_year", height = "600px")
                                        )
                                    ),
                                )
                            )
                            ),
                  
                  # CDA page, second tab
                  nav_panel("Epidemiological Week", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,  
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            selectInput("cda_variable_epiweek", "Select a Variable",
                                                        choices = c("Age Group" = "Age_Group",
                                                                    "Gender",
                                                                    "Imported Case" = "Imported_Case",
                                                                    "Serotype",
                                                                    "County" = "Residential_County_City"),
                                                        selected = "Age Group"),
                                            selectInput("cda_test_type_epiweek", "Type of Statistical Approach",
                                                        choices = c("Parametric" = "p", 
                                                                    "Non-parametric" = "np"),
                                                        selected = "Parametric"),
                                            sliderInput("cda_test_epiweek", "Epiweek(s) to Compare:",
                                                        min = min(dengue_daily_sf$Onset_Epiweek),
                                                        max = max(dengue_daily_sf$Onset_Epiweek),
                                                        value = c(min(dengue_daily_sf$Onset_Epiweek), max(dengue_daily_sf$Onset_Epiweek)),
                                                        step = 1, animate = TRUE, sep = ""),
                                            actionButton(inputId = "run_cda_epiweek",
                                                         label = "Run Test")
                                            
                                        )
                                    ),
                                    
                                    # CDA results
                                    card(
                                        card_header("Results"),
                                        card_body(
                                            plotOutput("cda_plot_epiweek", height = "600px")
                                        )
                                    ),
                                )
                            )
                  ),
                  # CDA page, third tab
                  nav_panel("Counties", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,  
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            selectInput("cda_variable_county", "Select a Variable",
                                                        choices = c("Age Group" = "Age_Group",
                                                                    "Gender",
                                                                    "Imported Case" = "Imported_Case",
                                                                    "Serotype"),
                                                        selected = "Age Group"),
                                            selectInput("cda_test_type_county", "Type of Statistical Approach",
                                                        choices = c("Parametric" = "p", 
                                                                    "Non-parametric" = "np"),
                                                        selected = "Parametric"),
                                            actionButton(inputId = "run_cda_county",
                                                         label = "Run Test")
                                            )
                                            
                                        ),
                                    
                                    # CDA results
                                    card(
                                        card_header("Results"),
                                        card_body(
                                            plotOutput("cda_plot_county", height = "600px")
                                        )
                                    ),
                                )
                            )
                  )
                  )
              ),
    
    # Time Series Analysis
    nav_panel("Time Series Analysis",
              navset_tab(
                  
                  # Time Series page, EDA
                  nav_panel("Overview of Outbreak Waves", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,  
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            selectInput("ts_variable", "Select a Variable",
                                                        choices = c("Age Group" = "Age_Group",
                                                                    "Gender",
                                                                    "Imported Case" = "Imported_Case",
                                                                    "Serotype"),
                                                        selected = "Age Group"),
                                            selectInput("ts_wave", "Outbreak Wave(s) to Compare",
                                                        choices = c("2014" = "Wave 1", 
                                                                    "2015" = "Wave 2",
                                                                    "2023" = "Wave 3",
                                                                    "2014 & 2015" = "Wave 1,2",
                                                                    "2014 & 2023" = "Wave 1,3",
                                                                    "2015 & 2023" = "Wave 2,3",
                                                                    "All 3 Waves" = "All"),
                                                        selected = "All")
                                        )
                                    ),
                                    
                                    # EDA results
                                    card(
                                        card_header("Results"),
                                        card_body(
                                            plotOutput("ts_eda_chart", height = "600px")
                                        )
                                    ),
                                )
                            )
                  ),
                  
                  # Time Series page, curve-fitting
                  nav_panel("Curve Fitting", 
                            card(
                                layout_columns(
                                    col_widths = c(4, 8), 
                                    fill = TRUE,  
                                    card(
                                        card_header("Filter"),
                                        card_body(
                                            #selecting model
                                            pickerInput(
                                                inputId = "model",
                                                label = "Select Models",
                                                choices = c("Week-based", "Cumulative Week-based", "Progress-based"),
                                                multiple = FALSE
                                            ),
                                            
                                            conditionalPanel(condition = "input.model == 'Week-based'", tagList(
                                                pickerInput(
                                                    inputId = "curve",
                                                    label = "Select Curve(s)",
                                                    choices = c(
                                                        "Exponential",
                                                        "GAM",
                                                        "Gamma",
                                                        "Lognormal",
                                                        "Negative Binomial",
                                                        "Poisson",
                                                        "Polynomial"
                                                    ),
                                                    multiple = TRUE,
                                                    options = list(
                                                        `actions-box` = TRUE,
                                                        `selected-text-format` = "count > 2",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                
                                                pickerInput(
                                                    inputId = "metric",
                                                    label = "Select Goodness of Fit Display(s)",
                                                    choices = c("AIC", "Residuals", "RMSE"),
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE)
                                                )
                                            )),
                                            
                                            # if choose cumulative
                                            conditionalPanel(condition = "input.model == 'Cumulative Week-based'", tagList(
                                                pickerInput(
                                                    inputId = "curve",
                                                    label = "Select Curve(s)",
                                                    choices = c(
                                                        "Cumulative Exponential",
                                                        "Cumulative GAM",
                                                        "Cumulative Gamma",
                                                        "Cumulative Lognormal",
                                                        "Cumulative Negative Binomial",
                                                        "Cumulative Poisson",
                                                        "Cumulative Polynomial"
                                                    ),
                                                    multiple = TRUE,
                                                    options = list(
                                                        `actions-box` = TRUE,
                                                        `selected-text-format` = "count > 2",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                
                                                pickerInput(
                                                    inputId = "metric",
                                                    label = "Select Goodness of Fit Display(s)",
                                                    choices = c("AIC", "Residuals", "RMSE"),
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE)
                                                )
                                            )),
                                            
                                            #choosing progress
                                            conditionalPanel(condition = "input.model == 'Progress-based'", tagList(
                                                pickerInput(
                                                    inputId = "curve",
                                                    label = "Select Curve(s)",
                                                    choices = c(
                                                        "Progress Exponential",
                                                        "Progress GAM",
                                                        "Progress Negative Binomial",
                                                        "Progress Poisson",
                                                        "Progress Polynomial"
                                                    ),
                                                    multiple = TRUE,
                                                    options = list(
                                                        `actions-box` = TRUE,
                                                        `selected-text-format` = "count > 2",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                pickerInput(
                                                  inputId = "predict",
                                                  label = "Select Outbreak to Predict",
                                                  choices = c("2014 Outbreak", "2015 Outbreak", "2023 Outbreak"),
                                                  multiple = FALSE
                                                )
                                            )),
                                            
                                            # if choose polynomial
                                            conditionalPanel(
                                                condition = "input.curve &&
               (input.curve.includes('Polynomial') || input.curve.includes('Cumulative Polynomial')) &&
               !input.curve.includes('Progress Polynomial')",
                                                pickerInput(
                                                    inputId = "degree",
                                                    label = "Select Degree of Freedom for Polynomial",
                                                    choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    multiple = FALSE
                                                )
                                            ),
                                            
                                            pickerInput(
                                                inputId = "train",
                                                label = "Select Training Data",
                                                choices = c("2014 Outbreak", "2015 Outbreak", "2023 Outbreak"),
                                                multiple = FALSE
                                            )
                                            )
                                        ),
                                    
                                    # Curve fitting results
                                    card(
                                        card_header("Fitted Curves"),
                                        card_body(
                                            plotOutput("fitPlot", height = "430px")
                                        )
                                    )),
                                uiOutput("dynamicPlots")
                            )
                  )
              ))
)




server <- function(input, output) {
    
    
    # Overview of Dengue Cases in Taiwan - Map Output
    output$tw_map <- renderTmap({
        
        req(
            input$tw_map_variable_year,
            input$tw_map_variable_epiweek,
            input$tw_map_age,
            input$tw_map_gender,
            input$tw_map_imported,
            input$tw_map_serotype
        )
        
        aggregated  <- dengue_daily_sf %>%
            filter(Onset_Year == input$tw_map_variable_year) %>%
            filter(Onset_Epiweek >= input$tw_map_variable_epiweek[1] &
                       Onset_Epiweek <= input$tw_map_variable_epiweek[2])
        
        if (input$tw_map_age != "All") {
            aggregated <- aggregated %>%
                filter(Age_Group == input$tw_map_age)
        }
        
        if (input$tw_map_gender != "All") {
            aggregated <- aggregated %>%
                filter(Gender == input$tw_map_gender)
        }
        
        if (input$tw_map_imported != "All") {
            aggregated <- aggregated %>%
                filter(Imported_Case == input$tw_map_imported)
        }
        
        if (input$tw_map_serotype != "All") {
            aggregated <- aggregated %>%
                filter(Serotype == input$tw_map_serotype)
        }
        
        if (nrow(aggregated) == 0) {
            showNotification("No dengue cases found. Please select another set of variables.", type = "warning")
            return(NULL)
        }
        print(aggregated)
        
        dengue_aggregated <- aggregated %>%
            group_by(Gender, Age_Group, Serotype, Imported_Case, Residential_County_City, geometry) %>%
            summarize(Count = n(), .groups = "drop")
        
        tm_shape(dengue_aggregated) + 
            tm_bubbles(fill = "Residential_County_City",
                       # size = "Count",
                       col = "black",
                       lwd = 1,
                       popup.vars = c("County" = "Residential_County_City", "Cases" = "Count"))
    })
    
    # Overview of Dengue Cases in Taiwan - Stats
    output$selected_year <- renderText({
        paste(input$tw_map_variable_year)
    })
    
    output$total_dengue_cases <- renderText({
        aggregated <- dengue_daily_sf %>%
            filter(Onset_Year == input$tw_map_variable_year) %>%
            summarize(Sum = n(), .groups = "drop")
        total_cases_sum <- sum(aggregated$Sum)
        paste(total_cases_sum)
    })
    
    output$top_county <- renderText({
        aggregated <- dengue_daily_sf %>%
            filter(Onset_Year == input$tw_map_variable_year)
        top_county <- aggregated %>%
            group_by(Residential_County_City) %>%
            summarize(Count = n(), .groups = "drop") %>%
            arrange(desc(Count)) %>%
            slice(1)
        paste(top_county$Residential_County_City, " with ", top_county$Count, " cases")
    })
    
    # Dengue Cases Over the Years
    dengue_summary <- reactive({
        
        dengue_summary_data <- dengue_daily %>%
            filter(Onset_Year >= input$chart_year[1],
                   Onset_Year <= input$chart_year[2])
        
        if (input$chart_variable == "Serotype") {
            dengue_summary_data <- dengue_summary_data %>% filter(!is.na(Serotype))
            }
        
        dengue_summary_data %>%
            group_by(!!sym(input$chart_variable), Onset_Year) %>%
            summarize(Count = n(), .groups = "drop")
        })

  # Bar Chart
  output$bar_chart <- renderPlotly({
      
      bar <- ggplot(dengue_summary(), aes(x = Onset_Year, 
                                   y = Count, 
                                   fill = factor(.data[[input$chart_variable]]))) +
          geom_bar(stat = "identity") +
          labs(title = paste("Dengue Cases by", input$chart_variable, "(Bar Chart)"),
               x = "Year",
               y = "Count of Cases",
               fill = input$chart_variable) +
          theme_minimal()
      
      ggplotly(bar)
      })

  # Line Chart
  output$line_chart <- renderPlotly({
      
      line <- ggplot(dengue_summary(), aes(x = Onset_Year, 
                                   y = Count, 
                                   color = factor(.data[[input$chart_variable]]))) +
          geom_line(size = 1) +
          labs(title = paste("Dengue Cases by", input$chart_variable, "(Line Chart)"),
               x = "Year",
               y = "Count of Cases",
               color = input$chart_variable) +
          theme_minimal() 
      
      ggplotly(line)
      })
  
  ##############
  # CDA
  # CDA - by Year
  cda_run_test_year <- eventReactive(input$run_cda_year, {
      req(input$cda_test_year, input$cda_variable_year, input$cda_test_type_year)
      grouped_data_year <- dengue_daily_sf %>%
          filter(Onset_Year >= input$cda_test_year[1],
                 Onset_Year <= input$cda_test_year[2]) %>% 
          group_by(Onset_Year, !!sym(input$cda_variable_year)) %>%
          summarize(Count = n(), .groups = "drop")
      return(grouped_data_year)
  })
  
  output$cda_plot_year <- renderPlot({
      
      req(cda_run_test_year())  
      grouped_data_year <- cda_run_test_year() 
      
      ggbetweenstats( 
          data = grouped_data_year,
          x = Onset_Year, 
          y = Count, 
          type = input$cda_test_type_year,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  
  # CDA - by Epiweek
  cda_run_test_epiweek <- eventReactive(input$run_cda_epiweek, {
      req(input$cda_variable_epiweek, input$cda_test_type_epiweek)
      grouped_data_epiweek <- dengue_daily_sf %>%
          filter(Onset_Epiweek >= input$cda_test_epiweek[1],
                 Onset_Epiweek <= input$cda_test_epiweek[2]) %>% 
          group_by(Onset_Epiweek, !!sym(input$cda_variable_epiweek)) %>%
          summarize(Count = n(), .groups = "drop")
      return(grouped_data_epiweek)
  })
  
  output$cda_plot_epiweek <- renderPlot({
      
      req(cda_run_test_epiweek())  
      grouped_data_epiweek <- cda_run_test_epiweek() 
      
      ggbetweenstats( 
          data = grouped_data_epiweek,
          x = Onset_Epiweek, 
          y = Count, 
          type = input$cda_test_type_epiweek,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  
  # CDA - by Counties
  cda_run_test_county <- eventReactive(input$run_cda_county, {
      req(input$cda_test_county, input$cda_variable_county, input$cda_test_type_county)
      grouped_data_county <- dengue_daily_sf %>%
          group_by(Residential_County_City, !!sym(input$cda_variable_county)) %>%
          summarize(Count = n(), .groups = "drop")
      return(grouped_data_county)
  })
  
  output$cda_plot_county <- renderPlot({
      
      req(cda_run_test_county())  
      grouped_data_county <- cda_run_test_county() 
      
      ggbetweenstats( 
          data = grouped_data_county,
          x = Residential_County_City, 
          y = Count, 
          type = input$cda_test_type_county,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  ##############
  # Time Series EDA
  ts_eda <- reactive({
      data <- dengue_daily_outbreak %>% filter(!is.na(Wave))  # Ensure valid Wave data
      
      # Filter by selected outbreak wave(s)
      selected_waves <- switch(input$ts_wave,
                               "Wave 1" = c("Wave 1"),
                               "Wave 2" = c("Wave 2"),
                               "Wave 3" = c("Wave 3"),
                               "Wave 1,2" = c("Wave 1", "Wave 2"),
                               "Wave 1,3" = c("Wave 1", "Wave 3"),
                               "Wave 2,3" = c("Wave 2", "Wave 3"),
                               "All" = c("Wave 1", "Wave 2", "Wave 3"))
      
      data <- data %>% filter(Wave %in% selected_waves)
      
      # If "Serotype" is selected, remove NA values in Serotype
      if (input$ts_variable == "Serotype") {
          data <- data %>% filter(!is.na(Serotype))
      }
      
      data
  })
  
  output$ts_eda_chart <- renderPlot({
      req(ts_eda()) 
      
      summary_data <- ts_eda() %>%
          group_by(Wave, !!sym(input$ts_variable)) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(Wave) %>%
          mutate(prop = count / sum(count))
      
      color_palettes <- list(
          "Age_Group" = c("mistyrose", "darkcyan", "snow3", "orchid4", "royalblue", "burlywood2", "maroon", "cadetblue", "lightsalmon3", "lightpink1", "aquamarine4", "plum4", "indianred", "lightsalmon", "antiquewhite"),
          "Gender" = RColorBrewer::brewer.pal(3, "Pastel1"),
          "Imported_Case" = RColorBrewer::brewer.pal(3, "Pastel1"),
          "Serotype" = RColorBrewer::brewer.pal(4, "Pastel1")
      )
      
      selected_palette <- color_palettes[[input$ts_variable]]
      
      ggplot(summary_data, aes(x = Wave, y = prop, fill = !!sym(input$ts_variable))) +
          geom_col(position = "fill") +
          scale_x_discrete(labels = c(
              "Wave 1" = "2014 Outbreak Wave",
              "Wave 2" = "2015 Outbreak Wave",
              "Wave 3" = "2023 Outbreak Wave"
          )) +
          labs(title = paste(input$ts_variable, "Proportion by Outbreak Year"),
               y = "Proportion", x = "Outbreak Year") +
          scale_y_continuous(labels = scales::percent) +
          theme_minimal() +
          scale_fill_manual(values = selected_palette)
      
  })
  
  
  ##############
  # Time Series Analysis
  # Helper function to read correct dataset
  get_wave <- function(wave_name) {
      switch(
          wave_name,
          "2014 Outbreak" = wave1,
          "2015 Outbreak" = wave2,
          "2023 Outbreak" = wave3
      )
  }
  
  # week-based ----------------------------------------------------------
  fitted_model <- reactive({
      req(input$curve, input$model == "Week-based")
      data <- get_wave(input$train)
      
      if (!"week_index" %in% names(data) ||
          !"cases" %in% names(data)) {
          data <- data %>%
              mutate(Onset_Date = as.Date(Onset_Date)) %>%
              group_by(week = floor_date(Onset_Date, unit = "week")) %>%
              summarise(cases = n(), .groups = "drop") %>%
              mutate(week_index = row_number())
      }
      
      aic_values <- list()
      residuals_long <- list()
      rmse_values <- list()
      
      all_preds <- purrr::map_dfr(input$curve, function(curve) {
          model <- switch(
              curve,
              "Exponential"       = glm(
                  log(cases + 1) ~ week_index,
                  family = gaussian(),
                  data = data
              ),
              "Lognormal"         = glm(
                  log(cases + 1) ~ week_index,
                  family = gaussian(),
                  data = data
              ),
              "GAM"               = mgcv::gam(
                  cases ~ s(week_index),
                  family = poisson(),
                  data = data
              ),
              "Gamma"             = glm(
                  cases ~ week_index,
                  family = Gamma(link = "log"),
                  data = data
              ),
              "Negative Binomial" = MASS::glm.nb(cases ~ week_index, data = data),
              "Poisson"           = glm(cases ~ week_index, family = poisson(), data = data),
              "Polynomial"        = glm(
                  cases ~ poly(week_index, as.numeric(input$degree)),
                  family = poisson(),
                  data = data
              )
          )
          
          preds <- switch(
              curve,
              "Exponential"       = exp(predict(model)),
              "Lognormal"         = exp(predict(model)),
              "GAM"               = predict(model, type = "response"),
              "Gamma"             = predict(model, type = "response"),
              "Negative Binomial" = predict(model, type = "response"),
              "Poisson"           = predict(model, type = "response"),
              "Polynomial"        = predict(model, type = "response")
          )
          
          aic_values[[curve]] <<- AIC(model)
          rmse_values[[curve]] <<- sqrt(mean((data$cases - preds)^2))
          
          residuals_long[[curve]] <<- tibble(
              week_index = data$week_index,
              residual = residuals(model, type = "response"),
              model = curve
          )
          
          tibble(
              week_index = data$week_index,
              fitted     = preds,
              model      = curve
          )
      })
      
      aics <- tibble(Model = names(aic_values),
                     AIC = unlist(aic_values))
      residuals_all <- bind_rows(residuals_long)
      rmses <- tibble(Model = names(rmse_values),
                      RMSE = unlist(rmse_values))
      
      list(
          data = data,
          preds = all_preds,
          aics = aics,
          residuals = residuals_all,
          rmses = rmses
      )
  })
  
  # cumulative week-based ----------------------------------------------------------
  fitted_model_cumulative <- reactive({
      req(input$curve, input$model == "Cumulative Week-based")
      data <- get_wave(input$train)
      
      if (!"week_index" %in% names(data) ||
          !"cases" %in% names(data)) {
          data <- data %>%
              mutate(Onset_Date = as.Date(Onset_Date)) %>%
              group_by(week = floor_date(Onset_Date, unit = "week")) %>%
              summarise(cases = n(), .groups = "drop") %>%
              mutate(week_index = row_number())
      }
      
      aic_values <- list()
      residuals_long <- list()
      rmse_values <- list()
      
      all_preds <- purrr::map_dfr(input$curve, function(curve) {
          clean_name <- gsub("^Cumulative ", "", curve)
          
          model <- switch(
              clean_name,
              "Exponential"       = glm(
                  log(cases + 1) ~ week_index,
                  family = gaussian(),
                  data = data
              ),
              "Lognormal"         = glm(
                  log(cases + 1) ~ week_index,
                  family = gaussian(),
                  data = data
              ),
              "GAM"               = mgcv::gam(
                  cases ~ s(week_index),
                  family = poisson(),
                  data = data
              ),
              "Gamma"             = glm(
                  cases ~ week_index,
                  family = Gamma(link = "log"),
                  data = data
              ),
              "Negative Binomial" = MASS::glm.nb(cases ~ week_index, data = data),
              "Poisson"           = glm(cases ~ week_index, family = poisson(), data = data),
              "Polynomial"        = glm(
                  cases ~ poly(week_index, as.numeric(input$degree)),
                  family = poisson(),
                  data = data
              )
          )
          
          preds <- switch(
              clean_name,
              "Exponential"       = exp(predict(model)),
              "Lognormal"         = exp(predict(model)),
              "GAM"               = predict(model, type = "response"),
              "Gamma"             = predict(model, type = "response"),
              "Negative Binomial" = predict(model, type = "response"),
              "Poisson"           = predict(model, type = "response"),
              "Polynomial"        = predict(model, type = "response")
          )
          
          cumulative_preds <- cumsum(preds)
          
          aic_values[[curve]] <<- AIC(model)
          rmse_values[[curve]] <<- sqrt(mean((
              cumsum(data$cases) - cumulative_preds
          )^2))
          
          residuals_long[[curve]] <<- tibble(
              week_index = data$week_index,
              residual = residuals(model, type = "response"),
              model = curve
          )
          
          tibble(
              week_index = data$week_index,
              fitted     = cumsum(preds),
              model      = curve
          )
      })
      
      data <- data %>% mutate(cases = cumsum(cases))
      aics <- tibble(Model = names(aic_values),
                     AIC = unlist(aic_values))
      residuals_all <- bind_rows(residuals_long)
      rmses <- tibble(Model = names(rmse_values),
                      RMSE = unlist(rmse_values))
      
      list(
          data = data,
          preds = all_preds,
          aics = aics,
          residuals = residuals_all,
          rmses = rmses
      )
  })
  
  
  # plotting curve + AIC + residuals for week-based and cumulative week-based
  output$aicPlot <- renderPlot({
      req("AIC" %in% input$metric)
      if (input$model == "Week-based") {
          data <- fitted_model()$aics
      } else {
          data <- fitted_model_cumulative()$aics
      }
      
      ggplot(data, aes(
          x = reorder(Model, AIC),
          y = AIC,
          fill = Model
      )) +
          geom_col() +
          geom_text(aes(label = round(AIC, 1)), vjust = -0.5, size = 4) +
          scale_fill_brewer(palette = "Pastel1") +
          theme_minimal() +
          labs(title = "AIC Values by Model", x = "Model", y = "AIC") +
          theme(legend.position = "none")
  })
  
  output$rmsePlot <- renderPlot({
      req("RMSE" %in% input$metric)
      if (input$model == "Week-based") {
          data <- fitted_model()$rmses
      } else {
          data <- fitted_model_cumulative()$rmses
      }
      
      ggplot(data, aes(
          x = reorder(Model, RMSE),
          y = RMSE,
          fill = Model
      )) +
          geom_col() +
          geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, size = 4) +
          scale_fill_brewer(palette = "Pastel1") +
          theme_minimal() +
          labs(title = "RMSE Comparison Across Models", x = "Model", y = "RMSE") +
          theme(legend.position = "none")
  })
  
  
  output$fitPlot <- renderPlot({
      if (input$model == "Cumulative Week-based") {
          fit <- fitted_model_cumulative()
          title_text <- "Cumulative Fitted Curves"
      } else {
          fit <- fitted_model()
          title_text <- "Fitted Curves"
      }
      
      ggplot(fit$data, aes(x = week_index)) +
          geom_point(
              aes(y = cases),
              color = "black",
              alpha = 0.6,
              size = 2
          ) +
          geom_line(data = fit$preds,
                    aes(y = fitted, color = model),
                    size = 1) +
          labs(
              title = title_text,
              x = "Week Index",
              y = "Number of Cases",
              color = "Model"
          ) +
          theme_minimal()
  })
  
  #residuals plot
  output$residualPlot <- renderPlot({
      req("Residuals" %in% input$metric)
      fit <- if (input$model == "Week-based") {
          fitted_model()
      } else {
          fitted_model_cumulative()
      }
      
      ggplot(fit$residuals, aes(x = week_index, y = residual, color = model)) +
          geom_hline(
              yintercept = 0,
              linetype = "dashed",
              color = "gray50"
          ) +
          geom_line(size = 0.7) +
          facet_wrap(~ model, scales = "free_y") +
          labs(title = "Residuals for All Models", x = "Week Index", y = "Residuals") +
          theme_minimal()
  })
  
  # progress-based -----------------------------
  #------------------ PROGRESS MODEL REACTIVE --------------------------
  fitted_model_progress <- reactive({
      req(input$model == "Progress-based",
          input$curve,
          input$train,
          input$predict)
      
      if (input$train == input$predict) {
          return(NULL)  # don't proceed if training == prediction
      }
      
      train_data <- get_wave(input$train) %>%
          arrange(week_index) %>%
          mutate(cum_cases = cumsum(cases),
                 progress = cum_cases / max(cum_cases))
      
      predict_data <- get_wave(input$predict) %>%
          arrange(week_index) %>%
          mutate(cum_cases = cumsum(cases),
                 progress = cum_cases / max(cum_cases))
      
      preds_all <- map_dfr(input$curve, function(curve) {
          clean_name <- gsub("^Progress ", "", curve)
          
          model <- switch(
              clean_name,
              "Exponential" = glm(
                  log(cases + 1) ~ progress,
                  family = gaussian(),
                  data = train_data
              ),
              "GAM"         = gam(
                  cases ~ s(progress),
                  family = poisson(),
                  data = train_data
              ),
              "Negative Binomial" = glm.nb(cases ~ poly(progress, 4), data = train_data),
              "Poisson"     = glm(
                  cases ~ poly(progress, 4),
                  family = poisson(),
                  data = train_data
              ),
              "Polynomial"  = glm(
                  cases ~ poly(progress, 4),
                  family = poisson(),
                  data = train_data
              )
          )
          
          predictions <- switch(
              clean_name,
              "Exponential" = exp(predict(model, newdata = predict_data)) - 1,
              "GAM"         = predict(model, newdata = predict_data, type = "response"),
              "Negative Binomial" = predict(model, newdata = predict_data, type = "response"),
              "Poisson"     = predict(model, newdata = predict_data, type = "response"),
              "Polynomial"  = predict(model, newdata = predict_data, type = "response")
          )
          
          tibble(
              progress = predict_data$progress,
              fitted = predictions,
              model = rep(curve, length(predictions)),
              week_index = predict_data$week_index
          )
      })
      
      list(data = get_wave(input$predict), preds = preds_all)
  })
  
  
  
  output$fitPlot <- renderPlot({
      if (input$model == "Week-based") {
          fit <- fitted_model()
          title_text <- "Fitted Curves (Week-based)"
          y_data <- fit$data$cases
          x_var <- "week_index"
      } else if (input$model == "Cumulative Week-based") {
          fit <- fitted_model_cumulative()
          title_text <- "Fitted Curves (Cumulative)"
          y_data <- fit$data$cases
          x_var <- "week_index"
      } else if (input$model == "Progress-based") {
          validate(
              need(
                  input$train != input$predict,
                  "Training and Prediction Outbreak must be different."
              )
          )
          
          fit <- fitted_model_progress()
          req(fit)  # In case NULL is returned
          
          title_text <- paste(
              "Fitted Curves (Progress-based)\nTrained on",
              input$train,
              "→ Predicted on",
              input$predict
          )
          y_data <- fit$data$cases
          x_var <- "progress"
      }
      
      ggplot() +
          geom_point(
              data = fit$data,
              aes_string(x = x_var, y = "cases"),
              color = "black",
              alpha = 0.6,
              size = 2
          ) +
          geom_line(
              data = fit$preds,
              aes_string(x = x_var, y = "fitted", color = "model"),
              linewidth = 1
          ) +
          labs(
              title = title_text,
              x = ifelse(
                  input$model == "Progress-based",
                  "Progress (0 to 1)",
                  "Week Index"
              ),
              y = "Number of Cases",
              color = "Model"
          ) +
          theme_minimal()
  })
  output$dynamicPlots <- renderUI({
      plots <- list()
      
      # Check input$metric and render in the order the user selected
      if (!is.null(input$metric)) {
          if ("AIC" %in% input$metric) {
              plots[[length(plots) + 1]] <- card(
                  card_header("AIC Plot"),
                  card_body(
                      plotOutput("aicPlot", height = "300px")
                  )
              )
          }
          if ("Residuals" %in% input$metric) {
              plots[[length(plots) + 1]] <- card(
                  card_header("Residual Plot"),
                  card_body(
                      plotOutput("residualPlot", height = "300px")
                  )
              )
          }
          if ("RMSE" %in% input$metric) {
              plots[[length(plots) + 1]] <- card(
                  card_header("RMSE Plot"),
                  card_body(
                      plotOutput("rmsePlot", height = "300px")
                  )
              )
          }
      }
      
      layout_column_wrap(width = 1, fill = TRUE, !!!plots)
  })
  
}


shinyApp(ui = ui, server = server)
