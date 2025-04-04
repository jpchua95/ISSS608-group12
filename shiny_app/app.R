pacman::p_load(shiny, shinycssloaders, tidyverse, plotly, sf, tmap, GGally, ggstatsplot, ggmosaic, bslib, RColorBrewer, scales, shinyWidgets, purrr, MASS, mgcv, Metrics, gt)

dengue_daily <- read_csv("data/dengue_daily_en.csv")
dengue_daily_sf <- st_as_sf(dengue_daily, 
                            coords = c("X_coord", "Y_coord"), 
                            crs = 3824)
dengue_daily_outbreak <- read_csv("data/dengue_daily_outbreak.csv")


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
                                            plotOutput("bar_chart") %>% withSpinner(color = "#70784d"),
                                            plotOutput("line_chart") %>% withSpinner(color = "#70784d")
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
                                            
                                            pickerInput(
                                                inputId = "model",
                                                label = "Select Models",
                                                choices = c("Week-based", "Cumulative Week-based", "Progress-based"),
                                                multiple = FALSE,
                                                options = list(`style` = "btn-success")
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
                                                        `style` = "btn-success",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                
                                                pickerInput(
                                                    inputId = "metric",
                                                    label = "Select Goodness of Fit Display(s)",
                                                    choices = c("AIC", "Residuals", "RMSE"),
                                                    selected = c("AIC", "Residuals", "RMSE"),
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE, `style` = "btn-success")
                                                )
                                            )),
                                            
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
                                                        `style` = "btn-success",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                
                                                pickerInput(
                                                    inputId = "metric",
                                                    label = "Select Goodness of Fit Display(s)",
                                                    choices = c("AIC", "Residuals", "RMSE"),
                                                    multiple = TRUE,
                                                    options = list(`actions-box` = TRUE, `style` = "btn-success")
                                                )
                                            )),
                                            
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
                                                        `style` = "btn-success",
                                                        `live-search` = TRUE
                                                    )
                                                ),
                                                pickerInput(
                                                    inputId = "predict",
                                                    label = "Select Outbreak to Predict",
                                                    choices = c("2014 Outbreak", "2015 Outbreak", "2023 Outbreak"),
                                                    multiple = FALSE,
                                                    options = list(`style` = "btn-success")
                                                )
                                            )),
                                            
                                            conditionalPanel(
                                                condition = "input.curve &&
                       (input.curve.includes('Polynomial') || input.curve.includes('Cumulative Polynomial')) &&
                       !input.curve.includes('Progress Polynomial')",
                                                
                                                pickerInput(
                                                    inputId = "degree",
                                                    label = "Select Degree of Freedom for Polynomial",
                                                    choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    multiple = FALSE,
                                                    options = list(`style` = "btn-success")
                                                )
                                            ),
                                            
                                            pickerInput(
                                                inputId = "train",
                                                label = "Select Training Data",
                                                choices = c("2014 Outbreak", "2015 Outbreak", "2023 Outbreak"),
                                                multiple = FALSE,
                                                options = list(`style` = "btn-success")
                                            ),
                                            
                                            pickerInput(
                                                inputId = "actual",
                                                label = "Select Actual Wave(s) Data",
                                                choices = c("2014 Outbreak", "2015 Outbreak", "2023 Outbreak"),
                                                multiple = TRUE,
                                                options = list(`actions-box` = TRUE, `style` = "btn-success")
                                            )
                                        )
                                    ),
                                    
                                    # Curve fitting results
                                    card(
                                        card_header("Results"),
                                        card_body(
                                            plotOutput("ts_output2", height = "600px")
                                        )
                                    ),
                                )
                            )
                  )
              ))
)




server <- function(input, output) {
    
    
    # Overview of Dengue Cases in Taiwan - Map Output
    output$tw_map <- renderTmap({
        tmap_mode("view")
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
            return("No dengue cases found. Please select another set of variables.")
        }
        
        dengue_aggregated <- aggregated %>%
            group_by(Gender, Age_Group, Serotype, Imported_Case, Residential_County_City, geometry) %>%
            summarize(Count = n(), .groups = "drop")
        
        tm_shape(dengue_aggregated) + 
            tm_bubbles(fill = "Residential_County_City",
                       # size = "Count",
                       col = "black",
                       lwd = 1)
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
  output$bar_chart <- renderPlot({
      
      ggplot(dengue_summary(), aes(x = Onset_Year, y = Count, fill = factor(.data[[input$chart_variable]]))) +
          geom_bar(stat = "identity") +
          labs(title = paste("Dengue Cases by", input$chart_variable, "(Bar Chart)"),
               x = "Year",
               y = "Count of Cases",
               fill = input$chart_variable) +
          theme_minimal()
      })

  # Line Chart
  output$line_chart <- renderPlot({
      
      ggplot(dengue_summary(), aes(x = Onset_Year, y = Count, color = factor(.data[[input$chart_variable]]))) +
          geom_line(size = 1) +
          labs(title = paste("Dengue Cases by", input$chart_variable, "(Line Chart)"),
               x = "Year",
               y = "Count of Cases",
               color = input$chart_variable) +
          theme_minimal() 
      })
  
  # CDA - by Year
  cda_run_test_year <- eventReactive(input$run_cda_year, {

      grouped_data_year <- dengue_daily_sf %>%
          group_by(Onset_Year, !!sym(input$cda_variable_year)) %>%
          summarize(Count = n(), .groups = "drop")
      return(grouped_data_year)
  })
  
  output$cda_plot_year <- renderPlot({
      
      req(cda_run_test_year())  
      grouped_data_year <- cda_run_test_year() 
      
      ggbetweenstats( 
          data = grouped_data_year,
          x = !!sym(input$cda_variable_year), 
          y = Count, 
          type = input$cda_test_type_year,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  
  # CDA - by Epiweek
  cda_run_test_epiweek <- eventReactive(input$run_cda_epiweek, {
      
      grouped_data_epiweek <- dengue_daily_sf %>%
          group_by(Onset_Epiweek, !!sym(input$cda_variable_epiweek)) %>%
          summarize(Count = n(), .groups = "drop")
      return(grouped_data_epiweek)
  })
  
  output$cda_plot_epiweek <- renderPlot({
      
      req(cda_run_test_epiweek())  
      grouped_data_epiweek <- cda_run_test_epiweek() 
      
      ggbetweenstats( 
          data = grouped_data_epiweek,
          x = !!sym(input$cda_variable_epiweek), 
          y = Count, 
          type = input$cda_test_type_epiweek,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  
  # CDA - by Counties
  cda_run_test_county <- eventReactive(input$run_cda_county, {
      
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
          x = !!sym(input$cda_variable_county), 
          y = Count, 
          type = input$cda_test_type_county,
          mean.ci = TRUE, 
          p.adjust.method = "fdr",
          messages = FALSE
      )
  })
  
  
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
  
}


shinyApp(ui = ui, server = server)
