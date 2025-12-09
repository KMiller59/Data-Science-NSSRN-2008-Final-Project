
library(shiny)
library(UsingR)
library(ggplot2)
library(dplyr)
library(scales)

NSSRN2008 <- read.csv("C:/Users/krist/OneDrive/School/PhD/824- Data Science/2008 NSSRN Data.csv")

ui <- pageWithSidebar(
  headerPanel("2008 National Sample Survey of Registered Nurses - Burnout"),
  
  sidebarPanel(
    h3("Navigate"),
    
    selectInput(
      "Section", "Which View?",
      choices = c(
        "Basic RN Information",
        "Burnout"
      )
    ),
    
    # Controls: Section 1
    conditionalPanel(
      condition = "input.Section == 'Basic RN Information'",
      selectInput(
        "basic_var", "Choose a variable to summarize:",
        choices = c(
          "Actively Licensed RN"        = "Actively_Licensed_RN", 
          "Year of First RN License"    = "Year_First_RN_License",
          "Initial RN degree"           = "Initial_RN_Degree_Or_Credential_For_PUF",
          "Employed in Nursing"         = "Is_Employed_in_Nursing_2008"
        )
      )
    ),
    
    # Section 2 controls
    conditionalPanel(
      condition = "input.Section == 'Burnout'",
      selectInput(
        "burnout_view",
        "Choose a burnout Graph", 
        choices = c(
          "Burnout vs Year Licensed as a Nurse" = "by_year",
          "Burnout vs Hours Worked per Week"    = "by_hours",
          "Burnout vs Initial Degree"           = "by_degree", 
          "Intention to leave vs Burnout"       = "by_intent"
        )
      )
    )
  ),
  
  mainPanel(
    h3("Visualization"), 
    plotOutput("main_plot", height = "700px"), 
    br(),
    h4("Summary"),
    tableOutput("summary_table")
  )
)

#Server
server <- function(input, output, session) {
  
  nssrn <- reactive({ NSSRN2008 })
  
#Main Plot
  output$main_plot <- renderPlot({
    df <- nssrn()
  
# Section 1: Basic RN Information
    if (input$Section == "Basic RN Information") {
      
 # Graph 1: Actively Licensed RN
      if (input$basic_var == "Actively_Licensed_RN") {
        
        dfActive <- df %>%
          filter(!is.na(Actively_Licensed_RN)) %>%
          mutate(
            Actively_Licensed_RN = factor(
              Actively_Licensed_RN, 
              levels = c(0, 1),
              labels = c("No", "Yes")
            )
          )
        
        ggplot(dfActive, aes(x = Actively_Licensed_RN)) +
          geom_bar(fill = "blue", color = "grey") +
          labs(
            title = "Nurses who currently hold a License",
            x = NULL, y = "Number of RNs"
          )
        
        # Graph 2: Year of first RN license
      } else if (input$basic_var == "Year_First_RN_License") {
        
        df2 <- df %>% filter(!is.na(Year_First_RN_License))
        
        ggplot(df2, aes(x = Year_First_RN_License)) +
          geom_histogram(binwidth = 2, fill = "red", color = "grey") +
          labs(
            title = "Year of First RN License",
            x = "Year", y = "Count"
          )
        
        # Graph 3: Initial RN Degree
      } else if (input$basic_var == "Initial_RN_Degree_Or_Credential_For_PUF") {
        
        dfDegree <- df %>%
          filter(!is.na(Initial_RN_Degree_Or_Credential_For_PUF)) %>%
          mutate(
            Initial_Degree = factor(Initial_RN_Degree_Or_Credential_For_PUF)
          )
        
        ggplot(dfDegree, aes(x = Initial_Degree)) +
          geom_bar(fill = "blue", color = "grey") +
          labs(
            title = "Originally Licensed as",
            x = NULL, y = "Number of RNs"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Graph 4: Employed in nursing in 2008
      } else if (input$basic_var == "Is_Employed_in_Nursing_2008") {
        
        dfWorking <- df %>%
          filter(!is.na(Is_Employed_in_Nursing_2008)) %>%
          mutate(
            Employed_2008 = factor(
              Is_Employed_in_Nursing_2008,
              levels = c(FALSE, TRUE),   # logical TRUE/FALSE
              labels = c("No", "Yes")
            )
          )
        
        ggplot(dfWorking, aes(x = Employed_2008)) +
          geom_bar(fill = "purple", color = "grey") +
          coord_flip() +
          labs(
            title = "Currently Employed in Nursing (2008)",
            x = NULL, y = "Number of RNs"
          )
      }
      
      # ===== SECTION 2: BURNOUT =====
    } else if (input$Section == "Burnout") {
      
      view <- input$burnout_view
      
      # Burnout and Year Licensed
      if (view == "by_year") {    
        
        dfyear <- df %>%
          filter(
            !is.na(Year_First_RN_License),
            !is.na(Reason_Not_RN_Burnout)
          ) %>%  
          mutate(
            YearLicensed = cut(
              Year_First_RN_License,
              breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010),
              labels = c("1950-1959", "1960-1969", "1970-1979",
                         "1980-1989", "1990-1999", "2000-2009"),
              right = FALSE,
              include.lowest = TRUE
            )
          )
        
        ggplot(dfyear, aes(x = Reason_Not_RN_Burnout, fill = YearLicensed)) +
          geom_bar(position = "dodge") +
          labs(
            title = "Left due to Burnout by Year First Licensed", 
            x = "Left due to Burnout", 
            y = "Number of Nurses", 
            fill = "Year First Licensed"
          )
        
        # Burnout and Hours Worked
      } else if (view == "by_hours") {
        
        dfhours <- df %>%
          mutate(
            HoursNumeric = as.numeric(Number_Hours_Worked)
          ) %>%
          filter(
            !is.na(HoursNumeric), 
            !is.na(Reason_Not_RN_Burnout)
          ) %>%
          mutate(
            HoursWorked = cut(
              HoursNumeric,
              breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
              labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70"),
              right = TRUE, 
              include.lowest = TRUE
            )
          )
        
        ggplot(dfhours, aes(x = Reason_Not_RN_Burnout, fill = HoursWorked)) +
          geom_bar(position = "dodge") +
          labs(
            title = "Left due to Burnout by Hours worked per Week",
            x = "Left Due to Burnout",
            y = "Number of Nurses",
            fill = "Hours worked"
          )
        
        # Burnout and Degree
      } else if (view == "by_degree") {
        
        dfdegree <- df %>%
          mutate(
            BurnoutChange = case_when(
              Job_Change_Burn_Out == "Yes" ~ "Burnout-related job change",
              Job_Change_Burn_Out == "No" ~ "No burnout-related job change",
              Job_Change_Burn_Out == "Not Applicable" ~ "No burnout-related job change",
              TRUE ~ NA_character_
            ),
            BurnoutChange = factor(
              BurnoutChange,
              levels = c("No burnout-related job change",
                         "Burnout-related job change")
            ),
            InitialDegree = factor(Initial_RN_Degree_Or_Credential_For_PUF)
          ) %>%
          filter(!is.na(BurnoutChange), !is.na(InitialDegree))
        
        ggplot(dfdegree, aes(x = InitialDegree, fill = BurnoutChange)) +
          geom_bar(position = "fill") +
          scale_y_continuous(labels = scales::percent_format()) +
          labs(
            title = "Burnout Related Job Changes by Initial RN Degree",
            x = "Initial RN Degree", 
            y = "Percent of Nurses", 
            fill = "Burnout Change"
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Intention to leave vs Burnout
      } else if (view == "by_intent") {    
        
        dfintent <- df %>%
          mutate(
            BurnoutChange = case_when(
              Job_Change_Burn_Out == "Yes" ~ "Burnout-related job change",
              Job_Change_Burn_Out == "No" ~ "No burnout-related job change",
              Job_Change_Burn_Out == "Not Applicable" ~ "No burnout-related job change",
              TRUE ~ NA_character_
            ),
            PlanToLeave = case_when(
              Plans_On_Principal_RN_Position == "No plans to leave within next 3 years" ~ "No plan to leave",
              Plans_On_Principal_RN_Position == "Not Applicable" ~ "No plan to leave",
              Plans_On_Principal_RN_Position == "Undecided" ~ "No plan to leave",
              
              Plans_On_Principal_RN_Position == "Yes, in 1 year to 3 years" ~ "Plan to leave",
              Plans_On_Principal_RN_Position == "Yes, have left or will leave within the next 12 months" ~ "Plan to leave",
              Plans_On_Principal_RN_Position == "Yes, have left or will leave within the next Yes2 months" ~ "Plan to leave",
              Plans_On_Principal_RN_Position == "Yes, in Yes year to 3 years" ~ "Plan to leave",
              
              TRUE ~ NA_character_
            )
          ) %>%
          filter(!is.na(BurnoutChange), !is.na(PlanToLeave))
        
        ggplot(dfintent, aes(x = BurnoutChange, fill = PlanToLeave)) +
          geom_bar(position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          labs(
            title = "Intention to Leave vs Burnout related Job Changes", 
            x = "Burnout related Job Changes",
            y = "Percent of Nurses",
            fill = "Intention to Leave"
          )
      }
    }
  })
  
  # -------- SUMMARY TABLE --------
  output$summary_table <- renderTable({
    df <- NSSRN2008
    
    if (input$Section == "Basic RN Information") {
      var <- input$basic_var
      
      df %>%
        filter(!is.na(.data[[var]])) %>%
        count(.data[[var]], name = "n")
    } else {
      NULL
    }
  })
}

# ================= RUN APP =================
shinyApp(ui = ui, server = server)
