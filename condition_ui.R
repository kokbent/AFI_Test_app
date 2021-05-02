#### Conditional symptoms picker ----
sidebar <- dashboardSidebar(
  width = 400,
  useShinyjs(),
  div(
    id = "side-panel",
    fluidRow(
      column(width = 6, 
             radioButtons("PLACEK", label = "Place", 
                          choices = c("Accra" = 0, "Kintampo" = 1), inline = T),
             numericInput("TEMP", "Temperature (deg Celscius)", 
                          value = 36.7, min = 34, max = 42)),
      column(width = 6,
             radioButtons("RAIN", label = "Rainy Season", 
                          choices = c("Yes (Apr to Oct)" = 1, "No" = 0), inline = T),
             radioButtons("AGEG", label = "Age Group", 
                          choices = c(NA, "1-2" = 0, "3-5" = 1, "6-15" = 2), inline = T))
    ),
    
    hr(),
    
    fluidRow(
      column(width = 6, actionButton("Calc", "Calculate", width = "80%")),
      column(width = 6, actionButton("ResetAll", "Reset All", width = "80%"))
    ),
    
    hr(),
    
    checkboxInput("BloodTest", label = "Blood test data?", value = F),
    
    conditionalPanel(
      condition = "input.BloodTest",
      fluidRow(
        column(width = 6,
               numericInput("WBC", "White Blood Cell 10^3/mm^3", value = ""),
               numericInput("PLT", "Platelet (10^3/mm^3)", value = ""),
               numericInput("PDW", "Platelet distribution width (%)", value = "")
        ),
        column(width = 6,
               numericInput("HGB", "Haemoglobin (g/dl)", value = ""),
               numericInput("GRA", "Granulocytes (%)", value = "")
        )
      ),
      
      fluidRow(
        column(width = 6,
               radioButtons("HACH", label = "Headache", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("VOMIT", label = "Vomit", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("DIRHEA", label = "Diarrhoea", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("ABPAIN", label = "Abdomen Pain", choices = c(NA, "yes" = 1, "no" = 0), inline = T)
        ),
        column(width = 6,
               radioButtons("CONUL", label = "Convulsion", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("RUNNO", label = "Running Nose", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("LETHA", label = "Lethargy", choices = c(NA, "yes" = 1, "no" = 0), inline = T)
        )
      )
    ),
    
    conditionalPanel(
      condition = "!input.BloodTest",
      fluidRow(
        column(width = 6,
               radioButtons("HACH", label = "Headache", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("VOMIT", label = "Vomit", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("DIRHEA", label = "Diarrhoea", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("CHILL", label = "Chills", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("COUGH", label = "Cough", choices = c(NA, "yes" = 1, "no" = 0), inline = T)
        ),
        column(width = 6,
               radioButtons("CONUL", label = "Convulsion", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("RUNNO", label = "Running Nose", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("JAUN", label = "Jaundice", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("SKINR", label = "Skin Rashes", choices = c(NA, "yes" = 1, "no" = 0), inline = T),
               radioButtons("ABPAIN", label = "Abdomen Pain", choices = c(NA, "yes" = 1, "no" = 0), inline = T)
        )
      )
    )
  )
)

