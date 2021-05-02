rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyjs)
library(purrr)
# source("TESTOUT_V3_functions.R")

P_symp_giv_test <- read_rds("data/V4/P_symp_giv_test.rds")
test_prev <- read_rds("data/V4/overall_test_prev.rds")
# in_symp <- data.frame(Symp = c("PLACE", "WBC"),
#                       Symp_status = c("K", 1), stringsAsFactors = F)

prod_like_draw <- function(df) {
  like_draw <- apply(df, 1, function (x) {
    a <- as.numeric(x["Alpha"])
    sa <- as.numeric(x["Sum_alpha"])
    rbeta(1000, a, sa - a)
  })
  
  prod_t <- apply(as.matrix(like_draw[,df$Test_status]), 1, prod)
  prod_f <- apply(as.matrix(like_draw[,!df$Test_status]), 1, prod)
  return(rbind(prod_f, prod_t))
}

prev_draw <- function(df) {
  tmp <- apply(df, 1, function (x) {
    a <- as.numeric(x["Alpha_prev"])
    sa <- as.numeric(x["Sum_alpha_prev"])
    rbeta(1000, a, sa - a)
  })
  return(t(tmp))
}

summary_draw <- function (mat) {
  Prob_T <- mat[2,] / (mat[2,] + mat[1,])
  df <- data.frame(Test_status = TRUE,
                   Prob = mean(Prob_T),
                   Lower = quantile(Prob_T, 0.025),
                   Upper = quantile(Prob_T, 0.975))
  return(df)
}

calc_probs_wsymp <- function (in_symp) {
  ## Calc P(Test|Symp) = P(Test|Tested, Symp)P(Tested|Symp)
  # Calc P(Test|Tested,Symp)
  in_symp_like <- in_symp %>%
    left_join(P_symp_giv_test) %>%
    filter(!is.na(Prob))
  
  like_draw_L <- split(in_symp_like, in_symp_like$Test) %>%
    map(~ prod_like_draw(.x))
  
  prev_draw_L <- test_prev %>%
    split(test_prev$Test) %>%
    map(~ prev_draw(.x))
  
  combine_draw <- map2(like_draw_L, prev_draw_L, ~ .x * .y) %>%
    map_df(~ summary_draw(.x), .id="Test")
  
  # Overall Prob
  return(combine_draw)
}

cov_name <- c("PLACE", "FEVER", "MYAL", "JPAIN", "CHILL", "HACH", 
              "NSEA", "VOMIT", "DIRHEA", "COUGH", "DIZY", "SORE", 
              "CONUL", "JAUN", "SKINR", "RUNNO", "MOUSO", "BOIL",  
              "EYEDIS", "SWOTON", "ABPAIN", "CONSTI", 
              "NECKSF", "LOSSCO", "LETHA", "ODEMA", "BODYIC", 
              "DYSPH", "AGEG", "WBC", "RBC", "HGB", "PLT", "RDW", 
              "MPV", "PDW", "LYM", "MON", "GRA", "TEMP")

test_out <- c("RDT", "MIC", "TACPLAS", "BLO", "URI")
test_out_name <- c("RDT", "Microscopy", "TAC - Plasmodium",
                   "Blood Culture", "Urine Culture")

# Define UI 
ui <- dashboardPage(
  
  dashboardHeader(title = "Test outcome based on symptoms", titleWidth = 500),
  
  dashboardSidebar(
    width = 500,
    useShinyjs(),
    div(
      id = "side-panel",
      radioButtons("PLACE", label = "Place", 
                   choices = c("Accra" = "F", "Kintampo" = "K")),
      selectInput("TEMP", "Temperature (deg Celscius)", 
                  choices = c(NA, "< 36.6" = 1, "36.6 - 37.6" = 2, 
                              "37.6 - 38.6" = 3, "> 38.6" = 4)),
      checkboxInput("BloodTest", label = "Blood test data?", value = F),
      conditionalPanel(
        condition = "input.BloodTest",
        fluidRow(column(width=6,
                        selectInput("WBC", "White Blood Cell 10^3/mm^3", 
                                    choices = c(NA, "< 7" = 1, "7 - 9.5" = 2, 
                                                "9.5 - 12" = 3, "> 12" = 4)),
                        selectInput("RBC", "Red Blood Cell 10^3/mm^3", 
                                  choices = c(NA, "< 4" = 1, "4 - 4.5" = 2, 
                                              "4.5 - 5" = 3, "> 5" = 4)),
                        selectInput("HGB", "Haemoglobin (g/dl)", 
                                  choices = c(NA, "< 10" = 1, "10 - 11" = 2, 
                                              "11 - 12" = 3, "> 12" = 4)),
                        selectInput("PLT", "Platelet (10^3/mm^3)", 
                                  choices = c(NA, "< 150" = 1, "150 - 250" = 2, 
                                              "250 - 350" = 3, "> 350" = 4)),
                        selectInput("MPV", "Mean platelet volume (um^3)", 
                                  choices = c(NA, "< 7.5" = 1, "7.5 - 8.25" = 2, 
                                              "8.25 - 9" = 3, "> 9" = 4))),
                 column(width=6,
                        selectInput("RDW", "Red cell distribution width (%)", 
                                  choices = c(NA, "< 16" = 1, "16 - 30" = 2, 
                                              "30 - 44" = 3, "> 44" = 4)),
                        selectInput("PDW", "Platelet distribution width (%)", 
                                  choices = c(NA, "< 13" = 1, "13 - 14" = 2, 
                                              "14 - 15" = 3, "> 15" = 4)),
                        selectInput("LYM", "Lymphocytes (%)", 
                                  choices = c(NA, "< 2" = 1, "2 - 3.5" = 2, 
                                              "3.5 - 5" = 3, "> 5" = 4)),
                        selectInput("MON", "Monocytes (%)", 
                                  choices = c(NA, "< 0.3" = 1, "0.3 - 0.55" = 2, 
                                              "0.55 - 0.8" = 3, "> 0.8" = 4)),
                        selectInput("GRA", "Granulocytes (%)", 
                                  choices = c(NA, "< 3" = 1, "3 - 5" = 2, 
                                              "5 - 7" = 3, "> 7" = 4)))
        )
        
      ),
      column(width = 6, 
             selectInput("AGEG", label = "Age Group", 
                         choices = c(NA, "1-2" = 0, "3-5" = 1, "6-15" = 2)),
             selectInput("MYAL", label = "Muscle Pain", choices = c(NA, "yes", "no")),
             selectInput("CHILL", label = "Chills", choices = c(NA, "yes", "no")),
             selectInput("NSEA", label = "Nausea", choices = c(NA, "yes", "no")),
             selectInput("DIRHEA", label = "Diarrhoea", choices = c(NA, "yes", "no")),
             selectInput("DIZY", label = "Dizzy", choices = c(NA, "yes", "no")),
             selectInput("CONUL", label = "Convulsion", choices = c(NA, "yes", "no")),
             selectInput("SKINR", label = "Skin Rashes", choices = c(NA, "yes", "no")),
             selectInput("MOUSO", label = "Mouth Sore (?)", choices = c(NA, "yes", "no")),
             selectInput("EYEDIS", label = "Eye Discharge", choices = c(NA, "yes", "no")),
             selectInput("ABPAIN", label = "Abdomen Pain", choices = c(NA, "yes", "no")),
             selectInput("NECKSF", label = "Stiff Neck", choices = c(NA, "yes", "no")),
             selectInput("LETHA", label = "Lethargy", choices = c(NA, "yes", "no")),
             selectInput("BODYIC", label = "Body Itch", choices = c(NA, "yes", "no"))),
      column(width = 6, 
             selectInput("FEVER", label = "Fever (Patient report)", 
                         choices = c(NA, "yes", "no")),
             selectInput("JPAIN", label = "Joint Pain", choices = c(NA, "yes", "no")),
             selectInput("HACH", label = "Headache", choices = c(NA, "yes", "no")),
             selectInput("VOMIT", label = "Vomit", choices = c(NA, "yes", "no")),
             selectInput("COUGH", label = "Cough", choices = c(NA, "yes", "no")),
             selectInput("SORE", label = "Sorehouse (?)", choices = c(NA, "yes", "no")),
             selectInput("JAUN", label = "Jaundice", choices = c(NA, "yes", "no")),
             selectInput("RUNNO", label = "Running Nose", choices = c(NA, "yes", "no")),
             selectInput("BOIL", label = "Boil", choices = c(NA, "yes", "no")),
             selectInput("SWOTON", label = "Swollen", choices = c(NA, "yes", "no")),
             selectInput("CONSTI", label = "Constipation", choices = c(NA, "yes", "no")),
             selectInput("LOSSCO", label = "Loss of Concious", choices = c(NA, "yes", "no")),
             selectInput("ODEMA", label = "Oadema", choices = c(NA, "yes", "no")),
             selectInput("DYSPH", label = "Dysphargia", choices = c(NA, "yes", "no"))),
      actionButton("ResetAll", "Reset All")
    )
  ),
  dashboardBody(
    plotOutput("prob_bar", height = 800)
  )
)

# Define server logic 
server <- function(input, output) {
  in_symp <- reactive({
    in_df <- data.frame(Symp = character(), Symp_status = character(), stringsAsFactors = F)
    
    for (s in cov_name) {
      if(input[[s]] != "NA" & input[[s]] != "") {
        if (is.numeric(input[[s]])) {
          stat <- round(input[[s]], 1) %>% as.character()
        } else stat <- as.character(input[[s]])
        tmp <- data.frame(Symp = s, Symp_status = stat)
        in_df <- rbind(in_df, tmp)
      }
    }
    in_df$Symp <- as.character(in_df$Symp)
    in_df$Symp_status <- as.character(in_df$Symp_status)
    
    print(in_df)
    
    overall_prob <- calc_probs_wsymp(in_df)
    overall_prob$Type <- "Posterior"
    
    in_df_base <- in_df %>% filter(Symp == "PLACE")
    baseline_prob <- calc_probs_wsymp(in_df_base)
    baseline_prob$Type <- "Prior"
    
    probs <- bind_rows(overall_prob, baseline_prob)
    probs
  })
  
  output$prob_bar <- renderPlot({
    in_symp() %>%
      ggplot(aes(Test, Prob, fill=Type, group=Type)) + 
      geom_col(position="dodge") +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(.9), 
                    width = 0.1) +
      geom_text(aes(label = round(Prob, 2)), position=position_dodge(width=1),
                vjust = 0) +
      scale_fill_manual(values = c("#E69F00", "#56B4E9"), 
                        name = "Probability Type",
                        labels = c("Updated probability", "Baseline at\nlocation")) +
      theme(text = element_text(size=15)) +
      scale_x_discrete(name = "Test", labels=test_out_name %>% sort) +
      scale_y_continuous(name = "Probability") +
      ggtitle("Chances of detecting the person being positive if given the test")
  })
  
  observeEvent(input$ResetAll,
               {
                 shinyjs::reset("side-panel")
               })
}

# Run the application 
shinyApp(ui = ui, server = server)