rm(list=ls(all=TRUE))
# Shiny app for exploring health facility distributions
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyjs)
library(purrr)
library(readr)
library(stringr)
source("app_functions.R")

test_out <- c("RDT", "MIC", "TACPLAS", "BLO", "URI")
test_out_name <- c("Malaria RDT", "Malaria Microscopy", "TAC - Plasmodium",
                   "Blood Culture", "Urine Culture")

# Define UI 
source("condition_ui.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "Test outcome based on symptoms", titleWidth = 400),
  sidebar,
  dashboardBody(
    fluidRow(
      plotOutput("prob_bar", height = 800)
    ),
    
    fluidRow(
      column(width = 2),
      column(
        width = 8,
        box(
          width = NULL,
          title = "Details",
          status = "primary",
          solidHeader = T,
          "Plot above displays the ", 
          strong("baseline"),
          " and ",
          strong("updated"),
          " predicted probability of positive outcomes of given diagnostic tests based on the input. 
          Baseline probability is calculated with only 'Place' and 'Rainy Season', whereas updated 
          probability is calculated based on all non-missing information you provide in the sidebar."
        )),
      column(width = 2)
    )
  )
)

# Define server logic 
server <- function(input, output) {
  container <- reactiveValues(prob = NULL)

  observeEvent(input$Calc, {
    if (input$BloodTest) {
      params <- read_rds("data/MC_wHEM_for_tools.rds")
      storage <- params$storage
      cov_name <- params$Xnames
      idisc <- 1:11
      icont <- 12:17
    } else {
      params <- read_rds("data/MC_woHEM_for_tools.rds")
      storage <- params$storage
      cov_name <- params$Xnames
      idisc <- 1:12
      icont <- 13
    }
    
    npos <- nrow(storage$mu)
    mod_input <- reactiveValuesToList(input)
    mod_input$AGEG1 <- as.numeric(mod_input$AGEG == 1)
    mod_input$AGEG2 <- as.numeric(mod_input$AGEG == 2)
    
    if (length(mod_input$AGEG1) == 0) mod_input$AGEG1 <- NA
    if (length(mod_input$AGEG2) == 0) mod_input$AGEG2 <- NA

    mod_input <- unlist(mod_input)
    in_symp <- as.numeric(mod_input[cov_name])
    names(in_symp) <- cov_name
    XNew <- rbind(in_symp, rep(NA, length(in_symp)))
    XNew[2, "PLACEK"] <- XNew[1, "PLACEK"]
    XNew[2, "RAIN"] <- XNew[1, "RAIN"]
    XNew[1, colnames(params$scales)] <- (XNew[1, colnames(params$scales)] - params$scales[1,])/params$scales[2,]
    XNew[2, colnames(params$scales)] <- (XNew[2, colnames(params$scales)] - params$scales[1,])/params$scales[2,]
    print(XNew)

    MNew <- is.na(XNew)
    ZNew <- ifelse(XNew, 1, -1)
    ZNew[,icont] <- XNew[,icont]
    ZNew[MNew] <- 0
    pred_new <- pred_base <- matrix(NA, npos, 5)

    for (i in 1:npos) {
      mu <- storage$mu[i,]
      Omega <- storage$Omega[i,] %>% matrix(length(mu), length(mu))
      lo <- ifelse(XNew, 0, -Inf)
      lo[is.na(lo)] <- -Inf
      hi <- ifelse(XNew, Inf, 0)
      hi[is.na(hi)] <- Inf

      for (p in 1:ncol(ZNew)) {
        Om12 <- Omega[p,-p]
        Om22_Inv <- solve(Omega[-p, -p])
        diffMat <- (ZNew[,-p] - rep(1, nrow(XNew)) %*% t(mu[-p]))
        mu_cond <- mu[p] + as.vector(t(Om12) %*% Om22_Inv %*% t(diffMat))
        Sig_cond <- as.vector(Omega[p, p] - t(Om12) %*% Om22_Inv %*% Om12)

        if (p %in% idisc) {
          ZNew[,p] <- rtnorm(nrow(XNew), mu_cond, sqrt(Sig_cond), lo[,p], hi[,p])
        } else {
          ZNew[,p] <- XNew[,p]
          cond <- is.na(ZNew[,p])
          ZNew[cond,p] <- rnorm(sum(cond), mu_cond[cond], sqrt(Sig_cond))
        }
      }

      ZtNew <- cbind(ZNew[, idisc] > 0, ZNew[, icont])
      B <- storage$Beta[i,] %>%
        matrix(ncol = length(test_out))
      
      pred <- (cbind(1, ZtNew) %*% B) %>% pnorm
      pred_new[i,] <- pred[1,]
      pred_base[i,] <- pred[2,]
    }

    # Discard first 10%
    pred_new <- pred_new[-(1:(0.1*npos)),]
    pred_base <- pred_base[-(1:(0.1*npos)),]

    # Create plottable df
    prob_new_df <- data.frame(Test = rep(test_out, each = 0.9*npos),
                              Type = rep("New", each = 0.9*npos),
                              Prob = as.vector(pred_new))
    prob_base_df <- data.frame(Test = rep(test_out, each = 0.9*npos),
                               Type = rep("Base", each = 0.9*npos),
                               Prob = as.vector(pred_base))
    container$prob <- rbind(prob_new_df, prob_base_df)
    container$prob$Test <- factor(container$prob$Test,
                                  levels = test_out,
                                  labels = test_out_name)
  })

  output$prob_bar <- renderPlot({
    if (is.null(container$prob)) return()
    mean_pred <- container$prob %>%
      group_by(Test, Type) %>%
      summarise(Prob = mean(Prob))
    container$prob %>%
      ggplot(aes(Type, Prob, fill=Type)) +
      geom_violin(position = position_dodge(width = 1),
                  scale = "width") +
      # geom_point(aes(colour = Type), data = mean_pred, position = position_dodge(width = 1)) +
      geom_text(aes(y = Prob, label = round(Prob, 2)), data = mean_pred,
                position = position_dodge(width = 1), vjust = 0) +
      # geom_text(aes(label = round(mean(Prob), 2)), position=position_dodge(width=1),
      #           vjust = 0) +
      facet_wrap(~ Test) +
      scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                        name = "Probability Type",
                        labels = c("Updated probability", "Baseline at\nlocation")) +
      theme(text = element_text(size=15),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            strip.text.x = element_text(size = 15, face = "bold")) +
      # scale_x_discrete(name = "Test", labels=test_out_name %>% sort) +
      scale_y_continuous(name = "Probability") +
      labs(title = "Chances of detecting the person being positive if given the test",
           x = "")
  })

  observeEvent(input$ResetAll,
               {
                 shinyjs::reset("side-panel")
               })
}

# Run the application 
shinyApp(ui = ui, server = server)