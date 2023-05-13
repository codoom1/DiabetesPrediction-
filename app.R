
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggpubr)
library(ggstance)
library(broom)
library(lme4)
library(MASS)
library(mgcv)
library(stargazer)
library(mice)
library(randomForest)
library(naivebayes)
library(xgboost)
library(caret)
library(magrittr)
library(ggpubr)
library(imager)
library(shiny)
library(mgcv)

diabetes1 <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv",
                     header = FALSE,
                     col.names = c("pregnancies", "glucose", "blood_pressure", "skin_thickness", "insulin", "bmi", "dpfunction", "age", "outcome"))


diabetes1[, !(names(diabetes1) %in%
               c("pregnancies","dpfunction","age", "outcome"))] <-
  lapply(diabetes1[, !(names(diabetes1)%in% c("pregnancies","dpfunction","age", "outcome"))], function(x) ifelse(x == 0, NA, x))

set.seed(123)

mids =mice(diabetes1, m=5, method = c(" ","pmm","pmm",
                                     "pmm","pmm","pmm"," ", " ", " "),printFlag = FALSE)


imputed_data=complete(mids)


#write.csv(imputed_data, 'imputed_data.csv')

diabetes=imputed_data


ui <- fluidPage(
  titlePanel("Model Fitting"),
  sidebarLayout(
    sidebarPanel(
      selectInput("response", "Select a response variable:",
                  choices = names(diabetes)[1:9],selected = names(diabetes)[9]),
      checkboxGroupInput("predictors", "Select predictor variables:",
                         choices = c("age", "dpfunction", "glucose", "bmi", 
                                     "insulin", "skin_thickness", "blood_pressure",
                                     "pregnancies"),
                         selected = c("age", "dpfunction", "glucose", "bmi", 
                                      "insulin", "skin_thickness", "blood_pressure",
                                      "pregnancies")),
      selectInput("family", "Select a family type:",
                  choices = c("binomial", "gaussian", "gamma")),
      checkboxInput("use_tensor_product", "Use tensor product terms in formula?", value = FALSE),
      checkboxGroupInput("tensor_product_vars", "Select variables for tensor product terms:",
                         choices = c("age", "dpfunction", "glucose", "bmi", 
                                     "insulin", "skin_thickness", "blood_pressure",
                                     "pregnancies"),
                         selected = c("age", "glucose", "bmi")),
      selectInput("method", "Select method for fitting GAM:",
                  choices = c('REML','GCV.Cp',"GACV.Cp","NCV"), selected = "REML"),
      actionButton("fit_model", "Fit Model")
    ),
    mainPanel(
      h4("Output:"),
      verbatimTextOutput("output")
    )
  )
)

server <- function(input, output) {
  model_fit <- reactive({
    predictors <- paste("s(", input$predictors, ")", sep = "")
    if(input$use_tensor_product) {
      tensor_product_vars <- paste(input$tensor_product_vars, collapse = ",")
      predictors <- paste(predictors, "+ te(", tensor_product_vars, ")", sep = "")
    }
    formula_str <- paste(input$response, "~", paste(predictors, collapse = "+"))
    formula_obj <- as.formula(formula_str)
    family <- match.arg(input$family, c("binomial", "gaussian", "gamma"))
    method <- match.arg(input$method, c('REML','GCV.Cp',"GACV.Cp","NCV"))
    mod <- gam(formula_obj, data = diabetes, family = family, method = method)
    list(formula_str = formula_str, mod = mod)
  })
  
  output$output <- renderPrint({
    req(input$fit_model)
    model_selected <- model_fit()
    mod <- model_selected$mod
    summary(mod)
  })
  
}


shinyApp(ui = ui, server = server)
