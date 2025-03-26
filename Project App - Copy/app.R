library(shiny)
library(tidyverse)
library(DT)
require(shinydashboard)
library(shinycssloaders)
library(shinya11y)
library(plotly)


ui <- {dashboardPage(
  skin="black",
  dashboardHeader(
    title = span(img(src="stats.logo.png", width=30, height=30,
                     alt="Bar Chart Icon"),"Linear Regression")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "home"),
      menuItem("Exploratory Data Analysis",tabName="exp"),
      menuItem("Model Building",tabName="building"),
      menuItem("Model Diagnosis",tabName="diagnosis"),
      menuItem("Predicting",tabName="predicting")
    )
  ),

  dashboardBody(
    use_tota11y(),
    tabItems(
      
#Home Tab
      {tabItem(tabName = "home",
              tags$h2("Welcome!"),
              box(
                HTML("This app will serve as an interactive tool to demonstrate
                     the basics of linear regression. Linear regression is one 
                     of the simplest and most widely used techniques in 
                     statistics and machine learning. It is a foundational 
                     concept that helps us understand the relationships between 
                     variables, making it a crucial tool in predictive modeling. 
                     Linear regression is a method for modeling the 
                     relationship between one dependent variable (the response) 
                     and one or more independent variables (predictors). The 
                     objective is to fit a straight line (in the simplest case) 
                     through a dataset that best represents this relationship."),
                width=12
              ),
              br(),
              box(
                HTML("This app will lay out all the steps required for a thorough 
                analysis including initial Exploratory Data Analysis, Model Building and 
                     Selection, Model Diagnosis and Prediction."),
                br(),
                HTML(
                  "First you will perform your initial exploratory data analyis 
                  by assessing the location and spread of variables then 
                  looking at potential relationships between predictors. After 
                  assessing distributions and any potential relationships in 
                  your data you will start to build your model. This will 
                  involve deciding which variable you would like to predict and 
                  then determining which predictor variables produce the best 
                  model for predicting your chosen response variable."
                ),
                br(),
                br(),
                HTML("Once you have a working model you can start to assess 
                     model assumptions using various diagnostic plots. After 
                     ensuring your assumptions are met you can start using your 
                     model to make predictions. In the predictions tab you can 
                     assess how good of a predictor your model is as well as 
                     generate your own predictions using your own imputed data."),
                width=12
              )
      )},
      
      
#Exploratory Data Analysis Tab    
      {tabItem(tabName = "exp",
              tags$h2("Exploratory Data Analysis"),
              fluidRow(
                box(
                  HTML("Before you begin assessing relationships between 
                  variables it is important to understand your data and the 
                  spread of your values. Exploratory Data Analysis (EDA) is an 
                  important first step when performing linear regression. It involves 
                  looking at and visualizing data to understand its main 
                  features, find patterns, and discover how different parts 
                  of the data are connected. EDA helps to spot any unusual data 
                  or outliers and is usually done before starting more detailed 
                       statistical analysis or building models."),
                width=12
              )
              ),
              fluidRow(
                
                box(
                  title = "Histograms",
                  background = "navy",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 4,
                  selectInput(inputId = "hist_var",
                              label = "Select Continuous Variable",
                              multiple=FALSE,
                              choices=list(
                                "Temperature"="Temperature",
                                "Humidity"="Humidity",
                                "SquareFootage"="SquareFootage",
                                "RenewableEnergy"="RenewableEnergy",
                                "EnergyConsumption"="EnergyConsumption",
                                "Occupancy"="Occupancy"
                              )
                  ),
                  br(),
                  plotOutput("hist"),
                  br(),
                  HTML("A histogram is a visual representation of the 
                  distribution of quantitative data. It is created by dividing 
                  the entire range of values into intervals (or 'bins') and 
                  counting how many values fall into each interval. Histograms 
                  are useful for recognizing patterns in data that may not be 
                  apparent from raw data alone, such as identifying the shape 
                  of the distribution (e.g., normal, skewed). These use only one 
                       variable so are an example of univariate analysis. Using 
                       the select box above select a variable and take a look at 
                       how the values are distributed")
                ),
                
                box(
                  title = "Scatter Plots",
                  background = "maroon",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 4,
                  selectInput(inputId = "scat_x",
                              label = "Select Continuous Variable for the x-axis",
                              multiple=FALSE,
                              choices=list(
                                "Temperature"="Temperature",
                                "Humidity"="Humidity",
                                "SquareFootage"="SquareFootage",
                                "RenewableEnergy"="RenewableEnergy",
                                "EnergyConsumption"="EnergyConsumption"
                              )
                  ),
                  br(),
                  selectInput(inputId = "scat_y",
                              label = "Select Continuous Variable for the y-axis",
                              multiple=FALSE,
                              choices=list(
                                "Temperature"="Temperature",
                                "Humidity"="Humidity",
                                "SquareFootage"="SquareFootage",
                                "RenewableEnergy"="RenewableEnergy",
                                "EnergyConsumption"="EnergyConsumption"
                              )
                  ),
                  plotOutput("scat"),
                  br(),
                  HTML("Scatter plot is one of the most important data 
                  visualization techniques and is used to plot the 
                  relationship between two variables, on a two-dimensional graph 
                  It is generally used to plot the relationship between one 
                       independent variable and one dependent variable, where an 
                       independent variable is plotted on the x-axis and a 
                       dependent variable is plotted on the y-axis so that you 
                       can visualize the effect of the independent variable on 
                       the dependent variable. These are an example of bivariatre analysis 
                    as 2 variables are compared against eachother. Using the 2 
                       select boxes above, select a variable for the x-axis and 
                       y-axis and take a look at the relationship between the two.")
                ),
                
                box(
                  title = "Boxplots",
                  background = "olive",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 4,
                  selectInput(inputId = "box_cont",
                              label = "Select Continuous Variable",
                              multiple=FALSE,
                              choices=list(
                                "Temperature"="Temperature",
                                "Humidity"="Humidity",
                                "SquareFootage"="SquareFootage",
                                "RenewableEnergy"="RenewableEnergy",
                                "EnergyConsumption"="EnergyConsumption"
                              )
                  ),
                  br(),
                  selectInput(inputId = "box_cat",
                              label = "Select Categorical Variable",
                              multiple=FALSE,
                              choices=list(
                                "DayOfWeek"="DayOfWeek",
                                "Holiday"="Holiday",
                                "HVACUsage"="HVACUsage",
                                "LightingUsage"="LightingUsage"
                              )
                  ),
                  plotOutput("box"),
                  br(),
                  HTML("Box plots show the spread of of a variable and can be 
                  seperated by groups to show possible differences. The spread 
                       of the rectangle represents the Interquartile Range, 
                       which is the difference between the medians of the lower 
                       and higher halfs of your data. The whiskers which stem 
                       out beyond the rectangle represent the entire range of 
                       variables. Select a categorical variable for the x-axis and a 
                       continuous variable for the y-axis and consider how the 
                       distribution of the categorical variable differs between 
                       the two groups.")
              )
              ),
              
              fluidRow(
                box(
                  title = "Summary Statistics",
                  background = "navy",
                  width = 12,
                  HTML("Summary statistics provide a quick and informative 
                       overview of a dataset by describing key characteristics 
                       of its variables. These statistics help identify 
                       patterns, trends, and potential anomalies in the data. 
                       Select a continuous variable to see some descriptive statistics."),
                  br(),
                  br(),
                  selectInput(inputId = "sum_var",
                              label = "Select Continuous Variable",
                              multiple=FALSE,
                              choices=list(
                                "Temperature"="Temperature",
                                "Humidity"="Humidity",
                                "SquareFootage"="SquareFootage",
                                "RenewableEnergy"="RenewableEnergy",
                                "EnergyConsumption"="EnergyConsumption",
                                "Occupancy"="Occupancy"
                              )
                  ),
                  tableOutput("summary"),
                )
              )
    )},


#Model Building Tab
      {tabItem(tabName="building",
              tags$h2("Model Building"),
              fluidRow(
                box(
                  HTML("Now it's time to build your model. Model building and 
                       selection is a crucial process in statistical 
                       applications, aiming to develop the most effective 
                       predictive model while balancing complexity and 
                       interpretability. It begins with selecting appropriate 
                       independent variables based on your exploratory data 
                       analysis, and statistical relevance. To start off select 
                       response variable, which will be the variable that you 
                       will predict values for later. Then select predictor 
                       variables which will be used to fit your linear 
                       regression model."),
                  width=12
                )
              ),
              
              fluidRow(
                box(
                selectInput(inputId = "response_var",
                            label = "Select Response Variable",
                            multiple=FALSE,
                            choices=list(
                              "Temperature"="Temperature",
                              "Humidity"="Humidity",
                              "SquareFootage"="SquareFootage",
                              "RenewableEnergy"="RenewableEnergy",
                              "EnergyConsumption"="EnergyConsumption",
                              "Occupancy"="Occupancy" 
                            )),
                width=12)
                
              ),
              
              fluidRow(
                
                box(HTML("A common method for variable selection is backwards 
                Selection. It begins with a full model that includes all 
                potential predictor variables and iteratively removes the 
                least significant one based on statistical criteria, such as p-values. 
                         The process continues until only statistically 
                         significant predictors remain. To perform backwards 
                         selection on this model, select all predictor variables 
                         and remove the one with the highest p-value until only 
                         statistically significant variables (typically p > 0.05) are left"),
                    br(),
                    br(),
                    uiOutput("predictor_selector"),
                    actionButton("run_model", "Run Regression"),
                    br(),
                    br(),
                    HTML("To the right you should see a summary of the model 
                         you have constructed. The 'estimates' tells us the 
                         average increase in the response variable associated 
                         with a one unit increase in the predictor variable, 
                         assuming all other predictor variables are held constant."),
                    br(),
                    br(),
                    HTML("The Pr(>|t|) column corresponds to the t-statistic. 
                         If this value is less than some alpha level (e.g. 0.05) 
                         than the predictor variable is said to be statistically 
                         significant."),
                    br(),
                    br(),
                    HTML("R-Squared describes the amount of variation that is 
                    captured by the developed model. It always ranges between 
                    0 and 1. The higher the value of R-squared, the better the 
                    model fits with the data."),
                  width=6
                ),
                box(verbatimTextOutput("modelsum"),
                  width=6
                )
              )
              )},
    

#Model Diagnosis Tab
    {tabItem(tabName="diagnosis",
            tags$h2("Model Diagnosis and Assumptions"),
            fluidRow(
              box(
                HTML("Model assumptions are critical in regression analysis 
                     because they ensure that statistical inferences, 
                     predictions, and hypothesis tests are valid and reliable. 
                     Violating these assumptions can lead to biased estimates, 
                     misleading results, and incorrect conclusions. Some 
                     important assumptions to assess are:"),
                br(),
                br(),
                HTML("Linearity – Assumes a straight-line relationship between 
                     predictors and the response variable. If violated, 
                     predictions become inaccurate."),
                br(),
                br(),
                HTML("Homoscedasticity – Assumes constant variance of residuals. 
                     If violated (heteroscedasticity), the model may give 
                     unreliable p-values and confidence intervals, affecting 
                     statistical inference."),
                br(),
                br(),
                HTML("Normality of Residuals – Ensures that residuals follow a 
                     normal distribution, which is important for valid 
                     hypothesis testing and confidence intervals. If violated, 
                     transformations or non-parametric methods may be required."),
                br(),
                br(),
                verbatimTextOutput("modelsumdiag"),
                width=6
              ),
              box(
                HTML("residual plot nonsense"),
                br(),
                br(),
                plotOutput("residualPlot"),
                HTML("The Residuals vs. Fitted plot is a key diagnostic tool in 
                     regression analysis that helps assess the validity of model 
                     assumptions. It plots the residuals (errors) on the y-axis 
                     against the fitted (predicted) values on the x-axis. 
                     Ideally, residuals should be randomly scattered around 
                     zero without any clear patterns, indicating that the 
                     assumptions of linearity and homoscedasticity (constant 
                     variance of residuals) hold. If a pattern emerges, such as 
                     a curved shape, it suggests that the relationship between 
                     predictors and the response variable may not be purely 
                     linear, indicating potential model misspecification."),
                br(),
                br(),
                plotOutput("qqPlot"),
                HTML("The Q-Q plot (quantile-quantile plot) of residuals is a 
                     diagnostic tool used to assess whether the residuals of a 
                     regression model follow a normal distribution, which is an 
                     important assumption in linear regression. The plot 
                     compares the quantiles of the residuals (y-axis) against 
                     the theoretical quantiles of a normal distribution (x-axis). 
                     If the residuals are normally distributed, the points 
                     should align closely along the 45-degree reference line. 
                     Deviations from this line indicate departures from 
                     normality—S-shaped patterns suggest skewness (left or 
                     right), while a bow-shaped pattern may indicate heavy 
                     tails or excess kurtosis."),
                br(),
                br(),
                plotOutput("modelplot"),
                HTML("The Residuals vs. Leverage plot is a diagnostic tool used 
                     to identify influential data points in a regression model. 
                     It plots leverage (x-axis), which measures how far an 
                     observation’s predictor values are from the mean, against 
                     residuals (y-axis), which indicate how far an observation’s 
                     actual value is from its predicted value. Points with high 
                     leverage have predictor values that differ significantly 
                     from the majority of the data, while large residuals 
                     indicate poor model fit for a specific observation."),
                width=6)
              )
            )},


#Predicting Tab
    {tabItem(tabName="predicting",
            tags$h2("Predicting"),
            fluidRow(box(
              HTML("To make predictions, the estimated regression equation is 
                   applied to new data, where known values of the independent 
                   variables are substituted into the model to compute the 
                   predicted outcome. The accuracy of these predictions 
                   depends on how well the model satisfies key assumptions, 
                   including linearity, independence, homoscedasticity, and 
                   normality of residuals."),
              width=12)),
            fluidRow(
              
                box(
                  HTML("Below you can see a plot of predicted values of all 
                       observations in your data set based off of your linear 
                       regression model plotted against the actual values. You 
                       want your values to fall on the red dotted line 
                       indicating a 1:1 correlation."),
                  br(),
                  br(),
                  plotOutput("predvact"),
                  width=6
                ),
              
                  box(HTML("Here you can enter some data of your own to obtain 
                           a prediction from your linear model. Consider how 
                           your prediction is effected when chaning differant 
                           variables and compare this to the values for those 
                           variables in the 'estimates' column of your model 
                           summary."),
                      br(),
                      br(),
                    uiOutput("user_inputs"),
                     

                     actionButton("predict_button", "Get Prediction"),
                     

                     br(), br(),
                     strong("Predicted Value:"),
                     tableOutput("prediction_result")),
                     width=6)
            )}

)
  )

  
)}




server <- function(input, output) {

  
  #Data reading and variable factorisation
  
    data <- read.csv("Energy_consumption_dataset.csv")
    data$DayOfWeek <- as.factor(data$DayOfWeek)
    data$Holiday <- as.factor(data$Holiday)
    data$HVACUsage <- as.factor(data$HVACUsage)
    data$LightingUsage <- as.factor(data$LightingUsage)
    
    
    #Model Building
    
    model <- reactive({
      req(input$predictor_vars)
      formula <- as.formula(paste(input$response_var, "~", paste(input$predictor_vars, collapse = " + ")))
      lm(formula, data = data)
    })
    
    
    #Exploratory data analysis tab outputs
    
    output$hist <- renderPlot({
      
      # Generate the histogram using the selected variable
      ggplot(data, aes_string(x = input$hist_var)) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
        labs(x = input$hist_var, 
             y = "Frequency") +
        theme_minimal()
    })
    
    output$box <- renderPlot({
      
      # Generate the boxplot using the selected variable
      ggplot(data, aes_string(x = input$box_cat, y=input$box_cont)) +
        geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
        labs(x = input$box_cat, 
             y = input$box_cont) +
        theme_minimal()
    })
    
    output$scat <- renderPlot({
      
      # Generate the boxplot using the selected variable
      ggplot(data, aes_string(x = input$scat_x, y=input$scat_y)) +
        geom_point(fill = "steelblue", color = "black", alpha = 0.7) +
        labs(x = input$scat_x, 
             y = input$scat_y) +
        theme_minimal()
    })
  
    output$summary <- renderTable({
      
      # Generate summary statistics for selected variable
      
      selected_data <- data[,input$sum_var]
      
      summary_stats <- data.frame(
        Statistic = c("Min", "Q1", "Mean", "Median", "Q3", "Max",
                      "Standard Deviation", "Interquartile Range"),
        Value = c(min(selected_data),
                  quantile(selected_data,probs=0.25),
                  mean(selected_data),
                  median(selected_data),
                  quantile(selected_data,probs=0.75),
                  max(selected_data),
                  sd(selected_data),
                  IQR(selected_data))
      )
      
      return(summary_stats)
    })
    
    
    #Model Building Outputs
    
    output$predictor_selector <- renderUI({
      remaining_vars <- setdiff(names(data), 
                                input$response_var)
      selectInput("predictor_vars", 
                  "Choose predictor variables:", 
                  choices = remaining_vars, 
                  selected = remaining_vars, 
                  multiple = TRUE)
    })
    
    observeEvent(input$run_model, {
      
      if (length(input$predictor_vars) == 0) {
        output$modelsum <- renderText("Please select at least one predictor variable.")
        return()
      }
      
      #Model Summary
      output$modelsum <- renderPrint(summary(model()))
      
    })
    
    
    #Model Diagnosis Tab Outputs
    
    observeEvent(input$run_model, {
      
      #Model Summary
      output$modelsumdiag <- renderPrint(summary(model()))
      
      #Residual v Fitted Plot
      output$residualPlot <- renderPlot({
      req(input$run_model)
      ggplot(data = data.frame(Fitted = fitted(model()), Residuals = resid(model())), aes(x = Fitted, y = Residuals)) +
        geom_point(color = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    })
      
      #Q-Q Plot
      output$qqPlot <- renderPlot({
      req(input$run_model)
      qqnorm(resid(model()), main = "Q-Q Plot of Residuals")
      qqline(resid(model()), col = "red", lwd = 2)
    })
      
      #Residual v Leverage Plot
      output$modelplot <- renderPlot({
      req(input$run_model)
      plot(model())
    })
    })   
    

    
    #Predicting Tab Outputs
    
    #Predicted v Actual values plot
    output$predvact <- renderPlot({
      req(input$run_model)
      
      #Predict response using model
      pred_values <- predict(model(), newdata = data)
      
      #Data Frame of actual values and predicted values
      plot_data <- data.frame(Actual = data[[input$response_var]], Predicted = pred_values)
      
      #Plot of predicted v actual
      ggplot(plot_data, aes(x = Actual, y = Predicted)) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Predicted vs Actual Values", x = "Actual Values", y = "Predicted Values") +
        theme_minimal()
    })
    
    #Creating inputs for prediction based on selected predictors
    output$user_inputs <- renderUI({
      req(input$predictor_vars)
      
      #Generate numeric inputs based on selected predictors
      input_fields <- lapply(input$predictor_vars, function(var) {
        if (is.numeric(data[[var]])) {
          numericInput(var, paste("Enter", var), value = mean(data[[var]], na.rm = TRUE))
        } else {
          selectInput(var, paste("Select", var), choices = unique(data[[var]]))
        }
      })
      
      do.call(tagList, input_fields)
    })
   
    #Predicting value based on user input 
    observeEvent(input$predict_button, {
      req(input$predictor_vars)
      
      #Data frame of user inputs
      new_data <- data.frame(t(sapply(input$predictor_vars, function(var) input[[var]])), stringsAsFactors = FALSE)
      
      #Ensure inputed values are in correct format
      for (var in input$predictor_vars) {
        if (is.factor(data[[var]])) {
          new_data[[var]] <- factor(new_data[[var]], levels = levels(data[[var]]))
        } else {
          new_data[[var]] <- as.numeric(new_data[[var]])
        }
      }
      
      #predict value
      pred_value <- predict(model(), newdata = new_data)
      
      #return predicted value
      output$prediction_result <- renderText(pred_value)
    })

    
}


shinyApp(ui = ui, server = server)
