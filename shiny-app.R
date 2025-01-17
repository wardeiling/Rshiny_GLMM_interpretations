# Load necessary libraries
library(shiny)
library(bslib)

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(
    version = 5, # Use Bootstrap 4 or 5
    bootswatch = "flatly" # Choose a bootswatch theme (e.g., "flatly", "cyborg", "journal")
  ),
  titlePanel("GLMM: Population-Averaged and Subject-Specific Interpretation"),
  
  tabsetPanel(
    # Introduction Tab
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h3("Introduction"),
                      withMathJax(
                      p("In this document, we will shed light on an issue that is well-known in the field of generalized linear mixed models (GLMMs), namely the discrepancy between population-averaged (PA; i.e., marginal) and subject-specific (SS; i.e., conditional-on-the-random-effect) interpretations of a fixed slope when the link function is non-linear. Note that subject-specific does not implicate the presence of a random slope. The main distinction between SS and PA models is whether the regression coefficients describe an individual's or the average population response to changing \\(x\\). An excellent example between the two is given by ", 
                        tags$a(href = "https://doi.org/10.2307/2531734", "Zeger et al. (1998)", target = "_blank"),
                        ":"),
                      tags$blockquote(
                        "For example, if \\(x_{it}\\) indicates whether subject \\(i\\) smokes at time \\(t\\), and \\(y_{it}\\) is the presence/absence of respiratory infection, the PA model estimates the difference in infection rates between smokers and nonsmokers; the SS model estimates the expected change in an individual's probability of infection given a change in smoking status. (p. 1051)")
                      ),
                      p("This document will explain why a discrepancy between the PA and SS interpretations of the fixed slope arises when the link function is non-linear. 
                        In this treatment, we will focus on very simple GLMMs with only one predictor and a random intercept. Accordingly, we have the following model for the GLMM with a log-link function:
                        $$ \\log \\left( \\frac{\\pi_{it}}{1 - \\pi_{it}} \\right) = \\beta_0 + \\beta_1 X_{it} + b_{0i} \\quad b_{0i} \\sim \\mathcal{N}(0, \\sigma_{b0}^2) $$
                        Where \\( Y_{it} \\) is the binary response variable, \\( \\pi_{it} = \\Pr(Y_{it} = 1) \\) is the probability of a positive response for subject \\( i \\) at time \\( t \\), 
                        \\( X_{it} \\) is a continuous or binary covariate and \\( b_{0i} \\) is the random intercept. 
                        \\( \\beta_0 \\) is the fixed intercept and \\( \\beta_1 \\) is the fixed slope for the covariate. 
                        "),
                        p("And for the mixed linear model (MLM; GLMM with identity link function):
                        $$ Y_{it} = \\beta_0 + \\beta_1 X_{it} + b_{0i} \\quad b_{0i} \\sim \\mathcal{N}(0, \\sigma_{b0}^2) $$
                        Where \\( Y_{it} \\) is the continuous response variable, \\( X_{it} \\) is a continuous covariate, and the other parameters are the same as in the GLMM with log-link function.
                        "
                        ),
                      h4("Tabs Overview"),
                      tags$ul(
                        tags$li("GLMM with Log-Link and Continuous Predictor"),
                        tags$li("GLMM with Log-Link and Binary Predictor"),
                        tags$li("MLM with Continuous Predictor")
                      ),
                      h4("How to Use"),
                      p("Navigate through the tabs, adjust parameters using sliders, and observe how model outputs 
                        are affected. Descriptions are provided in each tab to guide your exploration.")
               )
             )),
    
    # Tab for Generalized Mixed Linear Model with Log-Link
    tabPanel("GLMM with Log-Link and Continuous Predictor",
             sidebarLayout(
               sidebarPanel(
                 h4("Adjust Parameters"),
                 withMathJax(
                 sliderInput("beta_0", "\\(\\beta_0\\) (Intercept):", 
                             min = -5, max = 5, value = 0, step = 0.1),
                 sliderInput("beta_1", "\\(\\beta_1\\) (Slope):", 
                             min = -5, max = 5, value = 0.9, step = 0.1),
                 sliderInput("sigma_b0", "\\(\\sigma_{b0}\\) (Random Intercept SD):", 
                             min = 0.0, max = 5, value = 3, step = 0.1))
               ),
               mainPanel(
                 h4("GLMM with Log-Link and Continuous Predictor"),
                 p("In this tab, we show a dynamic version of the figure made by Dimitris Rizopoulos in de course",
                   tags$a(href = "https://www.drizopoulos.com/courses/EMC/CE08.pdf", "CE08 (slide 321)", target = "_blank"),
                   "where the GLMM with a non-linear logit function results in a discrepancy between the population-averaged and subject-specific relationships.
                   Adjust the sliders to see how the parameters influence the curves."),
                 plotOutput("glmmPlot", width = "750px", height = "550px"),
                 p("We can observe that the discrepancy between the two relationships increases as a function of the random intercept variance. Thus, when we fit a GLMM with a log-link function and the variance of the random intercept is non-zero, the fixed effects are interpreted according to the subject-specific interpretation. As a result, GLMMs are most useful when the main scientific objective is to make inferences about individuals rather than population averages.")
               )
             )),
    
    # Tab for GLMM with Binary Predictor
    tabPanel("GLMM with Log-Link and Binary Predictor",
             sidebarLayout(
               sidebarPanel(
                 h4("Adjust Parameters"),
                 withMathJax(
                   sliderInput("binary_beta_0", "\\(\\beta_0\\) (Intercept):", 
                               min = -5, max = 5, value = 0, step = 0.1),
                   sliderInput("binary_beta_1", "\\(\\beta_1\\) (Slope):", 
                               min = -5, max = 5, value = 0.9, step = 0.1),
                   sliderInput("binary_sigma_b0", "\\(\\sigma_{b0}\\) (Random Intercept SD):", 
                               min = 0.0, max = 5, value = 3, step = 0.1),
                 checkboxInput("show_interpolation", "Show Logistic Curve Interpolation", FALSE))
               ),
               mainPanel(
                 h4("GLMM with Log-Link and Binary Predictor"),
                 p("This situation is similar to the previous tab, but now we have a binary predictor. 
                   Adjust the sliders to see how the parameters influence the curves and toggle interpolation to observe the logistic curve."),
                 plotOutput("binaryGLMMPlot", width = "750px", height = "550px"),
                 p("In this case, we can see that the discrepancy between the two relationships is less pronounced due to the limited scale of the predictor combined with the logistic function.
                   Nevertheless, the disrepancy increass again as a function of the random intercept variance and completely dissappears when this variance is zero. 
                   Hence, the fixed effects are still interpreted according to the subject-specific interpretation, which is why GLMMs are most useful when the 
                   main scientific objective is to make inferences about individuals rather than population averages.")
               )
             )),
    
    # Tab for Mixed Linear Model
    tabPanel("MLM with Continuous Predictor",
             sidebarLayout(
               sidebarPanel(
                 h4("Adjust Parameters"),
                 withMathJax(
                   sliderInput("mlm_beta_0", "\\(\\beta_0\\) (Intercept):", 
                               min = -5, max = 5, value = 0, step = 0.1),
                   sliderInput("mlm_beta_1", "\\(\\beta_1\\) (Slope):", 
                               min = -5, max = 5, value = 0.9, step = 0.1),
                   sliderInput("mlm_sigma_b0", "\\(\\sigma_{b0}\\) (Random Intercept SD):", 
                               min = 0.0, max = 5, value = 3, step = 0.1),
                   checkboxInput("mlm_center_b0", "Center random intercept \\(b_0\\)", FALSE)
                 )
               ),
               mainPanel(
                 h4("Mixed Linear Model (MLM)"),
                 p("In this tab, we explore whether the discrepancy between the population-averaged and 
                 subject-specific relationships is present in mixed linear models (MLMs) with continuous 
                 predictors. Adjust the sliders to see how the parameters influence the curves and toggle
                 the centering of the random intercept."),
                 plotOutput("mlmPlot", width = "750px", height = "550px"),
                 p("In this case, we can see that the two relationships (slopes) are identical, which implies that the fixed effects can be interpreted
                 as both subject-specific and population-averaged. Nevertheless, there is a difference in the intercepts, which arises from the fact that
                 the expected value (mean) of the random intercept is not exactly zero due to the random sampling of only 20 individuals."),
                 p("An exception to the equivalence of population-averaged and subject-specific relationships in MLMs occurs when the endogenous covariate is time-varying, as shown by",
                   tags$a(href = "https://doi.org/10.1214/19-sts720", "Qian et al. (2020)", target = "_blank"),
                   ". In this case, the population-averaged interpretation may become invalid."
                  )
               )
             ))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Plot for GLMM with Log-Link
  output$glmmPlot <- renderPlot({
    beta_0 <- input$beta_0
    beta_1 <- input$beta_1
    sigma_b0 <- input$sigma_b0
    
    set.seed(123)
    n_individuals <- 20
    X_grid <- seq(-5, 5, length.out = 10000)
    b0 <- rnorm(n_individuals, mean = 0, sd = sigma_b0)
    
    logit_conditional <- beta_0 + beta_1 * X_grid
    pi_conditional <- 1 / (1 + exp(-logit_conditional))
    
    logit_individuals <- sapply(b0, function(b) beta_0 + beta_1 * X_grid + b)
    pi_individuals <- 1 / (1 + exp(-logit_individuals))
    pi_population <- rowMeans(pi_individuals)
    
    plot(X_grid, pi_conditional, type = "l", lwd = 4, col = "black",
         ylim = c(0, 1), xlim = c(-5, 5), xlab = expression(X[it]), ylab = "Probability",
         main = "GLMM with Continuous Predictor")
    for (i in 1:n_individuals) {
      lines(X_grid, pi_individuals[, i], col = "grey", lwd = 1)
    }
    lines(X_grid, pi_population, col = "red", lwd = 4)
    legend("bottomright", legend = c("conditional", "population", "individuals"),
           col = c("black", "red", "grey"), lty = c(1, 1, 1), lwd = c(4, 4, 1))
  })
  
  # Plot for GLMM with Binary Predictor
  output$binaryGLMMPlot <- renderPlot({
    beta_0 <- input$binary_beta_0
    beta_1 <- input$binary_beta_1
    sigma_b0 <- input$binary_sigma_b0
    show_interpolation <- input$show_interpolation
    
    set.seed(123)
    n_individuals <- 20
    X_grid_cont <- seq(0, 1, length.out = 10000)
    X_grid_bin <- c(0, 1)
    b0 <- rnorm(n_individuals, mean = 0, sd = sigma_b0)
    
    logit_individuals_bin <- sapply(b0, function(b) outer(beta_0 + beta_1 * X_grid_bin, b, "+"))
    pi_individuals_bin <- 1 / (1 + exp(-logit_individuals_bin))
    pi_population_bin <- rowMeans(pi_individuals_bin)
    
    plot(X_grid_cont, numeric(length(X_grid_cont)), type = "n", ylim = c(0, 1), xlim = c(0, 1),
         xlab = expression(X[it]), ylab = "Probability", main = "GLMM with Binary Predictor")
    
    if (show_interpolation) {
      logit_conditional <- beta_0 + beta_1 * X_grid_cont
      pi_conditional <- 1 / (1 + exp(-logit_conditional))
      logit_individuals_cont <- sapply(b0, function(b) beta_0 + beta_1 * X_grid_cont + b)
      pi_individuals_cont <- 1 / (1 + exp(-logit_individuals_cont))
      pi_population_cont <- rowMeans(pi_individuals_cont)
      
      lines(X_grid_cont, pi_conditional, col = "black", lwd = 4)
      for (i in 1:n_individuals) {
        lines(X_grid_cont, pi_individuals_cont[, i], col = "grey", lwd = 1)
      }
      lines(X_grid_cont, pi_population_cont, col = "red", lwd = 4)
    }
    
    points(X_grid_bin, 1 / (1 + exp(-(beta_0 + beta_1 * X_grid_bin))), col = "black", pch = 16, cex = 1.5)
    for (i in 1:n_individuals) {
      points(X_grid_bin, pi_individuals_bin[, i], col = "grey", pch = 16, cex = 0.6)
    }
    points(X_grid_bin, pi_population_bin, col = "red", pch = 16, cex = 1.5)
    
    # Add a legend
    if (show_interpolation) {
      
      legend("bottomright", legend = c("conditional (interpolation)", "population (interpolation)", "individuals (interpolation)", 
                                       "datapoint conditional", "datapoint population", "datapoints individuals"),
             col = c("black", "red", "grey", "black", "red", "grey"), 
             lty = c(1, 1, 1, NA, NA, NA), 
             pch = c(NA, NA, NA, 16, 16, 16), 
             lwd = c(4, 4, 1, NA, NA, NA), pt.cex = c(NA, NA, NA, 1.5, 1.5, 0.6))
    } else {
      legend("bottomright", legend = c("datapoint conditional", "datapoint population", "datapoints individuals"),
             col = c("black", "red", "grey"), 
             lty = c(NA, NA, NA), 
             pch = c(16, 16, 16), 
             lwd = c(NA, NA, NA), pt.cex = c(1.5, 1.5, 0.6))
    }
    
    
  })
  
  output$mlmPlot <- renderPlot({
    beta_0 <- input$mlm_beta_0
    beta_1 <- input$mlm_beta_1
    sigma_b0 <- input$mlm_sigma_b0
    center_b0 <- input$mlm_center_b0
    
    set.seed(123)
    n_individuals <- 20
    X_grid <- seq(-5, 5, length.out = 10000)
    
    # Generate random intercepts for individuals
    b0 <- rnorm(n_individuals, mean = 0, sd = sigma_b0)
    
    # Apply centering if the checkbox is ticked
    if (center_b0) {
      b0 <- b0 - mean(b0)  # Centering step
    }
    
    # Conditional-level prediction (mean individual, b0 = 0)
    y_conditional <- beta_0 + beta_1 * X_grid
    
    # Individual-specific predictions
    y_individuals <- sapply(b0, function(b) beta_0 + beta_1 * X_grid + b)
    
    # Population-level prediction (average across individuals)
    y_population <- rowMeans(y_individuals)
    
    # Plot the conditional-level curve
    plot(X_grid, y_conditional, type = "l", lwd = 4, col = "black",
         ylim = range(y_individuals), xlim = c(-5, 5), xlab = expression(X[it]), 
         ylab = expression(Y), main = "Mixed Linear Model Response Curves")
    
    # Add individual curves (grey lines)
    for (i in 1:n_individuals) {
      lines(X_grid, y_individuals[, i], col = "grey", lwd = 1)
    }
    
    # Add the population-level curve (red line)
    lines(X_grid, y_population, col = "red", lwd = 4)
    
    # Add a legend
    legend("topleft", legend = c("conditional", "population", "individuals"),
           col = c("black", "red", "grey"), lty = c(1, 1, 1), lwd = c(4, 4, 1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

