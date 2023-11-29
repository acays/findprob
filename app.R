# Load packages ----------------------------------------------------------------
library(shiny)
library(openintro)

# Load helper scripts ----------------------------------------------------------
source("distributions/chiTail.R")
source("distributions/FTail.R")
source("distributions/normTail.R")


# TODO

# Fix subtraction - DONE
# change all sliders to textboxes - DONE
# change binonmial UI to show <= instead of = DONE
# add = sign functionality use dbinom DONE
# fix binom code outputs - DONE


# Set defaults -----------------------------------------------------------------
defaults <- list(
  "tail" = "lower",
  "lower_bound" = "open",
  "upper_bound" = "open"
)

# Define UI --------------------------------------------------------------------
ui <- pageWithSidebar(

  # Title ----
  headerPanel("Finding Probabilty in a Distribution"),

  # Sidebar ----
  sidebarPanel(
    selectInput(
      inputId = "dist",
      label = "Distribution:",
      choices = c(
        "Normal" = "rnorm",
        "Binomial" = "rbinom",
        "t" = "rt",
        "F" = "rf",
        "Chi-Squared" = "rchisq"
      ),
      selected = "rnorm"
    ),

    br(),

    uiOutput("mean"),
    uiOutput("sd"),
    uiOutput("df1"),
    uiOutput("df2"),
    uiOutput("n"),
    uiOutput("p"),

    br(),
    br(),

    helpText("Model:"),
    div(textOutput("model"), style = "text-indent:20px;font-size:125%;"),
    br(),

    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),

    uiOutput("a"),
    uiOutput("b"),
    
 

    br(),

   
  ),

  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style = "font-size:150%;"),
    h4(strong('What R code to use')),
    verbatimTextOutput('rcode.pval'),
    hr()
  )
)

# Define server function -------------------------------------------------------
server <- function(input, output) {
  find_rnorm_code <- reactive({
    if (input$tail == "lower") {
      text <- paste0('pnorm(', input$a, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=TRUE)')
    } else if (input$tail == "upper") {
      text <- paste0('pnorm(', input$a, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=FALSE)')
    } else if (input$tail == "middle") {
      text <- paste0('pnorm(', input$b, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=TRUE)', ' - ', 'pnorm(', input$a, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=TRUE)')
    } else if (input$tail == "both") {
      text <- paste0('pnorm(', input$b, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=FALSE)', ' + ', 'pnorm(', input$a, ', ', 'mean=',input$mu, ", sd=",input$sd, ', lower.tail=TRUE)')
    }
    text

  })
  
  find_rt_code <- reactive({
    if (input$tail == "lower") {
      text <- paste0('pt(', input$a, ', ', 'df=',input$df, ', lower.tail=TRUE)')
    } else if (input$tail == "upper") {
      text <- paste0('pt(', input$a, ', ', 'df=',input$df, ', lower.tail=FALSE)')
    } else if (input$tail == "middle") {
      text <- paste0('pt(', input$b, ', ', 'df=',input$df, ', lower.tail=TRUE)', ' - ', 'pt(', input$a, ', ', 'df=',input$df, ', lower.tail=TRUE)')
    } else if (input$tail == "both") {
      text <- paste0('pt(', input$b, ', ', 'df=',input$df, ', lower.tail=FALSE)', ' + ', 'pt(', input$a, ', ', 'df=',input$df, ', lower.tail=TRUE)')
    }
    text
  })
  
  find_rf_code <- reactive({
    text <- paste0('pf(', input$a, ', ', 'df1=',input$df1, ", df2=",input$df2, ')')
  })
  
  find_rchisq_code <- reactive({
    text <- paste0('pchisq(', input$a, ', ', 'df=',input$df, ')')
    
  })
  
  find_rbinom_code_both_equality <- reactive({
    if (input$tail == "both" && input$lower_bound == "equal" && input$upper_bound == "open") {
      text <- paste0('pbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'dbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "both" && input$lower_bound == "equal" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'dbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "both" && input$lower_bound == "equal" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'dbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "both" && input$lower_bound == "open" && input$upper_bound == "equal") {
      text <- paste0('pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' + ', 'dbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "both" && input$lower_bound == "closed" && input$upper_bound == "equal") {
      text <- paste0('pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' + ', 'dbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "both" && input$lower_bound == "equal" && input$upper_bound == "equal") {
      text <- paste0('dbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')', ' + ', 'dbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } 
    
    text
    
  })
  
  find_rbinom_code_middle <- reactive({
    if (input$lower_bound == "open" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' - ', 'pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$lower_bound == "open" && input$upper_bound == "open") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' - ', 'pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$lower_bound == "closed" && input$upper_bound == "open") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' - ', 'pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$lower_bound == "closed" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)', ' - ', 'pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    }
  })
  
  find_rbinom_code <- reactive({
    
    
    if (input$tail == "equal") {
      text <- paste0('dbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p,')')
    } else if (input$tail == "lower" && input$lower_bound == "open") {
      text <- paste0('pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$tail == "lower" && input$lower_bound == "closed") {
      text <- paste0('pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$tail == "lower" && input$lower_bound == "equal") {
      text <- paste0('pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ')')
    } else if (input$tail == "upper" && input$lower_bound == "open") {
      text <- paste0('pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)')
    } else if (input$tail == "upper" && input$lower_bound == "closed") {
      text <- paste0('pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)')
    } else if (input$tail == "upper" && input$lower_bound == "equal") {
      text <- paste0('pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ')')
    } else if (input$tail == "middle") {
      text <- find_rbinom_code_middle()
    } else if (input$tail == "both" && input$lower_bound == "open" && input$upper_bound == "open") {
      text <- paste0('pbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$tail == "both" && input$lower_bound == "open" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'pbinom(', input$a-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$tail == "both" && input$lower_bound == "closed" && input$upper_bound == "open") {
      text <- paste0('pbinom(', input$b, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else if (input$tail == "both" && input$lower_bound == "closed" && input$upper_bound == "closed") {
      text <- paste0('pbinom(', input$b-1, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=FALSE)', ' + ', 'pbinom(', input$a, ', ', 'size=',input$n, ', ', ' prob=' , input$p, ', lower.tail=TRUE)')
    } else {
      text <- find_rbinom_code_both_equality() 
    }
    text
    
    
  })
  
  output$rcode.pval<- renderText({
    
    
    selected_dist <- input$dist
    
    
    
    if (selected_dist == "rnorm") {
      text <- find_rnorm_code()
      
    } else if (selected_dist == "rbinom") {
      text <- find_rbinom_code()
      
    } else if (selected_dist == "rt") {
      text <- find_rt_code()
    } else if (selected_dist == "rf") {
      text <- find_rf_code()
    } else if (selected_dist == "rchisq") {
      text <- find_rchisq_code()
    }
    
    text
    
    
    # deparse(displayedCode)
    
    
  
    
  })
 
  output$tail <- renderUI({
    # print("tail")
    if (input$dist == "rbinom") {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c(
          "Lower Tail" = "lower",
          "Upper Tail" = "upper",
          "Both Tails" = "both",
          "Middle" = "middle",
          "Equality" = "equal"
        ),
        selected = "lower"
      )
    }
    else if (input$dist == "rf" | input$dist == "rchisq") {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c("Upper Tail" = "upper"),
        selected = "upper"
      )
    }
    else {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c(
          "Lower Tail" = "lower",
          "Upper Tail" = "upper",
          "Both Tails" = "both",
          "Middle" = "middle"
        ),
        selected = "lower"
      )
    }
  })

  output$lower_bound <- renderUI({
  
    if (input$dist == "rbinom") {
      if (is.null(input$tail)) {
        shiny:::flushReact()
        return()
      }
 
      if (input$tail == "both") {
        selectInput(
          inputId = "lower_bound",
          label = "Lower bound:",
          choices = c(
            "<" = "open",
            "<=" = "closed",
            "=" = "equal"
          ),
          selected = "open"
        )
      }
      else if (input$tail == "middle") {
        selectInput(
          inputId = "lower_bound",
          label = "Lower bound:",
          choices = c(
            "<" = "open",
            "<=" = "closed"
          ),
          selected = "open"
        )
      }
      else if (input$tail == "lower") {
        selectInput(
          inputId = "lower_bound",
          label = "Bound:",
          choices = c(
            "<" = "open",
            "<=" = "closed",
            "=" = "equal"
          ),
          selected = "open"
        )
      }
      else if (input$tail == "upper") {
        selectInput(
          inputId = "lower_bound",
          label = "Bound:",
          choices = c(
            ">" = "open",
            ">=" = "closed",
            "=" = "equal"
          ),
          selected = "open"
        )
      }
    }
  })

  output$upper_bound <- renderUI({
    # print("upper bound")

    if (input$dist == "rbinom") {
      if (is.null(input$tail)) {
        shiny:::flushReact()
        return()
      }

      if (input$tail == "middle") {
        selectInput(
          inputId = "upper_bound",
          label = "Upper bound:",
          choices = c(
            "<" = "open",
            "<=" = "closed"
          ),
          selected = "open"
        )
      }
      else if (input$tail == "both") {
        selectInput(
          inputId = "upper_bound",
          label = "Upper bound:",
          choices = c(
            ">" = "open",
            ">=" = "closed",
            "=" = "equal"
          ),
          selected = "open"
        )
      }
    }
  })

  get_model_text <- reactive({
    if (is.null(input$tail)) {
      shiny:::flushReact()
      return()
    }

    low_less <- "<"
    low_greater <- ">"

    up_less <- "<"
    up_greater <- ">"
    
    if (input$dist == "rbinom" & input$tail != "equal") {
      if (is.null(input$lower_bound)) {
        shiny:::flushReact()
        return()
      }

      if (input$lower_bound == "closed") {
        low_less <- "<="
        low_greater <- ">="
      }
      
      if (input$lower_bound == "equal") {
        low_less <- "="
        low_greater <- "="
      }
      

      if (input$tail %in% c("middle", "both")) {
        if (is.null(input$upper_bound)) {
          shiny:::flushReact()
          return()
        }

        if (input$upper_bound == "closed") {
          up_less <- "<="
          up_greater <- ">="
        }
        
        if (input$upper_bound == "equal") {
          up_less <- "="
          up_greater <- "="
        }
        
        
      }
    }

    text <- ""
    if (length(input$tail) != 0) {
      if (input$tail == "lower") {
        # P(X < a)
        text <- paste0("P(X ", low_less, " a)")
      }
      else if (input$tail == "upper") {
        # P(X > a)
        text <- paste0("P(X ", low_greater, " a)")
      }
      else if (input$tail == "middle") {
        # P(a < X < b)
        text <- paste0("P(a ", low_less, " X ", up_less, " b)")
      }
      else if (input$tail == "both") {
        # P(X < a or X > b)
        text <- paste0("P(X ", low_less, " a or X ", up_greater, " b)")
      }
      else if (input$tail == "equal") {
        # P(X = a)
        text <- paste0("P(X = a)")
      }
    }

    return(text)
  })

  output$model <- renderText({
    # print("model")

    get_model_text()
  })

  #######################
  # Normal distribution #
  #######################

  output$mean <- renderUI({
    # print("mean")
    if (input$dist == "rnorm") {
      numericInput("mu",
        label=h4("Mean"),
        value = 0,
        min = -50,
        max = 50,
        step=1
      )
    }
  })
  
  

  output$sd <- renderUI({
    # print("sd")
    if (input$dist == "rnorm") {
      numericInput("sd",label=h4('Standard Deviation'),
        value = 1,min = 0.1,max = 30,step = 0.1)
    }
  })


  ##########################
  # t, F, X^2 distribution #
  ##########################

  output$df1 <- renderUI({
    if (input$dist %in% c("rt", "rchisq", "rf")) {
      numericInput(ifelse(input$dist %in% c("rt", "rchisq"), "df", "df1"),
                  label=h4('Degrees of freedom'),
                  value = 10,min=1,max=50, step=1)
    }
  })

  output$df2 <- renderUI({
    if (input$dist == "rf") {
      numericInput("df2",label=h4('Degrees of freedom (2)'),
                   value = 10,min=1,max=50, step=1)
    }
  })


  #########################
  # Binomial distribution #
  #########################

  output$n <- renderUI({
    # print("n")
    if (input$dist == "rbinom") {
      numericInput("n",label=h4('n'),
                   value = 10,min=1,max=250, step=1)
    }
  })

  output$p <- renderUI({
    if (input$dist == "rbinom") {
      numericInput("p",label=h4('p'),
                   value = 0.5,min=0,max=1, step=.01)
    }
  })




  output$a <- renderUI({
    # print("a")

    value <- 1
    min <- 0
    max <- 1
    step <- 1

    if (input$dist == "rnorm") {
      find_normal_step <- function(sd) {
        10^round(log(7 * sd / 100, 10))
      }

      if (is.null(input$mu) | is.null(input$sd)) {
        shiny:::flushReact()
        return()
      }

      mu <- input$mu
      sd <- input$sd
      if (is.null(mu)) mu <- 0
      if (is.null(sd)) sd <- 1

      value <- mu - 1.96 * sd
      min <- mu - 4 * sd
      max <- mu + 4 * sd
      step <- find_normal_step(sd)
      if (mu == 0 & sd == 1) {
        step <- .01
      }
    }
    else if (input$dist == "rt") {
      value <- -1.96
      min <- -6
      max <- 6
      step <- 0.01
    }
    else if (input$dist == "rf") {
      value <- round(qf(.95, as.numeric(input$df1), as.numeric(input$df2)), digits = 2)
      min <- 0
      max <- round(qf(.995, as.numeric(input$df1), as.numeric(input$df2)) * 1.05, digits = 2)
      step <- 0.01
    }
    else if (input$dist == "rchisq") {
      value <- round(qchisq(.95, as.numeric(input$df)), digits = 2)
      min <- 0
      max <- round(qchisq(.995, as.numeric(input$df)), digits = 2)
      step <- 0.01
    }
    else if (input$dist == "rbinom") {
      if (is.null(input$n)) {
        shiny:::flushReact()
        return()
      }

      value <- round(input$n / 4)
      min <- 0
      max <- input$n
      step <- 1
    }
    numericInput("a",
                 label=h4("a"),
                 value = value,
                 min =min,
                 max = max,
                 step=step
                 )
    #sliderInput("a", "a",
    #  value = value,
   #   min = min,
   #   max = max,
    #  step = step
   # )
  })

  output$b <- renderUI({
    # print("b")

    if (is.null(input$tail)) {
      shiny:::flushReact()
      return()
    }

    if (input$tail %in% c("middle", "both")) {
      value <- 1
      min <- 0
      max <- 1
      step <- 1

      if (input$dist == "rnorm") {
        find_normal_step <- function(sd) {
          10^round(log(7 * sd / 100, 10))
        }

        if (is.null(input$mu) | is.null(input$sd)) {
          shiny:::flushReact()
          return()
        }

        mu <- input$mu
        sd <- input$sd
        if (is.null(mu)) mu <- 0
        if (is.null(sd)) sd <- 1

        value <- mu + 1.96 * sd
        min <- mu - 4 * sd
        max <- mu + 4 * sd
        step <- find_normal_step(sd)
      }
      else if (input$dist == "rt") {
        value <- 1.96
        min <- -6
        max <- 6
        step <- 0.01
      }
      else if (input$dist == "rbinom") {
        if (is.null(input$n)) {
          shiny:::flushReact()
          return()
        }

        value <- round(input$n * 3 / 4)
        min <- 0
        max <- input$n
        step <- 1
      }
      
      numericInput("b",
                   label=h4("b"),
                   value = value,
                   min =min,
                   max = max,
                   step=step
      )
      #sliderInput("b", "b",
      #  value = value,
      #  min = min,
      #  max = max,
      #  step = step
     # )
    }
  })


  ############
  # Plotting #
  ############

  output$plot <- renderPlot({
    # print("plot")

    if (is.null(input$tail) | is.null(input$a)) {
      shiny:::flushReact()
      return()
    }

    L <- NULL
    U <- NULL

    error <- FALSE

    if (input$tail == "lower" | input$tail == "equal") {
      L <- input$a
    }
    else if (input$tail == "upper") {
      U <- input$a
    }
    else if (input$tail %in% c("both", "middle")) {
      if (is.null(input$b)) {
        shiny:::flushReact()
        return()
      }

      L <- input$a
      U <- input$b

      if (L > U) {
        error <- TRUE
      }
    }

    if (error) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", mar = c(1, 1, 1, 1))
      text(0, 0, "Error: Lower bound greater than upper bound.", col = "red", cex = 2)
    }
    else {
      if (input$dist == "rnorm" | input$dist == "rt") {
        M <- NULL
        if (input$tail == "middle") {
          M <- c(L, U)
          L <- NULL
          U <- NULL
        }

        if (input$dist == "rnorm") {
          if (is.null(input$mu) | is.null(input$sd)) {
            shiny:::flushReact()
            return()
          }

          normTail(m = input$mu, s = input$sd, L = L, U = U, M = M, axes = 3, cex.axis = 1.5)
          title(main = "Normal Distribution")
        }
        else if (input$dist == "rt") {
          if (is.null(input$df)) {
            shiny:::flushReact()
            return()
          }

          normTail(m = 0, s = 1, df = input$df, L = L, U = U, M = M, axes = 3, cex.axis = 1.5)
          title(main = "t Distribution")
        }
      }
      else if (input$dist == "rchisq") {
        if (is.null(input$df)) {
          shiny:::flushReact()
          return()
        }
        M <- NULL
        if (input$tail == "middle") {
          M <- c(L, U)
          L <- NULL
          U <- NULL
        }

        chiTail(U = U, df = input$df, xlim = c(0, round(qchisq(.995, input$df), digits = 2) + 1))
        title(main = "Chi^2 Distribution")
      }
      else if (input$dist == "rf") {
        req(U)

        M <- NULL
        if (input$tail == "middle") {
          M <- c(L, U)
          L <- NULL
          U <- NULL
        }

        FTail(U = U, df_n = input$df1, df_d = input$df2)
        title(main = "F Distribution")
      }
      else if (input$dist == "rbinom") {
        if (is.null(input$n)
        | is.null(input$p)
        | is.null(input$lower_bound)) {
          shiny:::flushReact()
          return()
        }

        if (input$tail %in% c("both", "middle") & is.null(input$upper_bound)) {
          shiny:::flushReact()
          return()
        }

        d <- dbinom(0:input$n, input$n, input$p)

        plot(0, 0,
          type = "n", xlim = c(-0.5, input$n + 0.5), ylim = c(0, max(d)),
          xlab = "", ylab = "", axes = FALSE
        )
        axis(1, cex.axis = 1.5)
        axis(2, cex.axis = 1.5)
        title(main = paste("Binomial Distribution"))



        for (k in 1:length(d))
        {
          col <- NA

          if (input$tail == "lower") {
            if (input$lower_bound == "open" & k - 1 < L) col <- "#569BBD"
            if (input$lower_bound == "closed" & k - 1 <= L) col <- "#569BBD"
            if (input$lower_bound == "equal" & k - 1 == L) col <- "#569BBD"
          }
          else if (input$tail == "upper") {
            if (input$lower_bound == "open" & k - 1 > U) col <- "#569BBD"
            if (input$lower_bound == "closed" & k - 1 >= U) col <- "#569BBD"
            if (input$lower_bound == "equal" & k - 1 == U) col <- "#569BBD"
          }
          else if (input$tail == "equal") {
            if (k - 1 == L) col <- "#569BBD"
          }
          else if (input$tail == "both") {
            if (input$lower_bound == "open" & input$upper_bound == "open" & (k - 1 < L | k - 1 > U)) col <- "#569BBD"
            if (input$lower_bound == "open" & input$upper_bound == "closed" & (k - 1 < L | k - 1 >= U)) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "open" & (k - 1 <= L | k - 1 > U)) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "closed" & (k - 1 <= L | k - 1 >= U)) col <- "#569BBD"
          }
          else if (input$tail == "middle") {
            if (input$lower_bound == "open" & input$upper_bound == "open" & k - 1 > L & k - 1 < U) col <- "#569BBD"
            if (input$lower_bound == "open" & input$upper_bound == "closed" & k - 1 > L & k - 1 <= U) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "open" & k - 1 >= L & k - 1 < U) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "closed" & k - 1 >= L & k - 1 <= U) col <- "#569BBD"
          }

          p <- matrix(c(-1.5 + k, 0, -0.5 + k, 0, -0.5 + k, d[k], -1.5 + k, d[k], -1.5 + k, 0), ncol = 2, byrow = TRUE)

          polygon(p, col = col)
        }
      }
    }
  })

  ################
  # Calculations #
  ################

  output$area <- renderText({
    if (is.null(input$tail) | is.null(input$a)) {
      shiny:::flushReact()
      return()
    }

    L <- input$a
    U <- NULL

    if (input$tail %in% c("both", "middle")) {
      if (is.null(input$b)) {
        shiny:::flushReact()
        return()
      }

      U <- input$b

      error <- FALSE
      if (L > U) error <- TRUE
      if (error) {
        return()
      }
    }


    isDBinom <- FALSE
    f <- function() NULL
    g <- function() NULL

    if (input$dist == "rnorm") {
      if (is.null(input$mu) | is.null(input$sd)) {
        shiny:::flushReact()
        return()
      }
      
      f <- function(x) pnorm(x, input$mu, input$sd)
    }
    else if (input$dist == "rt") {
      if (is.null(input$df)) {
        shiny:::flushReact()
        return()
      }

      f <- function(x) pt(x, input$df)
    }
    else if (input$dist == "rchisq") {
      if (is.null(input$df)) {
        shiny:::flushReact()
        return()
      }

      f <- function(x) pchisq(x, input$df)
    }
    else if (input$dist == "rf") {
      if (is.null(input$df1) | is.null(input$df2)) {
        shiny:::flushReact()
        return()
      }

      f <- function(x) pf(x, input$df1, input$df2)
    }
    else if (input$dist == "rbinom") {
      # https://www.statology.org/dbinom-pbinom-qbinom-rbinom-in-r/
      if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound)) {
        shiny:::flushReact()
        return()
      }
      else if (input$tail == "both" && input$upper_bound == "equal" 
               && input$lower_bound == "equal") {
        f <- function(x) dbinom(x, input$n, input$p)
        g <- function(x) dbinom(x, input$n, input$p)
        isDBinom <- TRUE
      } else if (input$tail == "both" && input$upper_bound == "equal" 
               && input$lower_bound == "open") {
        L <- L - 1
        f <- function(x) pbinom(x, input$n, input$p, TRUE)
        g <- function(x) dbinom(x, input$n, input$p)
    
        isDBinom <- TRUE
      } else if (input$tail == "both" && input$upper_bound == "open" 
               && input$lower_bound == "equal") {
        f <- function(x) dbinom(x, input$n, input$p)
        g <- function(x) pbinom(x, input$n, input$p, FALSE)
        isDBinom <- TRUE
      } else if (input$tail == "both" && input$upper_bound == "equal" 
               && input$lower_bound == "closed") {
        f <- function(x) pbinom(x, input$n, input$p, TRUE)
        g <- function(x) dbinom(x, input$n, input$p)
        isDBinom <- TRUE
      } else if (input$tail == "both" && input$upper_bound == "closed" 
               && input$lower_bound == "equal") {
        U <- U - 1
        f <- function(x) dbinom(x, input$n, input$p)
        g <- function(x) pbinom(x, input$n, input$p, FALSE)
        isDBinom <- TRUE
      }
      
      else if (input$tail != "equal" && (input$lower_bound != "equal" && is.null(input$upper_bound)) || 
               (input$upper_bound != "equal" && is.null(input$lower_bound)) ||
               (input$lower_bound != "equal" && !is.null(input$upper_bound) && input$upper_bound != "equal")) {
        f <- function(x) pbinom(x, input$n, input$p)

        
        if (input$tail %in% c("lower", "both") & input$lower_bound == "open") L <- L - 1
        if (input$tail %in% c("upper") & input$lower_bound == "closed") L <- L - 1
        if (input$tail %in% c("middle") & input$lower_bound == "closed") L <- L - 1
        
        if (input$tail %in% c("both", "middle")) {
          if (is.null(input$upper_bound)) {
            shiny:::flushReact()
            return()
          }
          
          if (input$tail == "both" & input$upper_bound == "closed") U <- U - 1
          if (input$tail == "middle" & input$upper_bound == "open") U <- U - 1
        }
      } 
      else {
        f <- function(x) dbinom(x, input$n, input$p)
        
        
        isDBinom <- TRUE
      }
      
     
    }

    val <- NA
    if (input$tail == "lower") {
      val <- f(L)
    } else if (input$tail == "upper" && isDBinom == FALSE) {
      val <- 1 - f(L)
    } else if (input$tail == "upper" && isDBinom == TRUE) {
      val <- f(L)
    } else if (input$tail == "equal") {
      val <- f(L)
    } else if (input$tail == "both" && isDBinom == FALSE) {
      val <- f(L) + (1 - f(U))
    } else if (input$tail == "both" && isDBinom == TRUE) {
      val <- f(L) + g(U)
    } else if (input$tail == "middle") {
      val <- f(U) - f(L)
    }

    text <- paste(get_model_text(), "=", signif(val, 4))


    text <- sub("a", input$a, text)
    if (input$tail %in% c("both", "middle")) {
      text <- sub("b", input$b, text)
    }
    
    text
    
  })
 
}

# Create the Shiny app object --------------------------------------------------
shinyApp(ui = ui, server = server)
