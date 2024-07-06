library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(waiter)
# waiting_screen_surv <- tagList(
#   spin_solar(),
#   br(),
#   h4("The computation may take some time. Please wait patiently.")
# ) 
waiting_screen_condis <- tagList(
  spin_solar(),
  br(),
  h4("Please wait for a couple seconds if your dataset is large.")
)
waiting_element_rf1 <- tagList(
  spin_cube_grid(),
  br(),
  h4("It may take some time if there are many variables.")
)
waiting_element_rf2 <- tagList(
  spin_pixel(),
  br(),
  h4("The process may require additional time if the tree structure is complex.")
)

library(survival) # KM, Cox PH
library(threg) # threshold
library(dynpred)
library(ggsurvfit)
library(crosstable)
library(flextable) # convert flextable to html

library(caret)
library(CondiS) # gmathbf can't use
library(glmnet) # ridge, lasso method
library(gbm) # gbm method
library(randomForest) # rf method
library(randomForestSRC) # random survival forest

library(rlang)
library(ggplot2)
library(plotly)
library(DT)
library(reshape2)
library(vroom)
library(dplyr)
library(purrr)
library(vtable)
library(kableExtra)
library(webshot) # for screenshot RSF tree plot
if(is.null(webshot:::find_phantom())) webshot::install_phantomjs()
Sys.setenv(OPENSSL_CONF="/dev/null") # enable target by html tag
library(htmlwidgets) # save html

source('function.R')

