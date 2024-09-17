# SURIA
SURIA: An interactive web-based platform for survival analysis. 

This website aims to allow non-programmers to analyze their data without spending extra time learning a new programming language or new methods.

The website is developed using the R Shiny framework. It provides users with two datasets for right-censored analysis: COVID-19 data from the Mexican government open-source platform and lung cancer data from the SEER database.

This website currently includes 4 methods for right-censored data analysis: the Kaplan-Meier method, Cox proportional hazards model, threshold regression, and random survival forest. <br>
To make censored data suitable for machine learning methods, the website provides a method for imputing censored data called CondiS. (Imputation is not recommended if most patients are still alive at the end of the study, because after imputation, all patients would be marked as deceased  or event occurred . However, the survival time will be constrained by a tuning parameter, whose maximum allowable value is the maximum follow-up time in the data.)

# The website
You can visit the website at [https://zihyingli.shinyapps.io/Python/](https://zihyingli.shinyapps.io/MyShinyApp/).

Alternatively, if you have R installed, you can use the following code to open the website.

    library(shiny)
    
    runGitHub(repo="SURIA", username="Zih-Ying", ref="main")
    
