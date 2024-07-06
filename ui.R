bs4DashPage(
  title = "A Website for Survival Analysis created by ZIH-YING, LI", 
  
  #### header ####
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = HTML('A Website for<br>Survival Analysis'), 
      image = "square.png"
    )
  ),
  #### sidebar ####
  sidebar = bs4DashSidebar(
    collapsed = T, 
    bs4SidebarMenu(
      ##### [side] intro ####
      bs4SidebarMenuItem(
        "Introduction",
        icon = icon("crow"),
        bs4SidebarMenuSubItem(
          "Website", tabName = "introduction_web", icon = icon("feather"),
          selected = T
        ),
        bs4SidebarMenuSubItem(
          "Data", tabName = "introduction_data", icon = icon("feather")
        )
      ),
      ##### [side] data process ####
      bs4SidebarHeader("Upload Data"),
      bs4SidebarMenuItem(
        "Data Process",
        tabName = "data_process",
        icon = icon("table-columns")
      ),
      ##### [side] EDA ####
      bs4SidebarHeader("Analysis"),
      bs4SidebarMenuItem(
        "Exploratory Data Analysis",
        tabName = "eda",
        icon = icon("chart-simple")
      ),
      ##### [side] Survival Right ####
      bs4SidebarMenuItem(
        "Right Censored Analysis",
        # "Survival Analysis (Right censored)",
        tabName = "survivalRight",
        icon = icon("tablets")
      ),
      ##### [side] Survival Interval ####
      bs4SidebarMenuItem(
        "Interval Censored Analysis",
        tabName = "survivalInterval",
        icon = icon("capsules")
      ),
      ##### [side] Condis ####
      bs4SidebarHeader("Imputation"),
      bs4SidebarMenuItem(
        "CondiS introduction",
        tabName = "imputation_intro",
        icon = icon("otter")
      ),
      bs4SidebarMenuItem(
        "Impute the censored data",
        tabName = "imputation",
        icon = icon("wand-magic-sparkles")
      )
    )
  ),
  #### right sidebar ####
  # controlbar = dashboardControlbar(),
  #### footer ####
  footer = bs4DashFooter(
    right = tagList(
      img(src="linktree.png", style="height:16px; filter:opacity(0.5); margin-bottom:5px;"), 
      "Author: ",
      a(
        href = "https://linktr.ee/ZihYingLi0302",
        target = "_blank", "ZIH-YING, LI "
      ), HTML("&emsp;"),
      icon("envelope"), "zihyingli0302@gmail.com", br(),
      icon("building-columns"), "Name of institute: Department of Statistics, Tamkang University"
    ),
    left = tagList(
      icon("image"), "Image source: ",
      a(
        href = "https://media.tenor.com/f4GKnTi51DwAAAAC/pixel-art.gif",
        target = "_blank", "Tenor"
      ), HTML("&emsp;"),
      icon("font-awesome"), "Icon source: ",
      a(
        href = "https://fontawesome.com/",
        target = "_blank", "Font Awesome"
      ), br(),
      paste("Last updated:", Sys.time())
    )
  ),
  #### body ####
  body = bs4DashBody(
    includeCSS("www/style.css"),
    shinyjs::useShinyjs(),
    useWaiter(), # include dependencies
    useWaitress(),
    withMathJax(), # latex math
    tags$head(HTML("<script>
      $(function () { $('[data-toggle=\"tooltip\"]').tooltip() })
    </script>")), # tooltip always show
    
    bs4TabItems(
      ##### [body] intro_web ####
      bs4TabItem(
        tabName = "introduction_web", 
        HTML('<div class="div-moon"><h1>Welcome!</h1></div>'), br(),
        
        h4(icon("feather-pointed"), "Introduction to the Website", style = "font-weight: bold; color: #2E2855;"),
        HTML('
          <p>This website is designed for users to upload data and conduct <b class="bold1">analysis</b> or <b class="bold1">imputation</b>. Users can upload data on the "Data Process" page, define variable types, and specify survival analysis variables. After clicking on "Transform Data" they can proceed with subsequent analysis or imputation.</p>
          <p>The analysis section allows user to do: <ul>
          <li><b class="bold1">Exploratory data analysis</b>. When the variable is categorical, a bar chart will be provided. For continuous variables, basic statistical information such as mean, standard deviation, and quartiles will be presented along with a histogram.</li>
          <li><b>Survival analysis for <b class="bold1">right-censored</b> data</b>. In this section, three survival methods are provided, including Kaplan-Meier, Cox proportional hazards model, threshold regression model and random survival forest. All three methods offer survival probability plots. The model methods provide estimated coefficients and relevant tests. Specifically, for threshold regression, three types of plots (survival, hazard, and density) are available.</li>
          <li><b>Survival analysis with <b class="bold1">interval</b> censoring</b>. future work</li></ul></p>
          <p>In the imputation section, a method called <b class="bold1">CondiS</b> is provided to impute censored data. This method utilizes a conditional survival distribution-based for imputation, aiming to increase the effective sample size for using machine learning-based survival analysis. Currently, it can only be applied to right-censored data.</p>
        '), htmltools::hr(), 
        bs4Card(
          title = "The Package Used for Estimation and Imputation", icon=icon("box-archive"), collapsed = T, status = "primary", width = 12,
          p("The website is built using the R Shiny framework and is deployed on shinyapps.io. The following packages are used for analysis, imputation, plotting, and table output:"),
          h6(icon("feather-pointed"), "Analysis", style = "font-weight: bold; color: #2E2855;"),
          HTML("<p>To estimate Kaplan-Meier and Cox Proportional Hazards models, we use the `survival` package. Threshold regression estimation is performed with the `threg` package. Random Survival Forest (RSF) analysis is conducted using the `randomForestSRC` package.</p>"),
          h6(icon("feather-pointed"), "Imputation", style = "font-weight: bold; color: #2E2855;"),
          HTML("<p>We utilize the `CondiS` package for imputation, where the `CondiS_X()` function offers various machine learning methods for further imputation. CondiS_X employs the `caret` package for preprocessing and training, encompassing multiple models. Among the eight methods provided by CondiS_X, ridge and lasso methods use the `glmnet` package, gradient boosting machine method uses the `gmathbf` package, random forest method uses the `randomForest` package, and artificial neural networks method uses the `nnet` package.</p>")
        ),
        bs4Card(
          title = "The Concordance Index", icon=icon("scroll"), status = "primary", collapsed = T, width = 12, 
          h6(icon("feather-pointed"), "Somers’ D", style = "font-weight: bold; color: #2E2855;"),
          HTML("
            <p>The concordance index for the Cox proportional hazards model uses Somers' D, which ranges from -1 to 1. The formula is as follows:
            $$D=\\frac{c-d}{c+d+t_x}$$
            where \\(c\\) represents the number of concordant pairs, 
            \\(d\\) represents the number of discordant pairs, and 
            \\(t_x\\) represents the number of pairs with tied predictions.</p>
            <p>The range can be converted to between 0 and 1 using the following formula:
            $$C=\\frac{D+1}{2}=\\frac{c+t_x/2}{c+d+t_x}$$</p>
          "),
          h6(icon("feather-pointed"), "Harrell’s C-index", style = "font-weight: bold; color: #2E2855;"),
          HTML("
           <p>The concordance index for the random survival forest uses Harrell's Concordance Index. The intuition behind Harrell's concordance index is that patients with shorter survival times should have higher predicted risks. 
           The calculation steps are as follows:</p>
           <ol>
           <li>Form all possible pairs from the data.</li>
           <li>Exclude pairs where the shorter survival time is censored (since it is uncertain if it is genuinely shorter), 
           or both \\(i\\) and \\(j\\) are censored (since it is uncertain who lived longer). 
           Define \\(Pair\\) as the total number of permissible pairs.</li>
           <li>For each permissible pair:<ul>
           <li>If \\(T_i\\neq T_j\\), count one if the shorter survival time has a worse predicted outcome, and 0.5 if the predicted outcomes are the same.</li>
           <li>If \\(T_i=T_j\\) and both died, count one if the predictions are the same; otherwise, count 0.5.</li>
           <li>If \\(T_i=T_j\\) but only one died, count one if the one who died has a worse predicted outcome; otherwise, count 0.5.</li>
           </ul></li>
           <li>Define \\(Concordance\\) as the sum of scores for all permissible pairs.</li>
           </ol>
           <p>The C-index is then defined as follows:
           $$C=\\frac{Concordance}{Pair}$$</p>
          ")
        )
      ),
      ##### [body] intro_data ####
      bs4TabItem(
        tabName = "introduction_data", 
        HTML('<div class="div-moon"><h1>About The Dataset</h1></div>'), br(),
        
        h4(icon("feather-pointed"), "Introduction to the Data", style = "font-weight: bold; color: #2E2855;"),
        p("This website provides three datasets for users to download. Below is an introduction to the sources and contents of the data:"), 
        div(
          style="margin-left: 30px; margin-right: 30px",
          fluidRow(h6(icon("table"), "The COVID-19 data from Mexico", style = "font-weight: bold;"), 
                   downloadBttn("download_covid", label = "COVID-19 dataset"),
                   downloadBttn("download_covid_mini", label = "COVID-19 sub-dataset")),
          HTML('
          <ul>
          <li>Data Source: <br>
            The data is sourced from the Epidemiological Surveillance System for Viral Respiratory Diseases, released by the Mexican Ministry of Health (Secretaría de Salud, SS), focusing on suspected cases of viral respiratory diseases.
          </li>
          <li>Data Selection: <br>
            Using the data from 2021, which comprises 40 variables, including six variables representing different methods for identifying whether patients are diagnosed with COVID-19, we selected patients whose samples were analyzed by "National Epidemiological Surveillance Laboratory (INDRE, LESP, and LAVE) and private laboratories accredited by InDRE" as the standard. Among these patients, those identified as "positive for the novel coronavirus" were filtered. The total number of records in the dataset is 717,889.
          </li>
          <li>Variable Transformation: <br>
            Time and censoring are calculated and transformed using the following three time variables recorded in the format YYYY-MM-DD:
            <table>
              <tr><th>Variable</th><th>Description</th></tr>
              <tr><td>date_last</td><td>The date of the last update.</td></tr>
              <tr><td>date_symp</td><td>The date on which the patient\'s symptoms began.</td></tr>
              <tr><td>date_died</td><td>The date the patient died.</td></tr>
            </table>
            If the date_died is "9999-99-99", it indicates that the patient is still alive and is marked as <b>censored</b> (\\(\\delta=0\\)). The time is calculated as <b>date_last - date_symp</b>. 
            Otherwise, it indicates that the patient has died and is marked as <b>death</b> (\\(\\delta=1\\)). The time is calculated as <b>date_died - date_symp</b>. Time is measured in days.
          </li>
          <li>Data Cleaning: <br>
            We have selected 19 variables, including gender, hospitalized, intubation, pneumonia, age, smoking, UCI, and presence of other diseases (obesity, hypertension, etc.).
            Firstly, our goal is hospitalized patients, so we exclude non-hospitalized patients (n=518,795).
            Next, we exclude patients with fields such as pneumonia, smoking, and presence of other diseases marked as "does not apply", "ignored", or "not specified" (n=3,079).
            Then, we exclude patients with a time of 0 days (n=395).
            The final effective sample size is 195,620.
          </li>
          <li>Sub-dataset: <br>
            We also provide a smaller dataset consisting of data from November 2021 (n=5,228). The follow-up time is set as November 30th, and we recalculate the time (patients with a time of death beyond November 30th are marked as censored). 
            We then exclude patients with a time of 0 days (n=200). The final effective sample size is 5,028.
          </li>
          <li>Data Cleaning Process Diagram: <br>
            <img src="mexico data flow.png", width = "80%", style = "margin-left: auto; margin-right: auto; display: block;">
          </li>
          </ul>
          '), br(),
          fluidRow(h6(icon("table"), "The lung cancer data from the SEER (Surveillance, Epidemiology, and End Results) dataset", style = "font-weight: bold;"),
                   downloadBttn("download_lung", label = "Lung cancer dataset"),
                   downloadBttn("download_lung_mini", label = "Lung cancer sub-dataset")),
          HTML('
          <ul>
          <li>Data Source: <br>
            The data source is the SEER*Stat database: SEER November 2022 Submission, specifically the Incidence - SEER Research Data, 17 Registries, Nov 2022 Sub (2000-2020) database. This database covers approximately 26.5% of the US population and contains one record for each of 9,208,295 tumors.
          </li>
          <li>Data Selection: <br>
            From the common cancer sites on the SEER website, we can observe that the highest incidence rates among the US population are breast cancer for females and prostate cancer for males, followed by lung and bronchus cancers for both genders. In terms of mortality rates, lung and bronchus cancers have the highest rates for both females and males, indicating that lung cancer is a common and highly lethal cancer.
            Therefore, we have chosen lung cancer as our target disease. Within the database, there are 6 variables defining the cancer patients. We have selected patients where {Site and Morphology.TNM 7/CS v0204+ Schema recode} equals \'Lung\'. In total, we have chosen 17 variables. The time variable is "Survival months", and there are two censoring variables: "COD to site recode" (cause of death) and "Vital status recode" (whether deceased). 
            The other variables can be broadly categorized into three groups: patient information, tumor information, and treatment methods. Patient information includes gender, age (patients aged 90 years and older will be categorized as 90 years old), age group (grouped every 5 years), race, median family income, and rural-urban status. Tumor information includes TNM staging, and treatment methods include surgery, radiation, and chemotherapy.
            In the initial step, patients with unknown time, blank TNM staging, and unknown ages were preliminarily excluded, while those with malignant tumors were retained. Following the filtering process, the sample size was 139,886.
          </li>
          <li>Data Cleaning: <br>
            The "COD to site recode" variable is reclassified into 3 categories: survival, lung and bronchus, and other diseases. 
            The "rural-urban" variable is reclassified into 3 categories: metropolitan, nonmetropolitan, and unknown, with unknown patients removed (n=234). 
            The TNM staging variable is merged into broader categories (e.g. M1a, M1b, M1c are merged into M1), and patients not covered by the AJCC staging scheme are removed (n=6,203). 
            The "surgery" variable is reclassified into 3 categories: Yes, No, and Unknown, with unknown patients removed (n=1,068). 
            The "radiation" variable is reclassified into two categories: Yes and No/Unknown (since the original classification combines None and unknown), and patients with unknown race are removed (n=683). 
            Patients under the age of 40 are grouped together (since the sample size for each group under 40 years old is too small).
            Then, we exclude patients with a time of 0 days (n=15,997). The final effective sample size is 115,701.
          </li>
          <li>Sub-dataset: <br>
            We selected a smaller subset from the orginal 115,701 records to enhance training efficiency. 
            We excluded patients who did not die from lung cancer (n=11,380) and those with unassessable T and N stages (n=14,714). 
            We retained patients who received radiation and chemotherapy, as the data could not distinguish between those who did not receive these treatments and those with unknown treatment status. 
            We also excluded patients aged 90 and above, marked as 90 (n=47). 
            Since SEER primarily collects data on the U.S. population, we focused our analysis on the white racial group. 
            The final adequate sample size was 17,661 observations.
          </li>
          <li>Data Cleaning Process Diagram: <br>
            <img src="SEER lung cancer data flow.png", width = "80%", style = "margin-left: auto; margin-right: auto; display: block;">
          </li>
          </ul>
          '), br(),
          fluidRow(h6(icon("table"), "The example dataset from CondiS", style = "font-weight: bold;"),
                   downloadBttn("download_condis", label = "CondiS example dataset")),
          p("This dataset is provided by the website created by the author of the CondiS package.")
        )
      ),
      ##### [body] data process ####
      bs4TabItem(
        tabName = "data_process", 
        #------------------#
        #--- title area ---#
        #------------------#
        HTML('<div class="div-moon">
          <h1>Upload Data</h1>
          <h4>The dataset should incorporate survival time and indicators for events or censoring.</h4>
        </div>'), br(),
        #------------#
        #--- step ---#
        #------------#
        bs4Card(
          title = 'The procedural flow', icon = icon("circle-info"), width = 12, status = "primary", collapsed = T, 
          fluidRow(
            HTML('
             Firstly, users need to upload data and define variable types along with the variables required for survival analysis. After data transformation, they can proceed with analysis or imputation. 
            '),
            img(src = "flow.png", style = "margin-left: auto; margin-right: auto;")
          )
        ),
        #-------------------#
        #--- upload data ---#
        #-------------------#
        fileInput('upload_data', 'FILE IMPORT: comma seperate (.csv) / tab seperate (.txt)', 
                  accept = c(".csv", ".txt"), width="100%"),
        htmltools::hr(),
        #-----------------#
        #--- data view ---#
        #-----------------#
        h6(icon("square-caret-down"), "Data (First 50 rows)"),
        column(
          width=12, dataTableOutput("upload_data_view"),
          style = "height:400px; overflow-y: scroll; overflow-x: scroll;"
        ), htmltools::hr(), 
        #----------------------------#
        #--- define variable type ---#
        #----------------------------#
        h6(icon("square-caret-down"), "Define Variable"),
        fluidRow(
          column(width = 4, 
                 h5("Data Variable", style="font-weight: bold;"),
                 htmlOutput("upload_data_variable")),
          column(width = 4, 
                 h5(tags$span(
                   "Type of Variable", 
                   bs4Dash::tooltip(
                     tag = icon("circle-info"), placement = "top", 
                     title = 'For numerical variables, the variable type will automatically be selected as "Continuous", while text variables will be automatically designated as "Categorical".'
                   )
                 ), style="font-weight: bold;"), 
                 uiOutput("upload_type_of_data")),
          column(width = 4, 
                 h5(tags$span(
                   "Define Survival Variable", 
                   bs4Dash::tooltip(
                     tag = icon("circle-info"), placement = "top", 
                     title = 'All variables will be automatically set as "Covariates", and users must select at least one "Time" and one "Censoring Indicator".'
                   )
                 ), style="font-weight: bold;"),
                 uiOutput("upload_survival_variable"))
        ), br(),
        #--------------------------#
        #--- run & split option ---#
        #--------------------------#
        fluidRow(
          column(
            width = 4, numericInputIcon(
              "random_seed", "Set seed for spliting data", sample(1:10000, 1),
              min = 1, step = 1, icon = icon("seedling")
            )),
          column(width = 4, numericInputIcon(
            "test_pct", "Set the proportion of the test set", 20, 
            min = 5, max = 95, step = 5, icon = list(NULL, icon("percent"))
          )),
          column(width = 4, br(), actionBttn('upload_transform_data', 'Transform Data', icon = icon('rotate'), style = "pill", color = "success"))
        ),
        htmltools::hr(), 
        HTML('<ul><b>Notification</b>: <br>
          <li><b>For censoring indicator, 1 = event, 0 = censoring.</b></li>
          <li>The website is deployed on shinyapps.io, which has a web idle time limit. If the analysis or imputation process takes too long, the connection may be interrupted, requiring a restart.</p>
          <li>The ID variable will be ignored in subsequent analysis options.</li>
          <li>Testing set is not available in survival analysis</li>
        </ul>')
      ),
      ##### [body] EDA ####
      bs4TabItem(
        tabName = "eda",
        fluidRow(
          column(4, pickerInput("eda_target_data", "Select the target dataset", choices = list())),
          column(4),
          column(4, pickerInput("eda_var_censor", "Censoring / Event Indicator", choices = list()))
        ), htmltools::hr(),
        tabsetPanel(
          type = "pills", id="EDA-tabset",
          ###### Categorical ######
          tabPanel(
            "Categorical Variable", br(),
            fluidRow(
              column(width = 3, pickerInput("user_var_cat", "Choose a Variable", choices = list())),
              column(2, 
                     checkboxInput("user_arrange_cat", "Arrange by frequency"),
                     checkboxInput("user_flip_cat", "Horizontal")),
              column(width=2, numericInput("user_ts_cat", "text label size", value = 5)),
              column(width=2, numericInput("user_adj_cat", tags$span("text label position", 
                                                                     bs4Dash::tooltip(tag = icon("circle-info"), placement = "right", 
                                                                             title = "Negative numbers are displayed above the bar, positive numbers below it, with the distance from zero indicating the magnitude of deviation from the bar.")), 
                                           value = -0.2, step = 0.1)),
              column(width=2, numericInput("user_angle_cat", "axis text rotate angle", value = 0, step = 5))
            ),
            htmltools::hr(), h6(icon("square-caret-down"), "Bar plot of selected variable"),
            plotOutput("user_plt_cat"),
            br(), h6(icon("square-caret-down"), "Table of selected variable"),
            DTOutput("user_tab_cat")
          ),
          ###### Continuous ######
          tabPanel(
            "Continuous Variable", br(),
            h6(icon("square-caret-down"), "Summary"),tableOutput("user_sum_con"),
            htmltools::hr(),fluidRow(
              column(
                width = 3,
                pickerInput("user_var_con", "Choose a Variable", choices = list())
              ),
              column(
                width = 1, br()
                # checkboxInput("interactive_con", "Interactive plot")
              ),
              column(
                width = 7,
                sliderInput("user_bin_con", tags$span("Number of bins",
                                                      bs4Dash::tooltip(tag = icon("circle-info"), placement = "right",
                                                              title = "The selection of the number of bins can significantly influence chart insights. If the number is too small, a substantial amount of information may be lost.")), 
                            min = 5, max = 100, value = 30, width = "100%")
              )
            ),
            htmltools::hr(), h6(icon("square-caret-down"), "Histogram of selected variable"),
            plotOutput("user_plt_con"),
            # plotlyOutput("user_plt_con")
          ),
          ###### Cross table ######
          tabPanel(
            "Cross Table", br(),
            uiOutput("eda_crosstable")
          )
        )
      ),
      ##### [body] Survival Right ####
      bs4TabItem(
        tabName = "survivalRight",
        #-----------------------#
        #--- time & censored ---#
        #-----------------------#
        fluidRow(
          column(4, pickerInput("surv_target_data", "Select the target dataset", choices = list())),
          column(4, pickerInput("surv_var_time", "Survival Time", choices = list())),
          column(4, pickerInput("surv_var_censor", "Censoring / Event Indicator", choices = list()))
        ), htmltools::hr(),
        tabsetPanel(
          type = "pills",
          ###### KM ######
          tabPanel(
            "Kaplan-Meier", br(),
            fluidRow(
              column(
                width = 10, 
                pickerInput("user_var_surv", "Strata Variable",
                            options = pickerOptions(container = "body"), choices = c())
              ),
              column(
                width = 1, offset = 1, br(), actionButton('user_act_km', 'Plot', icon = icon('chart-line'), status = "primary", outline = T, width = "100%")
              )
            ), htmltools::hr(),
            h6(icon("square-caret-down"), "Kaplan-Meier plot strata by selected variable"),
            plotOutput("user_plt_km"),
            # plotlyOutput("user_plt_km"),
            br(), h6(icon("square-caret-down"), "Summary of Kaplan-Meier estimate"),
            tableOutput("user_tab_km_all"), htmltools::hr(),
            uiOutput('user_view_km_detail')
            
          ),
          ###### Cox PH ######
          tabPanel(
            "Cox Proportional Hazards Model", br(),
            fluidRow(
              id = "COX",
              column(10,
                     virtualSelectInput("user_var_cox", "Select covariate(s)",
                                        choices = c(), dropboxWrapper = "body", 
                                        optionsCount = 5, multiple = TRUE, width = "100%")),
              column(1, offset = 1, br(), actionButton('user_act_cox', 'Fit', icon = icon('dumbbell'), status = "primary", outline = T, width = "100%"))
            ), 
            htmltools::hr(),
            tabsetPanel(
              type = "pills", 
              tabPanel(
                "Detail", br(),
                h6(icon("square-caret-down"), "Cox Proportional Hazards model"),
                # verbatimTextOutput("user_txt_cox_form"),
                helpText("$$h(t|X=x_1,\\cdots,x_k)=h_0(t)\\times exp(\\beta_1x_1+\\beta_2x_2+\\cdots +\\beta_kx_k)$$",
                         "$$HR=\\frac{h(t|x_1=1)}{h(t|x_1=0)}=exp(\\beta_1)$$"),
                br(), h6(icon("square-caret-down"), "Estimated coefficient and Single test \\(H_0:\\beta=0\\)"),
                htmlOutput("user_cox_single"),
                br(), h6(icon("square-caret-down"), "Global test \\(H_0:\\mbox{all}\\ \\beta_i=0\\)"),
                htmlOutput("user_cox_global"),
                br(), fluidRow(
                  h6(icon("square-caret-down"), "Harrell’s C-index (concordance index)"),
                  column(1, bs4Dash::tooltip(
                    tag = icon("circle-info"), placement = "right",
                    title = "The concordance of the test set is the concordance obtained by using the trained model to predict the test set data."
                  ))
                ),
                htmlOutput("user_cox_concordance")
                # verbatimTextOutput("user_cox_concordance")
              ),
              tabPanel(
                "Plot (overall)", br(),
                fluidRow(
                  h6(icon("square-caret-down"), "Cox PH model fitted survival plot (by default at the mean values of the covariates)  "),
                  column(1, bs4Dash::tooltip(
                    tag = icon("circle-info"), placement = "right",
                    title = "Categorical variable will choose the most one class, if there are multiple class, pick the first one in alphabetical order."
                  )) 
                ),
                plotOutput("user_plt_cox")
              ),
              tabPanel(
                "Plot (with strata variable)", br(), 
                uiOutput("user_cox_mod_var"),
                h6(icon("square-caret-down"), "Cox PH model fitted survival plot (with strata variable)"),
                plotOutput("user_plt_cox_sing")
              )
            )
          ),
          ###### threshold ######
          tabPanel(
            "Threshold Regression Model", br(),
            fluidRow(
              id = "THREG",
              column(5,
                     virtualSelectInput("threg_var_mu", 
                                        tags$span("Select covariate(s) for \\(\\mu\\)", 
                                                  bs4Dash::tooltip(tag = icon("circle-info"), placement = "right", 
                                                                   title = "The drift of the Wiener process, is the rate per unit time at which the level of the sample path is changing. The sample path approaches the threshold if \\(\\mu<0\\).")),
                                        choices = c(), dropboxWrapper = "body", 
                                        optionsCount = 5, multiple = TRUE, width = "100%")),
              column(5,
                     virtualSelectInput("threg_var_y0", 
                                        tags$span("Select covariate(s) for \\(ln(y_0)\\)", 
                                                  bs4Dash::tooltip(tag = icon("circle-info"), placement = "right", 
                                                                   title = "The initial value of the process and is taken as positive.")),
                                        choices = c(), dropboxWrapper = "body", 
                                        optionsCount = 5, multiple = TRUE, width = "100%")),
              column(1, offset = 1, br(), actionButton('fit_threg', 'Fit', icon = icon('dumbbell'), status = "primary", outline = T, width = "100%"))
            ), 
            htmltools::hr(),
            tabsetPanel(
              type = "pills", 
              tabPanel(
                "Formula", br(),
                helpText(
                  "A Wiener process \\(Y(t)\\) is used to model the latent health status process. An event is observed when \\(Y(t)\\) reaches 0 for the first time.",
                  br(),
                  "Wiener process involve three parameters: \\(\\mu\\), \\(y_0\\) and \\(\\sigma\\). Since the health status process is usually latent (i.e., unobserved), an arbitrary unit can be used to measure such a process. Hence the variance parameter \\(\\sigma^2\\) of the process is set to 1.",
                  br(),
                  "Assume that \\(\\mu\\) and \\(ln(y_0)\\) are linear in regression coefficients. Then they can be linked to the covariates with the following regression form: ",
                  "$$ln(y_0)=\\gamma_0+\\gamma_1X_1+\\cdots+\\gamma_kX_k$$",
                  "$$\\mu=\\beta_0+\\beta_1X_1+\\cdots+\\beta_kX_k$$",
                  br(),
                  "The first hitting time (FHT) of a Wiener process with \\(\\mu\\), \\(y_0\\) and \\(\\sigma=1\\) is an inverse Gaussian distribution with probability density function (p.d.f):",
                  "$$f(t|\\mu,y_0)=\\frac{y_0}{\\sqrt{2\\pi t^3}}exp[-\\frac{(y_0+\\mu t)^2}{2t}]$$",
                  br(),
                  "$$S(T=t|\\hat\\mu^g,\\hat y_0^g)=1-F(T=t|\\hat\\mu^g,\\hat y_0^g)$$",
                  "$$h(T=t|\\hat\\mu^g,\\hat y_0^g)=\\frac{f(T=t|\\hat\\mu^g,\\hat y_0^g)}{S(T=t|\\hat\\mu^g,\\hat y_0^g)}$$",
                  "$$HR=\\frac{h(T=t|\\hat\\mu^g,\\hat y_0^g)}{h(T=t|\\hat\\mu^0,\\hat y_0^0)}$$"
                )
              ),
              tabPanel(
                "Detail",
                br(), h6(icon("square-caret-down"), "Estimated coefficient and Single test \\(H_0:\\beta=0\\) or \\(H_0:\\gamma=0\\)"),
                htmlOutput("threg_coef"),
                br(), h6(icon("square-caret-down"), "Log likelihood and AIC"),
                htmlOutput("threg_test")
              ),
              tabPanel(
                "Plot", br(),
                sidebarLayout(
                  sidebarPanel(
                    width=2,
                    pickerInput("threg_plt_type", "Type of curves", 
                                choices = list("Hazard","Survival","Densiity")),
                    pickerInput("threg_plt_var", "Strata variable", choices = list())
                  ),
                  mainPanel(
                    width=10,
                    h1("Future work", align = "center")
                  )
                )
              )
            )
          ),
          ###### Random Survival Forest ######
          tabPanel(
            "Random Survival Forest", br(),
            fluidRow(
              id = "RSF",
              column(
                4, fluidRow(
                  column(5, numericInputIcon(
                    "rf_ntree", HTML("ntree<br>Number of trees"),
                    300, step = 50, min = 50,
                    icon = icon("tree")
                  )),
                  column(7, numericInputIcon(
                    "rf_mtry", tags$span(
                      HTML("mtry"),
                      bs4Dash::tooltip(
                        tag = icon("circle-info"), placement = "top",
                        title = "The mtry parameter is number of variables randomly selected as candidates for splitting a node."
                      ),
                      HTML("<br>Number of candidates variables"),
                    ), 3, step = 1, min = 1
                  ))
                )
              ),
              column(
                8, fluidRow(
                  column(6, numericInputIcon(
                    "rf_nodesize", tags$span(
                      HTML("nodesize"),
                      bs4Dash::tooltip(
                        tag = icon("circle-info"), placement = "top",
                        title = "Increase nodesize can speed up the calculation. In some big data settings this can also lead to better prediction performance."
                      ),
                      HTML("<br>Minumum number of observations in terminal node")
                    ), 15, width = "100%"
                  )),
                  column(6, numericInputIcon(
                    "rf_block_size", tags$span(
                      HTML("block size"),
                      bs4Dash::tooltip(
                        tag = icon("circle-info"), placement = "top",
                        title = "The smaller the number, the more time it takes."
                      ),
                      HTML("<br>To view the cumulative error rate every \\(n\\)th tree")
                    ), value = 300, min = 1, max = 300, step = 10, width = "100%"
                  ))
                )
              )
            ),
            fluidRow(
              column(7, virtualSelectInput(
                "rf_var", "Select covariate(s) for fitting",
                choices = c(), dropboxWrapper = "body", 
                optionsCount = 5, multiple = TRUE, width = "100%"
              )),
              column(2, selectInput(
                "rf_splitrule", "Spliting rule", choices = c("Log-rank", "Log-rank score")
              )),
              column(3, fluidRow(
                column(6, br(), actionButton('tune_rf', 'Tune parameter', icon = icon('dumbbell'), status = "secondary", outline = T, width = "100%")),
                column(6, br(), actionButton('fit_rf', 'Fit', icon = icon('dumbbell'), status = "primary", outline = T, width = "100%"))
              ))
            ),
            htmltools::hr(),
            tabsetPanel(
              type = "pills",
              tabPanel(
                "Information",
                HTML('
                <ul><b>Notification:</b> <br>
                  <li>When generating training datasets using the Bootstrap method, approximately 36% of the original samples will not appear in the training dataset (for all trees). 
                  These samples are referred to as Out-Of-Bag (OOB) data. Evaluating the model using these data is called OOB estimation. 
                  <b>Using OOB is equivalent to using a test set</b>, so there is no need to use a separate test set.</li>
                  <li>Click "Tune parameter" to find the optimal mtry and nodesize tuning parameter for a random forest using out-of-sample error.</li>
                  <li>Since random forests use bootstrapping to sample data, tuning parameters with the <b>same variables may yield different results each time</b>.</li>
                  <li>The type of boostrap is <b>sampling without replacement</b>. </li>
                </ul><br>
                <ul><b>Terminal node statistics (TNS): </b><br>
                  <li>RSF estimates the survival function and the cumulative hazard function.
                      Because \\(H(t)=-log[S(t)]\\) will <b>not hold for the ensemble</b>.</li>
                  <li>For the \\(h\\) terminal node of the tree, <b>survival function</b> and the <b>cumulative hazard function</b> are estimated using the bootstrapped <b>Kaplan-Meier</b> and <b>Nelson-Aalen</b> estimators:
                      $$S_h(t)=\\prod_{t_{j,h}\\leq t}(1-\\frac{d_{j,h}}{Y_{j,h}}),\\;\\;\\; H_h(t)=\\sum_{t_{j,h}\\leq t}\\frac{d_{j,h}}{Y_{j,h}}$$</li>
                  <li>All case within the \\(h\\) terminal node are assigned identical survival function and the cumulative hazard function. 
                      This is because the survival tree aims to divide the data into homogeneous groups of individuals exhibiting similar survival behavior.</li>
                  <ul>
                    <li><b>In-bag (IB) estimator</b></li>
                      $$S^{IB}(t|\\mathbf{X})=S_h(t),\\;\\;\\; H^{IB}(t|\\mathbf{X})=H_h(t),\\;if\\;\\mathbf{X}\\in h$$
                    <li><b>Out-of-bag (OOB) estimators</b></li>
                      Let \\(I_i=1\\) if and only if \\(i\\) is OOB.
                      $$S^{OOB}(t|\\mathbf{X})=S_h(t),\\;\\;\\; H^{OOB}(t|\\mathbf{X})=H_h(t),\\;if\\;\\mathbf{X}\\in h\\;and\\;I_i=1$$
                  </ul>
                </ul>
                <ul><b>Ensemble survival function and cumulative hazard function: </b><br>
                  $$\\bar S^{IB}(t|\\mathbf{X})=\\frac{1}{ntree}\\sum_{b=1}^{ntree}S_b(t|\\mathbf{X})\\;\\;\\; \\bar H^{IB}(t|\\mathbf{X})=\\frac{1}{ntree}\\sum_{b=1}^{ntree}H_b(t|\\mathbf{X})$$
                  Let \\(O_i\\) record trees where case \\(i\\) is OOB. 
                  $$\\bar S^{OOB}(t|\\mathbf{X})=\\frac{1}{|O_i|}\\sum_{b\\in O_i}S_b(t|\\mathbf{X})\\;\\;\\; \\bar H^{OOB}(t|\\mathbf{X})=\\frac{1}{|O_i|}\\sum_{b\\in O_i}H_b(t|\\mathbf{X})$$
                </ul>
                     ')
              ),
              tabPanel(
                "Detail", br(),
                h6(icon("square-caret-down"), "Information about the forest"),
                htmlOutput("rf_detail"),
                br(), h6(icon("square-caret-down"), "Variable importance"),
                plotOutput("rf_vimp", height = "auto"),
                br(), h6(icon("square-caret-down"), "Harrell’s C-index (concordance index)"),
                htmlOutput("rf_concordance")
                
              ),
              tabPanel(
                "Plot", 
                br(), fluidRow(
                  h6(icon("square-caret-down"), "Cumulative Out-of-bag (OOB) error rate"),
                  column(1, bs4Dash::tooltip(
                    tag = icon("circle-info"), placement = "right",
                    title = "The graph is meaningful only when the block size is smaller than ntree."
                  ))
                ),
                plotOutput("rf_oob_err_rate_plot", height = "auto"),
                br(), h6(icon("square-caret-down"), "The marginal effect of an x-variable on the mortality"),
                plotOutput("rf_plot_variable", height = "auto"),
                htmltools::hr(), 
                uiOutput('rf_ui_treeID'),
                fluidRow(
                  h6(icon("square-caret-down"), "Extracted tree from the forest"),
                  column(1, bs4Dash::tooltip(
                    tag = icon("circle-info"), placement = "right",
                    title = "If the tree is too large, the graph may appear blurry."
                  ))
                ),
                plotOutput("rf_tree_plt", height = "auto")
              )
            )
          )
        )
      ),
      
      ##### [body] Survival Interval ####
      bs4TabItem(
        tabName = "survivalInterval",
        h1("Future work", align = "center")
      ),
      ##### [body] CondiS intro ####
      bs4TabItem(
        tabName = "imputation_intro",
        HTML('<div class="div-moon">
              <h1>Why use CondiS?</h1>
              <h3>Using machine learning models but worrying about <b>CENSORING</b>?<br>
              <b>CondiS</b> got you covered!</h3>
            </div>'), br(),
        sidebarLayout(
          sidebarPanel(
            h5(icon("paw"), "Advantage", style = "color: #FF8000; font-weight: bold;"),
            HTML("
              <p>Given that many ML approaches drop individuals where censoring occurs, applying the CondiS method to the datasets would <b>increase the effective sample size</b>.</p>
              <p><b>Not strongly rely on the tuning parameter</b>. (<i>Pseudo</i> method is heavily relies on a tuning parameter)</p>
            "), br(),
            h5(icon("paw"), "Condis", style = "color: #F75000; font-weight: bold;"),
            HTML('
              <p>CondiS <b>imputes survival times for censored observations</b> based on their conditional survival distributions derived from Kaplan-Meier estimators.</p>
              <p>(Compare to <i>Pseudo</i> method will imputed all observations, and imputed times usually are different from the true times.)</p>
            '), br(),
            h5(icon("paw"), "Condis-X", style = "color: #FF5809; font-weight: bold;"),
            HTML('
              <p>When covariates are available, CondiS is extended by incorporating the covariate information through regression modeling (CondiS-X), which further increases the accuracy of the imputed survival time.
              8 machine learning algorithms, including: "glm", "ridge", "lasso", "gmathbf", "rf", "svm", "knn", "ann".</p>
              <p>Regarding which ML algorithm to use, it depends on the data. If there are lots of non-linear relationships and interactions in the data, nonparametric models might be the appropriate choice.</p>
              <b>* Condis-X is applied only to the "complete" dataset, i.e. dataset without censoring, covariate(s) have no missing. </b>
            ')
          ),
          mainPanel(
            h4(icon("cat"), "Schematic diagram of this package", icon("cat"), 
               style="text-align: center; color: #003060;  font-weight: bold;"),
            img(src="condis schema.png", width="100%"),
            HTML('<p>Image Source: <a href="https://biostatistics.mdanderson.org/shinyapps/CondiS/">https://biostatistics.mdanderson.org/shinyapps/CondiS/</a></p>')
          )
        )
      ),
      ##### [body] CondiS ####
      bs4TabItem(
        tabName = "imputation", 
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h5(icon("paw"), "CondiS", style = "font-weight: bold;"),
            pickerInput("impu_var_time", "Survival Time", choices = list()),
            pickerInput("impu_var_censor", "Censoring / Event Indicator", choices = list()),
            numericInput(
              inputId = "impu_tmax", value = 0,
              label = tags$span(
                "maximum follow-up time",
                bs4Dash::tooltip(
                  tag = icon("circle-info"), placement = "right", 
                  title = "A self-defined time-of-interest point. It is defaulted as the maximum follow up time."
                )
              )
            ), 
            actionBttn("impu_condis_go", "Run Imputation (CondiS)", icon=icon("play"), style = "unite", color = "success"),
            downloadButton('download_complete_condis', 'Download Imputed Data (CondiS)',
                           style="margin-top: 10px;") |>
              shinyjs::disabled(), 
            htmltools::hr(),
            #----------------#
            #--- CondiS-X ---#
            #----------------#
            fluidRow(
              h5(icon("paw"), "CondiS-X", style = "font-weight: bold;"),
              column(1, bs4Dash::tooltip(
                tag = icon("circle-info"), placement = "right",
                title = 'Condis-X is applied only to the "complete" dataset, i.e. run CondiS-X after CondiS.'
              )) 
            ),
            virtualSelectInput("impu_covariate", 
                               tags$span("Select covariate(s)", 
                                         bs4Dash::tooltip(tag = icon("circle-info"), placement = "right", 
                                                          title = "Since CondiS-X uses preProcess from caret to scale covariates, this function allows non-numeric predictors but will ignore them. This means that only numeric covariates are acceptable.")),
                               choices = c(), dropboxWrapper = "body", 
                               optionsCount = 5, multiple = TRUE, width = "100%") |>
              shinyjs::disabled(),
            pickerInput("impu_method", "Select method",
                        options = pickerOptions(container = "body"), 
                        choices = c("glm", "ridge", "lasso", "rf", "svm", "knn", "ann")),
            actionBttn("impu_condisX_go", "Run Imputation (CondiS-X)", icon=icon("play"), style = "unite", color = "success") |>
              shinyjs::disabled(),
            downloadButton('download_complete_condisX', 'Download Imputed Data (CondiS-X)',
                           style="margin-top: 10px;") |>
              shinyjs::disabled()
          ),
          #------------#
          #--- main ---#
          #------------#
          mainPanel(
            width = 9,
            fluidRow(
              h6(icon("square-caret-down"), "Data (first 100 rows)"),
              column(1, bs4Dash::tooltip(
                tag = icon("circle-info"), placement = "right",
                title = "CondiS only impute censored observation. "
              )) 
            ), 
            column(
              width=12, dataTableOutput("impu_condis_tab"),
              style = "height:400px; overflow-y: scroll; overflow-x: scroll;"
            ), br(),
            h6(icon("square-caret-down"), "Plot (Kaplan-Meier Plot vs. Imputed Survival Distribution)"),
            plotOutput("impu_condis_plt")
          )
        )
      )
    )
  ), 
  #### options ####
  dark = NULL, help = NULL, fullscreen = T
)