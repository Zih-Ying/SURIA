options(shiny.maxRequestSize = 30 * 1024^2) # maximum upload size: 30MB

# optimize fihser
my_test_args=crosstable_test_args()
my_test_args$test_tabular = function (x, y) 
{
  tab = table(x, y)
  exp = rowSums(tab) %*% t(colSums(tab))/sum(tab)
  if (any(dim(table(x, y)) == 1)) 
    test = list(p.value = NULL, method = NULL)
  else if (all(exp >= 5)) 
    test = chisq.test(x, y, correct = FALSE)
  else test = fisher.test(x, y, simulate.p.value = T)
  p = test$p.value
  method = test$method
  list(p.value = p, method = method)
}


function(input, output, session) {
  
  #### download data ####
  output$download_covid <- downloadHandler(
    filename = function() {
      "COVID-19 dataset.csv"
    },
    content = function(file) {
      write.csv(readRDS("data/2021Covid.rds"), file, row.names = FALSE)
    }
  )
  output$download_covid_mini <- downloadHandler(
    filename = function() {
      "COVID-19 sub-dataset.csv"
    },
    content = function(file) {
      write.csv(readRDS("data/2021Covid_mini.rds"), file, row.names = FALSE)
    }
  )
  output$download_lung <- downloadHandler(
    filename = function() {
      "Lung cancer dataset.csv"
    },
    content = function(file) {
      write.csv(readRDS("data/SEER lung.rds"), file, row.names = FALSE)
    }
  )
  output$download_lung_mini <- downloadHandler(
    filename = function() {
      "Lung cancer sub-dataset.csv"
    },
    content = function(file) {
      write.csv(readRDS("data/SEER lung_mini.rds"), file, row.names = FALSE)
    }
  )
  output$download_condis <- downloadHandler(
    filename = function() {
      "imputation sample data.csv"
    },
    content = function(file) {
      write.csv(readRDS("data/CondiS_sample.rds"), file, row.names = FALSE)
    }
  )
  #### upload new data ####
  #----------------------------------------------------------------------#
  #--- uploaded data; data variable; number of variable               ---#
  #--- continuous variable; categorical variable                      ---#
  #--- dataframe of define: variable, type, survVar; transformed data ---#
  #----------------------------------------------------------------------#
  uploaded_data <- reactiveValues(upload_dat=NA, dat_var=NA, num_var=NA,
                                  var_con=NA, var_cat=NA, dat_define=NA, 
                                  new_dat=NA, train=NA, test=NA)
  get_upload_data <- reactive({
    req(input$upload_data)
    
    # get data
    ext <- tools::file_ext(input$upload_data$name)
    uploaded_data$upload_dat <- switch(ext,
           csv = vroom::vroom(input$upload_data$datapath, delim = ","),
           txt = vroom::vroom(input$upload_data$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .txt file")
    )
    uploaded_data$dat_var <- colnames(uploaded_data$upload_dat)
    uploaded_data$num_var <- length(uploaded_data$dat_var)
    
    #----------------------------------#
    #--- initialize condis function ---#
    #----------------------------------#
    impute_data$butt_condis <- impute_data$butt_condisX <- 0
    disable("impu_condisx_go")
  })
  
  ##### show variable for user define #####
  observeEvent(input$upload_data, {
    get_upload_data()
    
    #--- view data ---#
    output$upload_data_view <- renderDataTable({
      datatable(head(uploaded_data$upload_dat, 50), options = list(dom = "ft", paging=F))
    })
    
    variables <- uploaded_data$dat_var
    n <- uploaded_data$num_var
    
    output$upload_data_variable <- renderUI({
      tagList(
        lapply(1:n,
               function(i){
                 column(width = 12, style="margin-bottom: 2px;",
                        textInput(paste0("upload_var_name", i),
                                  label = NULL,
                                  value = variables[i]) |>
                          shinyjs::disabled())
               }) |>
        fluidRow(),
        lapply(1:n, function(i) tags$style(type="text/css", paste0("#upload_var_name", i, "{height:32.2px}")))
      )
    })
    output$upload_type_of_data <- renderUI({
      lapply(1:n,
             function(i){
               selectInput(paste0("upload_var_type", i), label = NULL,
                           choices = c("Continuous", "Categorical", "ID"))
             })
    })
    output$upload_survival_variable <- renderUI({
      lapply(1:n,
             function(i){
               selectInput(paste0("upload_var_surv", i),
                           label = NULL, selected = "Covariate", 
                           choices = c("Time", "Censoring Indicator", "Covariate", "ID"))
             })
    })
    
    var_type <- ifelse(sapply(uploaded_data$upload_dat, class)=="numeric", 
                       "Continuous", "Categorical")
    lapply(1:n, 
           function(i) updateSelectInput(session, paste0("upload_var_type", i),
                                         selected = var_type[i]))
  })
  
  ##### reactive: transform data: con -> num, cate -> factor #####
  transform_data <- reactive({
    req(input$upload_data)

    survVar <- sapply(1:uploaded_data$num_var, function(i) parse(text=paste0("input$upload_var_surv", i)) |> eval())
    type <- sapply(1:uploaded_data$num_var, function(i) parse(text=paste0("input$upload_var_type", i)) |> eval())

    if(
      length(which(survVar == "Time")) < 1|
      length(which(survVar == "Censoring Indicator")) < 1
    ){
      showNotification('Users must select at least one "Time" and one "Censoring Indicator". <(｀^′)>', duration = 3, type='warning')
    # } else if (type[which(survVar == "Censoring Indicator")] != "Continuous"){
      # showNotification('Censoring indicator must be a continuous variable. (。•ω•)σ)´Д`)', duration = 3, type='warning')
    } else {
      uploaded_data$dat_define <- data.frame(
        variable = uploaded_data$dat_var,
        type = type,
        survVar = survVar
      )
      #--- continuous variables ---#
      uploaded_data$var_con <- uploaded_data$dat_define$variable[
        uploaded_data$dat_define$type == "Continuous" &
          uploaded_data$dat_define$survVar != "Censoring Indicator"
      ]
      #--- categorical variables ---#
      uploaded_data$var_cat <- uploaded_data$dat_define$variable[
        uploaded_data$dat_define$type == "Categorical" |
          uploaded_data$dat_define$survVar == "Censoring Indicator"
      ]
      #--- after transformed data ---#
      uploaded_data$new_dat <- uploaded_data$upload_dat %>%
        mutate(across(all_of(uploaded_data$var_con), as.numeric),
               across(all_of(uploaded_data$var_cat), as.factor)) %>%
        as.data.frame()

      showNotification('Transforming data ๛ก(ｰ̀ωｰ́ก)', duration = 3)
    }
  })
  
  ###### function: get grouped picker input #####
  grouped_pickerinput <- function(target, groupName = ""){
    if(groupName != ""){
      tmp1 <- uploaded_data$dat_define %>%
        filter(variable %in% target) %>%
        group_split(!!sym(groupName)) # split to list
      tmp2 <- sapply(1:length(tmp1), function(x) tmp1[[x]][groupName] |> unique()) # group name
      out <- tmp1 %>%
        setNames(tmp2) %>%
        map(~ as.list(.x$variable))
      return(out)
    }
  }
  
  ###### *update picker input with group ######
  observeEvent(input$upload_transform_data, {
    uploaded_data$new_dat <- NA
    transform_data()
    req(uploaded_data$new_dat)
    #-------------------#
    #--- train, test ---#
    #-------------------#
    set.seed(input$random_seed)
    id <- sample(nrow(uploaded_data$new_dat), nrow(uploaded_data$new_dat)*(input$test_pct/100))
    uploaded_data$test <- uploaded_data$new_dat[id,]
    uploaded_data$train <- uploaded_data$new_dat[-id,]
    showNotification(
      paste("Training set size =", format(nrow(uploaded_data$train), big.mark = ","), 
            "; Testing set size =", format(nrow(uploaded_data$test), big.mark = ",")),
      duration = 10, type = "message"
    )
    updatePickerInput(session, "eda_target_data", choices = c("Entire dataset", "Training set", "Testing set"))
    updatePickerInput(session, "surv_target_data", choices = c("Entire dataset", "Training set"))
    
    req(uploaded_data$dat_define)
    #-----------#
    #--- EDA ---#
    #-----------#
    updatePickerInput(session, "user_var_cat",
                      choices = grouped_pickerinput(uploaded_data$var_cat, "survVar"))
    updatePickerInput(session, "user_var_con",
                      choices = grouped_pickerinput(uploaded_data$var_con, "survVar"))

    #---------------------#
    #--- surv & CondiS ---#
    #---------------------#
    # time & censored
    choices_time <- uploaded_data$dat_define[
      which(uploaded_data$dat_define['survVar']=="Time"), "variable"
    ]
    choices_censor <- uploaded_data$dat_define[
      which(uploaded_data$dat_define['survVar']=="Censoring Indicator"), "variable"
    ]
    updatePickerInput(session, "eda_var_censor", choices = choices_censor)
    
    updatePickerInput(session, "surv_var_time", choices = choices_time)
    updatePickerInput(session, "surv_var_censor", choices = choices_censor)

    updatePickerInput(session, "impu_var_time", choices = choices_time)
    updatePickerInput(session, "impu_var_censor", choices = choices_censor)

    #-----------------#
    #--- covariate ---#
    #-----------------#
    # KM: only categorical
    choices_surv <- uploaded_data$dat_define[
      uploaded_data$dat_define['survVar']=="Covariate" &
        uploaded_data$dat_define['type']=="Categorical",
      'variable'
    ] |>
      grouped_pickerinput("type")
    updatePickerInput(session, "user_var_surv",
                      choices = choices_surv)

    # Cox, threg, RSF: categorical or continuous
    choices_vec <- uploaded_data$dat_define[
      uploaded_data$dat_define['survVar']=="Covariate" &
        uploaded_data$dat_define['type']!="ID",
      'variable'
    ]
    choices_cc <- choices_vec |>
      grouped_pickerinput("type")
    updateVirtualSelect("user_var_cox", choices = choices_cc)
    updateVirtualSelect("threg_var_mu", choices = choices_cc)
    updateVirtualSelect("threg_var_y0", choices = choices_cc)
    updateVirtualSelect("rf_var", choices = choices_cc, selected = choices_vec)

    # condis-X: only numeric
    choices_condis <- uploaded_data$dat_define[
      uploaded_data$dat_define['survVar']=="Covariate" &
        uploaded_data$dat_define['type']=="Continuous",
      'variable'
    ]
    if(length(choices_condis)>0){
      updateVirtualSelect("impu_covariate",
                          choices = grouped_pickerinput(choices_condis,"type"))
    }else{
      updateVirtualSelect("impu_covariate",
                          choices = c())
    }
  })
  
  #### storage ####
  user_store <- reactiveValues(
    plt_cat=NA, tab_cat=NA, # categorical plot, table
    tab_km=NA, plt_km=NA, detail_km=NA, # KM
    form_cox=NA, glob_cox=NA, sing_cox=NA, mod_cox=NA, # formula, global, single, model
    c_index=NA, c_index_test=NA, # C index
    plt_cox=NA, plt_cox_var_all=NA, # plot, fitted model's var
    plt_cox_var_sing=NA # cox's strata var
  )
  eda_process <- reactive({
    req(uploaded_data$new_dat, input$eda_target_data)
    dat <- switch(
      input$eda_target_data,
      "Entire dataset" = uploaded_data$new_dat, 
      "Training set" = uploaded_data$train, 
      "Testing set" = uploaded_data$test
    )
    return(dat)
  })
  ##### EDA: categorical #####
  user_react_cat <- reactive({
    dat <- eda_process()
    out <- func_cat(dat, 
                    input$user_var_cat, input$user_flip_cat, input$user_arrange_cat,
                    ts=input$user_ts_cat, adj=input$user_adj_cat, angle=input$user_angle_cat)
    user_store$plt_cat <- out[[1]]
    user_store$tab_cat <- out[[2]]
  })
  output$user_plt_cat <- renderPlot({
    user_react_cat()
    req(user_store$plt_cat)
    user_store$plt_cat
  })
  output$user_tab_cat <- renderDT({
    req(user_store$plt_cat)
    user_store$tab_cat
  })
  ##### EDA: continuous #####
  # summary
  output$user_sum_con <- renderTable({
    dat <- eda_process()
    vtable::sumtable(
      dat[uploaded_data$var_con], 
      summ = c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)','pctile(x)[75]','max(x)'),
      summ.names = c("N","Mean","Std.","Min","Q1","Median","Q3","Max"),
      out="return"
    )
  })
  
  # plt
  output$user_plt_con <- renderPlot({
  # output$user_plt_con <- renderPlotly({
    dat <- eda_process()
    p <- plt_con(dat, input$user_var_con, input$user_bin_con)
    p
  })
  
  ##### EDA: cross table #####
  w <- Waiter$new(id = "eda_crosstable", html = spin_throbber())
  output$eda_crosstable <- renderUI({
    dat <- eda_process()
    req(input$eda_var_censor)
    w$show()
    crosstable(
      dat, by=input$eda_var_censor,
      total = "row", percent_pattern = "{n} ({p_col})", 
      test=T, test_args = my_test_args,
      funs = c(`Min / Max` = minmax,
               `Median [IQR]` = mediqr,
               `Mean (std dev)` = meansd)
    ) %>%
      as_flextable(header_show_n = T) %>%
      htmltools_value()
  })
  #### storage ####
  surv_process <- reactive({
    req(uploaded_data$new_dat, input$surv_target_data, input$surv_var_censor)
    dat <- switch(
      input$surv_target_data,
      "Entire dataset" = uploaded_data$new_dat, 
      "Training set" = uploaded_data$train
      # "Testing set" = uploaded_data$test
    ) %>% 
      # censor 要轉回連續
      mutate(across(input$surv_var_censor, ~ as.numeric(.x)-1))
    return(dat)
  })
  ##### Surv: KM #####
  user_react_km <- reactive({
    dat <- surv_process()
    out <- calc_km(dat, input$user_var_surv,
                   input$surv_var_time, input$surv_var_censor)

    user_store$tab_km <- out[[1]]
    user_store$plt_km <- out[[2]]
    user_store$detail_km <- out[[3]]
  })
  
  observeEvent(input$user_act_km, {
    user_react_km()

    output$user_tab_km_all <- renderTable({
      user_store$tab_km
    })
    output$user_plt_km <- renderPlot({
    # output$user_plt_km <- renderPlotly({
      user_store$plt_km
      # ggplotly(user_store$plt_km, tooltip = c("x","y"))
    })
    output$user_view_km_detail <- renderUI({
      cat <- levels(user_store$detail_km$class)
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput('user_km_var_class', 'Choose a class', cat),
          htmltools::hr(), 
          verbatimTextOutput("user_txt_km_class")
        ),
        mainPanel(
          width = 9,
          h6(icon("square-caret-down"), "Detail of selected class"),
          tableOutput("user_tab_km_class")
        )
      )
      # tagList(
      #   selectInput('user_km_var_class', 'Choose a class', cat),
      #   htmltools::hr(), 
      #   verbatimTextOutput("user_txt_km_class")
      # )
    })
  })
  observeEvent(input$user_km_var_class,{
    if(!is.null(input$user_km_var_class)){
      output$user_txt_km_class <- renderPrint({
        paste('Detail of', input$user_var_surv, '=', input$user_km_var_class)
      })
      output$user_tab_km_class <- renderTable({
        user_store$detail_km %>%
          filter(class==input$user_km_var_class) %>%
          select(-class)
      }, digits = 4)
    }
  })
  
  ##### Surv: Cox ##### 
  #-------------------------------------------------#
  #--- show cox model's detail and plot (1 surv) ---#
  #-------------------------------------------------#
  user_react_cox <- reactive({
    dat <- surv_process()
    out <- calc_cox(dat, input$user_var_cox, 
                    input$surv_var_time, input$surv_var_censor)
    # user_store$form_cox <- out[["formula"]]
    user_store$glob_cox <- out[[2]]
    user_store$sing_cox <- out[[3]]
    user_store$mod_cox <- out[["model"]]
    user_store$plt_cox <- out[["plot"]]
    user_store$c_index <- out[["c.index"]]
    
    test <- uploaded_data$test %>% 
      mutate(across(input$surv_var_censor, ~as.numeric(.x)-1))
    # pred <- predict(user_store$mod_cox, newdata = test)
    # form <- paste("Surv(", input$surv_var_time, ",", input$surv_var_censor, ") ~ pred")
    # user_store$c_index_test <- concordance(formula(form), data = test)$concordance |>
    #   round(4)
    user_store$c_index_test <- concordance(user_store$mod_cox, newdata = test)$concordance |>
        round(4)
    
    user_store$plt_cox_var_all <- input$user_var_cox # all var fit to model
  })
  observeEvent(input$user_act_cox, {
    # waiter_show(html = waiting_screen_surv, color = "black")
    waitress <- Waitress$new("#COX", theme = "overlay-radius", infinite = T)
    waitress$start(h4("The computation may take some time. Please wait patiently."))
    user_react_cox()
    waitress$close()
    # waiter_hide()
    
    output$user_cox_global <- function(){
      knitr::kable(user_store$glob_cox, format = "html",
                   col.names = c("Likelihood ratio", "Scrore (logrank)", "Wald")) %>%
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE)
    }
    output$user_cox_single <- function(){
      knitr::kable(user_store$sing_cox, format = "html") %>%
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE)
    }
    output$user_cox_concordance <- function(){
      data.frame(Concordance = c(user_store$c_index, user_store$c_index_test)) |>
        `rownames<-`(c("target data","testing data")) |>
        knitr::kable(format = "html") |>
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE)
    }

    output$user_plt_cox <- renderPlot({
      user_store$plt_cox
    })

    output$user_cox_mod_var <- renderUI({
      if(length(user_store$plt_cox_var_all)>0){
        if(length(user_store$plt_cox_var_all)==1){
          choice <- user_store$plt_cox_var_all
        } else{
          choice <- user_store$plt_cox_var_all[
            sapply(uploaded_data$new_dat[,user_store$plt_cox_var_all],is.factor)
          ]
        }
        if(length(choice)<1){
          p(icon("quote-right"),"There is no available strata variables.",icon("quote-right"))
        }else{
          tagList(
            pickerInput("user_coxmod_var", "Select a Strata Variable",
                        choices = choice,
                        options = pickerOptions(container = "body", size = 5)),
            htmltools::hr(class = "hr-twill")
          )
        }
      }else{
        p(icon("quote-right"),"There is no covariates.",icon("quote-right"))
      }
    })
  })
  
  #------------------------------------------#
  #--- cox model's plot (different class) ---#
  #------------------------------------------#
  user_react_cox_class <- reactive({
    req(input$user_coxmod_var)
    dat <- surv_process()
    out <- plt_strata_cox(dat, user_store$mod_cox, 
                          user_store$plt_cox_var_all, input$user_coxmod_var)
    user_store$plt_cox_var_sing <- out[["plot"]]
  })
  observeEvent(input$user_coxmod_var, {
    req(input$user_coxmod_var)
    user_react_cox_class()
    
    output$user_plt_cox_sing <- renderPlot({
      user_store$plt_cox_var_sing
    })
  })
  
  ##### Surv: Threg ##### 
  observeEvent(input$fit_threg, {
    dat <- surv_process()
    req(input$threg_var_mu, input$threg_var_y0)
    # waiter_show(html = waiting_screen_surv, color = "black")
    waitress <- Waitress$new("#THREG", theme = "overlay-radius", infinite = T)
    waitress$start(h4("The computation may take some time. Please wait patiently."))
    
    mu <- paste(input$threg_var_mu, collapse = " + ")
    y0 <- paste(input$threg_var_y0, collapse = " + ")
    form <- paste("Surv(", input$surv_var_time, ",", input$surv_var_censor, ") ~", 
                      y0, "|", mu)
    fit <- threg(as.formula(form), data=dat)
    
    coef <- fit$coefficients
    se <- sqrt(diag(fit$var))
    z <- coef/se
    p <- signif(1 - pchisq((coef/sqrt(diag(fit$var)))^2, 1), 
                max(options()$digits - 4))
    p.value <- case_when(p<0.001 ~ "<0.001",
                   p<0.01 ~ "<0.01",
                   p<0.05 ~ "<0.05",
                   .default = as.character(round(p,3)))
    
    output$threg_coef <- function(){
      data.frame(coef, se, z, p.value) |>
        knitr::kable(format = "html", digits = 3) %>%
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE)
    }
    output$threg_test <- function(){
      t(data.frame(fit$loglik, fit$AIC)) |>
        `rownames<-`(c("Log likelihood", "AIC")) |>
        knitr::kable(format = "html", digits = 3, col.names = NULL) |>
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE, background = "#284f74", color = "#ffffff")
    }
    
    waitress$close()
    # waiter_hide()
  })
  
  #### Surv: RSF ####
  rsf_store <- reactiveValues(model = NA, plt_height = NA, plt_height_vimp = NA,
                              err_df = NA, vimp_df = NA)
  rsf_process <- reactive({
    req(uploaded_data$new_dat, input$surv_target_data, input$rf_var)
    rsf_store$plt_height <- ceiling(length(input$rf_var)/4)*300
    rsf_store$plt_height_vimp <- length(input$rf_var)*30
    
    dat <- switch(
      input$surv_target_data,
      "Entire dataset" = uploaded_data$new_dat,
      "Training set" = uploaded_data$train
      # "Testing set" = uploaded_data$test
    ) %>%
      mutate(across(!!sym(input$surv_var_censor), ~ as.numeric(.x)-1))
    x <- paste(input$rf_var, collapse = " + ")
    form <- paste("Surv(", input$surv_var_time, ",", input$surv_var_censor, ") ~", x)
    return(list("data"=dat, "formula"=form))
  })
  observeEvent(input$rf_ntree, {
    n <- input$rf_ntree
    updateNumericInputIcon(session, "rf_block_size", value = floor(n/10), max = n)
  })
  observeEvent(input$rf_var, {
    k = length(input$rf_var)
    updateNumericInputIcon(session, "rf_mtry", 
                           value = round(sqrt(k)), max = k)
  })
  ##### Surv: RSF: Tune ##### 
  observeEvent(input$tune_rf, {
    req(input$rf_var)
    # waitress <- Waitress$new("#RSF", theme = "overlay-radius", infinite = T)
    # waitress$start(h4("Computing optimal parameter..."))
    waitress1 <- Waitress$new("#rf_nodesize", theme = "overlay-percent", infinite = T)
    waitress2 <- Waitress$new("#rf_mtry", theme = "overlay-percent", infinite = T)
    waitress1$start(); waitress2$start()
    
    tmp <- rsf_process()
    para <- tune.rfsrc(formula(tmp[[2]]), tmp[[1]], ntreeTry = input$rf_ntree)$optimal

    nodesize <- para[[1]]
    mtry <- para[[2]]
    updateNumericInputIcon(session, "rf_nodesize", value = nodesize)
    updateNumericInputIcon(session, "rf_mtry", value = mtry, max = length(input$rf_var))
    
    waitress1$close(); waitress2$close()
    # waitress$close()
  })
  ##### Surv: RSF: fit ##### 
  observeEvent(input$fit_rf, {
    req(input$rf_var)
    tmp <- rsf_process()
    waitress <- Waitress$new("#RSF", theme = "overlay-radius", infinite = T)
    waitress$start(h4("The survival forest is growing ..."))
    
    tmp <- rsf_process()
    rsf_store$model <- rf <- rfsrc(
      formula(tmp[[2]]), tmp[[1]], forest=T, importance = T, 
      ntree = input$rf_ntree, nodesize = input$rf_nodesize, mtry = input$rf_mtry,
      block.size=input$rf_block_size, save.memory = T # to speed up calculation
    )
    
    output$rf_detail <- function(){
      knitr::kable(data.frame(
        c("Sample Size: ", "Training sample size: ", "Number of deaths: ", "Average number of terminal nodes: "),
        c(rf$n, rf$n*0.632, rf$ndead, mean(rf$leaf.count))
      ), format = "html", col.names = NULL, digits = 0) %>%
        # add_header_above(c("Information about the forest"=2)) |>
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE, background = "#284f74", color = "#ffffff")
    }
    output$rf_concordance <- function(){
      knitr::kable(data.frame(
        "OOB concordance: ",
        1-get.cindex(rf$yvar[,1], rf$yvar[,2], rf$predicted.oob)
      ), format = "html", col.names = NULL) %>%
        # add_header_above(c("Harrell’s C-index (concordance index)"=2)) |>
        kable_styling("responsive") |>
        kableExtra::column_spec(1, bold = TRUE, background = "#284f74", color = "#ffffff")
    }
    
    w0 <- Waiter$new(id = c("rf_vimp", "rf_oob_err_rate_plot"), html = spin_loaders(24))
    rsf_store$vimp_df <- data.frame(
      val = as.numeric(rf$importance), 
      class = names(rf$importance),
      sign = cut(as.numeric(rf$importance), breaks = c(-Inf,0,Inf))
    )
    output$rf_vimp <- renderPlot({
      req(rsf_store$vimp_df)
      w0$show()
      ggplot(rsf_store$vimp_df, aes(x = reorder(class, val), y = val))+
        geom_hline(yintercept = 0, color = 1, linetype = 2)+
        geom_col(aes(fill = sign))+
        geom_label(aes(y = 0,label = round(val,3)), alpha = 0.7)+
        theme_bw() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position="none"
        ) +
        labs(x="", y="Importance")+
        coord_flip()
    }, height = rsf_store$plt_height_vimp)
    
    id <- which(!is.na(rf$err.rate), arr.ind = T)
    rsf_store$err_df <- data.frame(x=id, y=rf$err.rate[id])
    output$rf_oob_err_rate_plot <- renderPlot({
      req(rsf_store$err_df)
      w0$show()
      ggplot(data=rsf_store$err_df, aes(x=x,y=y))+
        geom_line(color = "lightsteelblue3")+
        geom_point(color = "lightsteelblue4", shape=20, cex=3)+
        labs(x = 'Number of Trees', y = "OOB error rate")+
        theme_bw()
    })
    
    w1 <- Waiter$new(id = "rf_plot_variable", html = waiting_element_rf1)
    output$rf_plot_variable <- renderPlot({
        w1$show()
        par(cex.axis=1.2, cex.lab=1.5)
        plot.variable(rf)
      }, height = rsf_store$plt_height)
    
    output$rf_ui_treeID <- renderUI({
      numericInputIcon(
        "rf_treeID", "Select a single tree", value=1,
        min = 1, max = input$rf_ntree, step = 1, icon = icon("arrow-pointer")
      )
    })
    w2 <- Waiter$new(id = "rf_tree_plt", html = waiting_element_rf2)
    output$rf_tree_plt <- renderImage({
      req(input$rf_treeID)
      w2$show()
      tree <- plot(get.tree(rsf_store$model, input$rf_treeID))
      saveWidget(tree, "tmp.html")
      webshot("tmp.html", "tmp.png", selector = "#graph0", expand = 10)
      unlink("tmp.html") # delete file
      list(src = "tmp.png",
           contentType = "image/png")
    }, deleteFile = T) 
    
    waitress$close()
  })
  
  ##### CondiS: CondiS #####
  #---------------------------------#
  #--- tmax parameter auto input ---#
  #---------------------------------#
  observeEvent(input$impu_var_time, {
    req(input$impu_var_time)
    
    maxT <- uploaded_data$new_dat %>% 
      select(input$impu_var_time) %>% pull() |>
      max()
    tmax <- round(maxT,3)
    tmax <- ifelse(tmax>maxT, tmax-0.001, tmax)
    updateNumericInput(session, "impu_tmax", value=tmax, max=tmax)
  })
  #--------------#
  #--- impute ---#
  #--------------#
  impute_data <- reactiveValues(time = NA, status = NA,
                                condis_t = NA, condisX_t = NA,
                                butt_condis = 0, butt_condisX = 0,
                                newDF_condis = NA, newDF_condisX = NA,
                                exceptTime = NA)
  observeEvent(input$impu_condis_go, {
    req(input$impu_var_time, input$impu_var_censor, input$impu_tmax,
        uploaded_data$new_dat)
    waiter_show(html = waiting_screen_condis, color = "black")
    impute_data$butt_condis <- 1
    
    impute_data$time <- uploaded_data$new_dat[,input$impu_var_time]
    # 0: censored; 1: death
    impute_data$status <- as.numeric(uploaded_data$new_dat[,input$impu_var_censor])-1
    tmax <- input$impu_tmax

    impute_data$condis_t <- CondiS(impute_data$time, impute_data$status, tmax) |>
      as.numeric()
    
    impute_data$exceptTime <- uploaded_data$new_dat %>% select(-input$impu_var_time)
    impute_data$newDF_condis <- cbind(
      imputed.time = impute_data$condis_t,
      impute_data$exceptTime
    )
    
    enable("impu_condisX_go")
    enable("download_complete_condis")
    
    waiter_hide()
  })
  
  ##### CondiS: CondiS-X #####
  observeEvent(input$impu_condisX_go, {
    req(uploaded_data$new_dat, 
        input$impu_covariate, input$impu_method)
    if(req(impute_data$butt_condis) == 1){
      waiter_show(html = waiting_screen_condis, color = "black")
      impute_data$butt_condisX <- 1
      
      #--- only complete data can be used ---#
      cov_var <- uploaded_data$new_dat[, input$impu_covariate] |>
        data.frame()
      completeID <- complete.cases(cov_var)
      
      pred_time <- impute_data$condis_t[completeID]
      status <- impute_data$status[completeID]
      covariates <- cov_var %>% filter(completeID)
      method <- input$impu_method
      
      showNotification(
        paste("Only complete data will be acceptable. The process remove",sum(completeID==F),"incomplete observation(s)."),
        duration = 10, type = "message"
      )
      impute_data$condisX_t <- CondiS_X(pred_time, status, covariates, method) |>
        as.numeric()

      impute_data$newDF_condisX <- cbind(
        imputed.time = impute_data$condisX_t,
        impute_data$exceptTime[completeID,]
      )
      
      enable("download_complete_condisX")
      
      waiter_hide() 
    }
  })
  
  ##### CondiS: table #####
  output$impu_condis_tab <- renderDataTable({
    if(impute_data$butt_condis == 1 & impute_data$butt_condisX == 0){
      datatable(
        head(data.frame(
          impute_data$time, impute_data$status,
          impute_data$condis_t
        ), 100),
        options = list(dom = "ft", paging=F),
        colnames = c("Original.Time", "Censoring", "CondiS.Time")
      )
    }else if(impute_data$butt_condis == 1 & impute_data$butt_condisX == 1){
      datatable(
        head(data.frame(
          impute_data$time, impute_data$status,
          impute_data$condis_t, impute_data$condisX_t
        ), 100),
        options = list(dom = "ft", paging=F),
        colnames = c("Original.Time", "Censoring", "CondiS.Time", "CondiS-X.Time")
      ) 
    }
  })
  
  ##### CondiS: plot #####
  output$impu_condis_plt <- renderPlot({
    req(input$impu_var_time, input$impu_var_censor, uploaded_data$new_dat)
    #----------#
    #--- KM ---#
    #----------#
    data <- mutate(uploaded_data$new_dat, across(!!sym(input$impu_var_censor), as.numeric))
    form1 <- paste("Surv(", input$impu_var_time, ",", input$impu_var_censor, ") ~ 1")
    tmp1 <- survfit(as.formula(form1), data=data)
    km <- data.frame(
      t=tmp1$time, p=tmp1$surv, c=as.numeric(tmp1$n.censor!=0),
      l=tmp1$lower, u=tmp1$upper
    )
    
    p <- km %>% 
      ggplot(aes(x=t, y=p))+
      geom_step(aes(color="Kaplan-Meier"))+
      geom_point(aes(color="Kaplan-Meier"), shape=3, data = km[km$c==1,])+
      geom_ribbon(aes(x=t, ymin=l, ymax=u, fill="95% CI"), alpha=0.2)+
      ylim(0,1)+
      labs(x="Time",y="Survival Probability")+
      theme_bw()+
      theme(legend.position = "bottom",
            legend.title = element_blank())+
      scale_fill_manual(values='gray40')+
      scale_color_manual(values = "firebrick2")
    
    if(impute_data$butt_condis > 0){
      #--------------#
      #--- CondiS ---#
      #--------------#
      tmp2 <- survfit(Surv(
        impute_data$condis_t, 
        rep(1,length(impute_data$condis_t))
      )~1)
      condis <- data.frame(t=tmp2$time, p=tmp2$surv)
      p <- p+
        geom_step(aes(color="CondiS"), data=condis, linewidth=1)+
        scale_color_manual(values = c("cadetblue3","firebrick2"))
      
    }
    if(impute_data$butt_condisX > 0){
      #----------------#
      #--- CondiS-X ---#
      #----------------#
      tmp3 <- survfit(Surv(
        impute_data$condisX_t,
        rep(1,length(impute_data$condisX_t))
      )~1)
      
      condisX <- data.frame(t=tmp3$time, p=tmp3$surv)
      p <- p+
        geom_step(aes(color="CondiS-X"), data=condisX, linewidth=1)+
        scale_color_manual(values = c("cadetblue3","darkolivegreen3","firebrick2"))
    }
    
    return(p)
  })
  
  ##### CondiS: download imputed data ####
  output$download_complete_condis <- downloadHandler(
    filename = function() {
      paste(format(Sys.time(), "%Y-%m-%d"),"[CondiS] imputed data.csv")
    },
    content = function(file) {
      write.csv(impute_data$newDF_condis, 
                file, row.names = FALSE)
    }
  )
  output$download_complete_condisX <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d")," [CondiS-X] ",input$impu_method,"-imputed data.csv")
    },
    content = function(file) {
      write.csv(impute_data$newDF_condisX, 
                file, row.names = FALSE)
    }
  )
}