theme_angle <- function(angle) {
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=angle, face = "bold", size = 12, color = 'midnightblue'),
    axis.text.y = element_text(face = "bold", size = 12, color = 'midnightblue'),
    axis.title = element_text(face = "bold", size=14, color='lightsteelblue4'),
    plot.caption = element_text(size = 15),
    plot.title = element_text(face = "bold", size=15, color='lightsteelblue4')
  )
}
theme_angle_flip <- function(angle) {
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, color = 'midnightblue'),
    axis.text.y = element_text(angle=angle, face = "bold", size = 12, color = 'midnightblue'),
    axis.title = element_text(face = "bold", size=14, color='lightsteelblue4'),
    plot.caption = element_text(size = 15),
    plot.title = element_text(face = "bold", size=15, color='lightsteelblue4')
  )
}
theme_default <- function(){
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(face = "bold", size = 12, color = 'midnightblue'),
    axis.title = element_text(face = "bold", size=14, color='lightsteelblue4'),
    plot.caption = element_text(size = 15),
    plot.title = element_text(face = "bold", size=15, color='lightsteelblue4')
  )
}

func_cat <- function(data, var, flip=F, order=F, ts=5, adj=-0.2, angle=0, selectID=NA){
  ndata <- data %>% select(!!sym({{var}})) %>% table() %>% as.data.frame() 
  if(order){
    ndata <- ndata %>% arrange(desc(Freq))
  } else {
    ndata <- ndata
  }
  if (all(is.na(selectID))){
    target <- ndata
  } else {
    target <- ndata[selectID, ]
  }
  p <- target %>% 
    mutate(class = factor(!!sym({{var}}), levels=!!sym({{var}}))) %>% 
    ggplot(aes(class, Freq))+
    geom_bar(stat="identity", fill="steelblue2", alpha=.9, width=.4) +
    theme_bw()+
    xlab(var)
  if(flip){
    p <- p +
      geom_text(aes(label=Freq), hjust=adj, color='navy', size=ts) +
      coord_flip() +
      theme_angle_flip(angle)
  } else {
    p <- p +
      geom_text(aes(label=Freq), vjust=adj, color='navy', size=ts)+
      theme_angle(angle)
  }
  colnames(ndata) <- c(var, 'Frequency')
  list(p, ndata)
}
# func_cat(ndf, 'd1')

summ_con <- function(data, var){
  data_sum <- data %>% select(!!sym({{var}})) %>% pull() %>% summary() %>%
    format(scientific = FALSE) %>% as.data.frame()
  data_sum <- data_sum %>%
    mutate(value = scales::comma(as.numeric(.)))
  t(data_sum %>% select(value)) %>% as.data.frame()
}
# summ_con(ndf, 'time_month')

plt_con <- function(data, var, bin){
  data %>%
    ggplot(aes(x=!!sym({{var}})))+
    geom_histogram( bins=bin, fill="steelblue2", color="#e9ecef", alpha=.9) +
    theme_bw() +
    theme_default()+
    scale_x_continuous(breaks = function(x) pretty(x, n = 10))
}
# plt_con(ndf, 'age_90up', 90)

#### user ####
# get_option_groups_choices <- function(target_data){
#   lis <- target_data %>% group_split(survVar)
#   list_names <- sapply(1:length(lis), function(x) lis[[x]]$survVar|>unique())
#   choices <- lis %>% 
#     setNames(list_names) %>%
#     map(~ as.list(.x$variable))
#   return(choices)
# }

calc_km <- function(data, strata, time, censor){
  # data <- test1; strata <- "sex"; time <- "time"; censor="censored"
  #------------------------------------------------------#
  #--- 注意censored不能被轉成factor, 必須是continuous ---#
  #------------------------------------------------------#
  # data <- mutate(data, across(!!sym(censor), ~ as.numeric(.x)-1))
  form.chr <- paste("Surv(", time, ",", censor, ") ~", strata)
  surv <- survfit2(as.formula(form.chr), data=data)
  sum <- summary(surv)
  tab <- sum$table %>% as.data.frame()
  
  tab$variable <- paste0(strata,'=',sub(".*=", "", rownames(tab)))
  tab <- tab %>%
    select(variable, records, events, median, `0.95LCL`, `0.95UCL`) %>%
    rename(n=records)
  
  detail <- data.frame(
    class = factor(sub(".*=", "", sum[['strata']])),
    time = sum[['time']],
    n.risk=sum[['n.risk']],
    n.event=sum[['n.event']],
    survival=sum[['surv']],
    std.err=round(sum[['std.err']], 4),
    'Lower_0.95_CI'=sum[['lower']],
    'Upper0.95_CI'=sum[['upper']]
  ) %>% mutate(across(c(time, n.risk, n.event), as.integer))
  
  surv_median <- tab$median
  marks <- data.frame(time=surv_median, prob=0.5, row.names = NULL)
  p <- surv %>%
    ggsurvfit(linetype_aes = T) +
    # hline
    geom_hline(yintercept = 0.5, color = 'lightsteelblue4', linetype=2)+
    # vline
    geom_segment(data=marks, aes(x=time,y=prob,xend=time, yend=-Inf),
                 color = 'lightsteelblue4', linetype=2)+
    labs(x = 'Time', y = "Survival Probability")
  # p <- p+
  #   guides(colour=guide_legend(nrow=2,byrow=TRUE))
  
  return(list(tab, "plot"=p, detail))
}
# calc_km(df_surv, "sex", "time_month", "delta")[[1]]
# test <- readRDS("data/2021Covid_mini.rds")
# calc_km(test, 'sex', "time", "censored")
# test$censored <- factor(test$censored)

calc_cox <- function(data, input_covariate, time, censor){
  # data <- mutate(data, across(!!sym(censor), ~ as.numeric(.x)-1))
  x <- paste(input_covariate, collapse = " + ")
  form.chr <- paste("Surv(", time, ",", censor, ") ~", x)
  cox <- coxph(as.formula(form.chr), data=data)
  summ <- summary(cox)
  
  #---------------#
  #--- C index ---#
  #---------------#
  c.index <- round(as.numeric(cox$concordance["concordance"]), 4)
  # c.index <- cindex(as.formula(form.chr), data=data)$cindex
  
  #-----------------#
  #-- global test --#
  #-----------------#
  vec_global <- c(summ$logtest[3], summ$sctest[3], summ$waldtest[3])
  chr_global <- case_when(vec_global<0.001 ~ "<0.001",
                          vec_global<0.01 ~ "<0.01",
                          vec_global<0.05 ~ "<0.05",
                          .default = as.character(round(vec_global,3)))
  global <- data.frame(t(chr_global))
  colnames(global) <- c("LR","score","wald")
  rownames(global) <- "p-value"
  
  #-----------------#
  #-- single test --#
  #-----------------#
  vec_single <- as.data.frame(summ$coefficients)[,5]
  chr_single <- case_when(vec_single<0.001 ~ "<0.001",
                          vec_single<0.01 ~ "<0.01",
                          vec_single<0.05 ~ "<0.05",
                          .default = as.character(round(vec_single,3)))
  single <- cbind(as.data.frame(summ$coefficients), 
                  as.data.frame(summ$conf.int)[,3:4])[,c(1,3,2,6,7,4)] |>
    round(3) |>
    cbind(data.frame("p-value"=chr_single))
  rowName <- rownames(single); newNames <- vector()
  for (keyword in input_covariate) {
    indices <- grep(keyword, rowName)
    tmp <- strsplit(rowName[indices], keyword)
    class <- sapply(tmp,`[`, 2)
    newNames[indices] <- ifelse(
      is.na(class), keyword,
      paste0(keyword, " = ", class)
    )
  }
  rownames(single) <- newNames
  
  #--------#
  #- plot -#
  #--------#
  cox.fit <- survfit(cox)
  p <- ggplot(data.frame(time=cox.fit$time, surv=cox.fit$surv, 
                         u=cox.fit$upper, l=cox.fit$lower))+
    geom_step(aes(x=time, y=surv, linetype = 'Survival', color = 'Survival'),
              linetype=1, linewidth=0.8)+
    geom_step(aes(x=time, y=u, color = '0.95 C.I.'), linewidth=0.6)+
    geom_step(aes(x=time, y=l, color = '0.95 C.I.'), linewidth=0.6)+
    scale_color_manual("", values=c('lightsteelblue3','lightsteelblue4'))+
    labs(x = 'Time', y = "Survival Probability")+
    theme_bw()+
    theme(legend.position = "bottom")
  
  return(list("formula"=form.chr,
              "global test"=global,
              "est. and single test"=single,
              "model"=cox,
              "plot"=p,
              "c.index"=c.index))
}
# calc_cox(test, c("sex","smoke","intubation"), "time", "censored")[["plot"]]

# vars <- c("sex", "care")
# target <- "sex"
# covar <- setdiff(vars,target)
# 
# if(length(covar)>1){
#   # if covar>1
#   tmp1 <- apply(df_surv[,covar], 2, get_max_class); tmp1
# } else{
#   # if covar=1
#   tmp1 <- get_max_class(test1[,covar]); tmp1
# }
# 
# new <- with(
#   test1,
#   data.frame(target=levels(test1[,target])) |>
#     cbind(as.data.frame(t(tmp1)))
# ) |> setNames(c(target, covar)); new
# 
# # fit new data
# tmp2 <- survfit(tmp[["model"]], newdata = new);tmp2
# # plot(tmp2, conf.int = T)
# tmp4 <- as.data.frame(tmp2$surv)
# colnames(tmp4) <- tmp2$newdata[,1]; head(tmp4)
# tmp3 <- reshape2::melt(tmp4); tmp3
# tmp5 <- cbind(tmp3, time=rep(tmp2$time, length(tmp2$newdata[,1])))
# ggplot(tmp5)+
#   geom_step(aes(x=time, y=value, color=variable))+
#   theme(legend.position = "bottom")+
#   labs(color = colnames(tmp2$newdata)[1])

get_max_class <- function(vec_data){
  freq <- table(vec_data)
  class <- names(freq)[which.max(freq)]
  return(class)
}
# get_max_class(c("B","B","A","C","D","D"))

plt_strata_cox <- function(data, model, fitted, strata){
  if(length(fitted)>1){
    var_type <- sapply(data, class)[fitted]
    covariates <- setdiff(fitted, strata)
    # data & fitted model
    if(length(covariates)<=1){
      var_avg <- ifelse(
        is.factor(data[[covariates]]), 
        get_max_class(data[[covariates]]), 
        mean(data[[covariates]])
      )
    }else{
      var_avg <- sapply(data[covariates], 
                        function(x){
                          ifelse(is.factor(x), get_max_class(x), mean(x))})
    }
    new_dat <- with(
      data, 
      data.frame(levels(data[[strata]])) |>
        cbind(as.data.frame(t(var_avg)))
    ) |> setNames(c(strata, covariates))
    for(i in 1:length(var_type)){
      if(var_type[i]!="factor") new_dat[[names(var_type)[i]]] <- as.numeric(new_dat[[names(var_type)[i]]])
    }
    tmp_dat <- bind_rows(data[colnames(new_dat)], new_dat, .id = 'grp') %>% 
      mutate(across(where(is.character), factor)) %>%
      group_split(grp, .keep = FALSE)
    fitted_new <- survfit(model, newdata = tmp_dat[[2]])
  } else{
    fitted_new <- survfit(model, newdata = 
                            data.frame(levels(data[[strata]])) %>% 
                            `colnames<-`(strata))
  }
  
  # plot
  tmp <- as.data.frame(fitted_new$surv)
  colnames(tmp) <- fitted_new$newdata[,1]
  tmp <- melt(tmp)
  surv_df <- cbind(tmp, time=rep(fitted_new$time, length(fitted_new$newdata[,1])))
  p <- ggplot(surv_df)+
    geom_step(aes(x=time, y=value, color=variable, linetype=variable))+
    labs(color = colnames(fitted_new$newdata)[1], linetype = colnames(fitted_new$newdata)[1],
         x = 'Time', y = "Survival Probability")+
    theme_bw()+
    theme(legend.position = "bottom")
  
  return(list(
    # "new data"=tmp_dat[[2]],
    # "fitted model"=fitted_new,
    "plot"=p
  ))
}
# test <- read.csv("C:/Users/User/Downloads/imputation sample data.csv")
# test <- mutate(test,across(c("meno","size","grade","hormon","chemo"), factor))
# str(test)
# tmp <- calc_cox(test, c("meno","size","age"), "survival_time", "censoring_status")[[4]]
# summary(tmp)
# plt_strata_cox(test, tmp, c("meno","size","age"), "size")[[3]]
