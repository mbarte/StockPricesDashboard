################################################################################
###################### INSTALL AND LOAD PACKAGES ###############################
################################################################################

# Helper Functions


# Function to check packages already loaded into NAMESPACE
check_namespace <- function(pkgs) {

  pkgs_notloaded <- pkgs[!pkgs %in% loadedNamespaces()]
  if (length(pkgs_notloaded) == 0) {
    res <- NULL
  } else {
    res <- pkgs_notloaded
  }
  return(res)

}


# Function to install and load the specified packages
install_and_load <- function(pkgs, repos = getOption("repos")) {

  pkgs_inst <- pkgs[!pkgs %in% installed.packages()]

  if (length(pkgs_inst) == 0) {
    lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
    check_res <- check_namespace(pkgs)
    if (is.null(check_res)) {
      res <- "All packages correctly installed and loaded."
    } else {
      res <- paste0(
        "Problems loading packages ",
        paste0(check_res, collapse = ", "),
        "."
      )
    }

  } else {

    inst_res <- vector("character", length(pkgs_inst))

    for (i in seq_along(pkgs_inst)) {
      inst_res_tmp <- tryCatch(
        utils::install.packages(pkgs_inst[i], dependencies = TRUE, repos = repos, quiet = TRUE),
        error = function(e) e,
        warning = function(w) w
      )
      if (!is.null(inst_res_tmp)) {
        inst_res[i] <- inst_res_tmp$message
      }
    }

    pkgs_err <- pkgs_inst[!inst_res == ""]
    if (length(pkgs_err) == 0) {
      lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs)
      if (is.null(check_res)) {
        res <- "All packages correctly installed and loaded."
      } else {
        res <- paste0(
          "Problems loading packages ",
          paste0(check_res, collapse = ", "),
          "."
        )
      }
    } else {
      pkgs_noerr <- pkgs[!pkgs %in% pkgs_err]
      lapply(pkgs_noerr, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs_noerr)
      if (is.null(check_res)) {
        res <- paste0(
          "Problems installing packages ",
          paste0(pkgs_err, collapse = ", "),
          "."
        )
      } else {
        res <- c(
          paste0(
            "Problems installing packages ",
            paste0(pkgs_err, collapse = ", "),
            "."
          ),
          paste0(
            "Problems loading packages ",
            paste0(check_res, collapse = ", "),
            "."
          )
        )
      }
    }

  }

  message(toupper(
    paste0(
      "\n\n\n",
      "\n==================================================================",
      "\nResults:\n ",
      res,
      "\n=================================================================="
    )
  ))
  return(invisible(res))

}

################################################################################
####################### ADJUSTED CLOSE FORECAST ################################
################################################################################

aggregate_time_series <- function(data, time_unit = "day") {
    
  output_tbl <- data %>%
    mutate(date = floor_date(date, unit = "day")) %>%
    group_by(date) %>%
    summarize(adjusted) %>%
    ungroup() %>%
    mutate(label_text = str_glue("Date: {date}
                                 Adjusted close: {scales::dollar(adjusted)}"))
  return(output_tbl)
  
}

#function to plot the time series
plot_time_series_adj <- function(data) {
   
  g <- data %>%
    group_by(symbol) %>% 
    mutate(init_adj = if_else(date == min(date),adjusted,NA_real_)) %>%
    mutate(adjusted = round(100 * adjusted / sum(init_adj,na.rm=T),1)) %>%
    mutate(label_text = str_glue("stock: {symbol}
                                  Date: {date}
                                  Adjusted close: {scales::dollar(adjusted)}")) %>%  
    ungroup() %>% 
    ggplot(aes(date, adjusted, color = symbol), size = 0.6) +
    geom_line() +
    scale_colour_brewer("Stocks", type = "qual", palette = "Dark2")+
    labs(x = NULL,
         y= NULL) +
    theme_minimal()+
    theme(
      text = element_text('serif'),
      plot.title = element_text(face = 'bold', color = 'royalblue4', hjust = 0.5),
      axis.title = element_text(color = 'black'),
      plot.caption = element_text(face = 'italic', color = 'black'),
      panel.grid.major = element_line('grey', size = 0.5),
      panel.grid.minor = element_line('grey', size = 0.5),
      panel.grid.major.y = element_line('grey', size = 0.5),
      panel.ontop = FALSE #grid lines on top of data
    )
  g <- g + geom_point(aes(text = label_text), size = 0.01)
  
  ggplotly(g, tooltip = "text")
    
}

#function to generate the forecast
generate_forecast <- function(data, n_future = 12, seed = 123) {
  
  data <- data %>% 
    select(date,adjusted)
 
   train_tbl <- data %>%
    tk_augment_timeseries_signature(.date_var = date) %>% 
     select(-contains(".xts"), -diff, -(hour:am.pm))

  future_data_tbl <- data %>%
    tk_index() %>%
    tk_make_future_timeseries(
      length_out = n_future,
      inspect_weekdays = TRUE,
      inspect_months = TRUE
    ) %>%
    tk_get_timeseries_signature() 
  
  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  if (time_scale == "year") {
    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(
        adjusted ~ ., 
        data = train_tbl %>% select(adjusted, index.num) 
      )
  } else {
    set.seed(seed)
    model <- boost_tree(
      mode = "regression",
      mtry = 20,
      trees = 500,
      min_n = 3,
      tree_depth = 8,
      learn_rate = 0.01,
      loss_reduction = 0.01
    ) %>%
      set_engine(engine = "xgboost") %>%
      fit.model_spec(
        adjusted ~ ., 
         data = train_tbl %>% select(-date)
      )
  }
  
  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(adjusted = .pred, date = index) %>%
    add_column(key = "Prediction")
  
  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl) %>% 
    mutate(label_text = str_glue("Date: {date}
                                 Adjusted: {scales::dollar(adjusted)}")) 
  
  return(output_tbl)
}

#function to plot the forecast
plot_forecast <- function(data) {
    
  # Yearly - LM Smoother
  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  # Only 1 Prediction - points
  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()
  
  g <- data %>%
    ggplot(aes(date, adjusted)) +
    geom_line(aes(color=key), size = 0.6) +
    scale_colour_manual("Key", values = c("#2c3e50", "#E31A1C"))+
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")+
    theme(
      text = element_text('serif'),
      plot.title = element_text(face = 'bold', color = 'royalblue4',hjust = 0.5),
      axis.title = element_text(color = 'black'),
      plot.caption = element_text(face = 'italic', color = 'black'),
      panel.grid.major = element_line('grey', size = 0.5),
      panel.grid.minor = element_line('grey', size = 0.5),
      panel.grid.major.y = element_line('grey', size = 0.5),
      panel.ontop = FALSE #grid lines on top of data
    )

  # Yearly - LM Smoother
  if (time_scale == "year") {
    g <- g +
      geom_smooth(method = "lm",color = "green", size = 0.5)
  } else {
    g <- g + geom_smooth(method = "loess", color = "#FF7F00", span = 0.3, size = 0.3, alpha = 0.1, se = FALSE)
  }
  
    # Only 1 Prediction
    if (n_predictions == 1) {
      g <- g + geom_point(aes(text = label_text), size = 1)
    } else {
      g <- g + geom_point(aes(text = label_text), size = 0.01)
    }
    
    ggplotly(g, tooltip = "text")
}

################################################################################
############################ SALES FORECAST ####################################
################################################################################


plot_time_series_sales <- function(data) {
  
  g <- data %>%
    group_by(symbol) %>%
    mutate(init_sales_mln = if_else(date == min(date),sales_mln,NA_real_)) %>% 
    mutate(sales_mln = round(100 * sales_mln / sum(init_sales_mln,na.rm=T),1)) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Sales: {scales::dollar(sales_mln)}")) %>% 
    ungroup() %>%
    ggplot(aes(date, sales_mln, color = symbol), size = 0.5) +
    geom_line() +
    scale_colour_brewer("Stocks", type = "qual", palette = "Dark2")+
    labs(x = NULL,
         y= NULL) +
    theme_minimal()+
    theme(
      text = element_text('serif'),
      plot.title = element_text(face = 'bold', color = 'royalblue4',hjust = 0.5),
      axis.title = element_text(color = 'black'),
      plot.caption = element_text(face = 'italic', color = 'black'),
      panel.grid.major = element_line('grey', size = 0.5),
      panel.grid.minor = element_line('grey', size = 0.5),
      panel.grid.major.y = element_line('grey', size = 0.5),
      panel.ontop = FALSE #grid lines on top of data
    ) 
  g <- g + geom_point(aes(text = label_text), size = 0.001)
  
  ggplotly(g, tooltip = "text")
}

#function to generate the forecast
generate_forecast_sales <- function(data, n_future_sales = 12, seed = 123) {
  
  data <- data %>% 
    select(date,sales_mln)
  
  train_tbl <- data %>%
    tk_augment_timeseries_signature(.date_var = date) %>% 
    select(-contains(".xts"),-diff, -(hour:am.pm))
  
  future_data_tbl <- data %>%
    tk_index() %>%
    tk_make_future_timeseries(
      length_out = n_future_sales,
      inspect_weekdays = TRUE,
      inspect_months = TRUE
    ) %>%
    tk_get_timeseries_signature() 
  
  time_scale = "day" #<- data %>%
    # tk_index() %>%
    # tk_get_timeseries_summary() %>%
    # pull(scale)
    # 
  if (time_scale == "year") {
    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(
        sales_mln ~ ., 
        data = train_tbl %>% select(sales_mln, index.num) 
      )
  } else {
    set.seed(seed)
    model <- boost_tree(
      mode = "regression",
      mtry = 20,
      trees = 500,
      min_n = 3,
      tree_depth = 8,
      learn_rate = 0.01,
      loss_reduction = 0.01
    ) %>%
      set_engine(engine = "xgboost") %>%
      fit.model_spec(
        sales_mln ~ ., 
        data = train_tbl %>% select(-date)
      )
  }
  
  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(sales_mln = .pred, date = index) %>%
    add_column(key = "Prediction")
  
  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl) %>% 
    mutate(label_text = str_glue("Date: {date}
                                 Sales: {scales::dollar(sales_mln)}")) 
  
  return(output_tbl)
  
}


#function to plot the forecast
plot_forecast_sales <- function(data) {
  
  # Yearly - LM Smoother
  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  # Only 1 Prediction - points
  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()
  
  g <- data %>%
    ggplot(aes(date, sales_mln)) +
    geom_line(aes(color=key), size = 0.6) +
    scale_colour_manual("Key", values = c("#2c3e50", "#E31A1C"))+
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")+
    theme(
      text = element_text('serif'),
      plot.title = element_text(face = 'bold', color = 'royalblue4',hjust = 0.5),
      axis.title = element_text(color = 'black'),
      plot.caption = element_text(face = 'italic', color = 'black'),
      panel.grid.major = element_line('grey', size = 0.5),
      panel.grid.minor = element_line('grey', size = 0.5),
      panel.grid.major.y = element_line('grey', size = 0.5),
      panel.ontop = FALSE #grid lines on top of data
    )
  
  # Yearly - LM Smoother
  if (time_scale == "year") {
    g <- g +
      geom_smooth(method = "lm",color = "green", size = 0.5)
  } else {
    g <- g + geom_smooth(method = "loess", color = "#3D33FF", span = 0.2, size = 0.25, alpha = 0.1, se = FALSE)
  }
  
  # Only 1 Prediction
  if (n_predictions == 1) {
    g <- g + geom_point(aes(text = label_text), size = 1)
  } else {
    g <- g + geom_point(aes(text = label_text), size = 0.01)
  }
  
  ggplotly(g, tooltip = "text")
  
}
################################################################################
mediapon<- function(x){
  k <- c()
  s <- c()
  l = min(length(x), 30)
  for(i in 1:l) {
    k[i] = (l/(l+i))^log(i)
  }
  for(i in 1:l) {
    s[i] = k[i] * x[l-i+1]
  }
  
  result = sum(s) / sum(k)

  return(result)
}
  