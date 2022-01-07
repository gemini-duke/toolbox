# LCA table


tbl_lca <- function(lca_models = NULL) {
  
  if (!require(tidyverse)) {install.packages("tidyverse")}; require(tidyverse)
  if (!require(gtsummary)) {install.packages("gtsummary")}; require(gtsummary)
  if (!require(huxtable)) {install.packages("huxtable")}; require(huxtable)
  
  # sample size
  n <- lca_models[[1]]$Nobs
  
  # Smallest class size (%)
  small_class <- map_chr(lca_models, function(x) gtsummary::style_percent(min(x$P), digits = 1))
  # Log likelihood
  llik <- map_dbl(lca_models, function(x) x$llik)
  
  # BIC
  bic <- map_dbl(lca_models, function(x) x$bic)
  
  # AIC
  aic <- map_dbl(lca_models, function(x) x$aic)
  
  # CAIC and SABIC
  npar <- map_dbl(lca_models, function(x) x$npar)
  caic <- (-2 * llik) + npar * (log(n) + 1)
  sabic <- (-2 * llik) + npar * log((n + 2) / 24)
  
  # Approximate Weight of Evidence
  awe <- (-2 * llik) + npar * (log(n) + 1.5)
  
  # Bayes Factor
  bf <- c()
  for (i in 1:length(lca_models)) {
    if (i + 1 <= length(lca_models)) {
      a <- -0.05 * bic[i]
      b <- -0.05 * bic[i + 1]
      bf[i] <- gtsummary::style_sigfig(exp(a - b), digits = 3)
    } else {bf[i] <- NA}
  }
  
  
  # Entropy
  entropy <- function(x) {
    numerator <- -sum(x$posterior * log(x$posterior), na.rm = TRUE)
    denominator <- x$N * log(ncol(x$posterior))
    1 - (numerator / denominator)
  }
  ent <- map_dbl(lca_models, entropy)
  
  
  # Table
  lca_table <- tibble("Model" = c("1 Class", paste(2:length(lca_models), "Classes")),
                      "Smallest class\n size (%)" = small_class,
                      "LL" = gtsummary::style_number(llik, digits = 2),
                      "AIC" = gtsummary::style_number(aic, digits = 2),
                      "CAIC" = gtsummary::style_number(caic, digits = 2),
                      "BIC" = gtsummary::style_number(bic, digits = 2),
                      "SABIC" = gtsummary::style_number(sabic, digits = 2),
                      "AWE" = gtsummary::style_number(awe, digits = 2),
                      "BF" = bf,
                      "Entropy" = gtsummary::style_number(ent, digits = 2)) %>%
    huxtable::hux(add_colnames = TRUE) %>%
    set_bold(row = 1, col = everywhere) %>%
    set_bottom_border(row = 1, col = everywhere) %>%
    set_top_border(row = 1, col = everywhere) %>%
    set_bottom_border(row = final(1), col = everywhere) %>%
    set_align(row = everywhere, col = 2:10, value = "center") %>%
    add_footnote(text = "Note: LL = Log-likelihood; AIC = Akaike information criteria; CAIC = Consistent Akaike information criteria; BIC = Bayesian information criteria; SABIC = Sample-size adjusted BIC; AWE = Approximate weight of evidence criterion; BF = Bayes factor.")
  
}



# Average posterior probability (AvePP)
tbl_avepp <- function(lca_model = NULL) {
  
  if (!require(tidyverse)) {install.packages("tidyverse")}; require(tidyverse)
  if (!require(gtsummary)) {install.packages("gtsummary")}; require(gtsummary)
  if (!require(huxtable)) {install.packages("huxtable")}; require(huxtable)
  
  avepp_table <- round(aggregate(
    x = lca_model$posterior,
    by = list(lca_model$predclass),
    FUN = "mean"), digits = 2) %>%
    data.frame()
  colnames(avepp_table) <- c("Class", 1:nrow(avepp_table))
  huxtable::hux(avepp_table, add_colnames = TRUE) %>%
    set_bold(row = 1, col = everywhere) %>%
    set_bottom_border(row = 1, col = everywhere) %>%
    set_top_border(row = 1, col = everywhere) %>%
    set_bottom_border(row = final(1), col = everywhere) %>%
    set_align(row = everywhere, col = 2:ncol(avepp_table), value = "center") %>%
    set_align(row = everywhere, col = 1, value = "left")
  
}



elbow_plot <- function(lca_models = NULL) {
  
  if (!require(tidyverse)) {install.packages("tidyverse")}; require(tidyverse)
  
  # sample size
  n <- lca_models[[1]]$Nobs
  
  # Log-likelihood
  llik <- map_dbl(lca_models, function(x) x$llik)
  
  # BIC
  bic <- map_dbl(lca_models, function(x) x$bic)
  
  # AIC
  aic <- map_dbl(lca_models, function(x) x$aic)
  
  # CAIC and SABIC
  npar <- map_dbl(lca_models, function(x) x$npar)
  caic <- (-2 * llik) + npar * (log(n) + 1)
  sabic <- (-2 * llik) + npar * log((n + 2) / 24)
  
  # Approximate Weight of Evidence
  awe <- (-2 * llik) + npar * (log(n) + 1.5)
  
  
  # Plot
  class <- 1:length(lca_models)
  index <- rep(c("AIC", "CAIC", "BIC", "SABIC", "AWE"),
               length(class))
  
  lca_fval <- c(aic, caic, bic, sabic, awe)
  lca_fit <- data.frame(class, index, lca_fval)
  
  ggplot(lca_fit, aes(x = class, y = lca_fval)) +
    geom_line(aes(color = index)) +
    geom_point(aes(color = index)) +
    scale_x_continuous(name = "Number of classes",
                       limits = c(1, length(class)),
                       breaks = 1:length(class)) +
    scale_y_continuous(name = "Information criterion") +
    scale_color_viridis_d(name = "", option = "D") +
    theme(axis.title.x.bottom = element_text(size = 12,
                                             margin = margin(t = 12),
                                             color = "#333333"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90",
                                          size = 0.2),
          panel.background = element_rect(fill = "white", color = "#333333",
                                          size = 0.2),
          axis.text.y = element_text(hjust = 1,
                                     margin = margin(0, 4, 0, 0, "pt"),
                                     color = "#333333",
                                     size = 12),
          axis.text.x = element_text(color = "#333333", size = 12),
          axis.title.y = element_text(color = "#333333", size = 12,
                                      margin = margin(r = 12)),
          axis.ticks = element_line(size = 0.1, color = "#333333"),
          axis.ticks.length = unit(5, "pt"),
          legend.position = "bottom",
          legend.key = element_rect(fill = NA),
          legend.text = element_text(size = 12,
                                     color = "#333333"))
}


# Profile plot
profile_plot <- function(lca_model,
                         var_names = NULL,
                         class_names = NULL,
                         category_names = NULL,
                         binary = TRUE) {
  
  if (!require(tidyverse)) {install.packages("tidyverse")}; require(tidyverse)
  
  probs <- lca_model$probs
  
  if (is.null(var_names)) {
    var_names <- names(probs)
  }
  
  if (is.null(class_names)) {
    class_names <- paste("Class",
                         1:length(unique(lca_model$predclass)))
    class_names <- paste0(class_names, " (",
                         gtsummary::style_percent(lca_model$P, symbol = TRUE),
                         ")")
  } else {
    class_names <- paste0(class_names, " (",
                          gtsummary::style_percent(lca_model$P, symbol = TRUE),
                          ")")
  }
  
  if (binary) {
    
    p_df <- list()
    for (i in 1:length(probs)) {
      p_df[[i]] <- data.frame(probs[[i]]) %>%
        dplyr::select(-1) %>%
        tibble(class = class_names,
               var = var_names[i])
    }
    
    df <- bind_rows(p_df)
    colnames(df)[1] <- "value"
    
    ggplot(df, aes(x = var, y = value, color = class, group = class)) +
      geom_point(aes(color = class),
                 size = 2) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1),
                         expand = c(0, 0.1),
                         breaks = seq(0, 1, 0.2),
                         name = "Conditional item probability") +
      scale_x_discrete(name = "Indicators") +
      scale_color_viridis_d(name = "",
                            option = "D",
                            end = 0.8) +
      guides(color = guide_legend(nrow = 2)) + 
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = c("gray80", rep("grey90", 10)),
                                              size = c(0.8, rep(0.4, 10))),
            axis.text.x = element_text(size = 10, color = "black", margin = margin(t = -12)),
            axis.text.y = element_text(size = 10, color = "black"),
            legend.text = element_text(size = 10),
            axis.title.x = element_text(size = 10, margin = margin(t = 8, b = 0)),
            axis.title.y = element_text(size = 10, margin = margin(r = 8, b = 0)),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", legend.direction = "horizontal",
            legend.key.size = unit(10, units = "pt"))
    
  } else {
    
    if (is.null(category_names)) {
      category_names <- 1:ncol(probs[[1]])
      
    }
    
    p_df <- list()
    for (i in 1:length(probs)) {
      p <- data.frame(probs[[i]]) %>%
        tibble(class = class_names,
               var = var_names[i])
      colnames(p)[1:ncol(probs[[1]])] <- category_names
      p_df[[i]] <- p %>%
        pivot_longer(cols = 1:ncol(probs[[1]]),
                     names_to = "category",
                     values_to = "value")
    }
    
    df <- bind_rows(p_df)
    ggplot(df, aes(x = var, y = value, fill = category)) +
      geom_col(position = position_dodge(width = 0.7),
               width = 0.6) +
      facet_wrap(~class) +
      scale_y_continuous(limits = c(0, 1),
                         expand = c(0, 0.1),
                         breaks = seq(0, 1, 0.2),
                         name = "Conditional item probability") +
      scale_x_discrete(name = "Indicators") +
      scale_fill_viridis_d(name = "",
                           option = "D") +
      guides(fill = guide_legend(nrow = 2)) + 
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = c("gray80", rep("grey90", 10)),
                                              size = c(0.8, rep(0.4, 10))),
            axis.text.x = element_text(size = 10, color = "black", margin = margin(t = -12)),
            axis.text.y = element_text(size = 10, color = "black"),
            legend.text = element_text(size = 10),
            axis.title.x = element_text(size = 10, margin = margin(t = 8, b = 0)),
            axis.title.y = element_text(size = 10, margin = margin(r = 8, b = 0)),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(size = 10, face = "bold", margin = margin(b = 8), color = "black"),
            panel.spacing = unit(16, units = "pt"),
            legend.position = "bottom", legend.direction = "horizontal",
            legend.key.size = unit(10, units = "pt"))
    
  }
  
}
