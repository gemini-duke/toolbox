
# Figures in english ------------------------------------------------------
library(gtsummary)
library(tidyverse)
library(labelled)
library(gt)
library(webshot)

trial <- remove_labels(trial)
trial <- trial %>% select(trt, age, marker, stage)


# initial
trial %>%
                tbl_summary() %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig1.png")


# fig to rename

trial %>%
                tbl_summary(label = trt ~ "Chemotherapy Treatment") %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig2.png")

# fig to rename + 2 vars
var_names <- list(trt ~ "Chemotherapy Treatment",
                  age ~ "Age (years)",
                  marker ~ "Marker Level (ng/mL)",
                  stage ~ "T Stage")
trial %>%
                tbl_summary(label = var_names) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig3.png")


# fig statistic continuous var
trial %>%
                tbl_summary(label = var_names) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig4.png")

# fig mean
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(age ~ "{mean} ({sd})")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig5.png")

# fig all_continuous
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig6.png")

# fig statistic categorical var
trial %>%
                tbl_summary(label = var_names) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig7.png")

# fig n / N prop
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(trt ~ "{n} / {N} ({p}%)")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig8.png")

# fig all_categorical
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_categorical() ~ "{n} / {N} ({p}%)")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig9.png")


# fig all_con and all_cat
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig10.png")


# fig missing
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing") %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig11.png")


# fig digits
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ 0,
                                          all_categorical() ~ 0)) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig12.png")

# fig digits w/ options
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1))) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig13.png")


# fig by
var_names <- list(age ~ "Age (years)",
                  marker ~ "Marker Level (ng/mL)",
                  stage ~ "T Stage")
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig14.png")



# add_p
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p() %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig15.png")

# test arg
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct")) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig16.png")


# pvalue_fun
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig17.png")




# add_overall
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                add_overall() %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig18.png")

# add_overall argument
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                add_overall(col_label = "**Sample**, N = {N}") %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig19.png")

# bold_labels
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                add_overall(col_label = "**Sample**, N = {N}") %>%
                bold_labels() %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig20.png")

# italicize levels
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                add_overall(col_label = "**Sample**, N = {N}") %>%
                bold_labels() %>%
                italicize_levels() %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig21.png")

# bold_p
trial %>%
                tbl_summary(label = var_names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})",
                                             all_categorical() ~ "{n} / {N} ({p}%)"),
                            missing_text = "Missing",
                            digits = list(all_continuous() ~ c(2, 2),
                                          all_categorical() ~ c(0, 0, 1)),
                            by = trt) %>%
                add_p(test = list(all_continuous() ~ "wilcox.test",
                                  all_categorical() ~ "chisq.test.no.correct"),
                      pvalue_fun = \(x) style_pvalue(x, digits = 3)) %>%
                add_overall(col_label = "**Sample**, N = {N}") %>%
                bold_labels() %>%
                italicize_levels() %>%
                bold_p(t = 0.1) %>%
                as_gt() %>%
                gtsave(filename = "en/img/fig22.png")


