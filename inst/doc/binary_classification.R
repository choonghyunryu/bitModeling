## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(bitmodeling)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
library(bitmodeling)

data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event)

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes")   

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7)

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset()

## -----------------------------------------------------------------------------
data_recipe %>% 
  target_table()

## -----------------------------------------------------------------------------
dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  target_table()  

## -----------------------------------------------------------------------------
dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  target_table()  

## -----------------------------------------------------------------------------
dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset() %>% 
  set_formula() %>% 
  step_my_rose() %>% 
  target_table()

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset() %>% 
  set_formula() 

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset() %>% 
  set_formula() %>% 
  step_my_center()

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset() %>% 
  set_formula() %>% 
  step_my_center() %>% 
  step_my_dummy()

## -----------------------------------------------------------------------------
data_recipe <- dlookr::heartfailure %>% 
  target_to(death_event) %>% 
  set_positive("Yes") %>% 
  split_dataset(prop = 0.7) %>% 
  extract_dataset() %>% 
  set_formula() %>% 
  step_my_center() %>% 
  step_my_dummy() %>% 
  step_my_rose()

data_recipe %>% 
  target_table()

## -----------------------------------------------------------------------------
# Logistic Regression 모델의 적합
model_logistic <- get_training(data_recipe)

## -----------------------------------------------------------------------------
# 모델 객체의 출력
model_logistic

## -----------------------------------------------------------------------------
# Ridge Regression 모델의 적합
my_model <- get_training(data_recipe, classifiers = "ridge")

## ---- eval=FALSE--------------------------------------------------------------
#  get_training(
#    x,
#    classifiers = c("logistic", "lasso", "elastic", "ridge", "ranger", "xgboost"),
#    n_fold = 10L,
#    n_grid = 30L,
#    n_best = 10L,
#    n_trees = ifelse(classifiers %in% "ranger", 500L, 500L),
#    best_metric = c("f_meas", "recall", "sensitivity", "precision", "specificity",
#      "accuracy", "bal_accuracy", "detection_prevalence", "f_meas_05", "f_meas_2",
#      "j_index", "kap", "mcc", "npv", "ppv", "gmean"),
#    verbose = TRUE
#  )

## -----------------------------------------------------------------------------
info_model(my_model, "coef")

## ---- out.width="65%", fig.width=6, fig.height=5, dpi=250---------------------
viz_model(my_model, "coef")

## -----------------------------------------------------------------------------
info_model(my_model, "cmat")

## ---- out.width="65%", fig.width=5, fig.height=5, dpi=250---------------------
viz_model(my_model, "heatmap")

## ---- out.width="65%", fig.width=5, fig.height=5, dpi=250---------------------
viz_model(my_model, "mosaic")

## -----------------------------------------------------------------------------
info_model(my_model, "metrics") %>% 
  print(n = 20)

## ---- out.width="65%", fig.width=5, fig.height=5, dpi=250---------------------
viz_model(my_model, "roc")

## ---- out.width="65%", fig.width=7, fig.height=5, dpi=250---------------------
viz_model(my_model, "density")

## ---- eval=FALSE--------------------------------------------------------------
#  info_model(my_model, "quantiles")

## ---- echo=FALSE--------------------------------------------------------------
info_model(my_model, "quantiles") %>% 
  print(n = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  info_model(my_model, "quantiles", nbins = 7)

## ---- echo=FALSE--------------------------------------------------------------
info_model(my_model, "quantiles", nbins = 7)

## ---- out.width="65%", fig.width=8, fig.height=5, dpi=250---------------------
viz_model(my_model, "bin")

## ---- out.width="65%", fig.width=8, fig.height=5, dpi=250---------------------
viz_model(my_model, "bin", nbins = 7)

## -----------------------------------------------------------------------------
my_models <- multi_classifiers(data_recipe, classifiers = c("ridge", "ranger"))

## -----------------------------------------------------------------------------
compare_info_model(my_models, "metrics")

## -----------------------------------------------------------------------------
compare_info_model(my_models, "metrics", merge = TRUE)

## -----------------------------------------------------------------------------
compare_info_model(my_models, "metrics", merge = TRUE) %>% 
  arrange(.metric, desc(.estimate)) %>% 
  print(n = Inf)

## ---- out.width="65%", fig.width=8, fig.height=5, dpi=250---------------------
compare_viz_model(my_models, "bin")

## -----------------------------------------------------------------------------
# 모델 개발을 위한 첫째 recipe 정의
death_event_recipe <- dlookr::heartfailure %>%
  target_to(death_event) %>%
  set_positive("Yes") %>%
  split_dataset(prop = 0.7) %>%
  extract_dataset() %>%
  set_formula() %>%
  step_my_center() %>%
  step_my_dummy() %>%
  step_my_rose()
  
# 모델 개발을 위한 둘째 recipe 정의
diabetes_recipe <- dlookr::heartfailure %>%
  target_to(diabetes) %>%
  set_positive("Yes") %>%
  split_dataset(prop = 0.7) %>%
  extract_dataset() %>%
  set_formula() %>%
  step_my_center() %>%
  step_my_dummy() %>%
  step_my_rose()

# recipe를 성분으로 갖는 list 정의
recipes <- list(death_event_recipe, diabetes_recipe)

my_recipes <- multi_recipes(recipes, classifiers = c("ridge", "ranger"))

## -----------------------------------------------------------------------------
compare_info_model(my_recipes[[1]], "metrics")

## ---- out.width="65%", fig.width=8, fig.height=5, dpi=250---------------------
compare_viz_model(my_recipes[[1]], "bin")

## ---- eval=FALSE--------------------------------------------------------------
#  # 모델 개발을 위한 recipe 정의
#  data_recipe <- dlookr::heartfailure %>%
#    target_to(death_event) %>%
#    set_positive("Yes") %>%
#    split_dataset(prop = 0.7) %>%
#    extract_dataset() %>%
#    set_formula() %>%
#    step_my_center() %>%
#    step_my_dummy() %>%
#    step_my_rose()
#  
#  # logistic regrssion 적합
#  my_model <- get_training(data_recipe)
#  
#  # logistic regrssion 성능평가 보고서 출력
#  report_model(my_model)

## ---- eval=FALSE--------------------------------------------------------------
#  # 복수개의 classifiers별로 적합
#  # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost" classifiers를 순차 모델링
#  my_models <- multi_classifiers(data_recipe)
#  
#  # classifiers별로 성능을 비교할 수 있는 성능평가 보고서 출력
#  report_model(my_models, output_file = "compare_model_performance.xlsx")

## ---- eval=FALSE--------------------------------------------------------------
#  # 모델 개발을 위한 첫째 recipe 정의
#  death_event_recipe <- dlookr::heartfailure %>%
#    target_to(death_event) %>%
#    set_positive("Yes") %>%
#    split_dataset(prop = 0.7) %>%
#    extract_dataset() %>%
#    set_formula() %>%
#    step_my_center() %>%
#    step_my_dummy() %>%
#    step_my_rose()
#  
#  # 모델 개발을 위한 둘째 recipe 정의
#  diabetes_recipe <- dlookr::heartfailure %>%
#    target_to(diabetes) %>%
#    set_positive("Yes") %>%
#    split_dataset(prop = 0.7) %>%
#    extract_dataset() %>%
#    set_formula() %>%
#    step_my_center() %>%
#    step_my_dummy() %>%
#    step_my_rose()
#  
#  # recipe를 성분으로 갖는 list 정의
#  recipes <- list(death_event_recipe, diabetes_recipe)
#  
#  my_recipes <- multi_recipes(recipes)
#  
#  # recipes별로 classifiers의 성능을 비교할 수 있는 성능평가 보고서 출력
#  report_model_recipes(my_recipes)
#  
#  # 생성된 파일의 이름들
#  list.files(path = getwd(), pattern = "model_performance")

