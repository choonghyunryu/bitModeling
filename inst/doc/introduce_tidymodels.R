## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(bitmodeling)
library(dplyr)
library(ggplot2)

## ----fig1, echo=FALSE, out.width = "95%", fig.align='center'------------------
knitr::include_graphics("img/tidymodel_eco.jpg")

## ----fig3, echo=FALSE, out.width = "95%", fig.align='center'------------------
knitr::include_graphics("img/big_picture.png")

## ----fig4, echo=FALSE, out.width = "95%", fig.align='center'------------------
knitr::include_graphics("img/process.png")

## ---- eval=FALSE--------------------------------------------------------------
#  flights %>%
#    group_by(month, day) %>%
#    summarise(dep_delay_avg = mean(dep_delay, na.rm = TRUE),
#              .groups = "drop") %>%
#    arrange(desc(dep_delay_avg)) %>%
#    filter(row_number() <= 5)

## ----fig5, echo=FALSE, out.width = "95%", fig.align='center'------------------
knitr::include_graphics("img/overview.png")

## ---- eval=TRUE, message=FALSE------------------------------------------------
library(tidymodels)

set.seed(123)
# Create the data split object
heartfailure_split <- dlookr::heartfailure %>% 
  mutate(death_event = factor(death_event, levels = c("Yes", "No"))) %>% 
  initial_split(prop = 0.7, strata = death_event) 

# Create the training data
heartfailure_train <- training(heartfailure_split)

# Create the test data
heartfailure_test  <- testing(heartfailure_split)

## ---- eval=TRUE, message=FALSE------------------------------------------------
nrow(heartfailure_train)

nrow(heartfailure_test)

## -----------------------------------------------------------------------------
# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Print the model specification
logistic_model

## -----------------------------------------------------------------------------
# Fit to training data
logistic_fit <- logistic_model %>% 
  fit(death_event ~ ., data = heartfailure_train)

# Print model fit object
logistic_fit

## -----------------------------------------------------------------------------
# Predict outcome category
class_preds <- logistic_fit %>% 
  predict(new_data = heartfailure_test, type = "class")

## -----------------------------------------------------------------------------
# Predict outcome category
prob_preds <- logistic_fit %>% 
  predict(new_data = heartfailure_test, type = "prob")

## -----------------------------------------------------------------------------
heartfailure_result <- heartfailure_test %>% 
  select(death_event) %>% 
  bind_cols(class_preds, prob_preds)

head(heartfailure_result)

## -----------------------------------------------------------------------------
# Create confusion matrix
cmat <- heartfailure_result %>% 
  conf_mat(truth = death_event, estimate = .pred_class)

# Print confusion matrix object
cmat

## ---- out.width="65%"---------------------------------------------------------
cmat %>% 
  autoplot(type = "heatmap")

## ---- out.width="65%"---------------------------------------------------------
cmat %>% 
  autoplot(type = "mosaic")

## -----------------------------------------------------------------------------
cmat %>% 
  summary() %>% 
  print(n = 15)

## -----------------------------------------------------------------------------
# Calculate metrics across thresholds
threshold <- heartfailure_result %>% 
  roc_curve(truth = death_event, .pred_Yes)

threshold 

## ---- out.width="65%"---------------------------------------------------------
threshold %>% 
  autoplot()

## -----------------------------------------------------------------------------
heartfailure_result %>% 
  roc_auc(truth = death_event, .pred_Yes)

## ---- echo=FALSE, out.width = "65%", fig.align='left'-------------------------
knitr::include_graphics("img/bob.png")

## -----------------------------------------------------------------------------
heartfailure_train %>% 
  count(death_event) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))

## -----------------------------------------------------------------------------
# 레시피의 정의
heartfailure_recipe <- recipe(death_event ~ ., data = heartfailure_train) %>% 
  step_center(all_predictors(), -all_nominal()) %>% 
  themis::step_rose(death_event) 

# 레시피의 내용을 살핌
heartfailure_recipe

## -----------------------------------------------------------------------------
heartfailure_recipe %>% 
  prep()

## -----------------------------------------------------------------------------
heartfailure_recipe %>% 
  "$"("steps") %>% 
  "[["(1)

## -----------------------------------------------------------------------------
heartfailure_recipe %>% 
  prep() %>% 
  "$"("steps") %>% 
  "[["(1)

## -----------------------------------------------------------------------------
heartfailure_recipe %>% 
  prep() %>% 
  juice() %>% 
  count(death_event) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))  

## -----------------------------------------------------------------------------
heartfailure_test %>% 
  tibble::as_tibble()

## -----------------------------------------------------------------------------
heartfailure_recipe %>% 
  prep() %>% 
  bake(new_data = heartfailure_test)

## -----------------------------------------------------------------------------
# 원 데이터의 target 변수의 분포
heartfailure_test %>% 
  count(death_event) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))    

# 조리된 데이터의 target 변수의 분포
heartfailure_recipe %>% 
  prep() %>% 
  bake(new_data = heartfailure_test) %>% 
  count(death_event) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))    

## -----------------------------------------------------------------------------
# 워크플로우 정의하기
heartfailure_wf <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(heartfailure_recipe)

# 워크플로우 내용
heartfailure_wf

## -----------------------------------------------------------------------------
# 워크플로우 기반 모델 적합
heartfailure_fit <- heartfailure_wf %>% 
  last_fit(split = heartfailure_split)

# 적합된 결과로서의 "last_fit" 객체
heartfailure_fit

## -----------------------------------------------------------------------------
heartfailure_fit %>% 
  collect_metrics()

## -----------------------------------------------------------------------------
set.seed(12345)

# `10-Fold Cross-Validation` 정의
heartfailure_fold <- heartfailure_train %>% 
  vfold_cv(v = 10, strata = death_event)

# `10-Fold Cross-Validation` 내용
heartfailure_fold

## -----------------------------------------------------------------------------
# 모델 검증을 위한 메트릭 정의
heartfailure_metrics <- metric_set(
  roc_auc, sens, spec, f_meas
)

# 워크플로우에 Cross Validation 적용 모델링
heartfailure_fit_rs <- heartfailure_wf %>% 
  fit_resamples(
    resamples = heartfailure_fold,
    metrics = heartfailure_metrics,
    control = control_resamples(save_pred = TRUE)
  )

## -----------------------------------------------------------------------------
# 성능 평가 메트릭 조회
heartfailure_fit_rs %>% 
  collect_metrics()

## -----------------------------------------------------------------------------
# Confusion Matrix
heartfailure_fit_rs %>% 
  conf_mat_resampled(tidy = FALSE)

## -----------------------------------------------------------------------------
# 상대 비율로 계산한 Confusion Matrix
heartfailure_fit_rs %>% 
  conf_mat_resampled(tidy = FALSE) %>% 
  "$"(table) %>% 
  prop.table()

## -----------------------------------------------------------------------------
cmat %>% 
  "$"(table) %>% 
  prop.table()

## -----------------------------------------------------------------------------
lasso_model <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
  parsnip::set_engine("glmnet") %>% 
  parsnip::set_mode("classification")

## -----------------------------------------------------------------------------
# 명목형 변수의 가변수화
lasso_recipe <- heartfailure_recipe %>% 
  step_dummy(all_nominal(), -all_outcomes())

# 변경된 레시피
lasso_recipe

## -----------------------------------------------------------------------------
# Lasso 모형워크플로우 정의하기
lasso_wf <- workflows::workflow() %>% 
  workflows::add_model(lasso_model) %>% 
  workflows::add_recipe(lasso_recipe)

# 워크플로우 내용
lasso_wf

## -----------------------------------------------------------------------------
lasso_grid <- tibble::tibble(penalty = 10^seq(-2, -1, length.out = 30))

lasso_tune_grid <- lasso_wf %>%
  tune::tune_grid(
    resamples = heartfailure_fold, 
    grid = lasso_grid, 
    control = tune::control_grid(verbose = FALSE, save_pred = TRUE), 
    metrics = yardstick::metric_set(
      accuracy, bal_accuracy, detection_prevalence, f_meas, j_index, kap, mcc, 
      npv, ppv, precision, recall, sensitivity, specificity))

## -----------------------------------------------------------------------------
lasso_tune_grid %>%
  tune::collect_metrics()

## -----------------------------------------------------------------------------
best_metric <- "f_meas"

best_model <- lasso_tune_grid %>%
  tune::select_best(best_metric)

final_wf <- lasso_wf %>%
  tune::finalize_workflow(best_model)

## -----------------------------------------------------------------------------
coefs <- final_wf %>%
  fit(data = heartfailure_train) %>% 
  tidy() %>%
  filter(!term %in% "(Intercept)") %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  mutate(term = forcats::fct_reorder(term, estimate)) %>%
  filter(estimate != 0)  

coefs

## ---- out.width="65%", fig.width=6, fig.height=5, dpi=250---------------------
library(ggplot2)

coefs %>% 
  ggplot(aes(x = term, y = estimate, fill = `estimate > 0`)) + 
  geom_col(alpha = 0.8, show.legend = FALSE) + 
  coord_flip() + 
  labs(x = NULL, title = "death_event 예측에 영향을 주는 모델의 변수들") + 
  theme_minimal(base_family = "NanumSquare")

## -----------------------------------------------------------------------------
# 최종 모형 적합
final_fit <- final_wf %>%
  tune::last_fit(split = heartfailure_split)

# test 데이터셋의 target variable 예측
pred <- final_fit %>%
  tune::collect_predictions()

# 예측된 결과
pred

## -----------------------------------------------------------------------------
# Confusion Matrix 계산
cmat <- pred %>%
  yardstick::conf_mat(truth = death_event, .pred_class)

cmat

