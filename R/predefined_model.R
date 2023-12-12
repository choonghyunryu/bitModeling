#' 모델 메타 정의
#'
#' @description
#' parsnip 패키지를 이용해서, tidymodels 에코 시스템에서 사용할 수 있는 모델 정의
#'
#' @param model character. 모델의 이름. c("logistic", "lasso", "elastic", "ridge", 
#' "ranger", "xgboost")에서 선택.
#' @return model_spec 클래스 객체.
#' @examples
#' \donttest{
#' get_classifier("logistic")
#' 
#' get_classifier("lasso")
#' 
#' get_classifier("ridge") 
#' 
#' get_classifier("elastic") 
#' 
#' get_classifier("ranger") 
#' 
#' get_classifier("xgboost") 
#' }
#' 
#' @export
#' @importFrom parsnip logistic_reg set_engine set_mode
#' @importFrom tune tune
#' @importFrom purrr map
get_classifier <- function(model = c("logistic", "lasso", "elastic", "ridge", 
                                     "ranger", "xgboost")) {
  logistic_model <- parsnip::logistic_reg() %>% 
    parsnip::set_engine("glm") %>% 
    parsnip::set_mode("classification")
  
  lasso_model <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("classification")
  
  elastic_model <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 0.5) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("classification")
  
  ridge_model <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 0) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("classification")
  
  ranger_model <- parsnip::rand_forest(mtry = tune::tune(), 
                                       trees = tune::tune(), 
                                       min_n = tune::tune()) %>% 
    parsnip::set_engine("ranger", importance = "impurity") %>% 
    parsnip::set_mode("classification") 
  
  xgboost_model <- parsnip::boost_tree(trees = tune::tune(), 
                                       min_n = tune::tune(),
                                       tree_depth = tune::tune(),
                                       learn_rate = tune::tune(),
                                       sample_size = tune::tune(),
                                       mtry = tune::tune(),
                                       loss_reduction = tune::tune()) %>% 
    parsnip::set_engine("xgboost") %>% 
    parsnip::set_mode("classification")  
  
  models <- paste(model, "model", sep = "_")
  
  result <- models %>% 
    purrr::map(
      function(x) {
        get(x)
      }
    )
  
  names(result) <- model
  
  if (length(result) == 1) {
    result <- result[[1]]
  }
  
  result
}



#' Fit Binary Classification
#'
#' @description
#' recipe 클래스와 정의된 classifiers를 이용하여 Binary Classification을 적합함
#'
#' @param x recipe. 모델을 정의한 recipe 객체.
#' @param classifiers character. Binary classification 모델의 classifier. 
#' c("logistic", "lasso", "elastic", "ridge", "ranger", "xgboost")에서 선택.
#' @param n_fold integer. Cross-Validation을 수행한 n-Folds의 값. 기본값은 10.
#' @param n_grid integer. Hyper Parameters를 수행할 Grid의 개수. 기본값은 30.
#' @param n_best integer. Best 모델의 목록을 계산할 목록의 개수. 기본값은 10.
#' @param n_trees integer. model이 "ranger"나 "xgboost"일 경우에만 사용하는 인수.
#'  모델에서 생성할 개별 트리의 개수를 지정함. 기본값은 "ranger"는 500개, "xgboost"는 500개임.
#' @param best_metric character. Best 모델을 선정할 때 사용하는 성능지표. 
#' c("f_meas", "recall", "sensitivity", "precision", "specificity", "accuracy", 
#' "bal_accuracy", "detection_prevalence", "f_meas_05", "f_meas_2", "j_index", 
#' "kap", "mcc", "npv", "ppv", "gmean") 중에서 선택함. 기본값은 "f_meas"
#' @param verbose logical. 작업 경과의 정보를 출력할지의 여부, 기본값은 TRUE. 
#' @return workflow 클래스 객체.
#' 반환하는 workflow 객체는 다음과 같은 attributes를 갖음.
#' \itemize{
#' \item coef_term tbl_df. 모델을 정의한 terms의 추정 계수값 등의 정보. 
#' \item pred tbl_df. 모델에 test 셋을 적용해서 예측한 예측값 정보. 
#' \item cmat conf_mat. 모델의 성능 측정을 위한 confusion matrix.
#' \item metrics tbl_df. 모델의 성능 측정 메트릭 정보.
#' }
#' 
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#' 
#' # 정의한 recipe로 "logistic" 모델 빌드
#' my_model <- get_training(data_recipe)
#' 
#' my_model
#' 
#' # 정의한 recipe로 "xgboost" 모델 빌드
#' my_model <- get_training(data_recipe, classifiers = "xgboost")
#' }
#' 
#' @export
#' @import dplyr
#' @import yardstick
#' @import dials
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom rsample vfold_cv testing
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom tune tune_grid control_grid collect_metrics show_best select_best 
#' finalize_workflow last_fit collect_predictions fit_resamples
#' @importFrom forcats fct_reorder
#' @importFrom cli cli_h1 cli_progress_step
#' @importFrom purrr map
get_training <- function(x, classifiers = c("logistic", "lasso", "elastic", "ridge", 
                                            "ranger", "xgboost"), 
                         n_fold = 10L, n_grid = 30L, n_best = 10L, 
                         n_trees = ifelse(classifiers %in% "ranger", 500L, 500L),
                         best_metric = c("f_meas", "recall", "sensitivity", 
                                         "precision", "specificity", "accuracy", 
                                         "bal_accuracy", "detection_prevalence", 
                                         "f_meas_05", "f_meas_2", "j_index", 
                                         "kap", "mcc", "npv", "ppv", "gmean"), 
                         verbose = TRUE) {
  classifier <- match.arg(classifiers)
  best_metric <- match.arg(best_metric)
    
  if (verbose) {
    cli::cli_h1(glue::glue("Start {classifier} modeling"))
  }
  
  if (!classifier %in% c("logistic", "ridge", "lasso", "elastic")) {
    if (classifier %in% c("ranger")) {
      steps <- c("step_center", "step_dummy")
    } else if (classifier %in% c("xgboost")) {
      steps <- c("step_center")
    }
      
    flag_remove <- x$steps %>% 
      purrr::map_lgl(function(i) {
        step_nm <- class(i) %>% 
          unlist() %>% 
          "["(1)
        
        if (step_nm %in% steps) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      })
    
    x$steps <- x$steps[flag_remove]    
  }  

  wf <- workflows::workflow() %>%
    workflows::add_model(get_classifier(classifier)) %>%
    workflows::add_recipe(x)
  
  data_train <- x$template
  
  param_mtry <- floor(sqrt(ncol(data_train)))
  
  target_variable <- x$template %>% 
    attr("target")
  
  my_folds <- rsample::vfold_cv(data_train, v = n_fold, 
                                strata = all_of(target_variable))

  ## 모델 Hyper-parameters 설정      
  if (classifier %in% c("logistic")) {
    my_grid <- NULL
  } else if (classifier %in% c("ridge", "lasso", "elastic")) {
    my_grid <- tibble::tibble(penalty = 10^seq(-2, -1, length.out = n_grid))
  } else if (classifier %in% c("ranger")) {  
    ranger_prams <- dials::parameters(
      list(
        dials::finalize(dials::mtry(), data_train),        
        # trees = n_trees,
        dials::finalize(dials::min_n(), data_train)
      )
    )
    
    my_grid <- dials::grid_max_entropy(
      ranger_prams,
      size = n_grid
    ) %>% 
      bind_cols(
        data.frame(trees = n_trees)
      )
  } else if (classifier %in% c("logistic")) {  
    my_grid <- 1
  } else if (classifier %in% c("xgboost")) {  
    xgboost_prams <- dials::parameters(
      list(
        # trees = n_trees,
        dials::min_n(),
        dials::tree_depth(),
        dials::learn_rate(),
        sample_size = dials::sample_prop(),   
        dials::finalize(dials::mtry(), data_train),
        dials::loss_reduction()
      )
    )
      
    my_grid <- dials::grid_max_entropy(
      xgboost_prams,
      size = n_grid
      ) %>% 
      bind_cols(
        data.frame(trees = n_trees)
      )
  }
  
  # cross validation and hyper parameters
  if (verbose) {
    cli::cli_progress_step("Step 1 : cross validation and hyper parameters")    
  }  
  
  suppressMessages(
    if (classifier %in% c("logistic")) {
      my_res <- wf %>%
        tune::fit_resamples(
          resamples = my_folds, 
          metrics = yardstick::metric_set(
            accuracy, bal_accuracy, detection_prevalence, f_meas, f_meas_05, f_meas_2,
            j_index, kap, mcc, npv, ppv, precision, recall, sensitivity, specificity,
            gmean)          
        )
    } else {
      my_res <- wf %>%
        tune::tune_grid(
          resamples = my_folds, 
          grid = my_grid, 
          control = tune::control_grid(verbose = FALSE, save_pred = TRUE), 
          metrics = yardstick::metric_set(
            accuracy, bal_accuracy, detection_prevalence, f_meas, f_meas_05, f_meas_2,
            j_index, kap, mcc, npv, ppv, precision, recall, sensitivity, specificity,
            gmean))
    }
  )

  my_metrics <- my_res %>%
    tune::collect_metrics()
  
  # choice best model and fit final model with train set  
  if (verbose) {
    cli::cli_progress_step("Step 2 : choice best model and fit final model with train set")  
  } 
  
  suppressMessages({
    my_best <- my_res %>%
      tune::show_best(best_metric, n = n_best) 
    
    best_model <- my_res %>%
      tune::select_best(best_metric)
    
    final_wf <- wf %>%
      tune::finalize_workflow(best_model)
    
    fit_last <- final_wf %>%
      fit(data = data_train)
  })
  
  if (classifier %in% c("logistic", "ridge", "lasso", "elastic")) {
    coef_term <- fit_last %>%
      tidy() %>%
      filter(!term %in% "(Intercept)") %>%
      group_by(estimate > 0) %>%
      top_n(10, abs(estimate)) %>%
      ungroup() %>%
      mutate(term = forcats::fct_reorder(term, estimate)) %>%
      filter(estimate != 0)
    
    attr(fit_last , "coef_term") <- coef_term     
  } else {
    attr(fit_last , "coef_term") <- NULL     
  }  

  
  # predict with test set  
  if (verbose) {
    cli::cli_progress_step("Step 3 : predict with test set")  
  } 
  
  final_fit <- final_wf %>%
    tune::last_fit(split = x$template %>% 
                     attr("split") )
  
  pred <- final_fit %>%
    tune::collect_predictions()
  
  attr(fit_last , "pred") <- pred 
  
  # solve the evaluate metrics
  if (verbose) {
    cli::cli_progress_step("Step 4 : solve the evaluate metrics")  
  }   
  
  pred %>%
    count(.pred_class)
  
  cmat <- pred %>%
    yardstick::conf_mat(truth = target_variable, .pred_class)
  
  attr(fit_last , "cmat") <- cmat 
  
  eval_metrics <- metric_set(
    accuracy, bal_accuracy, detection_prevalence, f_meas, f_meas_05, f_meas_2,
    j_index, kap, mcc, npv, ppv, precision, recall, sensitivity, specificity,
    gmean)
  
  solved_metrics <- pred %>%
    eval_metrics(truth = !!eval(target_variable), estimate = .pred_class) 
  
  attr(fit_last , "metrics") <- solved_metrics %>% 
    bind_rows(
      pred %>%
        rename(positive = 2) %>% 
        mutate(target = !!rlang::sym(target_variable)) %>% 
        yardstick::roc_auc(truth = target, positive)
    )
  
  fit_last
}



#' Statistics for Model Performance
#'
#' @description
#' 모델링 함수로 생성한 모델에 대해서 성능 평가를 위한 통계를 계산함
#'
#' @param x workflow. get_training() 결과로 생성한 모델, workflow 객체.
#' @param stats character. 계산할 성능 평가 정보. 
#' c("coef", "cmat", "metrics", "quantiles")에서 선택.
#' @param nbins integer. 예측 확률을 비닝할 때 사용할 빈(Bins)의 개수. 
#' stats의 값이 "quantiles"일 경우만 적용되며 기본값은 10.
#' @return tbl_df, conf_mat 클래스의 객체. stats의 종류에 따라 반환하는 객체의 유형이 다름.
#' @details 
#' stats의 값이 "coef"일 경우에는 regression 기반의 모델은 terms 계수를 tbl_df 객체로 반환하고, 
#' tree 기반의 모델은 variable importance scores를 반환함.
#' regression 기반의 모델,
#' \itemize{
#' \item term factor. 모델을 정의한 terms. 
#' \item estimate numeric. 모델을 정의한 terms의 추정 계수. 
#' \item penalty numeric. 모델에 사용한 penalty. 
#' \item `estimate > 0` logical. 추정 계수가 0보다 큰가의 여부.
#' }
#' tree 기반의 모델,
#' \itemize{
#' \item term factor. 모델을 정의한 terms. 
#' \item Importance numeric. importance scores. 
#' \item Importance_scal numeric. 최대값 기준으로 스케일된 importance scores. 
#' }
#' 
#' stats의 값이 "cmat"일 경우에는 Confusion Matrix를 conf_mat 클래스 객체로 반환함.
#' 
#' stats의 값이 "metrics"일 경우에는 성능 평가 지표를 tbl_df 클래스 객체로 반환함.
#' \itemize{
#' \item .metric character. 성능 평가지표 이름. 
#' \item .estimator character. taget 변수의 데이터 유형. 
#' \item .estimate numeric. 성능 평가지표 값. 
#' }
#' 
#' stats의 값이 "quantiles"일 경우에는 예측 확률의 구간별 분포 정보를 tbl_df 클래스 객체로 반환함.
#' \itemize{
#' \item binn character. 예측 확률을 비닝한 구간. 
#' \item n integer. 데이터 건수. 
#' \item positive integer. 구간에서의 positive 데이터 건수. 
#' \item negative integer. 구간에서의 negative 데이터 건수. 
#' \item prob_positive numeric. 구간에서의 positive 비율. 
#' \item lift numeric. 구간에서의 positive 비율에 대한 lift. 
#' }
#' @seealso \code{\link{viz_model}}
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#' 
#' # 정의한 recipe로 모델 빌드
#' my_model <- get_training(data_recipe)
#' 
#' # 모델 terms의 계수 계산
#' info_model(my_model, "coef")
#' 
#' # 모델의 Confusion Matrix 계산
#' info_model(my_model, "cmat") 
#' 
#' # 모델의 성능 평가지표 계산
#' info_model(my_model, "metrics")
#' 
#' # 예측 확률의 구간별 분포 계산
#' info_model(my_model, "quantiles") 
#' 
#' # 예측 확률의 구간별 분포 계산, bins의 개수를 15개로 계산
#' info_model(my_model, "quantiles", nbins = 15) 
#' }
#' 
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom tidyr pivot_wider
#' @importFrom tune extract_fit_parsnip
#' @importFrom vip vi
info_model <- function(x, stats = c("coef", "cmat", "metrics", "quantiles"), 
                       nbins = 10) {
  stats <- match.arg(stats)
  
  target_variable <- x$pre$actions$recipe$recipe$template %>% 
    attr("target")
  
  if (stats %in% "coef") {
    info <- x %>% 
      attr("coef_term")
    if (is.null(info)) {
      info <- x %>% 
        tune::extract_fit_parsnip() %>% 
        vip::vi() %>% 
        mutate(Importance_scal = round(Importance / max(Importance) * 100, 2)) %>% 
        rename(term = Variable)
    }
  } else if (stats %in% "cmat") {
    info <- x %>% 
      attr("cmat")
  } else if (stats %in% "metrics") {
    info <- x %>% 
      attr("metrics")
  } else if (stats %in% "quantiles") {
      n <- x %>% 
        attr("pred") %>% NROW()
      
      prob <- x %>% 
        attr("pred") %>% 
        select(3) %>% 
        pull() %>% 
        sort()

      breaks <- seq(0, n, length.out = nbins + 1)
      
      freq <- prob[breaks] %>% table() %>% as.integer()
      binn <- levels(cut(prob, unique(c(0, prob[breaks]))))
      binns <- rep(binn, freq)
      
      info <- x %>% 
        attr("pred") %>% 
        rename(positive = 3) %>% 
        mutate(target = !!rlang::sym(target_variable)) %>%       
        arrange(positive) %>% 
        mutate(index = row_number()) %>% 
        mutate(binn = cut(index, breaks)) %>%
        count(binn, target) %>% 
        tidyr::pivot_wider(names_from = target, values_from = n) %>% 
        mutate_if(is.integer, function(x) ifelse(is.na(x), 0L, x)) %>% 
        mutate(binn = binns) %>% 
        rename(negative = 2,
               positive = 3) %>% 
        mutate(n = positive + negative,
               prob_positive = positive / n,
               lift = prob_positive / (sum(positive) / sum(n))) %>% 
        select(binn, n, positive, negative, prob_positive, lift)
  # } else if (stats %in% "intervals") {
  #   info <- x %>% 
  #     attr("pred") %>% 
  #     rename(positive = 3) %>% 
  #     mutate(target = !!rlang::sym(target_variable)) %>%
  #     arrange(positive) %>% 
  #     mutate(index = row_number()) %>%       
  #     mutate(binn = dlookr::binning(index, nbins = nbins, type = "quantile")) %>%
  #     count(binn, target) %>% 
  #     tidyr::pivot_wider(names_from = target, values_from = n) %>% 
  #     mutate_if(is.integer, function(x) ifelse(is.na(x), 0L, x)) %>% 
  #     rename(negative = 2,
  #            positive = 3) %>% 
  #     mutate(n = positive + negative,
  #            prob_positive = positive / n,
  #            lift = prob_positive / (sum(positive) / sum(n))) %>% 
  #     select(binn, n, positive, negative, prob_positive, lift)
  }
 
  info 
}



#' Visualization for Model Performance
#'
#' @description
#' 모델링 함수로 생성한 모델에 대해서 성능 평가를 위한 플롯을 시각화함
#'
#' @param x workflow. get_training() 결과로 생성한 모델, workflow 객체.
#' @param stats character. 시각화할 플롯의 종류. 
#' c("coef", "heatmap", "mosaic", "roc", "density", "bin")에서 선택.
#' @param nbins integer. 예측 확률을 비닝할 때 사용할 빈(Bins)의 개수. 
#' stats의 값이 "quantiles"일 경우만 적용되며 기본값은 10.
#' @return gg 클래스의 객체. ggplot2 시각화 객체
#' @details 
#' stats의 값에 따라 다음과 같은 플롯을 시각화함.
#' \itemize{
#' \item "coef": terms별 계수와의 관계 시각화. tree 기반의 모델링에서는 계수가 아닌, feature importance 값임.
#' \item "heatmap": Confusion Matrix 결과를 heatmap 플롯으로 시각화.
#' \item "heatmap": Confusion Matrix 결과를 mosaic 플롯으로 시각화.
#' \item "roc": ROC 커브 시각화.
#' \item "density": 예측확률의 target 클래스별 밀도 시각화.
#' \item "bin": 예측 확률의 구간별 target 클래스별 분포 시각화.
#' }
#' @seealso \code{\link{info_model}}
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#' 
#' # 정의한 recipe로 모델 빌드
#' my_model <- get_training(data_recipe)
#' 
#' # terms별 계수와의 관계 시각화
#' viz_model(my_model, "coef")
#' 
#' # Confusion Matrix 결과를 heatmap 플롯으로 시각화
#' viz_model(my_model, "heatmap")
#' 
#' # Confusion Matrix 결과를 mosaic 플롯으로 시각화
#' viz_model(my_model, "mosaic")
#' 
#' # ROC 커브 시각화
#' viz_model(my_model, "roc")
#'
#' # 예측확률의 target 클래스별 밀도 시각화
#' viz_model(my_model, "density")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화
#' viz_model(my_model, "bin")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화 (15개 bins)
#' viz_model(my_model, "bin", nbins = 15)
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @importFrom tune extract_fit_parsnip
#' @importFrom vip vip
viz_model <- function(x, stats = c("coef", "heatmap", "mosaic", "roc", "density",
                                   "bin"), nbins = 10) {
  stats <- match.arg(stats)
  
  target_variable <- x$pre$actions$recipe$recipe$template %>% 
    attr("target")
  
  positive_class <- x$pre$actions$recipe$recipe$template %>% 
    attr("positive")  
  
  engine <- x$fit$fit$spec$engine 
  
  suppressWarnings(
    alpha <- x$fit$fit$spec$args$mixture %>% 
      as.expression() %>% 
      as.character() %>% 
      sub("~", "", .) %>% 
      as.numeric()
  )

  if (engine %in% "glm") {
    subtitle <- "for logistic regression"    
  } else if (engine %in% "glmnet") {
    if (alpha == 1) {
      subtitle <- "for lasso regression"
    } else if (alpha == 0) {
      subtitle <- "for ridge regression"
    } else {
      subtitle <- "for elastic net regression"
    }
  } else {
    subtitle <- NULL
  }
  
  
  if (stats %in% "coef") {
    coefs <- x %>% 
      attr("coef_term")
    
    if (!is.null(coefs)) {
      p <- coefs %>% 
        ggplot(aes(x = term, y = estimate, fill = `estimate > 0`)) + 
        geom_col(alpha = 0.8, show.legend = FALSE) + 
        coord_flip() + 
        labs(x = NULL, title = glue::glue("{target_variable} 예측에 영향을 주는 모델의 변수들"), 
             subtitle = subtitle) + 
        theme_minimal(base_family = "NanumSquare")
    } else {
      p <- x %>% 
        tune::extract_fit_parsnip() %>% 
        vip::vip(aesthetics = list(fill = "coral")) +
        labs(title = glue::glue("{target_variable} 예측에 영향을 주는 모델의 변수들")) +        
        ggplot2::theme_minimal(base_family = "NanumSquare")
    }
  } else if (stats %in% "heatmap") {
    update_geom_defaults(geom = "tile", new = list(color = "black", alpha = 0.7))
    
    p <- x %>% 
      attr("cmat") %>% 
      autoplot(type = "heatmap") +
      labs(title = glue::glue("{target_variable} 예측 Confusion Matrix"), 
           subtitle = subtitle)       
  } else if (stats %in% "mosaic") {
    update_geom_defaults(geom = "rect", new = list(fill = "midnightblue", alpha = 0.7))
    
    p <- x %>% 
      attr("cmat") %>% 
      autoplot(type = "mosaic") +
      labs(title = glue::glue("{target_variable} 예측 Mosaic Confusion Matrix"), 
           subtitle = subtitle)        
  } else if (stats %in% "roc") {
    p <- x %>% 
      attr("pred") %>% 
      rename(positive = 2) %>% 
      mutate(target = !!rlang::sym(target_variable)) %>%       
      roc_curve(truth = target, positive) %>%
      autoplot() +
      labs(title = glue::glue("{target_variable} 예측 ROC Curve"), 
           subtitle = subtitle)         
  } else if (stats %in% "density") {
    p <- x %>% 
      attr("pred") %>% 
      rename(positive = 3) %>% 
      mutate(target = !!rlang::sym(target_variable)) %>%     
      ggplot(aes(x = positive, group = target, color = target)) +
      geom_density() + 
      labs(x = "예측확률", color = target_variable,
           title = glue::glue("예측확률별 Target class density"),
           subtitle = subtitle) + 
      theme_minimal(base_family = "NanumSquare") +
      theme(legend.position = "top")
  } else if (stats %in% "bin") {
    tab <- info_model(x, "quantiles", nbins = nbins) %>% 
      select(binn, positive, negative) %>% 
      tidyr::pivot_longer(-binn, names_to = "target", values_to = "n")
    
    p <- tab %>%
      ggplot(aes(x = binn, y = n, group = target, fill = target)) +
      geom_bar(stat = "identity", position = "fill") + 
      labs(x = glue::glue("{positive_class} 예측확률"), y = "비율", 
           title = glue::glue("예측확률 구간별 Target class 비율"),
           subtitle = subtitle, fill = target_variable) +
      theme_minimal(base_family = "NanumSquare") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  p   
}


#' Fit Binary Classification with Multi Classifiers
#'
#' @description
#' 여러 개의 classifiers로 Binary Classification modeling을 수행함
#'
#' @param x recipe. 모델을 정의한 recipe 객체.
#' @param classifiers character. 모델의 classifiers 이름을 담은 문자열 벡터. 
#' c("logistic", "lasso", "elastic", "ridge", "ranger", "xgboost")에서 복수 선택.
#' @param n_fold integer. Cross-Validation을 수행한 n-Folds의 값. 기본값은 10.
#' @param n_grid integer. Hyper Parameters를 수행할 Grid의 개수. 기본값은 30.
#' @param n_best integer. Best 모델의 목록을 계산할 목록의 개수. 기본값은 10.
#' @param n_trees integer. model이 "ranger"나 "xgboost"일 경우에만 사용하는 인수.
#'  모델에서 생성할 개별 트리의 개수를 지정함. 기본값은 "ranger"는 500개, "xgboost"는 500개임.
#' @param best_metric character. Best 모델을 선정할 때 사용하는 성능지표. 
#' c("f_meas", "recall", "sensitivity", "precision", "specificity", "accuracy", 
#' "bal_accuracy", "detection_prevalence", "f_meas_05", "f_meas_2", "j_index", 
#' "kap", "mcc", "npv", "ppv", "gmean") 중에서 선택함. 기본값은 "f_meas". 
#' @param parallel logical. 병렬 프로세싱으로 수행할지의 여부, 기본값은 FALSE.
#' @param cores integer. 병렬 프로세싱에서 사용할 코어의 개수.
#' @param future_globals_maxsize numeric. 병렬 프로세싱에서 개별 코어에서 
#' 사용할 글로벌 메모리의 최대값.
#' @param verbose logical. 작업 경과의 정보를 출력할지의 여부, 기본값은 TRUE. 
#' @return list. workflow 클래스 객체를 성분으로 갖는 list.
#' 개별 성분으로 반환하는 workflow 객체는 다음과 같은 attributes를 갖음.
#' \itemize{
#' \item coef_term tbl_df. 모델을 정의한 terms의 추정 계수값 등의 정보. 
#' \item pred tbl_df. 모델에 test 셋을 적용해서 예측한 예측값 정보. 
#' \item cmat conf_mat. 모델의 성능 측정을 위한 confusion matrix.
#' \item metrics tbl_df. 모델의 성능 측정 메트릭 정보.
#' }
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#'   
#' # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost" classifiers를 순차 모델링
#' my_models <- multi_classifiers(data_recipe)
#' 
#' # "lasso", "elastic", "ridge" classifiers를 순차 모델링
#' my_models <- multi_classifiers(data_recipe, 
#'                                classifiers = c("lasso", "elastic", "ridge"))
#' 
#' # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost"  classifiers를 병렬 모델링
#' my_models <- multi_classifiers(data_recipe, parallel = TRUE)
#' }
#' @export
#' @importFrom purrr map
#' @importFrom future plan multisession
#' @importFrom furrr future_map furrr_options
multi_classifiers <- function(x, classifiers = c("logistic", "lasso", "elastic",
                                                 "ridge", "ranger", "xgboost"), 
                              n_fold = 10, n_grid = 30, n_best = 10, 
                              n_trees = ifelse(classifiers %in% "ranger", 500L, 500L),
                              best_metric = c("f_meas", "recall", "sensitivity", 
                                              "precision", "specificity", "accuracy", 
                                              "bal_accuracy", "detection_prevalence", 
                                              "f_meas_05", "f_meas_2", "j_index", 
                                              "kap", "mcc", "npv", "ppv", "gmean"),
                              parallel = FALSE, cores = 6,
                              future_globals_maxsize = 500 * 1024^2,
                              verbose = TRUE) {
  classifiers <- match.arg(classifiers, several.ok = TRUE)
  best_metric <- match.arg(best_metric)  
  
  ## 병렬처리 파라미터 설정
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    
    options(future.globals.maxSize = future_globals_maxsize)
    options(future.rng.onMisuse = "ignore")    
  }
  
  if (parallel) {
    models <- classifiers %>% 
      furrr::future_map(
        function(classifier) {
          n_tree <- n_trees[which(classifiers %in% classifier)]
          
          get_training(x, classifier, n_fold, n_grid, n_best, n_tree, 
                       best_metric, verbose = FALSE)
        },
        .options = furrr::furrr_options(globals = c("x", "get_training", "get_classifier"))
      )    
  } else {
    models <- classifiers %>% 
      purrr::map(
        function(classifier) {
          n_tree <- n_trees[which(classifiers %in% classifier)]
          
          get_training(x, classifier, n_fold, n_grid, n_best, n_tree, 
                       best_metric, verbose)
        }
      )    
  }  

  attr(models, "classifiers") <- classifiers
  
  models
}


#' Compare Model Performance with Statistics
#'
#' @description
#' multi_classifiers()로 생성한 여러 모델링의 성능 평가 및 비교를 위한 통계를 계산함
#'
#' @param x list. multi_classifiers() 결과로 생성한 모델들의 list 객체.
#' @param stats character. 계산/비교할 성능 평가 정보. 
#' c("coef", "cmat", "metrics", "quantiles")에서 선택.
#' @param nbins integer. 예측 확률을 비닝할 때 사용할 빈(Bins)의 개수. 
#' stats의 값이 "quantiles"일 경우만 적용되며 기본값은 10.
#' @param merge logical. 모델별 개별 결과를 하나의 tbl_df 객체로 반환할지의 여부. 
#' 기본값은 FALSE로 개별 결과를 list의 성분으로 반환함. TRUE이면 하나의 tbl_df 객체로 반환
#' @return tbl_df, conf_mat 클래스의 객체. stats의 종류에 따라 반환하는 객체의 유형이 다름.
#' @details 
#' stats의 값이 "coef"인 경우에는 모델의 종류에 따라서, 
#' terms 계수나 variable importance scores를 tbl_df 객체로 반환함.
#' merge이 TRUE인 경우에는, classifier 변수가 추가됨 
#' \itemize{
#' \item classifier character. 모델의 종류.
#' \item term factor. 모델을 정의한 terms. 
#' \item estimate numeric. 모델을 정의한 terms의 추정 계수. 
#' \item penalty numeric. 모델에 사용한 penalty. 
#' \item `estimate > 0` logical. 추정 계수가 0보다 큰가의 여부.
#' \item Importance numeric. importance scores. 
#' \item Importance_scal numeric. 최대값 기준으로 스케일된 importance scores. 
#' }
#' 
#' stats의 값이 "cmat"일 경우에는 Confusion Matrix를 conf_mat 클래스 객체로 반환함.
#' merge이 TRUE인 경우에는 다음의 tbl_df 객체를 반환함.
#' \itemize{
#' \item classifier character. 모델의 종류.
#' \item Prediction factor. 예측 클래스. 
#' \item Truth factor. 실제 클레스. 
#' \item Freq numeric. 예측 및 실제 클래스별 돗수. 
#' } 
#' 
#' stats의 값이 "metrics"일 경우에는 성능 평가 지표를 tbl_df 클래스 객체로 반환함.
#' merge이 TRUE인 경우에는, classifier 변수가 추가됨 
#' \itemize{
#' \item classifier character. 모델의 종류.
#' \item .metric character. 성능 평가지표 이름. 
#' \item .estimator character. taget 변수의 데이터 유형. 
#' \item .estimate numeric. 성능 평가지표 값. 
#' }
#' 
#' stats의 값이 "quantiles"일 경우에는 예측 확률의 구간별 분포 정보를 tbl_df 클래스 객체로 반환함.
#' merge이 TRUE인 경우에는, classifier 변수가 추가됨 
#' \itemize{
#' \item classifier character. 모델의 종류.
#' \item binn character. 예측 확률을 비닝한 구간. 
#' \item n integer. 데이터 건수. 
#' \item positive integer. 구간에서의 positive 데이터 건수. 
#' \item negative integer. 구간에서의 negative 데이터 건수. 
#' \item prob_positive numeric. 구간에서의 positive 비율. 
#' \item lift numeric. 구간에서의 positive 비율에 대한 lift. 
#' }
#' 
#' @seealso \code{\link{compare_viz_model}}
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#'   
#' # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost" classifiers를 순차 모델링
#' my_models <- multi_classifiers(data_recipe)
#' 
#' compare_info_model(my_models, "coef")
#' compare_info_model(my_models, "cmat")
#' compare_info_model(my_models, "metrics")
#' compare_info_model(my_models, "quantiles")
#' 
#' compare_info_model(my_models, "coef", merge = TRUE)
#' compare_info_model(my_models, "cmat", merge = TRUE)
#' compare_info_model(my_models, "metrics", merge = TRUE)
#' compare_info_model(my_models, "quantiles", merge = TRUE)
#' 
#' # 성능지표별로 메트릭 크기순으로 정렬
#' compare_info_model(my_models, "metrics", merge = TRUE) %>% 
#'   arrange(.metric, desc(.estimate)) %>% 
#'   print(n = Inf)
#' }
#' @export
#' @import dplyr
#' @importFrom purrr map map_df
compare_info_model <- function(x, stats = c("coef", "cmat", "metrics", "quantiles"), 
                               nbins = 10, merge = FALSE) {
  stats <- match.arg(stats)
  
  classifiers <- attr(x, "classifiers")
  
  if (!merge) {
    result <- x %>% 
      purrr::map(
        function(x) {
          info_model(x, stats)
        }
      )

    names(result) <- classifiers    
  } else {
    result <- x %>% 
      length() %>% 
      seq() %>% 
      purrr::map_df(
        function(i) {
          if (stats %in% "cmat") {
            tibble::tibble(classifier = attr(x, "classifiers")[i]) %>% 
              bind_cols(
                info_model(x[[i]], stats) %>% 
                  "$"("table") %>% 
                  as.data.frame()
              )            
          } else {
            tibble::tibble(classifier = attr(x, "classifiers")[i]) %>% 
              bind_cols(
                info_model(x[[i]], stats)              
              )            
          }
        }
      )
  }
  
  result
}




#' Compare Model Performance with Visualization
#'
#' @description
#' multi_classifiers()로 생성한 여러 모델링의 성능 평가 및 비교를 위한 플롯을 시각화함
#'
#' @param x list. multi_classifiers() 결과로 생성한 모델들의 list 객체.
#' @param stats character. 시각화할 플롯의 종류. 
#' c("coef", "heatmap", "mosaic", "roc", "density", "bin")에서 선택.
#' @param nbins integer. 예측 확률을 비닝할 때 사용할 빈(Bins)의 개수. 
#' stats의 값이 "quantiles"일 경우만 적용되며 기본값은 10.
#' @param return logical. 시각화 결과를 반환할지의 여부. 
#' 기본값은 FALSE로 개별 결과를 list의 성분으로 반환하지 않음. 
#' TRUE이면 시각화 결과를 list로 반환.
#' @return list. gg 클래스를 성분으로 갖는 리스트 객체.
#' @details 
#' stats의 값에 따라 다음과 같은 플롯을 시각화함.
#' \itemize{
#' \item "coef": terms별 계수와의 관계 시각화.
#' \item "heatmap": Confusion Matrix 결과를 heatmap 플롯으로 시각화.
#' \item "heatmap": Confusion Matrix 결과를 mosaic 플롯으로 시각화.
#' \item "roc": ROC 커브 시각화.
#' \item "density": 예측확률의 target 클래스별 밀도 시각화.
#' \item "bin": 예측 확률의 구간별 target 클래스별 분포 시각화.
#' }
#' @seealso \code{\link{compare_info_model}}
#' @examples
#' \donttest{
#' # 모델 개발을 위한 recipe 정의
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy() %>% 
#'   step_my_rose()
#'   
#' # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost" classifiers를 순차 모델링
#' my_models <- multi_classifiers(data_recipe)
#' 
#' # terms별 계수와의 관계 시각화
#' compare_viz_model(my_models, "coef")
#' 
#' # Confusion Matrix 결과를 heatmap 플롯으로 시각화
#' compare_viz_model(my_models, "heatmap")
#' 
#' # Confusion Matrix 결과를 mosaic 플롯으로 시각화
#' compare_viz_model(my_models, "mosaic")
#' 
#' # ROC 커브 시각화
#' compare_viz_model(my_models, "roc")
#'
#' # 예측확률의 target 클래스별 밀도 시각화
#' compare_viz_model(my_models, "density")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화
#' compare_viz_model(my_models, "bin")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화 (15개 bins)
#' compare_viz_model(my_models, "bin", nbins = 15)
#' 
#' # terms별 계수와의 관계 시각화 결과 반환
#' compare_viz_model(my_models, "coef", return = TRUE)
#' }
#' @export
#' @import dplyr
#' @importFrom purrr map
compare_viz_model <- function(x, stats = c("coef", "heatmap", "mosaic", "roc", 
                                           "density", "bin"), 
                              nbins = 10, return = FALSE) {
  stats <- match.arg(stats)
  
  result <- x %>% 
    purrr::map(
      function(x) {
        print(viz_model(x, stats))
      }
    )

  if (return) {
    return(result)
  }
}


