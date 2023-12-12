#' Fit Binary Classification with Multi Recipes
#'
#' @description
#' 여러 개의 recipes로 Binary Classification modeling을 수행함
#'
#' @param x list. 모델을 정의한 recipe 객체를 성분으로 갖는 list 객체.
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
#' @return list. workflow 클래스 객체를 성분으로 갖는 list를 성분으로 갖는 list 객체.
#' @examples
#' \donttest{
#' # 모델 개발을 위한 첫째 recipe 정의
#' death_event_recipe <- dlookr::heartfailure %>%
#'   target_to(death_event) %>%
#'   set_positive("Yes") %>%
#'   split_dataset(prop = 0.7) %>%
#'   extract_dataset() %>%
#'   set_formula() %>%
#'   step_my_center() %>%
#'   step_my_dummy() %>%
#'   step_my_rose()
#'   
#' # 모델 개발을 위한 둘째 recipe 정의
#' diabetes_recipe <- dlookr::heartfailure %>%
#'   target_to(diabetes) %>%
#'   set_positive("Yes") %>%
#'   split_dataset(prop = 0.7) %>%
#'   extract_dataset() %>%
#'   set_formula() %>%
#'   step_my_center() %>%
#'   step_my_dummy() %>%
#'   step_my_rose()
#' 
#' # recipe를 성분으로 갖는 list 정의
#' recipes <- list(death_event_recipe, diabetes_recipe)
#' 
#' my_recipes <- multi_recipes(recipes)
#' 
#' # 첫번 째 recipe 안에서 classifiers별 성능 비교
#' compare_info_model(my_recipes[[1]], "coef")
#' compare_info_model(my_recipes[[1]], "cmat")
#' compare_info_model(my_recipes[[1]], "metrics")
#' compare_info_model(my_recipes[[1]], "quantiles")
#' 
#' compare_info_model(my_recipes[[1]], "coef", merge = TRUE)
#' compare_info_model(my_recipes[[1]], "cmat", merge = TRUE)
#' compare_info_model(my_recipes[[1]], "metrics", merge = TRUE)
#' compare_info_model(my_recipes[[1]], "quantiles", merge = TRUE)
#' 
#' # 성능지표별로 메트릭 크기순으로 정렬
#' compare_info_model(my_recipes[[1]], "metrics", merge = TRUE) %>% 
#'   arrange(.metric, desc(.estimate)) %>% 
#'   print(n = Inf)
#'   
#' # terms별 계수와의 관계 시각화
#' compare_viz_model(my_recipes[[1]], "coef")
#' 
#' # Confusion Matrix 결과를 heatmap 플롯으로 시각화
#' compare_viz_model(my_recipes[[1]], "heatmap")
#' 
#' # Confusion Matrix 결과를 mosaic 플롯으로 시각화
#' compare_viz_model(my_recipes[[1]], "mosaic")
#' 
#' # ROC 커브 시각화
#' compare_viz_model(my_recipes[[1]], "roc")
#'
#' # 예측확률의 target 클래스별 밀도 시각화
#' compare_viz_model(my_recipes[[1]], "density")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화
#' compare_viz_model(my_recipes[[1]], "bin")
#'
#' # 예측 확률의 구간별 target 클래스별 분포 시각화 (15개 bins)
#' compare_viz_model(my_recipes[[1]], "bin", nbins = 15)
#' 
#' # terms별 계수와의 관계 시각화 결과 반환
#' compare_viz_model(my_recipes[[1]], "coef", return = TRUE)
#' }
#' @export
#' @importFrom purrr map map_chr
#' @importFrom future plan multisession
#' @importFrom furrr future_map furrr_options
#' @importFrom glue glue
#' @importFrom cli cli_div cli_rule cli_end
multi_recipes <- function(x, classifiers = c("logistic", "lasso", "elastic", 
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
  
  target_variables <- x %>% 
    purrr::map_chr(
      function(i) {
        i$template %>% 
          attr("target")
      }
    )

  positives <- x %>% 
    purrr::map_chr(
      function(i) {
        i$template %>% 
          attr("positive")
      }
    )
  
  formulas <- x %>% 
    purrr::map_chr(
      function(i) {
        predictor <- i$term_info %>% 
          filter(role %in% "predictor") %>% 
          select(variable) %>% 
          pull()
        
        target <- i$template %>% 
          attr("target")
        
        variable <- i$template %>% 
          attr("split") %>% 
          "$"("data") %>% 
          names()
        
        n_diff <- setdiff(variable, predictor) %>% 
          length
        
        if (n_diff == 1) {
          formula <- glue::glue("{target} ~ .")
        } else {
          rhs <- paste(predictor, collapse = " + ")
          
          formula <- glue::glue("{target} ~ {rhs}")
        }
        
        formula
      }
    )  
  
  
  if (parallel) {
    models <- x %>% 
      length() %>% 
      seq() %>% 
      furrr::future_map(
        function(i) {
          if (verbose) {
            d <- cli::cli_div(theme = list(rule = list(
              color = "cyan",
              "line-type" = "double")))
            cli::cli_rule("Start recipe modeling [{i}/{length(formulas)}]", right = "{formulas[i]}")
            cli::cli_end(d)            
          }
          
          multi_classifiers(x[[i]], classifiers, n_fold, n_grid, n_best, n_trees, 
                            best_metric, verbose = FALSE)
        },
        .options = furrr::furrr_options(
          globals = c("x", "get_training", "get_classifier", "multi_classifiers"))
      )    
  } else {
    models <- x %>% 
      length() %>% 
      seq() %>%       
      purrr::map(
        function(i) {
          if (verbose) {
            d <- cli::cli_div(theme = list(rule = list(
              color = "cyan",
              "line-type" = "double")))
            cli::cli_rule("Start formula modeling [{i}/{length(formulas)}]", right = "{formulas[i]}")
            cli::cli_end(d)            
          }
          
          multi_classifiers(x[[i]], classifiers, n_fold, n_grid, n_best, n_trees, 
                            best_metric, verbose = verbose)
        }
      )    
  }  
  
  attr(models, "formulas") <- formulas
  attr(models, "classifiers") <- classifiers
  attr(models, "target") <- target_variables
  attr(models, "positive") <- positives
  
  models
}


