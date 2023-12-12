#' @rdname target_to.data.frame
#' @export
target_to <- function(.data, target, ...) {
  UseMethod("target_to", .data)
}


#' Target 변수 지정
#'
#' @description
#' Machine learning 과정에서 Target 변수를 지정함
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param target target 변수 이름.
#' @param ... arguments to be passed to methods.
#' @return model_df 클래스 객체.
#' model_df 클래스 객체의 속성은 다음과 같음.
#' \itemize{
#' \item target character. Target 변수 이름.
#' }
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' data_targeted <- dlookr::heartfailure %>% 
#'   target_to(death_event)
#'   
#' data_targeted   
#' }
#' 
#' @method target_to data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang enquo
#' @export
target_to.data.frame <- function(.data, target, ...) {
  tryCatch(vars <- tidyselect::vars_select(names(.data), !! rlang::enquo(target)),
           error = function(e) {
             pram <- as.character(substitute(target))
             stop(sprintf("Column %s is unknown", pram))
           }, finally = NULL)
  
  target_to_impl(.data, vars)
}

#' @import dplyr
#' @importFrom methods is
target_to_impl <- function(.data, target) {
  if (!target %in% names(.data)) {
    stop(sprintf("%s in not variable in %s", target,
                 as.character(substitute(.data))))
  }
  
  is_factor <- target %in% dlookr::find_class(.data, "categorical", index = FALSE)
  
  if (!is_factor) {
    .data <- .data %>% 
      mutate(target_variable = target) %>% 
      mutate({{target}} := factor(!! rlang::sym(target_variable))) %>% 
      select(target_variable) 
  }
  
  target_to <- grouped_df(.data, target)
  
  attr(target_to , "target") <- target %>% as.character()
  
  
  class(target_to) <- append("model_df", class(target_to))
  
  target_to
}



#' Positive 클래스 지정
#'
#' @description
#' Binary 클래스를 갖는 Target 변수에 Positive 클래스를 지정함
#'
#' @param .data a model_df.
#' @param positive character. positive 클래스.
#' @return model_df 클래스 객체.
#' model_df 클래스 객체의 속성은 다음과 같음.
#' \itemize{
#' \item target character. Target 변수 이름.
#' \item positive character. positive 클래스 이름.
#' }
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' data_targeted <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes")
#'   
#' data_targeted   
#' }
#' 
#' @export
set_positive <- function(.data, positive) {
  attr(.data , "positive") <- positive
  
  .data
}



#' 데이터셋 분리를 위한 설정
#'
#' @description
#' 데이터셋을 모델 개발을 위한 training 셋과 모델 평가를 위한 test 셋으로 분리하는 설정
#'
#' @param .data a model_df.
#' @param prop numeric. training 데이터셋의 비율.
#' @param breaks numeric. 수치형 계층화 변수를 계층화하는데 필요한 bins의 개수를 지정하는 단일 숫자.
#' @param pool numeric. 특정 그룹이 너무 작아서 다른 그룹으로 모아야 하는지 판단하는데
#' 사용되는, 데이터의 비율. 너무 작은 그룹을 계층화할 위험이 있으므로 이 인수를 
#' 기본값인 0.1 미만으로 낮추는 것을 권장하지 않음.
#' @param seed integer. 데이터셋을 분리할 때의 샘플링 난수 시드.
#' @return rsample 패키지에서 사용하는 initial_split 클래스 객체.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # training 데이터셋을 70%로 데이터 분리 설정
#' data_targeted <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7)
#'   
#' data_targeted   
#' }
#' @export
#' @importFrom rsample initial_split
split_dataset <- function(.data, prop = 3/4, breaks = 4, pool = 0.1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  } 
  
  rsample::initial_split(.data, prop = prop, strata = attr(.data, "target"), 
                         breaks = breaks, pool = pool)
}


#' 데이터셋 추출
#'
#' @description
#' initial_split 클래스 객체로부터 모델 개발을 위한 training 셋이나 모델 평가를 위한 test 셋을 추출
#'
#' @param .data a \code{\link{initial_split}}.
#' @param set character. 데이터셋의 종류. "train" 혹은 "test"로 설정. 
#' 기본값은 "train"으로 모델 개발을 위한 training 셋을 추출함.
#' @return grouped_df 클래스 객체.
#' grouped_df 클래스 객체의 기본 속성 외의, 속성은 다음과 같음.
#' \itemize{
#' \item target character. Target 변수 이름.
#' \item positive character. positive 클래스 이름.
#' }
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # training 데이터셋을 70%로 데이터 분리 설정
#' data_targeted <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset()
#'   
#' data_targeted   
#' }
#' @export
#' @importFrom rsample training testing
extract_dataset <- function(.data, set = "train") {
  if (set == "train") {
    tab <- rsample::training(.data)
  } else {
    tab <- rsample::testing(.data)
  }
  
  attr(tab , "target") <- .data %>% 
    "$"(data) %>% 
    attr("target")
  
  attr(tab , "positive") <- .data %>% 
    "$"(data) %>% 
    attr("positive")  
  
  attr(tab , "split") <- .data 
  
  tab
}


#' target 변수 분포 출력
#' @description 데이터셋 중 target 변수의 Positive와 Negative의 분포 출력
#' @param .data model_df, initial_split 혹은 recipe. 분포를 출력할 대상 객체. 
#' @return tbl_df.
#' \itemize{
#'   \item Target 변수
#'   \item 건수
#'   \item 백분율
#' }
#' @details tidymodels 패키지군 기반으로 모델링을 하는 단계에서, 
#' 원 데이터와 전처리 후의 데이터의 Target 변수에서의 Positive와 Negative의 분포 출력
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @examples
#' \dontrun{
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) 
#' 
#' # model_df 클래스에 대한 원 데이터의 분포
#' target_table(data_recipe)
#'
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7)
#' 
#' # initial_split 클래스에 대한, 분할하였을 때 Traning 셋의 데이터의 분포
#' target_table(data_recipe)
#' 
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset()
#' 
#' # initial_split 클래스에, 대한 분할 후의 Traning 셋의 데이터의 분포
#' target_table(data_recipe)
#' 
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_rose()
#' 
#' # recipe 클래스에 대한 전처리 후의 데이터의 분포
#' target_table(data_recipe)
#' }
#' 
#' @export
#' @import recipes
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom cli cli_alert_info
target_table <- function(.data) {
  if (is(.data)[1] == "recipe") {
    cli::cli_alert_info("Training 데이터의 Receipe Baked Target 변수 분포")
    
    target <- .data$template %>% 
      attr("target")

    tab <- .data %>% 
      prep() %>% 
      juice() %>% 
      count(!!rlang::sym(target)) %>% 
      ungroup() %>% 
      mutate(pct = round(n /sum(n) * 100, 2)) %>% 
      rename("건수" = n,
             "백분율" = pct)      
  } else if (is(.data)[1] == "model_df") {
    cli::cli_alert_info("원 데이터의 Target 변수 분포")
    
    target <- .data %>% 
      attr("target")
    
    tab <- .data %>% 
      count(!!rlang::sym(target)) %>% 
      ungroup() %>% 
      mutate(pct = round(n /sum(n) * 100, 2)) %>% 
      rename("건수" = n,
             "백분율" = pct)    
  } else if (is(.data)[1] %in% c("initial_split")) {
    cli::cli_alert_info("Training 데이터로 추출할 데이터의 Target 변수 분포")
    
    target <- .data$data %>% 
      attr("target")
    
    tab <- .data %>% 
      training() %>% 
      count(!!rlang::sym(target)) %>% 
      ungroup() %>% 
      mutate(pct = round(n /sum(n) * 100, 2)) %>% 
      rename("건수" = n,
             "백분율" = pct)      
  } else if (is(.data)[1] %in% c("grouped_df")) {
    cli::cli_alert_info("Training 데이터의 Target 변수 분포")
    
    target <- .data %>% 
      attr("target")
    
    if (is.null(target)) {
      stop("target_table()는 model_df 객체, initial_split(혹은 grouped_df) 객체, recipe 객체만 지원합니다.")
    }
    
    tab <- .data %>% 
      count(!!rlang::sym(target)) %>% 
      ungroup() %>% 
      mutate(pct = round(n /sum(n) * 100, 2)) %>% 
      rename("건수" = n,
             "백분율" = pct)       
  } else {
    stop("target_table()는 model_df 객체, initial_split(혹은 grouped_df) 객체, recipe 객체만 지원합니다.")
  }

  tab  
}



