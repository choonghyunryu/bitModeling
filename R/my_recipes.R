#' 모델 포뮬러 설정
#'
#' @description
#' Target 변수를 예측하기위한 formula를 지정함
#'
#' @param .data a \code{\link{grouped_df}}.
#' @param formula 모델을 정의하는 formula. 기본값은 `target 변수 ~ .`
#' @return recipe 클래스 객체.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # training 데이터셋을 70%로 데이터 분리 설정
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula()
#'   
#' data_recipe  
#' }
#' 
#' @export
#' @import recipes
#' @importFrom glue glue
set_formula <- function(.data, formula = NULL) {
  target <- .data %>% 
    attr("target")
  
  if (is.null(formula)) {
    formula <- glue::glue("{target} ~ .") %>% 
      as.formula()
  } 
  
  recipe(formula, data = .data)
}


#' 수치 데이터의 정규화
#'
#' @description
#' 평균이 0이 되도록 수치 변수를 정규화하는 레시피 스펙 정의
#'
#' @param .data recipe 클래스 객체. 이 레시피에 해당작업 시퀀스가 추가됨.
#' @param role 새 변수가 생성되지 않으므로, 이 단계에서는 사용하지 않음.
#' @param trained logical. 전처리로 수치를 추정했는지의 여부.
#' @param means numeric. 평균. 이 값은 prep()에 의해서 계산될 때까지 NULL임. 
#' @param na_rm logical. 계산할 떄, NA를 제거해야 하는지의 여부.
#' @param skip logical. 레시피가 bake()에 의해 전처리가 수행될 때 단계를 건너뛰어야 합니까? 
#' prep()가 실행될 때, 모든 전처리 작업이 수행되는 동안 일부 작업은 새 데이터에서 수행되지 않을 수 있습니다(예: 결과 변수 처리). 
#' skip = TRUE은 후속 작업의 계산에 영향을 미칠 수 있으므로 사용 시 주의해야 함.
#' @param id character. 식별하기 위한 고유값.
#' @return recipe 클래스 객체.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # training 데이터셋을 70%로 데이터 분리 설정
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() 
#'   
#' data_recipe 
#' 
#' data_recipe %>% 
#'   prep()
#'     
#' data_recipe %>% 
#'   prep() %>% 
#'   juice() 
#' }
#' 
#' @export
#' @import dplyr
#' @import recipes
#' @importFrom glue glue
#' @importFrom tidyselect matches
step_my_center <- function(.recipe, ..., role = NA, trained = FALSE, means = NULL,
                           na_rm = TRUE, skip = FALSE, id = rand_id("center")) {
  target <- .recipe$template %>% 
    attr("target")
  
  terms <- enquos(...)
  
  if (length(terms) > 0) {
    result <- .recipe %>% 
      add_step(recipes:::step_center_new(terms = terms, 
                               trained = trained, role = role, means = means,
                               na_rm = na_rm, skip = skip, id = id, 
                               case_weights = NULL)) 
  } else {
    result <- .recipe %>% 
      step_center(all_predictors(), -all_nominal(), -matches(glue::glue("^{target}$")), 
                  trained = trained, role = role, means = means, na_rm = na_rm, 
                  skip = skip, id = id)     
  }
  
  result
}


#' 범주형 데이터의 더미화
#'
#' @description
#' 명목척도의 범주형 변수를 하나 이상의 이진항으로 변환하는 레시피 스펙 정의
#'
#' @param .data recipe 클래스 객체. 이 레시피에 해당작업 시퀀스가 추가됨.
#' @param role 기본적으로 이 단계로 생성된 새 변수는 예측변수로 사용됨.
#' @param trained logical. 전처리로 수치를 추정했는지의 여부.
#' @param one_hot logical. 원-핫 인코딩 여부. 
#' @param naming function. 새 더미 변수에 대한 명명규칙을 정의하는 함수.
#' @param levels list. 각 변수에 대해서 더미변수를 생성하는데 필요한 정보가 포함된 list.
#' @param keep_original_cols logical. 출력 결과에 원래의 변수를 유지하는지의 여부. 기본값은 FALSE.
#' @param skip logical. 레시피가 bake()에 의해 전처리가 수행될 때 단계를 건너뛰어야 합니까? 
#' prep()가 실행될 때, 모든 전처리 작업이 수행되는 동안 일부 작업은 새 데이터에서 수행되지 않을 수 있습니다(예: 결과 변수 처리). 
#' skip = TRUE은 후속 작업의 계산에 영향을 미칠 수 있으므로 사용 시 주의해야 함.
#' @param id character. 식별하기 위한 고유값.
#' @return recipe 클래스 객체.
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # training 데이터셋을 70%로 데이터 분리 설정
#' data_recipe <- dlookr::heartfailure %>% 
#'   target_to(death_event) %>% 
#'   set_positive("Yes") %>% 
#'   split_dataset(prop = 0.7) %>% 
#'   extract_dataset() %>% 
#'   set_formula() %>% 
#'   step_my_center() %>% 
#'   step_my_dummy()
#'   
#' data_recipe 
#' 
#' data_recipe %>% 
#'   prep()
#'     
#' data_recipe %>% 
#'   prep() %>% 
#'   juice() 
#' }
#' 
#' @export
#' @import dplyr
#' @import recipes
#' @importFrom glue glue
#' @importFrom tidyselect matches
step_my_dummy <- function(.recipe, ..., role = "predictor", trained = FALSE,
                          one_hot = FALSE, naming = dummy_names, levels = NULL, 
                          keep_original_cols = FALSE, skip = FALSE, 
                          id = rand_id("dummy")) {
  target <- .recipe$template %>% 
    attr("target")
  
  terms <- enquos(...)  

  if (length(terms) > 0) {
    result <- .recipe %>% 
      add_step(recipes:::step_dummy_new(terms = terms, role = role, trained = trained, 
                              one_hot = one_hot, naming = naming, levels = levels, 
                              keep_original_cols = keep_original_cols, 
                              skip = skip, id = id))
  } else {
    result <- .recipe %>% 
      step_dummy(all_nominal(), -matches(glue::glue("^{target}$")),
                 role = role, trained = trained, one_hot = one_hot, 
                 naming = naming, levels = levels,
                 keep_original_cols = keep_original_cols, skip = skip, id = id)   
  }
  
  result
}


#' ROSE 알고리즘 적용
#' 
#' @description minority와 majority 클래스를 확대하여, 
#' ROSE(Randomly Over Sampling Examples) 기반 합성 데이터 샘플을 생성하는 
#' 레시피 스펙을 정의함. ROSE::ROSE()를 사용함.
#' 
#' @param .recipe recipe 객체. 이 recipe 객체에 대한 ROSE 순차작업을 추가함.
#' @param role. 새 변수가 생성되지 않으므로, 이 단계에서는 사용하지 않음.
#' @param trained logical. quantities에 대해서 전처리 추정 여부를 나타냄.
#' @param column character. ... 선택자에 의해서 채워질 변수의 이름. 
#' @param over_ratio numeric. majority-to-minority 빈도의 비율에 대한 숫자 값.
#' 기본값(1)은 다른 모든 수준이 가장 많이 발생하는 수준과 동일한 빈도를 갖도록 샘플링됨을 의미. 
#' 0.5 값은 minority 수준이 majority 수준보다 (최대) (대략) 절반의 행을 갖는다는 것을 의미.
#' @param minority_prop numeric. minority 클래스의 오버 샘플링 여부를 결정. 기본값은 0.5.
#' @param minority_smoothness numeric. minority 클래스의 조건부 커널 밀도를 
#' 추정하기 위해 스무딩 매개변수를 곱할 축소 계수. 기본값은 1.
#' @param majority_smoothness numeric. majority 클래스의 조건부 커널 밀도를 
#' 추정하기 위해 스무딩 매개변수를 곱할 축소 계수. 기본값은 1.
#' @param skip logical. 레시피가 bake()에 의해 전처리가 수행될 때 단계를 건너뛰어야 합니까? 
#' prep()가 실행될 때, 모든 전처리 작업이 수행되는 동안 일부 작업은 새 데이터에서 수행되지 않을 수 있습니다(예: 결과 변수 처리). 
#' skip = TRUE은 후속 작업의 계산에 영향을 미칠 수 있으므로 사용 시 주의해야 함.
#' @param seed integer. 시드 값. 
#' @param id character. 식별하기 위한 고유값.
#' 
#' @details themis::step_rose() 도움말 참고.
#' @return recipe 객체.
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @examples
#' # Generate data for the example
#' \donttest{
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
#' data_recipe %>% 
#'   recipes::prep() %>% 
#'   recipes::juice()
#' }
#' 
#' @export
#' @import dplyr
#' @import recipes
#' @importFrom glue glue
#' @importFrom tidyselect matches
#' @importFrom themis step_rose
step_my_rose <- function(.recipe, ..., role = NA, trained = FALSE, column = NULL, 
                         over_ratio = 1, minority_prop = 0.5, minority_smoothness = 1, 
                         majority_smoothness = 1, skip = TRUE, 
                         seed = sample.int(10^5, 1), id = rand_id("rose")) {
  target <- .recipe$template %>% 
    attr("target")
  
  terms <- enquos(...)  
  
  if (length(terms) > 0) {
    result <- .recipe %>% 
      add_step(themis:::step_rose_new(terms = terms, role = role, trained = trained,
                                      column = column, over_ratio = over_ratio, 
                                      minority_prop = minority_prop, 
                                      minority_smoothness = minority_smoothness, 
                                      majority_smoothness = majority_smoothness, 
                                      predictors = NULL, skip = skip, seed = seed, 
                                      id = id))
  } else {
    result <- .recipe %>% 
      themis::step_rose(matches(glue::glue("^{target}$")),
                        role = role, trained = trained, column = column, 
                        over_ratio = over_ratio, minority_prop = minority_prop, 
                        minority_smoothness = minority_smoothness, 
                        majority_smoothness = majority_smoothness, 
                        skip = skip, seed = seed, id = id)   
  }
  
  result
}

