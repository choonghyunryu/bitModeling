#' 적합된 모델 성능 보고서 생성
#' @description 적합한 모델의 classifiers별로 성능 평가 결과를 Excel 파일로 생성함
#' @param x workflow, 혹은 list. 성능 보고서를 생성할 모델 적합 객체. 
#' 단일 classifier일 경우에는 workflow 객체이고, 다중 classifiers일 경우에는 list 객체임.
#' @param output_file character. 생성할 Excel 파일의 이름.
#' @param output_dir character. 생성할 Excel 파일이 저장될 디렉토리.
#' @details 적합된 모델들의 성능을 비교하기 위한 여러 통계 정보화 플롯을 Excel 파일에 출력함.
#' classifiers별로 17개의 성능 평가지표를 비교할 수 있는 총괄 워크시트와 
#' 개별 classifiers의 상세 정보를 워크시트로 갖는 파일을 생성함.
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @seealso \code{\link{info_model}}, \code{\link{viz_model}}, \code{\link{compare_info_model}}, \code{\link{compare_viz_model}}
#' @examples
#' \dontrun{
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
#' # logistic regrssion 적합     
#' my_model <- get_training(data_recipe)
#' 
#' # logistic regrssion 성능평가 보고서 출력
#' report_model(my_model)
#'    
#' # 복수개의 classifiers별로 적합
#' # "logistic", "lasso", "elastic", "ridge", "ranger", "xgboost" classifiers를 순차 모델링
#' my_models <- multi_classifiers(data_recipe)
#' 
#' # classifiers별로 성능을 비교할 수 있는 성능평가 보고서 출력
#' report_model(my_models, output_file = "compare_model_performance.xlsx")
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import openxlsx
#' @importFrom here here
#' @importFrom purrr walk
#'  
report_model <- function(x, output_file = NULL, output_dir = getwd()) {
  options(warn=-1)
  on.exit(options(warn=0))
  
  path <- output_dir
  ## 이미지 파일 생성을 위한 임시 디렉토리 생성
  dir.create(here::here("temp"))   
  
  if (is.null(output_file))
    output_file <- "model_performance.xlsx"
  output_file <- paste(path, output_file, sep = "/")
  
  if (is(x) %in% "workflow") {
    multi <- FALSE
  } else {
    multi <- TRUE
  }

  ## Export to excel
  ## Create a new work book 
  wb <- openxlsx::createWorkbook()
  
  if (multi) {
    tab_metrics <- compare_info_model(x, "metrics", merge = TRUE) %>% 
      select(-.estimator) %>% 
      tidyr::pivot_wider(names_from = ".metric", values_from = ".estimate")
    
    classifiers <- attr(x, "classifiers")    
    
    ## Create a new worksheet for print table
    sname <- "Overall_classifiers"
    openxlsx::addWorksheet(wb, sheetName = sname, tabColour = "deepskyblue")
    
    ## Write data.frame to a new worksheet
    openxlsx::writeDataTable(wb, sheet = sname, tab_metrics)
    
    tab_metrics %>% 
      NROW() %>% 
      seq() %>% 
      purrr::walk(
        function(x) {
          writeFormula(wb, sheet = sname, startRow = x + 1, startCol = 1,
                       x = makeHyperlinkString(
                         sheet = tab_metrics$classifier[x], row = 1, col = 1, 
                         text = tab_metrics$classifier[x]
                       )
          )  
        }
      )
    
    setColWidths(wb, sheet = sname, cols = 1:NCOL(tab_metrics), widths = "auto")    
    
    ## classifiers 개별 시트 작성 루프  
    tab_metrics$classifier %>% 
      purrr::walk(
        function(nm_classifier) {
          idx <- classifiers %in% nm_classifier %>% 
            which()
          
          sheet_name <- nm_classifier
          openxlsx::addWorksheet(wb, sheetName = sheet_name)
          
          writeFormula(wb, sheet = sheet_name, startRow = 1, startCol = 7,
                       x = makeHyperlinkString(
                         sheet = "Overall_classifiers", row = idx + 1, col = 1, 
                         text = "Goto overall_classifiers"
                       )
          )  
          
          
          ## Confusion Matrix ----------------------------------------------------
          tab_cmat <- info_model(x[[idx]], "cmat") %>% 
            "$"("table") %>% 
            as.data.frame() %>% 
            tidyr::pivot_wider(names_from = 2, values_from = "Freq")
          
          title_str <- "<< Confusion Matrix >>"
          writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)   
          
          col_str <- "Truth"
          writeData(wb, sheet = sheet_name, col_str, startCol = 2, startRow = 3)           
          
          writeDataTable(wb, sheet = sheet_name, tab_cmat, 
                         startCol = 1, startRow = 4, rowNames = FALSE)
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, rows = 5:6, cols = 2:3, 
                   gridExpand = TRUE)
          
          
          ## Performance Metrics -------------------------------------------------
          tab_metrics <- info_model(x[[idx]], "metrics") %>% 
            select(-.estimator) %>% 
            rename("Metrics" = `.metric`,
                   "Estimate" = `.estimate`)
          
          title_str <- "<< Performance Metrics >>"
          writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 9)   
          
          writeDataTable(wb, sheet = sheet_name, tab_metrics, 
                         startCol = 1, startRow = 10, rowNames = FALSE)
          
          
          ## ROC curve 시각화 ----------------------------------------------------
          image_name <- here::here("temp", glue::glue("roc_{runif(1)}.png"))
          
          plot_roc <- viz_model(x[[idx]], "roc") 
          
          Cairo::CairoPNG(filename = image_name, width = 500, height = 500)
          plot(plot_roc)
          dev.off()
          
          title_str <- "<< ROC curve 시각화 >>"
          writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 9)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 4, height = 4,
                                startCol = 8, startRow = 10)
          
          
          ## Term Coefficients -------------------------------------------------
          tab_coefs <- info_model(x[[idx]], "coef")
          
          if (nm_classifier %in% c("ranger", "xgboost")) {
            title_str <- "<< Variable Importance >>"
          } else {
            title_str <- "<< Term Coefficients >>"
          }
          

          writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 31)   
          
          writeDataTable(wb, sheet = sheet_name, tab_coefs, 
                         startCol = 1, startRow = 32, rowNames = FALSE)
          
          
          
          ## Term Coefficients 시각화 --------------------------------------------
          image_name <- here::here("temp", glue::glue("coef_{runif(1)}.png"))
          
          plot_coef <- viz_model(x[[idx]], "coef") 
          
          Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
          plot(plot_coef)
          dev.off()
          
          if (nm_classifier %in% c("ranger", "xgboost")) {
            title_str <- "<< Variable Importance 시각화 >>"
          } else {
            title_str <- "<< Term Coefficients 시각화 >>"
          }
          
          writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 31)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 8, height = 5,
                                startCol = 8, startRow = 32)
          
          
          ## Bins by quantiles ---------------------------------------------------
          tab_quantiles <- info_model(x[[idx]], "quantiles")
          
          title_str <- "<< Bins by Quantiles >>"
          writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 58)   
          
          writeDataTable(wb, sheet = sheet_name, tab_quantiles, 
                         startCol = 1, startRow = 59, rowNames = FALSE)    
          
          
          ## Bins by quantiles 시각화 --------------------------------------------
          image_name <- here::here("temp", glue::glue("coef_{runif(1)}.png"))
          
          plot_bin <- viz_model(x[[idx]], "bin") 
          
          Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
          plot(plot_bin)
          dev.off()
          
          title_str <- "<< Bins by Quantiles 시각화 >>"
          writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 58)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 8, height = 5,
                                startCol = 8, startRow = 59)        
          
        }
      )    
  } else {
    sheet_name <- "Performance_Information"
    
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    
    ## Confusion Matrix --------------------------------------------------------
    tab_cmat <- info_model(x, "cmat") %>% 
      "$"("table") %>% 
      as.data.frame() %>% 
      tidyr::pivot_wider(names_from = 2, values_from = "Freq")
    
    title_str <- "<< Confusion Matrix >>"
    writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)   
    
    col_str <- "Truth"
    writeData(wb, sheet = sheet_name, col_str, startCol = 2, startRow = 3)           
    
    writeDataTable(wb, sheet = sheet_name, tab_cmat, 
                   startCol = 1, startRow = 4, rowNames = FALSE)
    
    s <- createStyle(numFmt = "#,##0")
    addStyle(wb, sheet = sheet_name, style = s, rows = 5:6, cols = 2:3, 
             gridExpand = TRUE)
    
    
    ## Performance Metrics -----------------------------------------------------
    tab_metrics <- info_model(x, "metrics") %>% 
      select(-.estimator) %>% 
      rename("Metrics" = `.metric`,
             "Estimate" = `.estimate`)
    
    title_str <- "<< Performance Metrics >>"
    writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 9)   
    
    writeDataTable(wb, sheet = sheet_name, tab_metrics, 
                   startCol = 1, startRow = 10, rowNames = FALSE)
    
    
    ## ROC curve 시각화 --------------------------------------------------------
    image_name <- here::here("temp", glue::glue("roc_{runif(1)}.png"))
    
    plot_roc <- viz_model(x, "roc") 
    
    Cairo::CairoPNG(filename = image_name, width = 500, height = 500)
    plot(plot_roc)
    dev.off()
    
    title_str <- "<< ROC curve 시각화 >>"
    writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 9)        
    openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                          width = 4, height = 4,
                          startCol = 8, startRow = 10)
    
    
    ## Term Coefficients -------------------------------------------------------
    tab_coefs <- info_model(x, "coef")
    
    if (x$fit$fit$spec$engine %in% c("ranger", "xgboost")) {
      title_str <- "<< Variable Importance >>"
    } else {
      title_str <- "<< Term Coefficients >>"
    }
    
    writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 31)   
    
    writeDataTable(wb, sheet = sheet_name, tab_coefs, 
                   startCol = 1, startRow = 32, rowNames = FALSE)
    
    
    
    ## Term Coefficients 시각화 ------------------------------------------------
    image_name <- here::here("temp", glue::glue("coef_{runif(1)}.png"))
    
    plot_coef <- viz_model(x, "coef") 
    
    Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
    plot(plot_coef)
    dev.off()
    
    if (x$fit$fit$spec$engine %in% c("ranger", "xgboost")) {
      title_str <- "<< Variable Importance 시각화 >>"
    } else {
      title_str <- "<< Term Coefficients 시각화 >>"
    }
    
    writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 31)        
    openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                          width = 8, height = 5,
                          startCol = 8, startRow = 32)
    
    
    ## Bins by quantiles -------------------------------------------------------
    tab_quantiles <- info_model(x, "quantiles")
    
    title_str <- "<< Bins by Quantiles >>"
    writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 58)   
    
    writeDataTable(wb, sheet = sheet_name, tab_quantiles, 
                   startCol = 1, startRow = 59, rowNames = FALSE)    
    
    
    ## Bins by quantiles 시각화 ------------------------------------------------
    image_name <- here::here("temp", glue::glue("coef_{runif(1)}.png"))
    
    plot_bin <- viz_model(x, "bin") 
    
    Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
    plot(plot_bin)
    dev.off()
    
    title_str <- "<< Bins by Quantiles 시각화 >>"
    writeData(wb, sheet = sheet_name, title_str, startCol = 8, startRow = 58)        
    openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                          width = 8, height = 5,
                          startCol = 8, startRow = 59)
  }
  
  ## Save excel file
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  ## Remove image file
  unlink(here::here("temp"), recursive = TRUE)  
}



#' 적합된 Multi Recipes 모델 성능 보고서 생성
#' @description 적합한 Multi Recipes 모델의 개별 recipes에 대해서 classifiers별로 
#' 성능 평가 결과를 Excel 파일로 생성함
#' @param x list. multi_recipes() 함수의 결과로, workflow 클래스 객체를 성분으로 
#' 갖는 list를 성분으로 갖는 list 객체.
#' @param output_file character. 생성할 Excel 파일의 이름. 기본값은 "model_performance.xlsx".
#' @param output_dir character. 생성할 Excel 파일이 저장될 디렉토리. 기본값은 현재 작업 디렉토리.
#' @param verbose logical. 작업 경과의 정보를 출력할지의 여부, 기본값은 TRUE. 
#' @details output_file의 기본값 "model_performance.xlsx"일 경우에 파일의 이름은
#' "model_performance_target변수이름_순번.xlsx"으로 생성됨
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @seealso \code{\link{report_model}}, \code{\link{multi_recipes}}
#' @examples
#' \dontrun{
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
#' # recipes별로 classifiers의 성능을 비교할 수 있는 성능평가 보고서 출력
#' report_model_recipes(my_recipes)
#' 
#' # 생성된 파일의 이름들
#' list.files(path = getwd(), pattern = "model_performance")
#' 
#' }
#' @export
#' @import dplyr
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr word str_replace str_extract
#'  
report_model_recipes <- function(x, output_file = NULL, output_dir = getwd(), 
                                 verbose = TRUE) {
  if (is.null(output_file)) {
    output_file <- "model_performance.xlsx"
  }

  formulas <- attr(x, "formulas")
  
  formulas %>% 
    length() %>% 
    seq() %>% 
    purrr::walk(
      function(i) {
        if (verbose) {
          d <- cli::cli_div(theme = list(rule = list(
            color = "cyan",
            "line-type" = "double")))
          cli::cli_rule("Creat file [{i}/{length(formulas)}]", right = "{formulas[i]}")
          cli::cli_end(d)            
        }
        
        post_fix <- formulas[i] %>% 
          stringr::word(1)
        
        pattern <- output_file %>% 
          stringr::str_replace("\\.xlsx", glue::glue("_{post_fix}"))
        
        suppressWarnings(
          nth <- list.files(path = output_dir, pattern = pattern) %>% 
            stringr::str_extract("[[:number:]]+") %>% 
            as.integer() %>% 
            max(na.rm = TRUE)
        )  

        if (is.infinite(nth)) {
          nth_str <- "01"
        } else {
          nth_str <- sprintf("%02d", nth + 1)
        }
          
        output_file <- output_file %>% 
          stringr::str_replace("\\.xlsx", glue::glue("_{post_fix}_{nth_str}\\.xlsx"))
        
        report_model(x[[i]], output_file = output_file, output_dir = output_dir) 
      }
    )
}

