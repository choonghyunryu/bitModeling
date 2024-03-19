#' 범주형 변수의 excel EDA 보고서 생성
#' @description 범주형 변수에 대한 EDA 수행 결과를 Excel 파일로 생성함
#' @param x data.frame, tbl_df. EDA를 수행할 데이터셋.
#' @param target_variable character. Target 변수의 이름.
#' @param positive character. Target 변수에서의 Positive 클래스의 이름
#' @param output_file character. 생성할 Excel 파일의 이름.
#' @param output_dir character. 생성할 Excel 파일이 저장될 디렉토리.
#' @param sample_percent numeric. 샘플로 분석할 경우의 샘플의 비율로서의 백분율.
#' @param parallel logical. 병렬 프로세싱으로 수행할지의 여부, 기본값은 FALSE.
#' @param cores integer. 병렬 프로세싱에서 사용할 코어의 개수.
#' @param future_globals_maxsize numeric. 병렬 프로세싱에서 개별 코어에서 
#' 사용할 글로벌 메모리의 최대값.
#' @param verbose logical. 작업 경과의 정보를 출력할지의 여부, 기본값은 TRUE. 
#' @details 범주형 변수의 총괄 정보를 제외한 모든 범주형 변수는 1개의 워크시트에 EDA 정보가 출력됨.
#' Excel은 256개의 워크시트만 지원하므로 범주형 변수 개수가 255개를 초과하는 것은 255개의 정보만 생성됨.
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @seealso \code{\link{eda_category}}
#' @examples
#' \dontrun{
#' testdata <- dlookr::heartfailure
#' target_variable <- "death_event"
#' 
#' # single core processing
#' eda_category(testdata, target_variable, positive = "Yes")
#' 
#' # 30% 샘플링한 데이터로 보고서를 생성함
#' eda_category(testdata, target_variable, positive = "Yes", sample_percent = 30)
#' 
#' # parallel processing
#' eda_category(testdata, target_variable, positive = "Yes", parallel = TRUE,
#'              cores = 8)
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import openxlsx
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom progress progress_bar
#' @importFrom rlang sym
#'  
eda_category <- function(x, target_variable, positive = "1", output_file = NULL, 
                         output_dir = getwd(), sample_percent = 100, parallel = FALSE, 
                         cores = parallel::detectCores() - 2,
                         future_globals_maxsize = 500 * 1024^2, verbose = TRUE) {
  options(warn=-1)
  on.exit(options(warn=0))
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  # Number of observations
  N <- NROW(x)
  # sampling with sample_percent
  if (sample_percent < 100) {
    N_sample <- ceiling(N * sample_percent / 100)
    idx <- sample(seq(N), size = N_sample)
    
    x <- x[idx, ]
  } 
  
  path <- output_dir
  
  if (is.null(output_file))
    output_file <- glue::glue("EDA_{target_variable}_categorical.xlsx")
  output_file <- paste(path, output_file, sep = "/")
  
  
  ## 범주형 변수 목록 추출
  var_cat <- x %>% 
    dlookr::find_class(type = "categorical", index = FALSE) %>% 
    setdiff(target_variable)
  
  if (length(var_cat) == 0) {
    stop("데이터셋에 범주형 변수가 없습니다. 분석하고 싶은 character는 factor로 변경하세요.")
  } 
  
  ## Target variable 기반의 target_df 클래스 정의
  targeted <- x %>% 
    mutate({{target_variable}} := factor(!! rlang::sym(target_variable))) %>%     
    dlookr::target_by(!!target_variable)
  
  ## 병렬처리 파라미터 설정
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    
    options(future.globals.maxSize = future_globals_maxsize)
    options(future.rng.onMisuse = "ignore")    
  }


  ## 범주형 변수별 relate 클래스 생성
  if (verbose) {
    if (parallel) {
      cli::cli_alert_info("Aggregating data with parallel process...")
    } else {
      pb <- progress::progress_bar$new(
        format = "- Aggregating data [:bar] :percent eta: :eta",
        total = length(var_cat), clear = FALSE, width = 80)      
    }
  }  
  
  if (parallel) {
    results_cat <- var_cat %>% 
      furrr::future_map_dfr(
        function(x) {
          options(dlookr_offline = TRUE)
          
          tab <- dlookr::relate(targeted, all_of(x))
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_levels <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()
          
          
          test <- summary(tab)
          
          statistic <- tibble::tibble(
            statistic = test$statistic,
            df = test$parameter,
            pvalue = test$p.value
          )
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare") +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}")) +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_blank())
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),
            viz = list(p)
          )
        },
        .options = furrr::furrr_options(globals = c("targeted", "target_variable"))
      )    
  } else {
    results_cat <- var_cat %>% 
      purrr::map_df(
        function(x) {
          options(dlookr_offline = TRUE)
          
          tab <- dlookr::relate(targeted, all_of(x))
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_levels <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()
          
          
          test <- summary(tab)
          
          statistic <- tibble::tibble(
            statistic = test$statistic,
            df = test$parameter,
            pvalue = test$p.value
          )
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare") +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}")) +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_blank())
          
          if (verbose) {
            pb$tick()
          }  
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),
            viz = list(p)
          )
        }
      )      
  }  
  
  ## 독립성 검정 결과
  tab_independence <- results_cat %>% 
    NROW() %>% 
    seq() %>% 
    purrr::map_df(
      function(x) {
        tab <- data.frame(variables = results_cat$variables[x]) %>% 
          bind_cols(results_cat$statistics[x]) %>% 
          bind_cols(results_cat$diag[x])
      }  
    ) %>% 
    arrange(pvalue)
  
  ## Export to excel
  ## Create a new work book 
  wb <- openxlsx::createWorkbook()
  
  ## Create a new worksheet for print table
  sname <- "범주형변수_총괄"
  openxlsx::addWorksheet(wb, sheetName = sname, tabColour = "deepskyblue")
  
  ## Write data.frame to a new worksheet
  openxlsx::writeDataTable(wb, sheet = sname, tab_independence)
  
  tab_independence %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        if (!is.na(tab_independence$statistic[x])) {
          writeFormula(wb, sheet = sname, startRow = x + 1, startCol = 1,
                       x = makeHyperlinkString(
                         sheet = tab_independence$variables[x], row = 1, col = 1, 
                         text = tab_independence$variables[x]
                       )
          )  
        }
      }
    )
  
  setColWidths(wb, sheet = sname, cols = 1:4, widths = "auto")

  ## 이미지 파일 생성을 위한 임시 디렉토리 생성
  dir.create(here::here("temp"))

  if (verbose) {
    pb <- progress_bar$new(
      format = "- Reporting result [:bar] :percent eta: :eta",
      total = NROW(results_cat), clear = FALSE, width = 80)
  }  
  
  ## 범주형변수 개별 시트 작성 루프  
  results_cat %>% 
    NROW() %>% 
    seq() %>% 
    # "["(1:2) %>%
    purrr::walk(
      function(x) {
        origin_x <- x
        x <- which(results_cat$variables %in% tab_independence[x, "variables"])
        
        sheet_name <- results_cat$variables[x]
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        
        predictor_variable <- sheet_name
        
        tab <- results_cat[x, "tabs"] %>% 
          pull() %>% 
          "[["(1)
        
        writeFormula(wb, sheet = sheet_name, startRow = 1, startCol = 9,
                     x = makeHyperlinkString(
                       sheet = "범주형변수_총괄", row = origin_x + 1, col = 1, 
                       text = "범주형변수_총괄 시트로 이동"
                     )
        )  
        
        ## 분할표 --------------------------------------------------------------
        tab_contigency <- tab %>% 
          as_tibble() %>% 
          tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
          janitor::adorn_totals("row") %>% 
          janitor::adorn_totals("col")  
        
        title_str <- "<< 분할표 >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)      
        writeDataTable(wb, sheet = sheet_name, tab_contigency, 
                       startCol = 1, startRow = 3, rowNames = FALSE)
        
        s <- createStyle(numFmt = "#,##0")
        addStyle(wb, sheet = sheet_name, style = s, rows = 4:6, cols = 2:(max(dim(tab)) + 2), 
                 gridExpand = TRUE)
        
        
        ## 상대도수 분할표 -----------------------------------------------------
        tab_relate <- tab %>% 
          as_tibble() %>% 
          bind_rows(
            tab %>% 
              as_tibble() %>% 
              group_by_at(vars(matches(target_variable))) %>% 
              summarise(n = sum(n)) %>% 
              bind_cols(data.frame(predictor = "Total")) %>% 
              select(1, 3, 2) %>% 
              rename({{predictor_variable}} := predictor)
          ) %>% 
          group_by_at(vars(matches(predictor_variable))) %>% 
          mutate(pct = round(n / sum(n) * 100, 2)) %>% 
          tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
        
        title_str <- "<< 분할표 - 상대도수 >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 9)            
        writeDataTable(wb, sheet = sheet_name, tab_relate, 
                       startCol = 1, startRow = 10, rowNames = FALSE)
        
        
        ## 독립성검정  ---------------------------------------------------------    
        test <- summary(tab)
        ctest <- data.frame(
          `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
          `Numberof Case` = test$n.cases,
          `Chisqure Statistic` = test$statistic,
          `df` = test$parameter,
          `p-value` = test$p.value
        )
        
        title_str <- "<< 독립성검정 >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 15)            
        writeDataTable(wb, sheet = sheet_name, ctest, startCol = 1, startRow = 16, 
                       rowNames = FALSE)
        
        
        ## 시각화 --------------------------------------------------------------
        image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))
        
        contigency_plot <- results_cat[x, "viz"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
        plot(contigency_plot)
        dev.off()
        
        title_str <- "<< 시각화 >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 20)        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 6, height = 3.5,
                              startCol = 1, startRow = 21)
        
        total_ratio <- tab %>% 
          as_tibble() %>% 
          rename(target := any_of(target_variable)) %>%   
          group_by(target) %>%   
          summarise(n = sum(n)) %>% 
          summarise(positive_ratio = sum(ifelse(target == positive, n, 0)) / sum(n) * 100) %>% 
          pull()
        
        annotation <- data.frame(
          x = NCOL(tab),
          y = total_ratio,
          label = glue::glue("전체백분율({round(total_ratio, 2)})"))
        
        
        p_ratio <- tab %>% 
          as_tibble() %>% 
          rename(target := any_of(target_variable)) %>% 
          group_by_at(vars(matches(predictor_variable))) %>% 
          summarise(positive_ratio = round(sum(ifelse(target == positive, n, 0)) / sum(n) * 100, 2)) %>% 
          ggplot(aes_string(x = predictor_variable, y = "positive_ratio", group = 1)) + 
          geom_point(colour = "blue", size = 3) +
          geom_label(aes_string(label = "positive_ratio"), fill = "lightgray",
                     colour = "blue", size = 3.5, nudge_x = 0) +
          geom_line(colour = "darkgray", linetype = 2) +
          geom_hline(yintercept = total_ratio, color = "red") +
          ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
          labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
               y = glue::glue("{target_variable} 백분율")) +
          hrbrthemes::theme_ipsum(base_family = "NanumSquare")
        
        image_name <- here::here("temp", glue::glue("ratio_{runif(1)}.png"))
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
        print(p_ratio)
        dev.off()
        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 6, height = 3.5,
                              startCol = 8, startRow = 21)      
        
        if (verbose) {
          pb$tick()
        }  
      }
    )
  
  ## Save excel file
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  ## Remove image file
  unlink(here::here("temp"), recursive = TRUE)  
}



#' 연속형 변수의 excel EDA 보고서 생성
#' @description 연속형 변수에 대한 EDA 수행 결과를 Excel 파일로 생성함
#' @param x data.frame, tbl_df. EDA를 수행할 데이터셋.
#' @param target_variable character. Target 변수의 이름.
#' @param positive character. Target 변수에서의 Positive 클래스의 이름
#' @param output_file character. 생성할 Excel 파일의 이름.
#' @param output_dir character. 생성할 Excel 파일이 저장될 디렉토리.
#' @param sample_percent numeric. 샘플로 분석할 경우의 샘플의 비율로서의 백분율.
#' @param trim_quantile numeric. 이상치 제거를 위해서 데이터를 trim할 상하의 분위(%).
#' 기본값은 NULL로 모든 데이터, 즉 [min, max]를 대상으로 개별 연속형 변수의 보고서를 생성함. 
#' 길이가 2인 수치벡터의 경우에는 하한과 상한을 백분위수로 표현함. 
#' 예를 들면, c(0, 99)의 경우에는 [max, 99백분위수] 범위의 데이터를 대상으로,
#' c(5, 95)의 경우에는 [5백분위수, 95백분위수] 범위의 데이터로 개별 연속형 변수의 보고서를 생성함.
#' 만약 이값이 NA이면, (Q1 - 1.5 * IQR, Q3 + 1.5 * IQR) 범위의 값을 대상으로 
#' 개별 연속형 변수의 보고서를 작성함. 즉, 이상치를 제거한 데이터로 개별 연속형 변수의 보고서를 생성함.
#' @param parallel logical. 병렬 프로세싱으로 수행할지의 여부, 기본값은 FALSE.
#' @param cores integer. 병렬 프로세싱에서 사용할 코어의 개수.
#' @param future_globals_maxsize numeric. 병렬 프로세싱에서 개별 코어에서 
#' 사용할 글로벌 메모리의 최대값.
#' @param verbose logical. 작업 경과의 정보를 출력할지의 여부, 기본값은 TRUE. 
#' @details 연속형 변수의 총괄 정보를 제외한 모든 연속형 변수는 1개의 워크시트에 EDA 정보가 출력됨.
#' Excel은 256개의 워크시트만 지원하므로 연속형 변수 개수가 255개를 초과하는 것은 255개의 정보만 생성됨.
#' @author 유충현
#' Maintainer: 유충현 <antony@hanwha.com>
#' @seealso \code{\link{eda_category}}
#' @examples
#' \dontrun{
#' testdata <- dlookr::heartfailure
#' target_variable <- "death_event"
#' 
#' # single core processing
#' eda_numeric(testdata, target_variable, positive = "Yes")
#' 
#' # 30% 샘플링한 데이터로 보고서를 생성함
#' eda_numeric(testdata, target_variable, positive = "Yes", sample_percent = 30)
#' 
#' # 이상치를 제거한 데이터로 개별 변수의 보고서를 작성
#' eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = NA)
#' 
#' # 데이터 상한의 5%를 제거한 데이터로 개별 변수의 보고서를 작성
#' eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = c(0, 95))
#' 
#' # parallel processing
#' eda_numeric(testdata, target_variable, positive = "Yes", parallel = TRUE,
#'             cores = 8)
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import openxlsx
#' @import funModeling
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom parallel detectCores
#' @importFrom broom tidy
#' @importFrom dlookr find_class target_by binning_by plot_outlier
#' @importFrom hrbrthemes theme_ipsum scale_fill_ipsum
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom Hmisc cut2
#' @importFrom purrr map_df
#' @importFrom Cairo CairoPNG
#' @importFrom ggrepel geom_label_repel
#' @importFrom tidyr pivot_wider
#' @importFrom janitor adorn_totals 
#' @importFrom rlang sym
#' @importFrom gridExtra grid.arrange
#' 
eda_numeric <- function(x, target_variable, positive = "1", output_file = NULL, 
                        output_dir = getwd(), sample_percent = 100, trim_quantile = NULL,
                        parallel = FALSE, cores = parallel::detectCores() - 2,
                        future_globals_maxsize = 500 * 1024^2, verbose = TRUE) {
  options(warn=-1)
  on.exit(options(warn=0))
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  # Number of observations
  N <- NROW(x)
  # sampling with sample_percent
  if (sample_percent < 100) {
    N_sample <- ceiling(N * sample_percent / 100)
    idx <- sample(seq(N), size = N_sample)
    
    x <- x[idx, ]
  } 
  
  path <- output_dir
  
  if (is.null(output_file))
    output_file <- glue::glue("EDA_{target_variable}_numerical.xlsx")
  output_file <- paste(path, output_file, sep = "/")
  
  
  ## 수치형 변수 목록 추출
  var_num <- x %>% 
    dlookr::find_class(type = "numerical", index = FALSE) %>% 
    setdiff(target_variable)
  
  if (length(var_num) == 0) {
    stop("데이터셋에 연속형 변수가 없습니다.")
  } 
  
  ## Target variable 기반의 target_df 클래스 정의
  targeted <- x %>% 
    mutate({{target_variable}} := factor(!! rlang::sym(target_variable))) %>%    
    dlookr::target_by(!!target_variable)
  

  ## 병렬처리 파라미터 설정
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    
    options(future.globals.maxSize = future_globals_maxsize)
    options(future.rng.onMisuse = "ignore")    
  }
  
  ## 연속형 변수별 relate 클래스 생성
  x_num <- x %>% 
    select(!!var_num, !!target_variable)
  
  if (verbose) {
    if (parallel) {
      cli::cli_alert_info("Aggregating data with parallel process...")
    } else {
      pb <- progress::progress_bar$new(
        format = "- Aggregating data [:bar] :percent eta: :eta",
        total = length(var_num), clear = FALSE, width = 80)      
    }
  }  
  
  if (parallel) {
    results_num <- var_num %>% 
      furrr::future_map_dfr(
        function(x) {
          options(dlookr_offline = TRUE)
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_minus <- sum(pull(targeted[, x]) < 0, na.rm = TRUE)
          
          tab_diag$n_zero <- sum(pull(targeted[, x]) == 0, na.rm = TRUE)
          
          tab_diag$n_unique <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()   
          
          tab_diag$n_outlier <- dlookr::diagnose_outlier(ungroup(targeted), x) %>% "$"("outliers_cnt")        
          
          if (is.null(trim_quantile)) {
            targeted2 <- targeted
          } else if (is.na(trim_quantile)) {
            coef <- 1.5
            stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
            iqr <- diff(stats[c(2, 4)])
            
            low <- stats[2L] - coef * iqr
            high <- stats[4L] + coef * iqr
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) > low) %>% 
              filter(!!rlang::sym(x) < high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))
          } else if (length(trim_quantile) == 2) {
            quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100,
                                  na.rm = TRUE)
            low <- quantiles[1]
            high <- quantiles[2]
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) >= low) %>% 
              filter(!!rlang::sym(x) <= high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))            
          }
          
          tab <- dlookr::relate(targeted2, all_of(x))
          
          tryCatch(expr = {
            test <- t.test(glue::glue("{x} ~ {target_variable}") %>% 
                             as.formula(), data = x_num)
          },
          error = function(e) e)
          
          if (!exists("test")) {
            statistic <- NA
          } else {
            statistic <- broom::tidy(test)
          }
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare") +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}"))
          
          p_out <- dlookr::plot_outlier(targeted2, x, base_family = "NanumSquare")
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),        
            viz = list(p),
            viz_out = list(p_out)
          )
        },
        .options = furrr::furrr_options(globals = c("targeted", "x_num", "target_variable"))
      )
  } else {
    results_num <- var_num %>% 
      purrr::map_df(
        function(x) {
          options(dlookr_offline = TRUE)
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_minus <- sum(pull(targeted[, x]) < 0, na.rm = TRUE)
          
          tab_diag$n_zero <- sum(pull(targeted[, x]) == 0, na.rm = TRUE)
          
          tab_diag$n_unique <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()   
          
          tab_diag$n_outlier <- dlookr::diagnose_outlier(ungroup(targeted), x) %>% "$"("outliers_cnt") 
          
          if (is.null(trim_quantile)) {
            targeted2 <- targeted
          } else if (is.na(trim_quantile)) {
            coef <- 1.5
            stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
            iqr <- diff(stats[c(2, 4)])
            
            low <- stats[2L] - coef * iqr
            high <- stats[4L] + coef * iqr
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) > low) %>% 
              filter(!!rlang::sym(x) < high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))
          } else if (length(trim_quantile) == 2) {
            quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100,
                                  na.rm = TRUE)
            low <- quantiles[1]
            high <- quantiles[2]
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) >= low) %>% 
              filter(!!rlang::sym(x) <= high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))              
          }
          
          tab <- dlookr::relate(targeted2, all_of(x))
          
          tryCatch(expr = {
            test <- t.test(glue::glue("{x} ~ {target_variable}") %>% 
                             as.formula(), data = x_num)
          },
          error = function(e) e)
          
          if (!exists("test")) {
            statistic <- NA
          } else {
            statistic <- broom::tidy(test)
          }
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare") +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}"))
          
          p_out <- dlookr::plot_outlier(targeted2, x)
          
          if (verbose) {
            pb$tick()
          }  
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),        
            viz = list(p),
            viz_out = list(p_out)
          )
        }
      )
  }
  
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "- Binning data     [:bar] :percent eta: :eta",
      total = length(var_num), clear = FALSE, width = 80)      
  }   
  
  ## Binning
  results_num_bin <- var_num %>% 
    purrr::map_df(
      function(x) {
        options(dlookr_offline = TRUE)
        
        discretize_rgr <- function (input, target, min_perc_bins = 0.1, max_n_bins = 5) {
          fpoints <- c()
          max_depth <- 20
          target <- as.character(target)
          min_n <- round(min_perc_bins * length(input))
          all_cuts <- funModeling:::recursive_gr_cuts_aux(input, target, fpoints, max_depth, min_n)
          max_n_bins <- max_n_bins - 1
          
          if (is.null(all_cuts)) {
            fpoints_top <- 1:max_n_bins
          } else {
            fpoints_top <- all_cuts[1:min(max_n_bins, length(all_cuts))]
          }
          
          fpoints_top_ord <- fpoints_top[order(fpoints_top)]
          input_bin <- Hmisc::cut2(input, cuts = c(fpoints_top_ord,
                                                   max(input)))
          return(input_bin)
        }
        
        binning_rgr <- function (.data, y, x, min_perc_bins = 0.1, max_n_bins = 5, ordered = TRUE) {
          y <- tidyselect::vars_select(names(.data), !!rlang::enquo(y))
          x <- tidyselect::vars_select(names(.data), !!rlang::enquo(x))
          
          if (tibble::is_tibble(.data)) {
            .data <- as.data.frame(.data)
          }
          
          uniq_y <- length(unique(.data[, y]))
          type_y <- class(.data[, y])[1]
          type_x <- class(.data[, x])[1]
          
          if (!is.data.frame(.data))
            stop("Data is not a data.frame.")
          
          if (!type_x %in% c("integer", "numeric"))
            stop("x is not numeric value.")
          
          if (uniq_y != 2) {
            stop("The number of levels of the y variable is not 2.")
          }
          
          if (!type_y %in% c("character", "factor", "ordered")) {
            stop("y is not character or (ordered)factor.")
          }
          
          if (any(is.na(.data[, x])))
            stop("x with a NA. This fuction not support missing.")
          
          if (any(is.na(.data[, y])))
            stop("y with a NA. This fuction not support missing.")
          
          if (length(unique(.data[, x])) < 5)
            stop("x must be number of unique values greater then 4.")
          
          if (requireNamespace("funModeling", quietly = TRUE)) {
            bins <- discretize_rgr(.data[, x], .data[, y],
                                   min_perc_bins = min_perc_bins, max_n_bins = max_n_bins)
          }
          else {
            stop("Package 'funModeling' needed for this function to work. Please install it.",
                 call. = FALSE)
          }
          
          bin_levels <- levels(bins)
          breaks <- gsub(pattern = "[]\\[)]", "", bin_levels)
          breaks_max <- gsub(pattern = "^[[:print:]]*,", "", breaks) %>%
            as.numeric() %>% max()
          breaks <- gsub(pattern = ",[[:print:]]*$", "", breaks) %>%
            as.numeric() %>% c(breaks_max)
          
          if (ordered == TRUE)
            bins <- ordered(bins)
          results <- bins
          attr(results, "type") <- "infogain"
          attr(results, "breaks") <- breaks
          attr(results, "levels") <- bin_levels
          attr(results, "raw") <- .data[, x]
          attr(results, "x_var") <- x
          attr(results, "y_var") <- y
          class(results) <- append("bins", class(results))
          attr(results, "target") <- .data[, y]
          class(results) <- append("infogain_bins", class(results))
          results
        }
        
        if (is.null(trim_quantile)) {
          targeted2 <- targeted
          x_num2 <- x_num
        } else if (is.na(trim_quantile)) {
          coef <- 1.5
          stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
          iqr <- diff(stats[c(2, 4)])
          
          low <- stats[2L] - coef * iqr
          high <- stats[4L] + coef * iqr
          
          targeted2 <- targeted %>% 
            filter(!!rlang::sym(x) > low) %>% 
            filter(!!rlang::sym(x) < high) 
          
          attr(targeted2, "type_y") <- attr(targeted, "type_y")
          class(targeted2) <- append("target_df", class(targeted2))
          
          x_num2 <- x_num %>% 
            filter(!!rlang::sym(x) > low) %>% 
            filter(!!rlang::sym(x) < high) 
        } else if (length(trim_quantile) == 2) {
          quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100,
                                na.rm = TRUE)
          low <- quantiles[1]
          high <- quantiles[2]
          
          targeted2 <- targeted %>% 
            filter(!!rlang::sym(x) >= low) %>% 
            filter(!!rlang::sym(x) <= high) 
          
          attr(targeted2, "type_y") <- attr(targeted, "type_y")
          class(targeted2) <- append("target_df", class(targeted2))   
          
          x_num2 <- x_num %>% 
            filter(!!rlang::sym(x) >= low) %>% 
            filter(!!rlang::sym(x) <= high)           
        }
        
        tryCatch(expr = {
          bin_rgr <- x_num2 %>%
            dplyr::select(all_of(x), all_of(target_variable)) %>%
            dplyr::filter(complete.cases(.)) %>%
            binning_rgr(all_of(target_variable), all_of(x))
        },
        error = function(e) e)
        
        if (!exists("bin_rgr")) {
          bin_rgr <- NA
        }
        
        tryCatch(expr = {
          suppressWarnings(
            bin_opt <- x_num2 %>%
              dplyr::select(all_of(x), all_of(target_variable)) %>%
              dplyr::filter(complete.cases(.)) %>%
              dlookr::binning_by(all_of(target_variable), all_of(x))            
          )
        },
        error = function(e) e)
        
        if (!exists("bin_opt")) {
          bin_opt <- NA
        }
        
        if (verbose) {
          pb$tick()
        }  
        
        tibble::tibble(
          variables = x,
          bin_rgr = list(bin_rgr),
          bin_opt = list(bin_opt)
        )
      }
    )
  
  
  ## Merge statistics and bins
  results_num <- results_num %>% 
    inner_join(
      results_num_bin,
      by = "variables"
    )
  
  ## 모평균 검정 결과
  tab_mean_test <- results_num %>% 
    NROW() %>% 
    seq() %>% 
    purrr::map_df(
      function(x) data.frame(variables = results_num$variables[x],
                             stringsAsFactors = FALSE) %>% 
        bind_cols(results_num$statistics[x]) %>% 
        bind_cols(results_num$diag[x])
    ) %>% 
    arrange(p.value) %>% 
    select(variables:conf.high, n_missing:n_outlier) %>% 
    rename(`mean_difference_0_and_1` = estimate,
           `estimate_mean_0` = estimate1,
           `estimate_mean_1` = estimate2,
           `df`= parameter,
           `p_value`= p.value,
           `confidence_interval_low`= conf.low,
           `confidence_interval_high`= conf.high)
  
  
  ## Export to excel
  ## Create a new work book 
  wb <- openxlsx::createWorkbook()
  
  ## Create a new worksheet for print table
  sname <- "연속형변수_총괄"
  openxlsx::addWorksheet(wb, sheetName = sname, tabColour = "deepskyblue")
  
  ## Write data.frame to a new worksheet
  openxlsx::writeDataTable(wb, sheet = sname, tab_mean_test)
  
  tab_mean_test %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        if (!is.na(tab_mean_test$df[x])) {
          writeFormula(wb, sheet = sname, startRow = x + 1, startCol = 1,
                       x = makeHyperlinkString(
                         sheet = tab_mean_test$variables[x], row = 1, col = 1, 
                         text = tab_mean_test$variables[x]
                       )
          )  
        }
      }
    )
  
  setColWidths(wb, sheet = sname, cols = 1:4, widths = "auto")
  
  ## 이미지 파일 생성을 위한 임시 디렉토리 생성
  dir.create(here::here("temp"))
  
  if (verbose) {
    pb <- progress_bar$new(
      format = "- Reporting result [:bar] :percent eta: :eta",
      total = NROW(results_num), clear = FALSE, width = 80)
  }  
  
  ## 연속형변수 개별 시트 생성 루프
  if (is.null(trim_quantile)) {
    header_str <- " "
  } else if (is.na(trim_quantile)) {
    header_str <- "원 자료에서 이상치를 제거한 분포입니다."
  } else if (length(trim_quantile) == 2) {
    low <- trim_quantile[1]
    high <- trim_quantile[2]
    
    header_str <- glue::glue("원 자료 [{low}, {high}] 백분위 범위의 데이터 분포입니다.") %>% 
      as.character()
  }
  
  results_num %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        origin_x <- x
        x <- which(results_num$variables %in% tab_mean_test[x, "variables"])
        
        sheet_name <- results_num$variables[x]
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        
        predictor_variable <- sheet_name
        
        tab <- results_num[x, "tabs"] %>% 
          pull() %>% 
          "[["(1)
        
        writeFormula(wb, sheet = sheet_name, startRow = 1, startCol = 9,
                     x = makeHyperlinkString(
                       sheet = "연속형변수_총괄", row = origin_x + 1, col = 1, 
                       text = "연속형변수_총괄 시트로 이동"
                     )
        )  
        
        ## Header for trimed information
        writeData(wb, sheet = sheet_name, header_str, startCol = 1, startRow = 1)   
        
        s <- createStyle(fontColour = "#0000FF", halign = "left", textDecoration = "bold")
        addStyle(wb, sheet = sheet_name, style = s, rows = 1, cols = 1, 
                 gridExpand = TRUE)
        
        ## Distributions
        title_str <- "1. Distributions"      
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)   
        
        ## 통계량 --------------------------------------------------------------
        subtitle_str <- "<< 통계량 >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = 3)      
        writeDataTable(wb, sheet = sheet_name, tab, 
                       startCol = 1, startRow = 4, rowNames = FALSE)
        
        s <- createStyle(numFmt = "#,##0")
        addStyle(wb, sheet = sheet_name, style = s, rows = 5:7, cols = 3, 
                 gridExpand = TRUE)
        
        ## 시각화 --------------------------------------------------------------
        image_name <- here::here("temp", glue::glue("density_{runif(1)}.png")) 
        
        density_plot <- results_num[x, "viz"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
        plot(density_plot)
        dev.off()
        
        subtitle_str <- "<< 분포 시각화 >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = 10)        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 8, height = 5,
                              startCol = 1, startRow = 11)
        
        ## Binning by recursive information gain ratio maximization
        row_bir <- 37
        title_str <- "2. Binning by recursive information gain ratio maximization"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_bir)
        
        ## Frequency table with RIR --------------------------------------------
        bin_rgr <- results_num[x, "bin_rgr"] %>%
          pull() %>%
          "[["(1)
        
        if (length(bin_rgr) == 1) {
          subtitle_str <- "Binning 되지 않는 분포입니다."
          
          s <- createStyle(fontColour = "#FF0000", halign = "left", textDecoration = "bold")
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_bir + 1)    
          addStyle(wb, sheet = sheet_name, style = s, rows = row_bir + 1, cols = 1, 
                   gridExpand = TRUE)        
          
          row_bir_viz <- row_bir + 3
        } else {
          tab_rgr <- summary(bin_rgr) %>% 
            mutate(rate = round(rate * 100, 2)) %>% 
            rename(Frequency = freq,
                   `Rate(%)` = rate)
          
          subtitle_str <- "<< Frequency Table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = row_bir + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_rgr,
                         startCol = 1, startRow = row_bir + 2, rowNames = FALSE)
          
          conditionalFormatting(wb, sheet = sheet_name, cols = 2, 
                                rows = (row_bir + 3):(row_bir + 4 + NROW(tab_rgr)), 
                                type = "databar") 
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, 
                   rows = (row_bir + 3):(row_bir + 4 + NROW(tab_rgr)), cols = 2, 
                   gridExpand = TRUE)
          
          ## 시각화 ------------------------------------------------------------
          row_bir_viz <- row_bir + 5 + NROW(tab_rgr)
          
          image_name <- here::here("temp", glue::glue("binn_rir_{runif(1)}.png")) 
          
          Cairo::CairoPNG(filename = image_name, width = 1000, height = 700)
          plot(bin_rgr)
          dev.off()
          
          subtitle_str <- "<< 분포 시각화 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_bir_viz)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 7.85, height = 5.5,
                                startCol = 1, startRow = row_bir_viz + 1)
          
          ## Binning Information -----------------------------------------------
          tab <- tibble::tibble(x_var = attr(bin_rgr, "raw"), 
                                y_var = attr(bin_rgr, "target")) %>% 
            mutate(x_var = cut(x_var, breaks = unique(attr(bin_rgr, "breaks")), right = FALSE)) %>% 
            rename({{predictor_variable}} := x_var) %>% 
            rename({{target_variable}} := y_var) %>% 
            table() %>% 
            t()
          
          tab_contigency <- tab %>% 
            as_tibble() %>% 
            tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
            janitor::adorn_totals("row") %>% 
            janitor::adorn_totals("col")  
          
          subtitle_str <- "<< 분할표 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_contigency,
                         startCol = 11, startRow = row_bir + 2, rowNames = FALSE)
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, rows = (row_bir + 3):(row_bir + 5), 
                   cols = 12:(12 + NCOL(tab) + 2), 
                   gridExpand = TRUE)
          
          tab_relate <- tab %>% 
            as_tibble() %>% 
            bind_rows(
              tab %>% 
                as_tibble() %>% 
                group_by_at(vars(matches(target_variable))) %>% 
                summarise(n = sum(n)) %>% 
                bind_cols(data.frame(predictor = "Total")) %>% 
                select(1, 3, 2) %>% 
                rename({{predictor_variable}} := predictor)
            ) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            mutate(pct = round(n / sum(n) * 100, 2)) %>% 
            tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
          
          subtitle_str <- "<< 분할표 - 상대도수 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 8)    
          writeDataTable(wb, sheet = sheet_name, tab_relate,
                         startCol = 11, startRow = row_bir + 9, rowNames = FALSE)
          
          test <- summary(tab)
          ctest <- data.frame(
            `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
            `Numberof Case` = test$n.cases,
            `Chisqure Statistic` = test$statistic,
            `df` = test$parameter,
            `p-value` = test$p.value
          )
          
          subtitle_str <- "<< 독립성검정 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 14)    
          writeDataTable(wb, sheet = sheet_name, ctest,
                         startCol = 11, startRow = row_bir + 15, rowNames = FALSE)        
          
          data <- as.data.frame(tab) %>% 
            select(a = 2, b = 1, n = Freq) 
          
          first <- data[1, 1] %>% as.character
          
          y <- data %>% 
            filter(a %in% first) %>% 
            select(b, n)
          
          y_lab <- y$b %>% rev() %>% as.character()
          y <- y$n %>% rev()
          
          y_cumsum <- cumsum(y)
          y_center <- y / 2
          
          y_pos <- numeric(length(y))
          for (j in seq(y)) {
            if (j == 1) {
              y_pos[j] <- y_center[j]
            } else {
              y_pos[j] <- y_cumsum[j-1] + y_center[j]
            }
            y_pos[j] <- y_pos[j] / sum(y)
          }
          
          suppressWarnings(
            contigency_plot <- data %>% 
              group_by(a) %>% 
              mutate(x_width = sum(n)) %>% 
              ggplot(aes(x = a, y = n)) +
              geom_col(aes(width = x_width, fill = b),
                       color = "white", size = 2, 
                       position = position_fill(reverse = FALSE)) +
              facet_grid(~ a, space = "free", scales = "free", switch = "x") +
              scale_x_discrete(name = predictor_variable) +
              scale_y_continuous(name = target_variable, breaks = y_pos, labels = y_lab) +
              labs(title = sprintf("%s ~ %s", target_variable, predictor_variable)) +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    strip.background = element_blank(),
                    panel.spacing = unit(0, "pt")) +
              hrbrthemes::theme_ipsum(base_family = "NanumSquare") +
              hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
              theme(legend.position = "none",
                    panel.grid.major.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size = 12),
                    panel.spacing = unit(0, "pt"))            
          )
          
          row_bir_viz <- row_bir + 5 + NROW(tab_rgr)
          
          image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          plot(contigency_plot)
          dev.off()
          
          subtitle_str <- "<< 분포 시각화 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, 
                    startRow = row_bir + 19)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 11, startRow = row_bir + 20)
          
          total_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>%   
            group_by(target) %>%   
            summarise(n = sum(n)) %>% 
            summarise(positive_ratio = sum(ifelse(target == positive, n, 0)) / sum(n) * 100) %>% 
            pull()
          
          annotation <- data.frame(
            x = NCOL(tab),
            y = total_ratio,
            label = glue::glue("전체백분율({round(total_ratio, 2)})"))
          
          
          p_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>% 
            rename(predictor := any_of(predictor_variable)) %>% 
            mutate(predictor = factor(predictor, levels = dimnames(tab)[[2]])) %>% 
            group_by(predictor) %>% 
            summarise(positive_ratio = round(sum(ifelse(target == positive, n, 0)) / sum(n) * 100, 2)) %>% 
            ggplot(aes(x = predictor, y = positive_ratio, group = 1)) + 
            geom_point(colour = "blue", size = 3) +
            ggrepel::geom_label_repel(aes(label = positive_ratio), colour = "blue", size = 3.5) +
            geom_line(colour = "darkgray", linetype = 2) +
            geom_hline(yintercept = total_ratio, color = "red") +
            ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
            labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
                 x = predictor_variable,
                 y = glue::glue("{target_variable} 백분율")) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare")
          
          image_name <- here::here("temp", glue::glue("bin_ratio_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          print(p_ratio)
          dev.off()
          
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 18, startRow = row_bir + 20)   
          
        }
        
        
        ## Optimal Binning for Scoring Modeling
        row_opt <- row_bir_viz + ifelse(length(bin_rgr) == 1, 0, 30)
        
        title_str <- "3. Optimal Binning for Scoring Modeling"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_opt)
        
        ## Frequency table with RIR --------------------------------------------
        bin_opt <- results_num[x, "bin_opt"] %>%
          pull() %>%
          "[["(1)
        
        if (length(bin_opt) == 1) {
          subtitle_str <- "Binning 되지 않는 분포입니다."
          
          s <- createStyle(fontColour = "#FF0000", halign = "left", textDecoration = "bold")
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_opt + 1)    
          addStyle(wb, sheet = sheet_name, style = s, rows = row_opt + 1, cols = 1, 
                   gridExpand = TRUE) 
          
          row_out <- row_opt + 1 + 3
        } else {
          tab_opt <- attr(bin_opt, "performance")
          
          subtitle_str <- "<< Frequency Table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_opt + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_opt, startCol = 1, 
                         startRow = row_opt + 2, rowNames = FALSE)
          
          conditionalFormatting(wb, sheet = sheet_name, cols = 2, 
                                rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
                                type = "databar") 
          conditionalFormatting(wb, sheet = sheet_name, cols = 7, 
                                rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
                                type = "databar")       
          conditionalFormatting(wb, sheet = sheet_name, cols = 11, 
                                rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
                                type = "databar", style = "#FFA500")       
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, 
                   rows = (row_opt + 3):(row_opt + 2 + NROW(tab_opt)), cols = 2:6, 
                   gridExpand = TRUE)
          
          ## 시각화 ------------------------------------------------------------
          image_name <- here::here("temp", glue::glue("binn_opt_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 1200, height = 700)
          plot(bin_opt)
          dev.off()
          
          subtitle_str <- "<< 분포 시각화 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = (row_opt + 4 + NROW(tab_opt)) + 1)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 9.43, height = 5.5, startCol = 1, 
                                row_opt + 4 + NROW(tab_opt) + 2)        
          
          row_out <- row_opt + 4 + NROW(tab_opt) + 2 + 29
          
          ## Binning Information -----------------------------------------------
          tab <- tibble::tibble(x_var = attr(bin_opt, "raw"), 
                                y_var = attr(bin_opt, "target")) %>% 
            mutate(x_var = cut(x_var, breaks = unique(attr(bin_opt, "breaks")))) %>% 
            rename({{predictor_variable}} := x_var) %>% 
            rename({{target_variable}} := y_var) %>% 
            table() %>% 
            t()
          
          
          tab_contigency <- tab %>% 
            as_tibble() %>% 
            tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
            janitor::adorn_totals("row") %>% 
            janitor::adorn_totals("col")  
          
          subtitle_str <- "<< 분할표 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, 
                    startRow = row_opt + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_contigency,
                         startCol = 18, startRow = row_opt + 2, rowNames = FALSE)
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, rows = (row_opt + 3):(row_opt + 5), 
                   cols = 19:(19 + NCOL(tab) + 2), 
                   gridExpand = TRUE)
          
          tab_relate <- tab %>% 
            as_tibble() %>% 
            bind_rows(
              tab %>% 
                as_tibble() %>% 
                group_by_at(vars(matches(target_variable))) %>% 
                summarise(n = sum(n)) %>% 
                bind_cols(data.frame(predictor = "Total")) %>% 
                select(1, 3, 2) %>% 
                rename({{predictor_variable}} := predictor)
            ) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            mutate(pct = round(n / sum(n) * 100, 2)) %>% 
            tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
          
          subtitle_str <- "<< 분할표 - 상대도수 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, startRow = row_opt + 8)    
          writeDataTable(wb, sheet = sheet_name, tab_relate,
                         startCol = 18, startRow = row_opt + 9, rowNames = FALSE)
          
          test <- summary(tab)
          ctest <- data.frame(
            `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
            `Numberof Case` = test$n.cases,
            `Chisqure Statistic` = test$statistic,
            `df` = test$parameter,
            `p-value` = test$p.value
          )
          
          subtitle_str <- "<< 독립성검정 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, startRow = row_opt + 14)    
          writeDataTable(wb, sheet = sheet_name, ctest,
                         startCol = 18, startRow = row_opt + 15, rowNames = FALSE)        
          
          data <- as.data.frame(tab) %>% 
            select(a = 2, b = 1, n = Freq) 
          
          first <- data[1, 1] %>% as.character
          
          y <- data %>% 
            filter(a %in% first) %>% 
            select(b, n)
          
          y_lab <- y$b %>% rev() %>% as.character()
          y <- y$n %>% rev()
          
          y_cumsum <- cumsum(y)
          y_center <- y / 2
          
          y_pos <- numeric(length(y))
          for (j in seq(y)) {
            if (j == 1) {
              y_pos[j] <- y_center[j]
            } else {
              y_pos[j] <- y_cumsum[j-1] + y_center[j]
            }
            y_pos[j] <- y_pos[j] / sum(y)
          }
          
          suppressWarnings(
            contigency_plot <- data %>% 
              group_by(a) %>% 
              mutate(x_width = sum(n)) %>% 
              ggplot(aes(x = a, y = n)) +
              geom_col(aes(width = x_width, fill = b),
                       color = "white", size = 2, 
                       position = position_fill(reverse = FALSE)) +
              facet_grid(~ a, space = "free", scales = "free", switch = "x") +
              scale_x_discrete(name = predictor_variable) +
              scale_y_continuous(name = target_variable, breaks = y_pos, labels = y_lab) +
              labs(title = sprintf("%s ~ %s", target_variable, predictor_variable)) +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    strip.background = element_blank(),
                    panel.spacing = unit(0, "pt")) +
              hrbrthemes::theme_ipsum("NanumSquare") +
              hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
              theme(legend.position = "none",
                    panel.grid.major.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size = 12),
                    panel.spacing = unit(0, "pt"))            
          )
          
          row_bir_viz <- row_opt + 5 + NROW(tab_opt)
          
          image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          plot(contigency_plot)
          dev.off()
          
          subtitle_str <- "<< 분포 시각화 >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, 
                    startRow = row_opt + 19)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 18, startRow = row_opt + 20)
          
          total_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>%   
            group_by(target) %>%   
            summarise(n = sum(n)) %>% 
            summarise(positive_ratio = sum(ifelse(target == 1, n, 0)) / sum(n) * 100) %>% 
            pull()
          
          annotation <- data.frame(
            x = NCOL(tab),
            y = total_ratio,
            label = glue::glue("전체백분율({round(total_ratio, 2)})"))
          
          
          p_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            summarise(positive_ratio = round(sum(ifelse(target == 1, n, 0)) / sum(n) * 100, 2)) %>% 
            ggplot(aes_string(x = predictor_variable, y = "positive_ratio", group = 1)) + 
            geom_point(colour = "blue", size = 3) +
            ggrepel::geom_label_repel(aes_string(label = "positive_ratio"), colour = "blue", size = 3.5) +
            geom_line(colour = "darkgray", linetype = 2) +
            geom_hline(yintercept = total_ratio, color = "red") +
            ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
            labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
                 y = glue::glue("{target_variable} 백분율")) +
            hrbrthemes::theme_ipsum(base_family = "NanumSquare")
          
          image_name <- here::here("temp", glue::glue("bin_ratio_{runif(1)}.png"))          
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          print(p_ratio)
          dev.off()
          
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 25, startRow = row_opt + 20)   
        }
        
        title_str <- "4. Distribution with Information of Outlires"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_out)
        
        subtitle_str <- "<< 분포 시각화 >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                  startRow = row_out + 1)        
        
        image_name <- here::here("temp", glue::glue("plot_outlier_{runif(1)}.png"))          
        
        outlier_plot <- results_num[x, "viz_out"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400,
                        pointsize = 9)
        gridExtra::grid.arrange(outlier_plot[[1]])
        dev.off()
        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 9.43, height = 5.5, startCol = 1, 
                              row_out + 2)            
        
        if (verbose) {
          pb$tick()
        }        
      }
    )
  
  
  ## Save excel file
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  ## Remove image file
  unlink(here::here("temp"), recursive = TRUE)  
}

