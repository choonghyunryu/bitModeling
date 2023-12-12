yardstick_event_level <- function () {
  opt <- getOption("yardstick.event_first")
  if (is.null(opt)) {
    return("first")
  }
  
  if (identical(opt, TRUE)) {
    "first"
  }
  else if (identical(opt, FALSE)) {
    "second"
  }
  else {
    abort("Global option `yardstick.event_first` is set, but is not `TRUE` or `FALSE`.")
  }
}


validate_event_level <- function (event_level) {
  if (identical(event_level, "first")) {
    return(invisible())
  }
  if (identical(event_level, "second")) {
    return(invisible())
  }
  rlang::abort("`event_level` must be 'first' or 'second'.")
}


#' @export
#' @importFrom yardstick new_class_metric
f_meas_05 <- function(data, ...) {
  UseMethod("f_meas_05")
}
f_meas_05 <- yardstick::new_class_metric(
  f_meas_05,
  direction = "maximize"
)

#' @export
#' @importFrom yardstick f_meas_vec metric_summarizer
f_meas_05.data.frame <- function(data,
                                 truth,
                                 estimate,
                                 beta = 0.5,
                                 estimator = NULL,
                                 na_rm = TRUE,
                                 case_weights = NULL,
                                 event_level = yardstick_event_level(),
                                 ...) {
  yardstick::metric_summarizer(
    metric_nm = "f_meas_05",
    metric_fn = yardstick::f_meas_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level,
    metric_fn_options = list(beta = beta)
  )
}


#' @export
#' @importFrom yardstick new_class_metric
f_meas_2 <- function(data, ...) {
  UseMethod("f_meas_2")
}
f_meas_2 <- yardstick::new_class_metric(
  f_meas_2,
  direction = "maximize"
)

#' @export
#' @importFrom yardstick f_meas_vec metric_summarizer
f_meas_2.data.frame <- function(data,
                                truth,
                                estimate,
                                beta = 2,
                                estimator = NULL,
                                na_rm = TRUE,
                                case_weights = NULL,
                                event_level = yardstick_event_level(),
                                ...) {
  yardstick::metric_summarizer(
    metric_nm = "f_meas_2",
    metric_fn = yardstick::f_meas_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level,
    metric_fn_options = list(beta = beta)
  )
}



#' @export
#' @importFrom yardstick new_class_metric
gmean <- function(data, ...) {
  UseMethod("gmean")
}
gmean <- yardstick::new_class_metric(
  gmean,
  direction = "maximize"
)


#' @export
#' @importFrom yardstick f_meas_vec metric_summarizer
gmean.data.frame <- function(data,
                             truth,
                             estimate,
                             estimator = NULL,
                             na_rm = TRUE,
                             case_weights = NULL,
                             event_level = yardstick_event_level(),
                             ...) {
  yardstick::metric_summarizer(
    metric_nm = "gmean",
    metric_fn = gmean_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level
  )
}


#' @export
#' @importFrom yardstick finalize_estimator metric_vec_template
#' @importFrom rlang check_dots_empty
gmean_vec <- function (truth, estimate, estimator = NULL, na_rm = TRUE, 
                       case_weights = NULL, event_level = yardstick_event_level(), ...) 
{
  estimator <- yardstick::finalize_estimator(truth, estimator)
  
  gmean_impl <- function(truth, estimate, ..., case_weights = NULL) {
    rlang::check_dots_empty()
    data <- yardstick_table(truth, estimate, case_weights = case_weights)
    gmean_table_impl(data, estimator, event_level)
  }
  
  yardstick::metric_vec_template(metric_impl = gmean_impl, truth = truth, 
                      estimate = estimate, na_rm = na_rm, estimator = estimator, 
                      case_weights = case_weights, cls = "factor")
}

gmean_table_impl <- function (data, estimator, event_level) {
  if (is_binary(estimator)) {
    gmean_binary(data, event_level)
  }
  else {
    w <- get_weights(data, estimator)
    out_vec <- gmean_multiclass(data, estimator)
    stats::weighted.mean(out_vec, w, na.rm = TRUE)
  }
}


gmean_multiclass <- function (data, estimator) {
  n <- sum(data)
  tp <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn <- n - (tpfp + tpfn - tp)
  fp <- tpfp - tp
  numer <- tn
  denom <- tn + fp
  undefined <- denom <= 0
  
  if (any(undefined)) {
    counts <- tpfn - tp
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_spec_undefined_multiclass(events, counts)
    numer[undefined] <- NA_real_
    denom[undefined] <- NA_real_
  }
  
  if (is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }
  
  spec <- numer/denom  
  
  
  numer <- diag(data)
  denom <- colSums(data)
  undefined <- denom <= 0
  if (any(undefined)) {
    counts <- rowSums(data) - numer
    counts <- counts[undefined]
    events <- colnames(data)[undefined]
    warn_sens_undefined_multiclass(events, counts)
    numer[undefined] <- NA_real_
    denom[undefined] <- NA_real_
  }
  
  if (is_micro(estimator)) {
    numer <- sum(numer, na.rm = TRUE)
    denom <- sum(denom, na.rm = TRUE)
  }
  
  sens <- numer/denom
  
  sqrt(spec * sens)
}


gmean_binary <- function (data, event_level) {
  negative <- neg_val(data, event_level)
  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])
  undefined <- denom <= 0
  if (undefined) {
    positive <- setdiff(colnames(data), negative)
    count <- data[negative, positive]
    warn_gmean_undefined_binary(positive, count)
    return(NA_real_)
  }
  spec <- numer/denom
  
  relevant <- pos_val(data, event_level)
  numer <- sum(data[relevant, relevant])
  denom <- sum(data[, relevant])
  undefined <- denom <= 0
  if (undefined) {
    not_relevant <- setdiff(colnames(data), relevant)
    count <- data[relevant, not_relevant]
    warn_gmean_undefined_binary(relevant, count)
    return(NA_real_)
  }
  sens <- numer/denom  
  
  sqrt(spec * sens)
}



neg_val <- function (xtab, event_level) {
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }
  if (is_event_first(event_level)) {
    colnames(xtab)[[2]]
  }
  else {
    colnames(xtab)[[1]]
  }
}


pos_val <- function (xtab, event_level) {
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }
  if (is_event_first(event_level)) {
    colnames(xtab)[[1]]
  }
  else {
    colnames(xtab)[[2]]
  }
}


warn_gmean_undefined_binary <- function (event, count) {
  message <- paste0("While computing binary `sens()`, no true events were detected ", 
                    "(i.e. `true_positive + false_negative = 0`). ", "\n", 
                    "Sensitivity is undefined in this case, and `NA` will be returned.", 
                    "\n", "Note that ", count, " predicted event(s) actually occured for the problematic ", 
                    "event level, '", event, "'.")
  warn_gmean_undefined(message = message, events = event, counts = count, 
                       class = "yardstick_warning_gmean_undefined_binary")
}


warn_gmean_undefined <- function (message, events, counts, ..., class = character()) {
  rlang::warn(message = message, class = c(class, "yardstick_warning_gmean_undefined"), 
              events = events, counts = counts, ...)
}


yardstick_table <- function (truth, estimate, ..., case_weights = NULL) {
  rlang::check_dots_empty()
  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }
  if (!is.factor(truth)) {
    abort("`truth` must be a factor.", .internal = TRUE)
  }
  if (!is.factor(estimate)) {
    abort("`estimate` must be a factor.", .internal = TRUE)
  }
  levels <- levels(truth)
  n_levels <- length(levels)
  if (!identical(levels, levels(estimate))) {
    abort("`truth` and `estimate` must have the same levels in the same order.", 
          .internal = TRUE)
  }
  if (n_levels < 2) {
    abort("`truth` must have at least 2 factor levels.", 
          .internal = TRUE)
  }
  if (is.null(case_weights)) {
    out <- table(Prediction = estimate, Truth = truth)
    out <- unclass(out)
    storage.mode(out) <- "double"
  }
  else {
    out <- hardhat::weighted_table(Prediction = estimate, 
                                   Truth = truth, weights = case_weights)
  }
  out
}


is_class_pred <- function (x) {
  inherits(x, "class_pred")
}


is_binary <- function (x) {
  identical(x, "binary")
}


is_event_first <- function (event_level) {
  validate_event_level(event_level)
  identical(event_level, "first")
}


as_factor_from_class_pred <- function (x) {
  if (!rlang::is_installed("probably")) {
    rlang::abort(paste0("A <class_pred> input was detected, but the probably package ", 
                        "isn't installed. Install probably to be able to convert <class_pred> ", 
                        "to <factor>."))
  }
  probably::as.factor(x)
}


warn_spec_undefined_multiclass <- function (events, counts) {
  message <- paste0("While computing multiclass `spec()`, some levels had no true negatives ", 
                    "(i.e. `true_negative + false_positive = 0`). ", "\n", 
                    "Specificity is undefined in this case, and those levels will be removed from the averaged result.", 
                    "\n", "Note that the following number of predicted negatives actually occured for each problematic event level:", 
                    "\n", paste0("'", events, "': ", counts, collapse = "\n"))
  warn_spec_undefined(message = message, events = events, counts = counts, 
                      class = "yardstick_warning_spec_undefined_multiclass")
}

