#' Simulate stratified time-to-event outcome randomized trial
#'
#' @param x the object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
generate_tte <- function(x, ...) {
  UseMethod("generate_tte", x)
}

#' @export
generate_tte <- function(x, block, ...) {
  
  ans <- simtrial::simPWSurv(
    n = x$sample_size,
    strata = tibble(Stratum = x$failure$stratum %>% unique(), p = x$prevalence_ratio),
    block = block,
    enrollRates = x$enroll %>% 
      summary() %>% 
      mutate(duration = diff(c(0, (x$enroll %>% summary())$end_time))) %>% 
      select(duration, rate) %>% 
      mutate(Stratum = "All"),
    failRates = x$failure %>% 
      group_by(arm, stratum) %>% 
      mutate(duration = diff(c(0, end_time))) %>% 
      select(stratum, period_order, arm, duration, rate) %>% 
      rename(Stratum = stratum, period = period_order, Treatment = arm),
    dropoutRates = x$dropout %>% 
      group_by(arm, stratum) %>% 
      mutate(duration = diff(c(0, end_time))) %>% 
      select(stratum, period_order, arm, duration, rate) %>% 
      rename(Stratum = stratum, period = period_order, Treatment = arm)
  )
  
  return(ans)
}