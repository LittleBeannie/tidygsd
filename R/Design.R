#' Design class
#'
#' A class for clinical trial designs.
#'
#' @export
Design <- R6::R6Class(
  "Design",
  public = list(
    
    #' @field n_sim number of simulation
    n_sim = NULL,
    #' @field sample_size the sample size at final analysis
    sample_size = NULL,
    #' @field stratum_name the name of strata
    stratum_name = NULL,
    #' @field stratum_ratio the ratio between strata
    stratum_ratio = NULL,
    #' @field enroll number of simulation
    enroll = NULL,
    #' @field failure number of simulation
    failure = NULL,
    #' @field dropout number of simulation
    dropout = NULL,
    
    #' @description Validate input parameters.
    #' @param n_sim number of simulation
    #' @param sample_size sample size
    validate = function(n_sim, sample_size) {
      # validate n_sim
      if (!is.numeric(n_sim) | n_sim <= 0) {
        stop("`n_sim` must be positive integer.", call. = FALSE)
      }
      
      
      # validate sample_size
      if (!is.numeric(sample_size) | sample_size <= 0) {
        stop("`sample_size` must be positive integer.", call. = FALSE)
      }
    },
    
    #' @description initialize the design/simulation
    #' @param n_sim number of simulation
    #' @param sample_size the sample size at final analysis
    #' @return A new `Design` object.
    initialize = function(n_sim, sample_size) {
      self$validate(n_sim, sample_size)
      self$n_sim <- n_sim
      self$sample_size <- sample_size
    },
    
    #' @description Input enrollment rate over time
    #' @param name the label/name of the strata
    #' @param ratio the prevalence ratio between strata
    #' @return A new `Design` object.
    input_stratum = function(name, ratio) {
      self$stratum_name <- name
      self$stratum_ratio <- ratio
    },
    
    #' @description Input enrollment rate over time
    #' @param dist R6 class to define the enrollment distribution
    #' @return A new `Design` object.
    input_enroll = function(dist) {
      self$enroll <- dist
      invisible(self)
    },
    
    #' @description Input failure rate over time 
    #' @param arm arm label
    #' @param stratum stratum label
    #' @param dist R6 class to define the enrollment distribution
    #' @return A new `Design` object.
    input_failure = function(arm, stratum, dist) {
      temp <- dist %>% summary() %>% mutate(arm = arm, stratum = stratum)
      self$failure <- rbind(self$failure, temp)
      invisible(self)
    },
    
    #' @description Input dropout rate over time 
    #' @param arm arm label
    #' @param stratum stratum label
    #' @param dist R6 class to define the enrollment distribution
    #' @return A new `Design` object.
    input_dropout = function(arm, stratum, dist) {
      temp <- dist %>% summary() %>% mutate(arm = arm, stratum = stratum)
      self$dropout <- rbind(self$dropout, temp)
      invisible(self)
    }
  )
)

#' Input enrollment rate
#'
#' @param x the object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
input_stratum <- function(x, ...) {
  UseMethod("input_stratum", x)
}

#' @export
input_stratum.Design <- function(x, name, ratio, ...) {
  x$input_stratum(name, ratio)
  return(x)
}



#' Input enrollment rate
#'
#' @param x the object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
input_enroll <- function(x, ...) {
  UseMethod("input_enroll", x)
}

#' @export
input_enroll.Design <- function(x, dist, ...) {
  x$input_enroll(dist)
  return(x)
}

#' Input failure rate
#'
#' @param x An object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
input_failure <- function(x, ...) {
  UseMethod("input_failure", x)
}

#' @export
input_failure.Design <- function(x, arm, stratum, dist, ...) {
  x$input_failure(arm, stratum, dist)
  return(x)
}

#' Input dropout rate
#'
#' @param x An object
#' @param ... Additional arguments (not used).
#'
#' @return TBA
#'
#' @export
input_dropout <- function(x, ...) {
  UseMethod("input_dropout", x)
}

#' @export
input_dropout.Design <- function(x, arm, stratum, dist, ...) {
  x$input_dropout(arm, stratum, dist)
  return(x)
}

