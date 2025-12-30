#' Add age-based heterogeneity to select parameters
#'
#' @param values (list, named) Values list
#' @param disease (character) Disease short name
#' @param param_names (character) list of parameters to adjust
#'
#' @returns A list matching the format of [values]
#' @export
ageify_params <- function(values, disease, param_names){
  if (!(disease %in% c("sarscov2", "sarscov1", "measles", "smallpox", "flu1918"))) cli::cli_abort(
    "Age-based parameter assumptions are not defined for disease {.val {disease}}. Please update the definition of {.fn ageify_params} to include age-based assumptions for the desired disease."
  )

  # generating age-based parameters
  # (multiplier corresponding to each age group)
  mult <- get_age_mult(disease)

  purrr::walk(
    param_names, # parameters to ageify
    \(name) values[[name]] <<- values[[name]] * mult
  )

  # return values list
  values
}

#' Get age-based multipliers for parameters
#'
#' @param disease (character) Disease short name
#'
#' @returns
#' @export
get_age_mult <- function(disease){
  # piecewise linear W-shaped curve for 1918 flu
  if (disease == "flu1918") {
    # anchor points for piecewise function,
    # using indices corresponding to each age group for x coordinate and target multipliers for y-coordinate
    anchor_x <- c(1,2,6,10,17)
    # anchor_y <- c(2.537577,20/42,462/672,20/42,2.537577)
    max_mult <- 1.589692
    anchor_y <- c(max_mult,20/42,462/672,20/42,max_mult)
    piecewise <- approxfun(anchor_x, anchor_y) # piecewise function
    mult <- piecewise(1:17)
    # sum(mult*hosp_dist)
  } else { # simple three age-group adjustment for everything else
    if (disease == "sarscov2") mult <- c(0.9, 1, 1.1)
    if (disease == "sarscov1") mult <- c(0.908858, 1, 1.1)
    if (disease == "measles") mult <- c(1.1, 0.96, 1)
    if (disease == "smallpox") mult <- c(100/95, 0.965319, 100/95)
    mult <- c(
      rep(mult[1], 4), # youth (0-19)
      rep(mult[2], 9), # adults (20-64)
      rep(mult[3], 4) # seniors (65+)
    )
  }
}
