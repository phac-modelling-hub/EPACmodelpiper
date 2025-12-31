#' Calculate transmissibility from other model parameters from values list
#'
#' Uses R0 derived for the hosp model; see https://gitlab.cscscience.ca/mhd/firm/EPACmodel_PandPrep
#'
#' @param values (list, named) Values list
#'
#' @returns Transmissibility value (numeric)
#' @export
calculate_transmissibility <- function(values){
  transmissibility_formula(
    rho = calculate_rho(values),
    pop = values$pop,
    R0 = values$R0,
    prop_hosp = values$prop_hosp,
    prop_nonhosp_death = values$prop_nonhosp_death,
    days_infectious_I_R = values$days_infectious_I_R,
    days_infectious_I_A = values$days_infectious_I_A,
    days_infectious_I_D = values$days_infectious_I_D
  )
}

#' Calculate spectral radius of a contact matrix
#'
#' @param values (list, named) Values list
#'
#' @returns Spectral radius of the contact matrix (numeric)
#' @export
calculate_rho <- function(values){
  max(abs(eigen(values$contact.pars$p.mat*values$contact.pars$c.hat, only.values = TRUE)$value))
}

#' Transmissiblity formula using model parameters
#' 
#' @param rho (numeric) Spectral radius of the contact matrix
#' @param pop (numeric) Population by age group
#' @param R0 (numeric) Average number of secondary cases from an index case in a wholly susceptible population
#' @param prop_hosp (numeric) Proportion of infections that are hospitalized
#' @param prop_nonhosp_death (numeric) Proportion of infections that are not hospitalized but are fatal
#' @param days_infectious_I_R (numeric) Infectious period for infections that simply recover
#' @param days_infectious_I_A (numeric) Infectious period for infections that are hospitalized
#' @param days_infectious_I_D (numeric) Infectious period for infections that are fatal (outside of hospital)
#'
#' @returns Transmissibility value (numeric)
#' @export
transmissibility_formula <- function(
    rho,
    pop,
    R0,
    prop_hosp, prop_nonhosp_death,
    days_infectious_I_R, days_infectious_I_A, days_infectious_I_D
){
  # calculate pop-averaged props used in average gamma calc
  h <- sum(prop_hosp*pop/sum(pop))
  d <- sum(prop_nonhosp_death*pop/sum(pop))

  # calculate avg infectious period
  gamma <- (1-h)*(1-d)*1/days_infectious_I_R + h*1/days_infectious_I_A + (1-h)*d*1/days_infectious_I_D

  # return transmissibility
  R0*gamma/rho
}
