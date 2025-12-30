#' Update values list with new parameters
#'
#' @param params (named vector) New parameters to update
#' @param model.name (character) Short name of model in [{EPACmodel}]
#' @param state (named vector) Initial state vector
#' @param pop (tibble) table of population by age group
#' @param age_param_names (character) Names of parameters to change by age. If NULL, don't update parameters by age.
#' @param disease (character) Short name of disease for which to update parameters by age. Must match an option available in [ageify_params]
#'
#' @returns
#' @export
update_values <- function(params, model.name, state, pop, age_param_names = NULL, disease = NULL){
    # start with defaults
    values = EPACmodel::get_default_values(model.name)

    # handle dynamic branching over params list
    # seems like this shouldn't be necessary
    # if dynamic branching works as you'd think it would...
    if(is.list(params)) params <- unlist(params)

    # update params from sample
    purrr::walk(
        names(params),
        \(name) values[[name]] <<- params[[name]]
    )

    # ageify params
    if(!is.null(age_param_names)) values <- ageify_params(values, disease, age_param_names)

    # update state
    values$state <- state

    # update population sizes
    values$pop <- pop$value

    # add contact parameters using updated population data setting weights
    values$contact.pars <- EPACmodel::mk_contact_pars(
        age.group.lower = seq(0, 80, by = 5),
        setting.weight = values$setting.weight,
        pop.new = pop$value
    )

    # calculate transmissibility from R0 and other (agefied) params
    values$transmissibility <- calculate_transmissibility(values)

    # generate flows by age
    values$flow <- EPACmodel:::calculate_flow_hosp(values)

    # validate flows
    if(any(values$flow < 0)) cli::cli_abort(
        "Negative values were detected in the flows for {.val {disease}}. Please inspect {.var values$flow} being computed in {.fn update_values}."
    )

    # return updated values
    values
}
