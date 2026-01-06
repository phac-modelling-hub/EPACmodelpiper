#' Update flow in values list
#' 
#' @param values (list) List of value for input into the model
#' 
#' @returns Values (list)
#' @export
update_flow <- function(values){
    # calculate transmissibility from R0 and other (agefied) params
    values$transmissibility <- calculate_transmissibility(values)

    # generate flows by age
    values$flow <- EPACmodel:::calculate_flow_hosp(values)

    # validate flows
    if(any(values$flow < 0)) cli::cli_abort(
        "Negative values were detected in the flows for {.val {disease}}. Please inspect {.var values$flow} being computed in {.fn update_values}."
    )
    
    values
}