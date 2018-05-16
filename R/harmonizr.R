#' @title Harmonizr
#' @name Harmonizr
#' @author Abhinandan Satpute
#'
#' @description  Check whether the input field(column) names vector has all mandatory fileds or not
#'
#' @param input_field_names_vector vector of field names extracted from dataframe, list or vector
#' @param mandatory_fields_vector vector of mandatory fields
#' @param case_sensitive_check `TRUE` to keep the case sensitive check on while matching the input fields with the mandatory fields
#' @param sequence_check `TRUE` to check if the order of input fileds is exactly same as mandatory fields
#'
#' @return returns TRUE if the \code{input_field_names_vector} has all \code{mandatory_fields_vector} else FALSE
#' @examples
#' input_fields <- c("platform", "process", "Site", "Product", "Year")
#' reference_fields <- c("Platform", "Process", "Site", "Product")
#' 
#' consistency_status <- is_consistent(input_fields, reference_fields, case_sensitive_check = FALSE)
#' consistency_status
#' TRUE
#' 
#' consistency_status <- is_consistent(input_fields, reference_fields)
#' consistency_status
#' FALSE
#' 
#' is_consistent(input_fields, reference_fields, case_sensitive_check = FALSE, sequence_check = TRUE)
#' FALSE
#' 


is_consistent <- function(input_field_names_vector, mandatory_fields_vector, case_sensitive_check = TRUE, sequence_check = FALSE) {

  if(!case_sensitive_check){
    input_field_names_vector <- tolower(input_field_names_vector)
    mandatory_fields_vector <- tolower(mandatory_fields_vector)
  }

  field_index  <- 1

  while (field_index <= length(mandatory_fields_vector)) {

    mandatory_field_name <- mandatory_fields_vector[field_index]

    if(sequence_check){

      if (!(mandatory_field_name == input_field_names_vector[field_index])) {
        return(FALSE)
      }
    }else{

      if (!(mandatory_field_name %in% input_field_names_vector)) {
        return(FALSE)
      }
    }

    field_index <- field_index + 1
  }

  return(TRUE)
}
