#' @title HomogenizR
#' @name Homogeni
#' @author Abhinandan Satpute
#'
#' @description  Check whether the input field(column) names vector has all mandatory fileds or not
#'
#' @param input_field_names_vector vector of field names extracted from dataframe, list or vector
#' @param mandatory_fields_vector vector of mandatory fields
#' @param case_sensitive_check to specify the case sensitveness criteria while matching field names
#' @param sequence_check to specify whether to take the order of input fileds into the consideration or not
#'
#' @return returns TRUE if the \code{input_field_names_vector} has all \code{mandatory_fields_vector} else FALSE


is_homogeneous <- function(input_field_names_vector, mandatory_fields_vector, case_sensitive_check = TRUE, sequence_check = FALSE) {

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
