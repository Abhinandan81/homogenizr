
library(roxygen2)

is_homogeneous <- function(input_data_frame_field_names, mandatory_fields_vector, case_sensitive_check = TRUE, sequence_check = FALSE) {

  if(!case_sensitive_check){
    input_data_frame_field_names <- tolower(input_data_frame_field_names)
    mandatory_fields_vector <- tolower(mandatory_fields_vector)
  }

  field_index  <- 1

  while (field_index <= length(mandatory_fields_vector)) {

    mandatory_field_name <- mandatory_fields_vector[field_index]

    if(sequence_check){

      if (!(mandatory_field_name == input_data_frame_field_names[field_index])) {
        return(FALSE)
      }
    }else{

      if (!(mandatory_field_name %in% input_data_frame_field_names)) {
        return(FALSE)
      }
    }

    field_index <- field_index + 1
  }

  return(TRUE)
}
