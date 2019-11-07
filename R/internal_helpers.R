# Internal functions

## Check whether something is a whole number

is_whole_number <- function(x){
  if(is.integer(x)){
    TRUE
  } else if(is.numeric(x)){
    if((x%%1) == 0 ){
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

is_whole_positive <- function(x){
  if(is_whole_number(x) && x >= 0) TRUE else FALSE
}

