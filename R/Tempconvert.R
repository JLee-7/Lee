#' @title Temperature Conversion
#' @description Function allows users to select an input and output unit of temperature, the current supported units are Farenheit, Celsius, and Kelvin
#' @param tempIn Choose unit of measurement for input, F (farenheit), C (celsius), K (kelvin)
#' @param tempOut Choose unit of measurement for output, F (farenheit), C (celsius), K (kelvin)
#' @param temp Numeric value for input temperature
#' @return numeric value in measurement chosen by tempout
#' @examples
#' TempConvert("F","C",212)
#'

TempConvert <-  function(tempIn,tempOut,temp){
  if (tempIn == "F"){
    if (tempOut == "F"){
      temp2 = temp
    }
    else if (tempOut == "C"){
      temp2 = (temp - 32)*5/9
    }
    else if (tempOut == "K"){
      temp2 = ((temp - 32)*5/9) + 273.15
    }
  }
  else if (tempIn == "C"){
    if (tempOut == "C"){
      temp2 = temp
    }
    else if (tempOut == "K"){
      temp2 = temp + 273.15
    }
    else if (tempOut == "F"){
      temp2 = ((9/5)*temp)+32
    }
  }
  else if (tempIn == "K"){
    if (tempOut == "K"){
      temp2 = temp
    }
    else if (tempOut == "C"){
      temp2 = temp - 273.15
    }
    else if (tempOut == "F"){
      temp2 = ((temp - 273.15)*(9/5))+32
    }
  }
  return(temp2)
}


