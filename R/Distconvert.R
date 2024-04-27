#' @title Distance Conversion
#' @description Function allows users to select an input and output unit of Distance, the current supported units are inches(in), feet (ft), yards (yd), miles (mi), millimeters (mm), centimeters (cm), meters (m), and kilometers (km)
#' @param DistIn Choose unit of measurement for input, in (inches), ft (feet), yd (yards), mi (miles), mm (millimeters), cm (centimeters), m (meters), km (kilometers)
#' @param DistOut Choose unit of measurement for output, in (inches), ft (feet), yd (yards), mi (miles), mm (millimeters), cm (centimeters), m (meters), km (kilometers)
#' @param dist1 Numeric value for input distance
#' @return numeric value in the measurement chosen by DistOut
#' @examples
#' DistConvert("km","mi",100)
#'

DistConvert <- function(DistIn, DistOut, dist1){
  if (DistIn == "in"){
    if (DistOut == "in"){
      dist2 = dist1
    }
    else if (DistOut == "ft"){
      dist2 = dist1 / 12
    }
    else if (DistOut == "yd"){
      dist2 = dist1 / 12
    }
    else if (DistOut == "mi"){
      dist2 = dist1 / 63360
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 25.4
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 2.54
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 0.0254
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.0000254
    }
  }
  else if (DistIn == "ft"){
    if (DistOut == "ft"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 12
    }
    else if (DistOut == "yd"){
      dist2 = dist1 / 3
    }
    else if (DistOut == "mi"){
      dist2 = dist1 / 5280
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 304.8
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 30.48
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 0.3048
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.0003048
    }
  }
  else if (DistIn == "yd"){
    if (DistOut == "yd"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 36
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 3
    }
    else if (DistOut == "mi"){
      dist2 = dist1 / 1760
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 914.4
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 91.44
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 0.9144
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.0009144
    }
  }
  else if (DistIn == "mi"){
    if (DistOut == "mi"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 63360
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 5280
    }
    else if (DistOut == "yd"){
      dist2 = dist1 *1760
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 1609344
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 160934.4
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 1609.344
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 1.609344
    }
  }
  else if (DistIn == "mm"){
    if (DistOut == "mm"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1  * 0.039370078740157
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 0.0032808399
    }
    else if (DistOut == "yd"){
      dist2 = dist1 * 0.010936133
    }
    else if (DistOut == "mi"){
      dist2 = dist1 * 0.0000006214
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 0.1
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 0.001
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.000001
    }
  }
  else if (DistIn == "cm"){
    if (DistOut == "cm"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 0.39370078740157
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 0.032808398950131
    }
    else if (DistOut == "yd"){
      dist2 = dist1 * 0.010936133
    }
    else if (DistOut == "mi"){
      dist2 = dist1 * 0.0000062137
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 10
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 0.01
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.00001
    }
  }
  else if (DistIn == "m"){
    if (DistOut == "m"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 39.3700787402
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 3.280839895
    }
    else if (DistOut == "yd"){
      dist2 = dist1 * 1.0936132983
    }
    else if (DistOut == "mi"){
      dist2 = dist1 * 0.0006213712
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 1000
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 100
    }
    else if (DistOut == "km"){
      dist2 = dist1 * 0.001
    }
  }
  else if (DistIn == "km"){
    if (DistOut == "km"){
      dist2 = dist1
    }
    else if (DistOut == "in"){
      dist2 = dist1 * 39370.1
    }
    else if (DistOut == "ft"){
      dist2 = dist1 * 3280.8398950131
    }
    else if (DistOut == "yd"){
      dist2 = dist1 * 1093.6132983377
    }
    else if (DistOut == "mi"){
      dist2 = dist1 * 0.6213711922
    }
    else if (DistOut == "mm"){
      dist2 = dist1 * 1000000
    }
    else if (DistOut == "cm"){
      dist2 = dist1 * 100000
    }
    else if (DistOut == "m"){
      dist2 = dist1 * 1000
    }
  }
  return(dist2)
}
