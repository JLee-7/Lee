#' @title Velocity Conversion
#' @description Function allows users to select an input and output unit of velocity, the current supported units are miles per hour (mph), kilometers per hour (kph), and meters per second (m/s)
#' @param veloIn Choose unit of measurement for input, mph (miles per hour), kph (kilometers per hour), m/s (meters per second)
#' @param veloOut Choose unit of measurement for output, mph (miles per hour), kph (kilometers per hour), m/s (meters per second)
#' @param velo Numeric value for input velocity
#' @return numeric value in the measurement chosen by veloout
#' @examples
#' VeloConvert("mph","kph",60)
#'

VeloConvert <-  function(veloIn, veloOut, velo){
  if (veloIn == "mph"){
    if (veloOut == "mph"){
      velo2 = velo
    }
    else if (veloOut == "kph"){
      velo2 = velo * 1.609344
    }
    else if (veloOut == "m/s"){
      velo2 = velo * 0.447
    }
  }
  else if (veloIn == "m/s"){
    if (veloOut == "m/s"){
      velo2 = velo
    }
    else if (veloOut == "mph"){
      velo2 = velo * 2.23694
    }
    else if (veloOut == "kph"){
      velo2 = velo * 3.6
    }
  }
  else if (veloIn == "kph"){
    if (veloOut == "kph"){
      velo2 = velo
    }
    else if (veloOut == "mph"){
      velo2 = velo / 1.609344
    }
    else if (veloOut == "m/s"){
      velo2 = velo * (5/18)
    }
  }
  return(velo2)
}
