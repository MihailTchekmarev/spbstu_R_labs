tonum <- function(elem) {
  if (suppressWarnings(is.na(as.numeric(gsub(" ", "", elem))))) {
    return(elem)
  } else {
    return(as.numeric(gsub(" ", "", elem)))
  }
}

func <- function(d) {
  c <- ncol(d)
  i <- 1
  res <- data.frame()
  repeat {
    res <- rbind(res, lapply(d[i, ], tonum))
   
    i <- i + 1
    if (i > c) {
      break
    }
  }
  return(res)
}

#data <- read.csv("test_data_01.csv")
#func(data)
