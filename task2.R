func <- function(d) {
  res <- d[[1]]
  i <- 2
  out <- c()
  res$mean_temp <- res$temp
  repeat {
    for (elem in res$id) {
      if (!(elem %in% data[[i]]$id)) {
        out <- c(out, elem)
      } else {
        res$mean_temp[[which(res$id == elem)[[1]]]] <- res$mean_temp[[which(res$id == elem)[[1]]]] + data[[i]]$temp[[which(data[[i]]$id == elem)[[1]]]]
      }
    }    
    i <- i + 1
    if (i > 7) {
      res <- res[!(res$id %in% out),]
      res$mean_temp <- res$mean_temp / 7
      res1 <- data.frame(id = res$id, mean_temp = res$mean_temp)
      return(res1)
    }
  }
}

#load("data.RData")
#print(func(data))
