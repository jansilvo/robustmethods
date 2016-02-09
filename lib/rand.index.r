rand.index <- function(true.labels, estimated.labels) {
  tab <- table(true.labels, estimated.labels)
  nidot <- apply(tab, 1, sum)
  ndotj <- apply(tab, 2, sum)
  n <- sum(tab)
  num <- sum(choose(tab, 2)) - 
    sum(choose(nidot, 2)) * sum(choose(ndotj, 2)) / choose(n, 2)
  denom <- 0.5 * (sum(choose(nidot, 2)) + sum(choose(ndotj, 2))) -
    sum(choose(nidot, 2)) * sum(choose(ndotj, 2)) / choose(n, 2)
  return(num/denom)
}

choose <- function(n, k, order.matters = F)
{
  if(length(n) < length(k))
    n <- rep(n, length = length(k))
  if(length(n) > length(k))
    k <- rep(k, length = length(n))
  which <- !is.na(n) & !is.na(k)
  if(sum(which) == 0) {
    warning("All missing values in choose")
    return(rep(NA, length(n)))
  }
  if(any(!which)) {
    n <- n[which]
    k <- k[which]
  }
  if(any((n != round(n)) | k != round(k))) {
    warning("n and k are not integers, will be rounded")
    n <- round(n)
    k <- round(k)
  }
  result <- rep(0, length(n))
  zeros <- (k < 0) | (n < k)
  if(any(zeros)) {
    n <- n[!zeros]
    k <- k[!zeros]
  }
  result[!zeros] <- round(exp(lgamma(n + 1) - lgamma(n - k + 1) - {
    if(order.matters)
      0
    else lgamma(k + 1)
  }
                              ))
  if(any(!which)) {
    y <- rep(NA, length(which))
    y[which] <- result
    y
  }
  else result
}
