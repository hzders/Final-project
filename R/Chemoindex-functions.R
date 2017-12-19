#code

getci <- function(a){
  x <- c() 
  for (i in 1:nrow(a)){
    if (a[i,"Region.A"] < 0 | a[i,"Region.B"] < 0 | a[i,"origin"] < 0) {
      stop("unreasonable number in the dataset")
    } else {
      x[i] <- (a[i,"Region.A"] - a[i,"Region.B"]) / (sum(a[i,"Region.A"] + a[i,"Region.B"] + a[i,"origin"]))
    } 
  }
  a$CI <- x
  return (a)
  
}

moreci <- function(a){
  b <- getci(a)
  #Define a function called stderr to calculate standard error
  stderr <- function(x) {
    sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
  }
  if ("CI" %in% names(b)) {   #Users need to have a column called CI as the result from the function1
    mychemnames <- unique(b$Chemicals) #extract the non-redundent names from the col1 which are the chemicals tested
    myrows <- length(mychemnames)
    result <- matrix( ,nrow = myrows, ncol=3)
    colnames(result) <- c("Mean", "SD", "SE")
    rownames(result) <- mychemnames
    for (i in (1: length(mychemnames))){
      result[i,1]  <- mean(b[b$Chemicals == mychemnames[i],][,"CI"])
      result[i,2]  <- sd(b[b$Chemicals == mychemnames[i],][,"CI"])
      result[i,3]  <- stderr(b[b$Chemicals == mychemnames[i],][,"CI"])
    } #loop through the each chemical which can contain multiple samples
    
  } else {
    stop ("Chemo Index is not found. Run function1")
  }
  return (result)
}

barci <- function(a){
  b <- moreci(a)
  return(barplot(b[,1], main="Chemotaxis Index", xlab = "Chemical",
                 ylim = c(-1,1), ylab = "CI",  ))
}

