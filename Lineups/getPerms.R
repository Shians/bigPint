getPerms <- function(N){
  x = 1:N
  x1 = combn(x, N/2) #how many ways can we take half the elements to form the 1st group
  NC = NCOL(x1)
  x2 = x1[, NC:1] # simplified way to generate the complementary groups that include values not in x1
  grp1 = t(x1[,1:(NC/2)]) # We only need half of the rows, the 2nd half containing the same set in reverse order
  grp2 = t(x2[,1:(NC/2)])
  allComb = cbind(grp1, grp2)
  return(allComb)
}
