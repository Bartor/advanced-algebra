projection <- function(u, v) {
  if (all(u == 0)) u else sum(v * u) / sum(u * u) * u
}

normalize <- function(vector) {
  vector / sqrt(sum(vector ^ 2))
}

ortogonize <- function(baseMatrix, normal = FALSE) {
  resultMatrix <- baseMatrix
  for (vec in 2:ncol(baseMatrix)) {
    result <- baseMatrix[,vec]
    for (proj in 1:(vec - 1)) {
      result = result - projection(resultMatrix[,proj], result)
    }
    resultMatrix[,vec] <- if (normal) normalize(result) else result
  }
  if (normal) {
    resultMatrix[,1] <- normalize(resultMatrix[,1])
  }
  resultMatrix
}

print(ortogonize(matrix(c(3, 1, 2, 2), 2, 2)))


