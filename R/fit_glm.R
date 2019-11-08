# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about
# package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring:
# Build and Reload Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

#' Small function to maximize  accuracy of an binomial glm earth model.
#'
#' @param formula standart R formula (Eg: Spescies~ .).
#' @param data data frame .
#' @param maxDegree using to tune model.
#' @param method use "acc" for accuracy  or "auc" for are under the curve.
#' @return Returns a list consists maximum ac value, maximum interactions and optimum degree of the model.
#' @examples
#' library(earthInfo)
#' data("iris")
#' fit <- fitGlm(Sepal.Width~., data=iris, maxDegree = 10)
#' @export
fitGlm<- function(formula, data, maxDegree = 9, method="acc", ...) {
  suppressMessages(require(caret, quietly = TRUE))
  suppressMessages(require(earth, quietly = TRUE))
  for (degree in 1:maxDegree) {
      fitTemp <- earth(formula, data = data, degree = degree,
                       nk = 1000, glm=list(family=binomial),keepxy = TRUE)
      suppressMessages(tempGOF <- earthGOF(fitTemp))
      auc <- as.numeric(tempGOF$auc)
      acc <- as.numeric(tempGOF$confusionMatrix$overall["Accuracy"])
      if (method=="acc") {
        metric <- acc
      } else {
        metric <- auc
      }
      maxInter <- sum(rowSums(as.data.frame(fitTemp$cuts[fitTemp$selected.terms, ] > 0)) >= degree)
      if (maxInter < 1)
        break
      cat("degree : ", degree, "Max Interaction : ", maxInter, "metric (", method,") : ",metric, "\n")
      if (!exists("fit", inherits = FALSE) || (fit < metric)) {
        fit <- metric
        last <- list(degree = degree, Interaction = maxInter, metric = metric,
                     model = fitTemp)
      }
  }
  return(last)
}
