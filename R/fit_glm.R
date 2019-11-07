# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about
# package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring:
# Build and Reload Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

#' Small function to maximize  accuracy of an binomial glm earth model.
#'
#' @param formula standart R formula (Eg: Spescies~ .).
#' @param data data frame .
#' @param max_degree using to tune model.
#' @param method use "acc" for accuracy  or "auc" for are under the curve.
#' @return Returns a list consists maximum ac value, maximum interactions and optimum degree of the model.
#' @examples
#' library(earthInfo)
#' data("iris")
#' fit <- fit_GCV(Sepal.Width~., data=iris, maxDegree = 10)
#' @export
fit_glm <- function(formula, data, max_degree = 9, method="acc", ...) {
  suppressMessages(require(caret, quietly = TRUE))
  suppressMessages(require(earth, quietly = TRUE))
  for (degree in 1:max_degree) {
      fitTemp <- earth(formula, data = data, degree = degree,
                       nk = 1000, glm=list(family=binomial),keepxy = TRUE)
      suppressMessages(tempGOF <- earth_GOF(fitTemp))
      auc <- as.numeric(tempGOF$auc)
      acc <- as.numeric(tempGOF$confusion_matrix$overall["Accuracy"])
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
