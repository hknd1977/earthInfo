# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about
# package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring:
# Build and Reload Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'

#' Small function to minimize GCV value of an earth model.
#'
#' @param formula standart R formula (Eg: Spescies~ .).
#' @param data data frame .
#' @param max_degree using to tune model.
#' @return Returns a list consists minimum GCV value, maximum interactions and optimum degree of the model.
#' @examples
#' library(earthInfo)
#' data("iris")
#' fit <- fitGCV(Sepal.Width~., data=iris, maxDegree = 10)
#' @export
fitGCV <- function(formula, data, max_degree = 3, ...) {

    suppressMessages(require(caret, quietly = TRUE))
    suppressMessages(require(earth, quietly = TRUE))
    for (degree in 1:max_degree) {
        fitTemp <- earth(formula, data = data, degree = degree, nk = 1000, keepxy = TRUE)
        maxInter <- sum(rowSums(as.data.frame(fitTemp$cuts[fitTemp$selected.terms, ] > 0)) >= degree)
        if (maxInter < 1)
            break
        cat("degree : ", degree, "Max Interaction : ", maxInter, "GCV : ", fitTemp$gcv, "\n")
        if (!exists("fit", inherits = FALSE) || (fit > fitTemp$gcv)) {
            fit <- fitTemp$gcv
            Interaction <- maxInter
            last <- list(degree = degree, Interaction = Interaction, metric = fitTemp$gcv,
                         model = fitTemp)
        }
    }
    return(last)
}
