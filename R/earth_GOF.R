#' Goodness of fitting values of an earth model.
#'
#' @param model an earth model (keepxy argument should be set TRUE).
#' @return The goodness of fitting  values of \code{model}.
#' @examples
#' # an example for normal lm earth model
#'
#' library(earth)
#' library(earthInfo)
#' data("iris")
#' fit <- earth(Sepal.Width~., data=iris, degree=2,keepxy = TRUE)
#' gof <- earthGOF(fit)
#' summary(gof,digits = 3)
#'
#'
#'
#' # an example for a binomial glm earth model
#'
#' library(earth)
#' library(earthInfo)
#' data(etitanic)
#' fit <- earth(survived~., data=etitanic, degree =2, glm = list(family=binomial),keepxy = TRUE)
#' gof <- earthGOF(fit)
#' summary(gof)
#' @export
earthGOF <- function(model) {
    suppressMessages(require(caret, quietly = TRUE))
    suppressMessages(require(pROC, quietly = TRUE))
    if (class(model) != "earth") {
        stop("wrong model type.")
    }
    if (is.null(model$y)) {
        stop("please use keepxy when you are fitting the model")
    }
    # if the model type is lm earth
    if (is.null(model$glm.list)) {
        y <- model$y
        n <- nrow(model$data)
        k <- length(model$selected.terms)
        bx <- model.matrix(model)[, -1]
        model.lm <- lm(y ~ bx)
        modelSummary <- summary(model.lm)
        corTest <- cor.test(y, predict(model))
        pearsonCorrelation <- as.numeric(round(cor(y, predict(model))))
        error <- y - predict(model)
        sdRatio <- sd(error)/sd(y)
        coefOfVariation <- sd(error) * 100/mean(y)
        RMSE <- sqrt(mean(error^2))
        ME <- mean(error)
        RAE <- sqrt(sum(error^2)/sum(y^2))
        MAPE <- mean(abs(error/y)) * 100
        MAD <- mean(abs(error))
        Rsq <- as.numeric(1 - (sum(error^2)/(var(y) * (n - 1))))
        adjRSQ <- as.numeric(1 - ((1 - Rsq) * (n - 1)/(n - k - 1)))
        AIC <- n * log(mean(error^2), base = exp(1)) + 2 * k
        AICc <- n * log(mean(error^2), base = exp(1)) + (2 * k) + (2 * k * (k + 1)/(n - k - 1))
        # plot(y,predict(model))
        info <- structure(list(n = n,
                               k = k,
                               sdRatio = sdRatio,
                               coef_of_var = coefOfVariation,
                               pCorr = pearsonCorrelation,
                               RMSE = RMSE,
                               ME = ME,
                               RAE = RAE,
                               MAPE = MAPE,
                               MAD = MAD,
                               RSQ = Rsq,
                               adjRSQ = adjRSQ,
                               AIC = AIC,
                               AICc = AICc,
                               GCV = model$gcv,
                               corTest = corTest,
                               modelSummary = modelSummary), class = "infoEarth")
    } else {

        ## if the model type is a binomial glm earth --------------------------------------------------------
        if (length(model$levels) > 2) {
            stop("only able to process binomial earth-glm model")
        }
        suppressMessages(earthROC <- roc(predictor = as.vector(predict(model,type="response")), response = as.vector(model$y)))
        auc <- auc(earthROC)
        allCoords <- coords(earthROC, seq(0.05, 0.95, by = 0.1), transpose = FALSE)
        allCoords$sum <- allCoords[, 2] + allCoords[, 3]
        threshold <- as.numeric(coords(earthROC, input = "threshold", x = "best", transpose = FALSE)["threshold"])

        confusionMatrix <- confusionMatrix(data = as.factor(ifelse(predict(model,type="response") > threshold, 1, 0)), as.factor(model$y),
            positive = "1")
        info <- structure(list(dev_ratio = model$glm.stats[5],
                               AIC = model$glm.stats[6],
                               auc = auc, allCoords = as.data.frame(allCoords),
                               threshold = threshold,
                               confusionMatrix = confusionMatrix,
                               earthROC = earthROC), class = "infoEarthGLM")
    }

    return(info)
}


# Summary function for infoEarth class ----------------------------------------
#' @export
summary.infoEarth <- function(x, digits = 3) {
    tmp <- as.data.frame(x[c(1:15)])
    tmp <- as.data.frame(tmp)
    row.names(tmp) <- NULL
    print(tmp,digits = digits)
}
#' @export
print.infoEarth <- function(x, digits = 3) {
    options(digits = digits)
    tmp <- as.data.frame(x[c(1:15)])
    row.names(tmp) <- NULL
    print(tmp)
}
# Summary function for infoEarthGLM class -------------------------------------
#' @export
summary.infoEarthGLM <- function(x, digits = 3, plot = FALSE) {
    options(digits = digits)
    cat("Area under the curve : ", x$auc, "\n\n")
    cat("Coordinates table \n")
    print(x$allCoords)
    cat("\n\n")
    cat("Best threshold : ", x$threshold, "\n\n")
    print(x$confusionMatrix)
    if (plot) plot(x$earthROC)
}
#' @export
print.infoEarthGLM <- function(x, digits = 3) {
    options(digits = digits)
    cat("Area under the curve : ", x$auc, "\n\n")
    cat("Coordinates table \n")
    print(x$allCoords)
    cat("\n\n")
    cat("Best threshold : ", x$threshold, "\n\n")
    print(x$confusionMatrix)
}
