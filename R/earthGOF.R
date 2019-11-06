earth_GOF <- function(model) {
  require(caret,quietly = TRUE)
  require(pROC,quietly = TRUE)
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
        model_summary <- summary(model.lm)
        cor_test <- cor.test(y, predict(model))
        pearson_correlation <- round(cor(y, predict(model)))
        error <- y - predict(model)
        # resid(model) veya residuals(model) 'de hata vektörünü veriyor
        sd_ratio <- sd(error)/sd(y)
        coef_of_variation <- sd(error) * 100/mean(y)
        RMSE <- sqrt(mean(error^2))
        ME <- mean(error)
        RAE <- sqrt(sum(error^2)/sum(y^2))
        MAPE <- mean(abs(error/y)) * 100
        MAD <- mean(abs(error))
        Rsq <- 1 - (sum(error^2)/(var(y) * (n - 1)))
        adj_RSQ <- 1 - ((1 - Rsq) * (n - 1)/(n - k - 1))
        AIC <- n * log(mean(error^2), base = exp(1)) + 2 * k
        AICc <- n * log(mean(error^2), base = exp(1)) + (2 * k) + (2 * k * (k + 1)/(n - k - 1))
        # plot(y,predict(model))
        info <- structure(list(n = n, k = k, sdratio = sd_ratio, coef_of_variation = coef_of_variation, pearson_correlation = pearson_correlation,
            RMSE = RMSE, ME = ME, RAE = RAE, MAPE = MAPE, MAD = MAD, RSQ = Rsq, adj_RSQ = adj_RSQ, AIC = AIC, AICc = AICc,
            GCV = model$gcv, cor_test = cor_test, model_summary <- model_summary), class = "infoEarth")
    } else {

        ## if the model type is a binomial glm earth --------------------------------------------------------
        if (length(model$levels) > 2) {
            stop("only able to process binomial earth-glm model")
        }
        earth_ROC <- roc(predictor = as.vector(fitted(model)), response = as.vector(model$y))
        plot_ROC <- plot(earth_ROC)
        auc <- auc(earth_ROC)
        all_coords <- coords(earth_ROC, seq(0.05, 0.95, by = 0.1), transpose = FALSE)
        all_coords$sum <- all_coords[, 2] + all_coords[, 3]
        threshold <- as.numeric(coords(earth_ROC, input = "threshold", x = "best", transpose = FALSE)["threshold"])

        confusion_matrix <- confusionMatrix(data = as.factor(ifelse(fitted(model) > threshold, 1, 0)), as.factor(model$y))
        info <- structure(list(dev_ratio = model$glm.stats[5], AIC = model$glm.stats[6], auc = auc, all_coords = as.data.frame(all_coords),
            threshold = threshold, confusion_matrix = confussion_matrix, earth_ROC = earth_ROC, plot_ROC <- plot_ROC), class = "infoEarthGLM")
    }

    return(info)
}


# Summary function for infoEarth class ----------------------------------------

summary.infoEarth <- function(x, digits = 3) {
    options(digits = digits)
    tmp <- as.data.frame(x[c(1:15)])
    row.names(tmp) <- NULL
    print(tmp)
}

# Summary function for infoEarthGLM class -------------------------------------

summary.infoEarthGLM <- function(x, digits = 3,plot=FALSE) {
    options(digits = digits)
    cat("Area under the curve : ", x$auc, "\n\n")
    cat("Coordinates table \n")
    print(x$all_coords)
    cat("\n\n")
    cat("Best threshold : ", x$threshold, "\n\n")
    print(x$confusion_matrix)
    if (plot) plot(x$earth_ROC)
}
