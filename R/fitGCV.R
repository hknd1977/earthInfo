# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more about
# package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package authoring:
# Build and Reload Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift + T'


# GCV for döngüsüyle en küçük GCV nin hangi derecede olduğunu bulmak için fonksiyon hazırlanıyor

fitGCV <- function(formula, data, maxDegree = 3, ...) {
    suppressMessages(require(caret, quietly = TRUE))
    suppressMessages(require(earth, quietly = TRUE))
    for (degree in 1:maxDegree) {
        # fonksiyon parametreleri içinde verilen maksismum dereceye göre for döngüsü başlatılıyor
        fitTemp <- earth(formula, data = data, degree = degree, nk = 1000, keepxy = TRUE)  # standart değerler üzerinden model kuruluyor
        maxInter <- sum(rowSums(as.data.frame(fitTemp$cuts[fitTemp$selected.terms, ] > 0)) >= degree)  # ilgili dereceki interaksiyon derecesi hesaplanıyor
        if (maxInter < 1) 
            break  # değişkenler arasında uygun interaksiyon yoksa çıkacak
        cat("degree : ", degree, "Max Interaction : ", maxInter, "GCV : ", fitTemp$gcv, "\n")  #bilgi vermek için değerler yazılıyor
        if (!exists("fit") || (fit > fitTemp$gcv)) {
            # eğer yeni model gcv'si küçükse çalışcak
            fit <- fitTemp$gcv
            Interaction <- maxInter
            last <- list(degree = degree, Interaction = Interaction, metric = fitTemp$gcv)
        }
    }
    return(last)
}
