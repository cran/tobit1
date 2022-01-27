## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, out.width = "100%",
                      fig.align = "center", cache = FALSE, echo = TRUE)
options(knitr.table.format = "latex", knitr.booktabs = TRUE)

## -----------------------------------------------------------------------------
library("tobit1")
library("dplyr")

## -----------------------------------------------------------------------------
print(charitable, n = 5)

## -----------------------------------------------------------------------------
charitable <- charitable %>% mutate(logdon = log(donation) - log(25))

## -----------------------------------------------------------------------------
char_form <- logdon ~ log(donparents) + log(income) +
    education + religion + married + south
if (requireNamespace("AER")){
    library("AER")
    ml_aer <- tobit(char_form, data = charitable)
}
if (requireNamespace("censReg")){
    library("censReg")
    ml_creg <- censReg(char_form, data = charitable)
}
ml <- tobit1(char_form, data = charitable)

## -----------------------------------------------------------------------------
scls <- update(ml, method = "trimmed")
ols <- update(ml, method = "lm")

## ----models, echo = FALSE-----------------------------------------------------
if (requireNamespace("modelsummary")){
    modelsummary::msummary(list("OLS" = ols, "maximum likehihood" = ml, "SCLS" = scls),
                           title = "Estimation of charitable giving models",
                           label = "tab:models", single.row = TRUE, digits = 3)
}

## -----------------------------------------------------------------------------
haustest(scls, ml, omit = "(Intercept)")

## ----histnorm, fig.cap = "Empirical distribution of the response and normal approximation"----
if (requireNamespace("ggplot2") & requireNamespace("dplyr")){
    library("ggplot2")
    library("dplyr")
    moments <- charitable %>% filter(logdon > 0) %>% summarise(mu = mean(logdon), sigma = sd(logdon))
    ggplot(filter(charitable, logdon > 0), aes(logdon)) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 10) +
        geom_function(fun = dnorm, args = list(mean = moments$mu, sd = moments$sigma)) +
        labs(x = "log of charitable giving", y = NULL)
}

