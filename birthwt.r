## Core data manipulation for the Birthwt data set

library(Hmisc)

data(birthwt, package="MASS")

yesno <- c("No", "Yes")

birthwt <- within(birthwt, {
    smoke <- factor(smoke, labels = yesno)
    low <- factor(low, labels = yesno)
    ht <- factor(ht, labels = yesno)
    ui <- factor(ui, labels = yesno)
    race <- factor(race, levels = 1:3, labels = c("White", "Black", "Other"))
    lwt <- lwt/2.2  ## weight in kg
})

label(birthwt$age) <- "Mother age"
units(birthwt$age) <- "years"
label(birthwt$bwt) <- "Baby weight"
units(birthwt$bwt) <- "grams"
label(birthwt, self = TRUE) <- "Hosmer & Lemeshow's low birth weight study."
