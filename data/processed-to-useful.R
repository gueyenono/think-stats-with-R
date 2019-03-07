################################################################
# ~~**~~ DATA CLEANING TO MAKE IT USEABLE WITH THE BOOK ~~**~~ #
################################################################


# Import useful packages --------------------------------------------------

library(readr)
library(dplyr)
library(magrittr)


# Custom function for replacing specific values inside a vector

replace_values <- function(x, values, replacement){
	i <- x %in% values
	x[i] <- replacement
	x
}

# 2002FemPreg -----------------------------------------------

# Cleaning

fempreg2002 <- readRDS("data/processed/dat/2002FemPreg.Rds") %>%
	select(caseid, prglngth, outcome, pregordr, birthord, birthwgt_lb, birthwgt_oz, agepreg, finalwgt) %>%
	mutate(
		agepreg = agepreg / 100,
		birthwgt_lb = replace_values(birthwgt_lb, values = 97:99, NA),
		birthwgt_oz = replace_values(birthwgt_oz, values = 97:99, NA),
		totalwgt_lb = birthwgt_lb + (birthwgt_oz / 16),
		birthwgt_lb = if_else(birthwgt_lb > 20, NA_real_, birthwgt_lb)
	)

# Validation

table(fempreg2002$outcome)
table(fempreg2002$birthwgt_lb)

# Export

write_rds(fempreg2002, "data/processed/used-in-book/2002FemPrg.Rds")

# We do not need to make the MakePregMap() function because the tidyverse makes it easy to perform
caseid <- 10229
filter(fempreg2002, caseid == 10229)
