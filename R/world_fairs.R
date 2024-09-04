
##Load packages
library(tidyverse)
library(gtsummary)
library(here)

#Load dataset
world_fairs <- read.csv(here("./data/worlds_fairs.csv"))

#Code for inline statistic
stats <- list(n = nrow(world_fairs))

#Descriptive stats table
tbl_summary(
	world_fairs,
	by="category",
	include = c(country, visitors, cost),
	label = list(
		country ~ "Country",
		visitors ~ "Visitors",
		cost ~ "Cost"),
	statistic = list(
		visitors ~ c("{p25},{p75}({median})"),
		cost ~ c("{p25},{p75}({median})"))) |>
	bold_labels()

#Univariate regression
tbl_uvregression(
	world_fairs,
	y = cost,
	include = c(country, category, visitors, cost),
	method = lm,
	label = list(
		country ~ "Country",
		category ~ "Category",
		visitors ~ "Visitors"
	)
)
#Create object for inline text
cost_table <-
	tbl_uvregression(
		world_fairs,
		y = cost,
		include = c(country, category, visitors, cost),
		method = lm,
		label = list(
			country ~ "Country",
			category ~ "Category",
			visitors ~ "Visitors"
		)
	)

#Figure: Histogram
visitors_hist <- hist(world_fairs$visitors,
											main="Attendance to Fairs, Globally",
											xlab="Visitors",
											col="orange",
)
#Save histogram as pdf
pdf(here::here("Figures","visitors_hist.pdf"))
dev.off()

#Function of standard deviation
sd_function <- function(x, multiplier =1) {
	n <- length(x)
	mean_val <-sum(x, na.rm=TRUE)/n
	sd_val <- sqrt(sum(!is.na(x-mean_val)^2) / (n-1))
	return(sd_val)
}
sd_function(world_fairs$cost)
