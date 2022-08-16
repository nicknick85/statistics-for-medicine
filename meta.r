RestoreSizes <- function(dat)
{
	100 * dat[, 1] / dat[, 2]
}


GetCIs <- function(dat)
{
	x <- dat[, 1]
	n <- dat[, 2]

	tbl <- data.frame()

	for (i in 1:length(x))
	{
		if (is.na(x[i]))
		{
			tbl <- rbind(tbl, c(NA, NA, NA))
		}
		else
		{
			bt_res <- binom.test(x[i], n[i])
			tbl <- rbind(tbl, c(bt_res$estimate, bt_res$conf.int))
		}
	}
	
	names(tbl) <- c("mean", "lower", "upper")	

	bt_res <- binom.test(sum(x[!is.na(x)]), sum(n[!is.na(x)]))
	sm <- c(bt_res$estimate, bt_res$conf.int)

	list(table = tbl, summary = sm)
}


FormatCI <- function(ci)
{
	paste(format(ci[1], digits = 3, nsmall = 3), " (", format(ci[2], digits = 3, nsmall = 3), ", ", format(ci[3], digits = 3, nsmall = 3), ")", sep = "")
}


# Need forestplot and dplyr libraries

PlotCIs <- function(names, tbl, sm)
{
	na <- is.na(tbl$mean)

	ci <- apply(tbl, 1, FormatCI)

	base_data <- tibble(
			mean = tbl$mean[!na],
			lower = tbl$lower[!na],
			upper = tbl$upper[!na],
			ci = ci[!na],
			study = names[!na])
	
	summary <- tibble(
			mean = sm[1],
			lower = sm[2],
			upper = sm[3],
			ci = FormatCI(sm),
			study = "Summary",
			summary = TRUE)

	header <- tibble(
			study = "Study",
			ci = "95% CI",
			summary = TRUE)

	empty_row <- tibble(mean = NA_real_)					

	output <- bind_rows(header, base_data, empty_row, summary)

	
	m <- floor(100 * min(base_data$lower)) / 100; M <- ceiling(100 * max(base_data$upper)) / 100; t <- c(m, (1:10)/10, M); 
	t <- t[t <= M & t >= m]

	output %>%
		forestplot(labeltext = c(study, ci), is.summary = summary, xlog = FALSE, zero = sm[1], xticks = t,
				col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"))	
}