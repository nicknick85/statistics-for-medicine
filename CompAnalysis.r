# Comparison of binary samples

BinComp <- function(
		x,         # Success of non-exposed patients or control
		y)         # Success of exposed patients or case
{
	N1 <- length(x);
	N2 <- length(y);
	N <- N1 + N2;
	mz <- mean(c(x, y));
	sqs <- mz * (1 - mz);
	
	sqDlt <- (mean(x) - mean(y))^2 / (sqs * (1/N1 + 1/N2));    
		# sqDlt coincides with chisq.test(matrix(c(sum(y), sum(!y), sum(x), sum(!x)), 2, 2), correct = FALSE)$statistic

	sqDltCorr <- ((N-1) / N) * sqDlt;
	pValue <- 2*pnorm(-sqrt(sqDltCorr));
	
	list(chiSq = sqDlt, chiSqCorr = sqDltCorr, pValue = pValue);
}

# Power of BinComp

BinCompPwr <- function(
		N1,         # length(x)
		N2,         # length(y)
		w,          # Cohen's effect size: 0.1 - small, 0.3 - medium, 0.5 - large
		alph = 0.05)
{
	N <- N1 + N2;
	tht <- w * sqrt(N);
	q <- qnorm(1 - alph/2);
	pwr <- 1 - (pnorm(q, tht) - pnorm(-q, tht));
		# pwr coincides with pwr.chisq.test(w, N, 1, alph)$power

	pwr;	
}

# Cohen's effect size calculation

CohenW <- function(p1, p2, N1, N2)
{
	N <- N1 + N2;

	wg1 <- N1 / N;
	wg2 <- N2 / N;
	
	p <- wg1 * p1 + wg2 * p2;
	sgm <- sqrt(p * (1 - p));
	dlt <- (p1 - p2) / sgm;

	dlt / sqrt(1/wg1 + 1/wg2);
}
