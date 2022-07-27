# pso library is needed


# True positive rate (sensitivity)

TPR <- function(pred, y)
{
	sum(y[pred]) / sum(y)
}


# True negative rate (specificity)

TNR <- function(pred, y)
{
	sum(1 - y[!pred]) / sum(1 - y)
}


# Weighted accuracy

WA <- function(pred, y, w)
{
	w * TPR(pred, y) + (1 - w) * TNR(pred, y)
}

# Accuracy

ACC <- function(pred, y)
{
	WA(pred, y, sum(y) / length(y))  # The same as "sum(as.numeric(pred) == y) / length(y)"
}


# Balanced accuracy

BA <- function(pred, y)
{
	WA(pred, y, 1/2)
} 


# Model

LnrClsPred <- function(x1, x2, a, b)
{
	x2 < a * x1 + b
}


# Objective function 

LnrClsObj <- function(ab, x1, x2, y, w)
{
	WA(LnrClsPred(x1, x2, ab[1], ab[2]), y, w)
}


# PSO optimization

LnrClsFit <- function(x1, x2, y, w, lower, upper)   
{
	best <- 0;	

	for (i in 1:5)
	{
		res <- psoptim(c(1, 1), LnrClsObj, x1 = x1, x2 = x2, y = y, w = w, lower = lower, upper = upper, control = list(fnscale = -1, s = 50, maxit = 1000));
		if (-res$value > best)
		{
			best <- -res$value;
			bestRes <- res;
		}
	}

	bestRes;
}


# LOOCV

LnrClsLOOCV <- function(x1, x2, y, w, lower, upper, seed = 2022)
{
	set.seed(seed);
	pred <- c();
	val <- c();
	
	for (i in 1:length(y))
	{
		fitRes <- LnrClsFit(x1[-i], x2[-i], y[-i], w, lower, upper);
		pred <- c(pred, LnrClsPred(x1[i], x2[i], fitRes$par[1], fitRes$par[2]));
		val <- c(val, fitRes$value);
	}

	data.frame(val, pred, y)
}

