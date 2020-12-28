
se      <- function(x) sqrt(var(x)/length(x));

auc <- function(yhat, y, na.rm=FALSE) {

	
	if(na.rm) {
		rows <- which(!is.na(yhat) & !is.na(y))
		yhat <- yhat[rows]
		y <- y[rows]
	}
	
	R1 <- sum(rank(yhat)[y==1]);
	n1 <- length(which(y==1));
	U1 <- R1 - n1*(n1+1)/2;
	U1/(n1*(length(y)-n1));
}


prec <- function(yhat, y, cutoff=.5, na.rm=FALSE) {

	if(na.rm) { 
		rows <- which(!is.na(yhat) & !is.na(y))
		yhat <- yhat[rows]
		y <- y[rows]
	}

	tp <- length(which(yhat >= cutoff & y));
	fp <- length(which(yhat >= cutoff & !y));
	
	ifelse( fp+tp > 0, tp/(tp+fp), 0 );
}


recall <- function(yhat, y, cutoff=.5, na.rm=FALSE) {

	if(na.rm) { 
		rows <- which(!is.na(yhat) & !is.na(y))
		yhat <- yhat[rows]
		y <- y[rows]
	}

	tp <- length(which(yhat >= cutoff & y));
	fn <- length(which(yhat < cutoff & y));
	
	ifelse( tp+fn > 0, tp/(tp+fn), 0 );
}


accuracy <- function(yhat, y, cutoff=.5, na.rm=FALSE) {

	if(na.rm) { 
		rows <- which(!is.na(yhat) & !is.na(y))
		yhat <- yhat[rows]
		y <- y[rows]
	}

	length(which( (yhat >= cutoff & y) | (yhat < cutoff & !y) )) / length(y)

}


f.measure <- function(yhat, y, cutoff=.5, na.rm=FALSE, lambda=1) {

	p <- prec(yhat, y, cutoff, na.rm);
	r <- recall(yhat, y, cutoff, na.rm);
	ifelse( p>0 || r>0, (1+lambda)*p*r / (lambda*p + r), 0)

}
