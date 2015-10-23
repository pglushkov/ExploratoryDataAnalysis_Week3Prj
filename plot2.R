plot2 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
#	EM_CLASS = readRDS('Source_Classification_Code.rds');

	years = c(1999, 2002, 2005, 2008);
	em = vector(mode='numeric',length=length(years));
	
	for (yr in 1:length(years)) {
		idxs = which(EM_DATA$year == years[yr] & EM_DATA$fips == 24510);
		em[yr] = sum(EM_DATA$Emissions[idxs]);
	}

	windows()
	plot(em~years, pch='.', ylab = "Emission in Baltimore city, tons", xlab="Year");
	lines(em~years);
}