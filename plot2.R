plot2 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
#	EM_CLASS = readRDS('Source_Classification_Code.rds');

	years = c(1999, 2002, 2005, 2008);
	em = vector(mode='numeric',length=length(years));
	
	for (yr in 1:length(years)) {
		idxs = which(EM_DATA$year == years[yr] & EM_DATA$fips == 24510);
		em[yr] = sum(EM_DATA$Emissions[idxs]);
	}

	# dbg plotting
	#windows()
	#plot(em~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total PM2.5 emission in Baltimore city');
	#lines(em~years);
	
	png('plot2.png');
	plot(em~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total PM2.5 emission in Baltimore city');
	lines(em~years);
	dev.off();
}