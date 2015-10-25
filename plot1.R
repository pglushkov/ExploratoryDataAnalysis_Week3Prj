plot1 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
#	EM_CLASS = readRDS('Source_Classification_Code.rds');

	years = c(1999, 2002, 2005, 2008);
	em = vector(mode='numeric',length=length(years));
	
	for (yr in 1:length(years)) {
		em[yr] = sum(EM_DATA$Emissions[EM_DATA$year == years[yr]]);
	}

	# dbg plotting
	#windows()
	#plt = plot(em~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total PM2.5 emission across USA');
	#lines(em~years);
	
	png(filename='plot1.png');
	plt = plot(em~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total PM2.5 emission across USA');
	lines(em~years);
	dev.off();
}