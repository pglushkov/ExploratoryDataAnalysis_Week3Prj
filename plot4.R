plot4 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
	EM_CLASS = readRDS('Source_Classification_Code.rds');

	# First thing - lets find all SCC types that have to do with coal
	idxs = which(has_to_do_with_coal(EM_CLASS$SCC.Level.Three) |  
		 	 has_to_do_with_coal(EM_CLASS$SCC.Level.Four));
	scc_types = as.character(EM_CLASS$SCC[idxs]);

	years = c(1999, 2002, 2005, 2008);
	em = vector(mode='numeric',length=length(years));
	
	# now, for each selected year, lets calculate sum of emissions related to coal
	for (yr in 1:length(years)) {
		total_em = 0;
		TMP = EM_DATA[EM_DATA$year == years[yr],];
		for (scc in 1:length(scc_types)) {
			idxs = which(TMP$SCC == scc_types[scc]);
			total_em = total_em + sum(TMP$Emissions[idxs]);
			#print(sprintf('year = %f, scc = %f ...\n', years[yr], scc_types[scc]));
		}
		em[yr] = total_em;
	}

	windows()
	plot(em~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total emission from coal cumbustion across USA');
	lines(em~years);
}

has_to_do_with_coal <- function(le_string) {
	return(grepl("coal", tolower(le_string)));
}