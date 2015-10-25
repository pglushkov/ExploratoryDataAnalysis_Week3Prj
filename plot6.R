plot6 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
	EM_CLASS = readRDS('Source_Classification_Code.rds');

	# First thing - lets find all SCC types that have to do with coal
	idxs = which(has_to_do_with_motor(EM_CLASS$SCC.Level.Three) |  
		 	 has_to_do_with_motor(EM_CLASS$SCC.Level.Four));
	scc_types = as.character(EM_CLASS$SCC[idxs]);

	years = c(1999, 2002, 2005, 2008);
	em_balt = vector(mode='numeric',length=length(years));
	em_la = vector(mode='numeric',length=length(years));
	
	# now, for each selected year, lets calculate sum of emissions related to coal
	for (yr in 1:length(years)) {
		total_em1 = 0;
		total_em2 = 0;

		idxs1 = which(EM_DATA$year == years[yr] & EM_DATA$fips == '24510');
		idxs2 = which(EM_DATA$year == years[yr] & EM_DATA$fips == '06037');
		TMP1 = EM_DATA[idxs1,];
		TMP2 = EM_DATA[idxs2,];
		for (scc in 1:length(scc_types)) {
			idxs1 = which(TMP1$SCC == scc_types[scc]);
			total_em1 = total_em1 + sum(TMP1$Emissions[idxs1]);
			idxs2 = which(TMP2$SCC == scc_types[scc]);
			total_em2 = total_em2 + sum(TMP2$Emissions[idxs2]);

		}
		em_balt[yr] = total_em1;
		em_la[yr] = total_em2;
	}

	print(em_balt);
	print(em_la);

	# dbg plotting
	#windows()
	#plot(em_balt~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total emission from motors', ylim=range(c(em_balt, em_la)) );
	#lines(em_balt~years, col = 'black');
	#lines(em_la~years, col = 'red');
    #legend("topleft", c("Baltimore", "LA"), col = c("black", "red"), lty=c(1, 1) );
	  
	png('plot6.png');
	plot(em_balt~years, pch='.', ylab = "Emission, tons", xlab="Year", main='Total emission from motors', ylim=range(c(em_balt, em_la)) );
	lines(em_balt~years, col = 'black');
	lines(em_la~years, col = 'red');
    legend("topleft", c("Baltimore", "LA"), col = c("black", "red"), lty=c(1, 1) );

	dev.off();
}

has_to_do_with_motor <- function(le_string) {
	return(grepl("motor", tolower(le_string)));
}