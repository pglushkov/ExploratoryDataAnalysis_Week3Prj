plot3 <- function() {

	EM_DATA = readRDS('summarySCC_PM25.rds');
#	EM_CLASS = readRDS('Source_Classification_Code.rds');

	BALT_DATA = EM_DATA[EM_DATA$fips == 24510, ];

	BALT_DATA_TYPES = split(BALT_DATA, BALT_DATA$type);

	em_types = vector(mode='character', length = length(BALT_DATA_TYPES));
	for (em_type in 1:length(BALT_DATA_TYPES)) {
		em_types[em_type] = BALT_DATA_TYPES[[em_type]]$type[1];
	}
	years = c(1999, 2002, 2005, 2008);

	# lets form a new data frame that contains all information we need in a handy way and plot it!
	# allocating space for columns of future dataset
	ems = vector(mode='numeric', length = length(years) * length(em_types));
	yrs_final = vector(mode='numeric', length = length(years) * length(em_types));
	em_types_final = vector(mode='numeric', length = length(years) * length(em_types));
	
	for (yr in 1:length(years)) {
		for (em_type in 1:length(em_types)) {
			idxs = which(BALT_DATA_TYPES[[em_type]]$year == years[yr]);
			ems[yr + (em_type - 1)*length(em_types)] = sum(BALT_DATA_TYPES[[em_type]]$Emissions[idxs]);
			yrs_final[yr + (em_type - 1)*length(em_types)] = years[yr];
			em_types_final[yr + (em_type - 1)*length(em_types)] = em_types[em_type];		
		}
	}

	# make the dataset from calculated columns
    final_data = data.frame(Year = yrs_final, EmissionType = em_types_final, Emission=ems);

	# dbg plotting
	#qplot(Year, Emission, data = final_data, color = EmissionType, geom = 'line',
	#		main='PM2.5 emission level in Baltimore city by source type');
			
	png('plot3.png');
	print(qplot(Year, Emission, data = final_data, color = EmissionType, geom = 'line',
			main='PM2.5 emission level in Baltimore city by source type'));
	dev.off();
}