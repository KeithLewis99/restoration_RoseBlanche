##code for estimating fish abundance and biomass from electrofishing stations 27-11-2013
amec.efishing8<-function(filename){
##load FSA package for depletion algorithms
##source("http://www.rforge.net/FSA/InstallFSA.R")
library(FSA)
	##read in data file
	filename.paste<-paste(filename,'.csv', sep="")
	 all.data<-read.table(filename.paste, header=T, sep=",")
	 print(head(all.data))
	 print(noquote(""))
	 print(noquote(""))
##browser()
if(sum(as.numeric(is.na(all.data$Weight.g)), as.numeric(is.na(all.data$Length.mm)))>0) print(noquote('**********************DATA HAS BLANKS OR NAs - in weight or length - change to zero or delete entry*****************************'))
if(sum(as.numeric(is.na(all.data$Weight.g)), as.numeric(is.na(all.data$Length.mm)))>0) browser()
  ##generate alternate summary sheet with traditional biomass calculation estimates list as "Y" else indicate "N"
  traditional.biomass.work.up<-"N"
	 ##toggle whether you want to add to summary file - hit 'Y' for yes and 'N' for no
	 ifelse(sum(as.numeric(dir()=="e.fishing.summary.data.csv"))==1, add.to.summary<-'Y', add.to.summary<-'N')
	 
	 ##make sure key columns are numeric and return appropriate message
	 ifelse(is.numeric(all.data$Length.mm),print(noquote('length data OK')), print(noquote('LENGTH DATA NOT NUMERIC!!!!')))
ifelse(!is.na(all.data$Length.mm),print(noquote('length data OK')), print(noquote('LENGTH DATA Has BLANKS!!!!')))
	 ifelse(!is.na(all.data$Weight.g),print(noquote('weight data OK')), print(noquote('WEIGHT DATA HAS BLANKS!!!!!')))
	 
	 print(noquote(""))
	 print(noquote(""))
	 print(noquote(""))
##add loop that permits calculation of estimates for multiple years
unique.year<-unique(all.data$Year)
for(bbb in 1:length(unique.year)){
  year.data<-all.data[all.data$Year==unique.year[bbb],]

##add a loop that permits calculation of estimates for multiple sites
unique.site<-unique(year.data$Site)
for(aaa in 1:length(unique.site)){
  year.site.data<-year.data[year.data$Site==unique.site[aaa],]
  unique.station<-unique(year.site.data$Station)
  for(ccc in 1:length(unique.station))
  {
   ##get subset of data that is for site aaa
  data<-year.site.data[year.site.data$Station==unique.station[ccc],]
print(c(as.character(unique.site[aaa]), unique.year[bbb], unique.station[ccc]))
	 ##add junk column to use to add up number of fish in each sweep
	 junk<-rep(1,length(data[,1]))
	 data<-cbind(data,junk)
	 data$Weight.g<-as.numeric(data$Weight.g)
	 
	 ##create loop that generates a qa/qc graph for each species, calculates the biomass and abundance delury estimates and saves the summary data
	 ##do QA/QC for each species of fish on length weight regressions
	 ##determine the number of unique species
	  unique.species<-unique(data$Species)
	  print(noquote(c('Breakdown of fish species at',filename)))
		print(with(data,table(Species)))
  ##if(unique.site[aaa]=="Lower" & unique.year[bbb]=="1998") browser()
		print(noquote(""))
		print(noquote(""))
		##if(as.character(unique.site[aaa])=="Pinus" & unique.year[bbb]==2013 & unique.station[ccc]==3) browser()
	##determine proportion of various species for abundance and biomass
	##first aggregate biomass across species, year, site and station
		species.proportion<-as.data.frame.table((tapply(data$Weight.g, list(data$Species, data$Year, data$Site, data$Station), sum)))
  ##
  ##species.proportion<-na.omit(species.proportion)
 		names(species.proportion)<-c('Species','Year','Site','Station','biomass.caught')
  species.proportion<-na.omit(species.proportion)
 	##calculate biomass proportions and save to file called species proportion
 		biomass.proportion<-species.proportion$biomass/sum(species.proportion$biomass, na.rm=T)
		species.proportion<-cbind(species.proportion,biomass.proportion)
  
  ##browser()	
  ##species.proportion<-species.proportion[,-2]
	##now calculate proportions based on abundance and add to the species proportion dataframe
 		temp.abundance.caught<-as.data.frame.table((tapply(data$junk, list(data$Species), sum)))
 		names(temp.abundance.caught)<-c('Species','abundance.caught')
  temp.abundance.caught<-na.omit(temp.abundance.caught)
 		abundance.proportion<-temp.abundance.caught$abundance.caught/sum(temp.abundance.caught$abundance.caught)
		species.proportion<-cbind(species.proportion,abundance.proportion)
  species.proportion<-cbind(species.proportion, temp.abundance.caught$abundance.caught)
  names(species.proportion)<-c('Species', "Year", "Site", "Station", "biomass.caught", 'biomass.proportion', 'abundance.proportion', 'abundance.caught')
		print(noquote('+++++++++++++++++++     SPECIES BY SPECIES ANALYSIS        +++++++++++++++++++++++++'))
	  ##set up graph window for one graph per page
		 par(mfrow=c(1,1))
		##create loop that goes through the analysis for each species
		 for(a in 1:length(unique.species)){
	 		print(noquote('#############################################################################################'))
		 	##print species name to ID for user
		 	print(noquote(as.character(unique.species[a])))
		 	print(noquote(""))
		 	##plot length vs. weight to look for outliers
		 	##select out data for species of interest
		 	qa.species.data<-data[data$Species==unique.species[a],]
	 		##plot length vs. weight to look for outliers
		 	with(qa.species.data, plot(Weight.g~Length.mm))
	 		mtext(unique.species[a])
	 		##generate lowess smoother to overlay on plot if number of points is greater than 3
	 		if(length(unique(qa.species.data$Weight.g))>6) {
      
	 		smooth<-loess(Weight.g~Length.mm, data=qa.species.data)
	 		smooth.x.data<-seq(min(qa.species.data$Length.mm),max(qa.species.data$Length.mm))
	 		smooth.y.data<-predict(smooth, smooth.x.data)
	 		lines(smooth.y.data~smooth.x.data, col='red')}
	 		##use following command to identify the source of outliers
	 		##identify(qa.species.data$Length.mm,qa.species.data$Weight.g, n=1, labels=row.names(qa.species.data))
	 		print(noquote('LENGTH-WEIGHT GRAPH PRODUCED TO EXAMINE OUTLIERS'))
	 		print(noquote(""))
	 		print(noquote('HIT ENTER TO CONTINUE TO SPECIES BIOMASS ANALYSIS'))
	 		####browser()
	  	
	 		##get zippin depletion estimate for biomass for a given species using the FSA (fisheries stock assessment) package 	
	 		##first summarize biomass by sweep
	 		biomass.summary.data<-as.data.frame.table((tapply(qa.species.data$Weight.g, list(qa.species.data$Sweep), sum)))
 			names(biomass.summary.data)<-c('Sweep','biomass.sum')
	 		##calculate the cumulative catch by sweep and save to biomass.summary.data
 			biomass.cum.catch<-cumsum(biomass.summary.data$biomass.sum)
	 		biomass.summary.data<-cbind(biomass.summary.data,biomass.cum.catch)

	 		##generate biomass delury linear model (no longer being utilized except to determine if data is suitable)
	 		biomass.delury.model<-lm(biomass.sum~ biomass.cum.catch, data=biomass.summary.data)
			##calculate the x-intercept for delury estimate
	 		##biomass.delury.estimate<--biomass.delury.model[[1]][[1]]/biomass.delury.model[[1]][[2]]
	 	
	 	
	 		##DO ZIPPIN DEPLETION IF DEPLETION SLOPE IS NEGATIVE SO AS TO AVOID ERROR MESSAGE
	 		##generate the zippin graph
	 				with(biomass.summary.data, plot(biomass.sum~biomass.cum.catch, ylab='biomass per pass (g)', xlab='cumulative catch', ylim=c(0,max(biomass.summary.data$biomass.sum))))
	 				mtext.label<-paste('Biomass by sweep:',unique.species[a])
					mtext(mtext.label)
		
					
	 		ifelse(biomass.delury.model[[1]][[2]]<0 & length(biomass.summary.data[,1])>2,
				{
	 				##generate biomass zippin model using the FSA package and acquire estimate and 95% CI's (if slope is negative)
	 				biomass.zippin.model<-removal(biomass.summary.data$biomass.sum, method="CarleStrub")
	 				biomass.zippin.estimate<-summary(biomass.zippin.model)[[1]]
	 				biomass.zippin.ucl<-confint(biomass.zippin.model)[[3]]
	 				biomass.zippin.lcl<-confint(biomass.zippin.model)[[1]]
	 		 
					##generate the zippin graph
	 				with(biomass.summary.data, plot(biomass.sum~biomass.cum.catch, ylab='biomass per pass (g)', xlab='cumulative catch', ylim=c(0,max(biomass.summary.data$biomass.sum)), xlim=c(0,biomass.zippin.ucl)))
					##abline(biomass.delury.model)
					##add points to depletion graph to indicate the estimate (green) and the 95% CI's (red)
					points(x=biomass.zippin.estimate, y=0, pch=8, col="green", cex=1.5)
					points(x=biomass.zippin.lcl, y=0, pch=8, col="red", cex=1.5)
					points(x=biomass.zippin.ucl, y=0, pch=8, col="red", cex=1.5)
					mtext.label<-paste('Biomass - Zippin depletion model:',unique.species[a])
					mtext(mtext.label)
		
					##return depletion estimate and CI's for the species or return a message saying the data was inappropriate for depletion method
					print(noquote(""))
					print(noquote(as.character(unique.species[a])))
					print(noquote(""))
					print(noquote('Zippin estimate + 95% cl (biomass):'))
					print(noquote(c(biomass.zippin.estimate, biomass.zippin.lcl, biomass.zippin.ucl)))
					print(noquote('BIOMASS DEPLETION DATA PLOTTED'))
	 				print(noquote(""))
	 				
	  	
			}, print(noquote('SPECIES BIOMASS DATA INAPPROPRIATE FOR DEPLETION ANALYSIS! NO ESTIMATE OR GRAPH PRODUCED'))) ##alternative if slope is greater than zero
			
			##provide data on how much fish biomass was actually captured.
			print(noquote(""))
			print(noquote('Biomass captured during survey:'))
			print(sum(biomass.summary.data$biomass.sum))
			print(noquote(""))
			print(noquote('_______________________________________________________________________________________________'))
			print(noquote('HIT ENTER TO CONTINUE TO SPECIES ABUNDANCE'))
			####browser()
		
			##REPEAT ABOVE STEPS FOR ABUNDANCE
			##first summarize by sweep
	 		abundance.summary.data<-as.data.frame.table((tapply(qa.species.data$junk, list(qa.species.data$Sweep), sum)))
 			names(abundance.summary.data)<-c('Sweep','abundance.sum')
	 		abundance.cum.catch<-cumsum(abundance.summary.data$abundance.sum)
	 		summary.abundance.data<-cbind(abundance.summary.data,abundance.cum.catch)

	 		##generate abundance linear model (no longer being utilized except to determine if data is suitable)
	 		abundance.delury.model<-lm(abundance.sum~ abundance.cum.catch, data=abundance.summary.data) ##no longer being used
			##calculate the x-intercept for delury estimate
	 		##abundance.delury.estimate<--abundance.delury.model[[1]][[1]]/abundance.delury.model[[1]][[2]]
	 	
	 		##generate the zippin graph
	 				with(abundance.summary.data, plot(abundance.sum~abundance.cum.catch, ylab='abundance', xlab='cumulative catch', ylim=c(0,max(abundance.summary.data$abundance.sum))))
					mtext.label<-paste('Abundance by sweep:',unique.species[a])
				mtext(mtext.label)
		
	 		ifelse(abundance.delury.model[[1]][[2]]<0 & length(abundance.summary.data[,1])>2,
				{
	 	
	 			##generate abundance zippin model using the FSA package
	 			abundance.zippin.model<-removal(abundance.summary.data$abundance.sum, method="CarleStrub")
	 			abundance.zippin.estimate<-summary(abundance.zippin.model)[[1]]
	 			abundance.zippin.ucl<-confint(abundance.zippin.model)[[3]]
	 			abundance.zippin.lcl<-confint(abundance.zippin.model)[[1]]
		
				##abundance.delury.model<-lm(abundance.sum~ abundance.cum.catch, data=abundance.summary.data)
	 			##abundance.delury.estimate<--abundance.delury.model[[1]][[1]]/abundance.delury.model[[1]][[2]]
	 		 
				with(abundance.summary.data, plot(abundance.sum~abundance.cum.catch, ylim=c(0,max(abundance.summary.data$abundance.sum)), xlim=c(0,abundance.zippin.ucl)))
				##abline(abundance.delury.model)
				points(x=abundance.zippin.estimate, y=0, pch=8, col="green", cex=1.5)
				points(x=abundance.zippin.lcl, y=0, pch=8, col="red", cex=1.5)
				points(x=abundance.zippin.ucl, y=0, pch=8, col="red", cex=1.5)
				mtext.label<-paste('Abundance Zippin depletion model:',unique.species[a])
				mtext(mtext.label)
		
				
				print(noquote('Zippin estimate + 95% cl (abundance):'))
				print(noquote(as.character(unique.species[a])))
				print(noquote(""))
				print(noquote('Zippin estimate + 95% cl (abundance):'))
				abundance.delury.estimate<--abundance.delury.model[[1]][[1]]/abundance.delury.model[[1]][[2]]
				print(c(abundance.zippin.estimate, abundance.zippin.lcl, abundance.zippin.ucl))
				print(noquote(""))
				print(noquote('ABUNDANCE DEPLETION DATA PLOTTED'))
	 		
					
			}, print(noquote('SPECIES ABUNDANCE DATA INAPPROPRIATE FOR DEPLETION ANALYSIS! NO ESTIMATE OR GRAPH PRODUCED')))
		
			print(noquote(""))
			print(noquote('Abundance captured during survey:'))
			print(sum(abundance.summary.data$abundance.sum))
			print(noquote(""))
	 		print(noquote('#############################################################################################'))
			print(noquote(""))
			print(noquote(""))
			print(noquote("HIT ENTER TO CONTINUE WITH NEXT SPECIES"))
	 		####browser()
	 	}
	 
	print(noquote('SPECIES BY SPECIES ANALYSIS COMPLETE'))
	print(noquote('HIT ENTER TO CONTINUE WITH OVERALL BIOMASS'))
	##changing graphing parameters so both zippin depletion and bargraph appear on same panel
	par(mfrow=c(2,1), las=0)
  print(c(unique.site[aaa],unique.site[aaa],unique.station[ccc],unique.species[a]))
  
	##combine all species biomass within a sweep
	combined.biomass.summary.data<-as.data.frame.table((tapply(data$Weight.g, list(data$Sweep), sum)))
 	names(combined.biomass.summary.data)<-c('Sweep','biomass.sum')
	##calculate cumulative catch across sweeps and add to combined biomass.summary.data
 	biomass.cum.catch<-cumsum(combined.biomass.summary.data$biomass.sum)
	combined.biomass.summary.data<-cbind(combined.biomass.summary.data,biomass.cum.catch)

	##ensure that there is a negative slope for removal analysis
	combined.biomass.delury.model<-lm(biomass.sum~ biomass.cum.catch, data=combined.biomass.summary.data)
	
  ##zippin estimate will work with only one data point (it just makes that the estimate) but confidence intervals won't.  So we will get the estimate and only calculate conf limits if there is enough data.  alternatively they are set as NAs
  		
	 				##generate biomass zippin model using the FSA package
	 				combined.biomass.zippin.model<-removal(combined.biomass.summary.data$biomass.sum, method="CarleStrub")
	 				combined.biomass.zippin.estimate<-summary(combined.biomass.zippin.model)[[1]]
  ifelse(combined.biomass.delury.model[[1]][[2]]<0 & length(combined.biomass.summary.data)>2, {
         
	 				combined.biomass.zippin.ucl<-confint(combined.biomass.zippin.model)[[3]]
	 				combined.biomass.zippin.lcl<-confint(combined.biomass.zippin.model)[[1]]},
{
  combined.biomass.zippin.ucl<-NA
  combined.biomass.zippin.lcl<-NA
  
  
})
  ##browser()
  ##do full set of calculations if we have estimate and conf limits
  if(!is.na(combined.biomass.zippin.estimate) & !is.na(combined.biomass.zippin.ucl) & !is.na(combined.biomass.zippin.lcl)) {
  
					##plot zippin depletion data and overlay points for zippin estimate(green) and 95%CI's (red)
	 				with(combined.biomass.summary.data, plot(biomass.sum~biomass.cum.catch, ylim=c(0,max(combined.biomass.summary.data$biomass.sum)), xlim=c(0,combined.biomass.zippin.ucl)))
					##abline(combined.biomass.delury.model)
					points(x=combined.biomass.zippin.estimate, y=0, pch=8, col="green", cex=1.5)
					points(x=combined.biomass.zippin.lcl, y=0, pch=8, col="red", cex=1.5)
					points(x=combined.biomass.zippin.ucl, y=0, pch=8, col="red", cex=1.5)
					mtext('Zippin depletion model for all fish biomass')

					##return values for zippen estimates and CI's
					##combined.biomass.delury.estimate<--biomass.delury.model[[1]][[1]]/biomass.delury.model[[1]][[2]]
					print(noquote(""))
					print(noquote('Zippin estimate + 95% cl (combined biomass):'))
					print(noquote(c(combined.biomass.zippin.estimate, combined.biomass.zippin.lcl, combined.biomass.zippin.ucl)))
					print(noquote(""))
##browser()
					##break down overall estimate by species based on proportions
					species.biomass.contributions<-combined.biomass.zippin.estimate*species.proportion$biomass.proportion
					species.proportion<-cbind(species.proportion,species.biomass.contributions)
					species.biomass.contributions.ucl<-combined.biomass.zippin.ucl*species.proportion$biomass.proportion
					species.proportion<-cbind(species.proportion,species.biomass.contributions.ucl)
	 				species.biomass.contributions.lcl<-combined.biomass.zippin.lcl*species.proportion$biomass.proportion
           species.proportion<-cbind(species.proportion,species.biomass.contributions.lcl)
           
         ##get standardized estimates	 				
           stand.species.biomass.contr<-(species.biomass.contributions/data$Area[1])*100
	 				stand.species.biomass.contr.ucl<-(species.biomass.contributions.ucl/data$Area[1])*100
	 				stand.species.biomass.contr.lcl<-(species.biomass.contributions.lcl/data$Area[1])*100
	 				species.proportion<-cbind(species.proportion,stand.species.biomass.contr)
	 				species.proportion<-cbind(species.proportion,stand.species.biomass.contr.ucl)
	 				species.proportion<-cbind(species.proportion,stand.species.biomass.contr.lcl)

					with(species.proportion, barplot(species.biomass.contributions, names.arg=Species, ylab="biomass (g)"))
}

     ##if we have no estimate or conf limits do the following
     if(is.na(combined.biomass.zippin.estimate) & is.na(combined.biomass.zippin.ucl) & is.na(combined.biomass.zippin.lcl)) {
print("CATCH DATA RESULTED IN ZIPPIN MODEL FAILURE")
 
 species.biomass.contributions<-NA
 species.proportion<-cbind(species.proportion,species.biomass.contributions)
 species.biomass.contributions.ucl<-NA
 species.biomass.contributions.lcl<-NA
 species.proportion<-cbind(species.proportion,species.biomass.contributions.ucl)
 species.proportion<-cbind(species.proportion,species.biomass.contributions.lcl)
 stand.species.biomass.contr<-NA
 stand.species.biomass.contr.ucl<-NA
 stand.species.biomass.contr.lcl<-NA
 species.proportion<-cbind(species.proportion,stand.species.biomass.contr)
 species.proportion<-cbind(species.proportion,stand.species.biomass.contr.ucl)
 species.proportion<-cbind(species.proportion,stand.species.biomass.contr.lcl)
 
}
   
        ##do as many calculations as we can if we have estimate but no conf limits
        if(!is.na(combined.biomass.zippin.estimate) & (is.na(combined.biomass.zippin.ucl) | is.na(combined.biomass.zippin.lcl))) {
          
          print(noquote("NO GRAPH PRODUCED - NO CONFIDENCE INTERVALS CALCULATED!!!!!!!!!!!!!!"))
          ##return values for zippen estimates and CI's
          print(noquote(""))
          print(noquote('Zippin estimate (95% cls could not be calculated) (combined biomass):'))
          print(noquote(c(combined.biomass.zippin.estimate)))
          print(noquote(""))
          ##browser()
          ##break down overall estimate by species based on proportions
          species.biomass.contributions<-combined.biomass.zippin.estimate*species.proportion$biomass.proportion
          species.proportion<-cbind(species.proportion,species.biomass.contributions)
          species.biomass.contributions.ucl<-combined.biomass.zippin.ucl*species.proportion$biomass.proportion
          species.proportion<-cbind(species.proportion,species.biomass.contributions.ucl)
          species.biomass.contributions.lcl<-combined.biomass.zippin.lcl*species.proportion$biomass.proportion
          species.proportion<-cbind(species.proportion,species.biomass.contributions.lcl)
          
          ##browser()	 				
          stand.species.biomass.contr<-(species.biomass.contributions/data$Area[1])*100
          stand.species.biomass.contr.ucl<-(species.biomass.contributions.ucl/data$Area[1])*100
          stand.species.biomass.contr.lcl<-(species.biomass.contributions.lcl/data$Area[1])*100
          species.proportion<-cbind(species.proportion,stand.species.biomass.contr)
          species.proportion<-cbind(species.proportion,stand.species.biomass.contr.ucl)
          species.proportion<-cbind(species.proportion,stand.species.biomass.contr.lcl)
          
          with(species.proportion, barplot(species.biomass.contributions, names.arg=Species, ylab="biomass (g)"))
        }
        
					print(noquote('_______________________________________________________________________________________________'))
				
			print(noquote('HIT ENTER TO CONTINUE WITH OVERALL ABUNDANCE'))
			##browser()

			##REPEAT ABOVE FOR ABUNDANCE
			combined.abundance.summary.data<-as.data.frame.table((tapply(data$junk, list(data$Sweep), sum)))
	 		names(combined.abundance.summary.data)<-c('Sweep','abundance.sum')
	 		abundance.cum.catch<-cumsum(combined.abundance.summary.data$abundance.sum)
	 		combined.abundance.summary.data<-cbind(combined.abundance.summary.data,abundance.cum.catch)

	 		combined.abundance.delury.model<-lm(abundance.sum~ abundance.cum.catch, data=combined.abundance.summary.data)
	 				##ifelse(combined.abundance.delury.model[[1]][[2]]<0,
    
           
			##abundance.delury.estimate<--abundance.delury.model[[1]][[1]]/abundance.delury.model[[1]][[2]]
	 		##generate abundance zippin model using the FSA package
	 		combined.abundance.zippin.model<-removal(combined.abundance.summary.data$abundance.sum, method="CarleStrub")
	 		combined.abundance.zippin.estimate<-summary(combined.abundance.zippin.model)[[1]]
	 		
        ifelse(combined.abundance.delury.model[[1]][[2]]<0 & length(combined.abundance.summary.data)>2, {
          
          combined.abundance.zippin.ucl<-confint(combined.abundance.zippin.model)[[3]]
          combined.abundance.zippin.lcl<-confint(combined.abundance.zippin.model)[[1]]},
{
  combined.abundance.zippin.ucl<-NA
  combined.abundance.zippin.lcl<-NA
    
})
        
  
        ##do full set of calculations if we have estimate and conf limits
        if(!is.na(combined.abundance.zippin.estimate) & !is.na(combined.abundance.zippin.ucl) & !is.na(combined.abundance.zippin.lcl)) { 	
  
			##plot zippin depletion data and overlay points for zippin estimate(green) and 95%CI's (red)
	 		with(combined.abundance.summary.data, plot(abundance.sum~abundance.cum.catch, ylim=c(0,max(combined.abundance.summary.data$abundance.sum)), xlim=c(0,combined.abundance.zippin.ucl)))
			##abline(combined.abundance.delury.model)
			points(x=combined.abundance.zippin.estimate, y=0, pch=8, col="green", cex=1.5)
			points(x=combined.abundance.zippin.lcl, y=0, pch=8, col="red", cex=1.5)
			points(x=combined.abundance.zippin.ucl, y=0, pch=8, col="red", cex=1.5)

			mtext('Zippin depletion model for all fish abundance')

			##return values for zippen estimates and CI's
			##combined.abundance.delury.estimate<--abundance.delury.model[[1]][[1]]/abundance.delury.model[[1]][[2]]
			print(noquote(""))
			print(noquote('Zippin estimate + 95% cl (combined abundance):'))
			
			print(noquote(c(combined.abundance.zippin.estimate, combined.abundance.zippin.lcl, combined.abundance.zippin.ucl)))
			print(noquote(""))
			print(noquote('_______________________________________________________________________________________________'))

			##break down overall estimate by species based on proportions and add to species proportion file
			species.abundance.contributions<-combined.abundance.zippin.estimate*species.proportion$abundance.proportion
			species.proportion<-cbind(species.proportion,species.abundance.contributions)
			species.abundance.contributions.ucl<-combined.abundance.zippin.ucl*species.proportion$abundance.proportion
			species.proportion<-cbind(species.proportion,species.abundance.contributions.ucl)
	 		species.abundance.contributions.lcl<-combined.abundance.zippin.lcl*species.proportion$abundance.proportion
	 		species.proportion<-cbind(species.proportion,species.abundance.contributions.lcl)
       stand.species.abundance.contr<-(species.abundance.contributions/data$Area[1])*100
	 		stand.species.abundance.contr.ucl<-(species.abundance.contributions.ucl/data$Area[1])*100
	 		stand.species.abundance.contr.lcl<-(species.abundance.contributions.lcl/data$Area[1])*100
	 		species.proportion<-cbind(species.proportion,stand.species.abundance.contr)
	 		species.proportion<-cbind(species.proportion,stand.species.abundance.contr.ucl)
	 		species.proportion<-cbind(species.proportion,stand.species.abundance.contr.lcl)
##browser()
			##make barplot breaking down abundance by species using the proportion method
			with(species.proportion, barplot(species.abundance.contributions, names.arg=Species, ylab="abundance"))
			mtext(unique.site[aaa])
	 		
}
     ##if we have no estimate or conf limits do the following:
           if(is.na(combined.abundance.zippin.estimate))
     { 
  print("CATCH DATA RESULTED IN ABUNDANCE ZIPPIN MODEL FAILURE")
  
  species.abundance.contributions<-NA
  species.proportion<-cbind(species.proportion,species.abundance.contributions)
  species.abundance.contributions.ucl<-NA
  species.abundance.contributions.lcl<-NA
  species.proportion<-cbind(species.proportion,species.abundance.contributions.ucl)
  species.proportion<-cbind(species.proportion,species.abundance.contributions.lcl)
  stand.species.abundance.contr<-NA
  stand.species.abundance.contr.ucl<-NA
  stand.species.abundance.contr.lcl<-NA
  species.proportion<-cbind(species.proportion,stand.species.abundance.contr)
  species.proportion<-cbind(species.proportion,stand.species.abundance.contr.ucl)
  species.proportion<-cbind(species.proportion,stand.species.abundance.contr.lcl)
}
           ##do as many calculations as we can if we have estimate but no conf limits
           if(!is.na(combined.abundance.zippin.estimate) & (is.na(combined.abundance.zippin.ucl) | is.na(combined.abundance.zippin.lcl))) 
           {   
             
             ##plot zippin depletion data and overlay points for zippin estimate(green) and 95%CI's (red)
             print(noquote("NO GRAPH PRODUCED - NO CONFIDENCE INTERVALS CALCULATED!!!!!!!!!!!!!!"))
             ##return values for zippen estimates and CI's
             print(noquote(""))
             print(noquote('Zippin estimate (95% cls could not be calculated) (combined abundance):'))
              print(noquote(combined.abundance.zippin.estimate))
             print(noquote(""))
             print(noquote('_______________________________________________________________________________________________'))
             
             ##break down overall estimate by species based on proportions and add to species proportion file
             species.abundance.contributions<-combined.abundance.zippin.estimate*species.proportion$abundance.proportion
             species.proportion<-cbind(species.proportion,species.abundance.contributions)
             species.abundance.contributions.ucl<-combined.abundance.zippin.ucl*species.proportion$abundance.proportion
             species.proportion<-cbind(species.proportion,species.abundance.contributions.ucl)
             species.abundance.contributions.lcl<-combined.abundance.zippin.lcl*species.proportion$abundance.proportion
             species.proportion<-cbind(species.proportion,species.abundance.contributions.lcl)
             stand.species.abundance.contr<-(species.abundance.contributions/data$Area[1])*100
             stand.species.abundance.contr.ucl<-(species.abundance.contributions.ucl/data$Area[1])*100
             stand.species.abundance.contr.lcl<-(species.abundance.contributions.lcl/data$Area[1])*100
             species.proportion<-cbind(species.proportion,stand.species.abundance.contr)
             species.proportion<-cbind(species.proportion,stand.species.abundance.contr.ucl)
             species.proportion<-cbind(species.proportion,stand.species.abundance.contr.lcl)
             ##browser()
             ##make barplot breaking down abundance by species using the proportion method
             with(species.proportion, barplot(species.abundance.contributions, names.arg=Species, ylab="abundance"))
             mtext(unique.site[aaa])
             
           }
          
             
  ##return the raw data for species proportion for a given site
			print(species.proportion)
			print(noquote(""))
  ##browser()
			

  
			ifelse(add.to.summary=='Y', print(noquote('ADD TO SUMMARY COMMAND TURNED ON - MAKE SURE THAT e.fishing.summary.data.csv IS IN ANALYSIS FOLDER OTHERWISE A NEW DATA FILE WILL BE CREATED')), print(noquote('ADD TO SUMMARY COMMAND TURNED OFF - IF OLD SUMMARY FILE EXISTS IN ANALYSIS FOLDER IT WILL BE OVERWRITTEN, OTHERWISE NEW FILE CALLED e.fishing.summary.data.csv WILL BE CREATED IN ANALYSIS FOLDER')))
			##browser()
			
			##If add to summary has been toggled it will load the previous file, otherwise it will make the new summary file or overwrite the old file based on this data alone
			##ifelse(add.to.summary=='Y' | aaa*bbb*ccc>1, e.fishing.summary.data<-read.table('e.fishing.summary.data.csv', header=T, sep=','), e.fishing.summary.data<-species.proportion)
  if(aaa*bbb*ccc==1) e.fishing.summary.data<-species.proportion
			##if(add.to.summary!='Y') write.table(e.fishing.summary.data, 'e.fishing.summary.data.csv', sep=',', row.names=F)
			##if add to summary has been toggled, it will add this sites data to the previous summary sheet
			if(add.to.summary=='Y' | aaa*bbb*ccc>1) e.fishing.summary.data<-rbind(e.fishing.summary.data,species.proportion)
			##remove any duplicate entries in the summary file and save it
			if(add.to.summary=='Y' | aaa*bbb*ccc>1) e.fishing.summary.data<-unique(e.fishing.summary.data)
  ##browser()
  		
}
}}
##browser()
##take summary sheet and add traditional estimates of biomass, calculated from mean weights of abundance
##browser()
trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$species.abundance.contributions

ucl.trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$species.abundance.contributions.ucl

lcl.trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$species.abundance.contributions.lcl

stand.trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$stand.species.abundance.contr

ucl.stand.trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$stand.species.abundance.contr.ucl

lcl.stand.trad.species.biomass.contributions<-e.fishing.summary.data$biomass.caught/e.fishing.summary.data$abundance.caught*e.fishing.summary.data$stand.species.abundance.contr.lcl

e.fishing.summary.data<-cbind(e.fishing.summary.data,trad.species.biomass.contributions)
e.fishing.summary.data<-cbind(e.fishing.summary.data,ucl.trad.species.biomass.contributions)
e.fishing.summary.data<-cbind(e.fishing.summary.data,lcl.trad.species.biomass.contributions)
e.fishing.summary.data<-cbind(e.fishing.summary.data,stand.trad.species.biomass.contributions)
e.fishing.summary.data<-cbind(e.fishing.summary.data,ucl.stand.trad.species.biomass.contributions)
e.fishing.summary.data<-cbind(e.fishing.summary.data,lcl.stand.trad.species.biomass.contributions)

##print out the full summary file
print(e.fishing.summary.data)

##if summary data file already exists and traditional work up not required, add summary data to existing file
##browser()
##if((add.to.summary=='Y' | aaa*bbb*ccc>1) & traditional.biomass.work.up!="Y")
if((add.to.summary=='Y') & traditional.biomass.work.up!="Y") {
  existing.data<-read.table('e.fishing.summary.data.csv', header=T, sep=",")
  existing.data<-rbind(existing.data, e.fishing.summary.data)
  existing.data<-unique(existing.data)
  write.table(existing.data, 'e.fishing.summary.data.csv', sep=',', row.names=F)
  print(noquote('NEW DATA ADDED TO EXISTING EFISHING DATA SUMMARY FILE'))}
##if no summary data file exists, write a new one

if(add.to.summary=='N' & traditional.biomass.work.up!="Y")  {write.table(e.fishing.summary.data, 'e.fishing.summary.data.csv', sep=',', row.names=F)
  print(noquote('NEW SUMMARY DATA WRITTEN TO e.fishing.summary.data'))}                                                         


##if toggled, calculate biomass based on traditional way (multiply abundance estimate by mean weight of for each species)
if(traditional.biomass.work.up=="Y"){
  ##browser()
  trad.e.fishing.summary.data<-e.fishing.summary.data[,c("Species","Year","Site","Station","biomass.caught","biomass.proportion","abundance.proportion","abundance.caught","species.abundance.contributions","species.abundance.contributions.ucl","species.abundance.contributions.lcl", "stand.species.abundance.contr","stand.species.abundance.contr.ucl","stand.species.abundance.contr.lcl","trad.species.biomass.contributions","ucl.trad.species.biomass.contributions","lcl.trad.species.biomass.contributions","stand.trad.species.biomass.contributions","ucl.stand.trad.species.biomass.contributions","lcl.stand.trad.species.biomass.contributions")]


 write.table(trad.e.fishing.summary.data, 'trad.e.fishing.summary.data.csv', sep=',', row.names=F)
print(noquote('TRADITIONAL EFISHING SUMMARY WRITTEN TO: trad.e.fishing.summary.data'))}
			browser()

}

