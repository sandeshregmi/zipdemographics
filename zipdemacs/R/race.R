
#' A race demography function.
#' @param t_number race table from acs package.
#' @param end_year survey year
#' @keywords zipdemacs
#' @export
#' @examples
#' race()
#Race Function
#This function leverages acs package and generates
#zipcode level race related summary data.
#race data table B19013 (columns 1,3,4,6)
#library("devtools")
#library("roxygen2")
#pop by povert status table B06012 columns 1,2,3,4


race<-function(t_number, end_year){
  

#api.key.install("7876242c28cec9d378629ab9ba095801e0651d07")

library(acs)
library(choroplethr)

#acs.tables.install()

zip_geo = geo.make(zip.code = "*")


race.data = acs.fetch(geography=zip_geo, table.number=t_number, col.names = "pretty", endyear = end_year)

zip_demographics = data.frame(region = as.character(geography(race.data)$zipcodetabulationarea), 
                              total_population = as.numeric(estimate(race.data[,1])))

zip_demographics$region = as.character(zip_demographics$region)

race_df = data.frame(white_alone_not_hispanic = as.numeric(estimate(race.data[,3])), 
                     black_alone_not_hispanic = as.numeric(estimate(race.data[,4])), 
                     asian_alone_not_hispanic = as.numeric(estimate(race.data[,6])), 
                     hispanic_all_races = as.numeric(estimate(race.data[,12])))

zip_demographics$percent_white = (race_df$white_alone_not_hispanic / zip_demographics$total_population * 100) 
zip_demographics$percent_black = (race_df$black_alone_not_hispanic / zip_demographics$total_population * 100) 
zip_demographics$percent_asian = (race_df$asian_alone_not_hispanic / zip_demographics$total_population * 100) 
zip_demographics$percent_hispanic = (race_df$hispanic_all_races / zip_demographics$total_population * 100)
zip_demographics

}

