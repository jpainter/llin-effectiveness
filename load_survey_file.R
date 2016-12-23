
library(dplyr)


# functions for loading data files ####
if (!existsFunction( "survey_GIS_data" ) ) source( "../DHS/getSurveyGIS.R")

openSurveyFile = function(
    country = NA ,
    # survey_year = NA,
    survey = NA,
    year = NA,
    tab = NA
)
{
    
    x = NULL
    
    file = paste0( "../DHS/" , 
        ifelse( country %in% "DRC", "Congo Democratic Republic", country),
        "/", survey, " ", year,  "/", tab, ".rda")
    
    if ( file.exists( file ) ){
        load( file ) # file will be loaded as 'x'
        
    } else {
        
        filecodes = data_frame( abrev  = c("kr", "br", "ir", "pr", "hr", "hr" ), 
                                full = c("Children's Recode","Supplemental Births Recode","Individual Recode",
                                         "Household Member Recode", "Household Recode", "Supplemental Household Recode")) 
        
        dir = 
            paste0(  "../DHS/", country, "/", year, "_", survey, "/"  )
        
        files = list.files(dir)
        
        prefix = paste0(  tolower( countrycode( country, "country.name", "iso2c") ),
                          filecodes$abrev[ match(tab, filecodes$full) ] )
        
        middle = substr( files[-grep(prefix, files, fixed=T)][1], 5, 8 )
        
        suffix = ".rds"
        
        file = paste0( dir, prefix, middle, suffix) 
        
        if ( file.exists( file ) ){
            x = readRDS( file ) # file will be loaded as 'x'
        }
        
    }
    
    return(x)
}

# TODO: get printout working.  R does not normally print from inside a function.
load_survey_file = function( 
    .country = "Angola", 
    # .survey_year = "MIS 2011",
    .year = 2011 ,
    .survey = "DHS",
    .file = "Household Member Recode" ,
    design = FALSE, # return survey design object
    dataset = TRUE, # returns dataset (x) 
    geo = FALSE, 
    printout = FALSE,
    vars = NULL  # if not specified, will select variables from vars() [dhs_variable_selection.R]
){
    
    # no vars given, get basic list of variables
    if ( is.null(vars) ){ 
        source("dhs_variable_selection.R")
        vars = some_variables()
    } else {
        vars = tolower(vars)
    }
    
    if (printout){ cat(vars) }
    
    x = try(
        openSurveyFile(country = .country, survey = .survey, year = .year,
                       tab = .file )
    )
    
    if ( 
         (class(x) == "try-error" | class(x) == "logical" | is.null(x) ) ){
        x = try(
            openSurveyFile(country = .country,  survey = .survey, year = .year,
                           tab = paste("Supplemental", .file )
            )
        )
    }
    
    if ( 
        (class(x) == "try-error" | class(x) == "logical" | is.null(x) ) ){
        x = try(
            openSurveyFile(country = .country,  survey = .survey, year = .year,
                           tab = .file 
            )
        )
    }
    
    
    if (geo){
        g = try(
            survey_GIS_data( country = .country,  survey = .survey, year = .year)
            
        )
        
        if ( class(g) == "data.frame" ) x = x %>% left_join(g, by=c("hv001"="dhsid") )
            
    } else {  g = NULL }
    
    
    
    # include available data
    vars_in_x = sapply(vars, function(y)  y %in% names(x))  # check if variable is in dataset
    x = x %>% select_( .dots = vars[vars_in_x] )
    
    
    if (printout){
        cat( "The", .file, "file has", nrow(x), "rows and ", ncol(x), "columns", "\n"
        )
    }
    
    return( x ) 
}
    
