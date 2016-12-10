## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 

data <- datacomm
datalabel <- datalabelcomm

#################################################
## We will use barplot for select_one variable

## extracting unique choice questions
datalabel[datalabel$type=="select_one" ,1]
datalabel[datalabel$type=="select_mutiple" ,7]

data.single <- data[ ,c( "organization_name"  ,  "observer_gender",                       
                         "governorate", "district",                              
                          "sub_district", "community",                             
                         "target", "gender_interviewee",
                         "missing_civil_docs" ,                   
                          "missing_civil_docs_why", "missing_civil_docs_why_obtain",         
                         "families_asked_leave_shelter", "families_asked_leave_shelter_reasons",  
                         "problem_dispute_which_entity", "IDP_host_relationship",                 
                         "IDP_host_relationship_reasons", "children_school",                       
                         "children_school_why_no", "education_infra_target",                
                         "access_health_services", "access_health_services_what_barriers",  
                         "access_health_services_where", "health_infra_target",                   
                         "pregnant_women_access", "specialized_services_disabilities",     
                         "access_water_how", "access_water",                          
                         "access_water_what_challenges", "wash_infra_target",                     
                         "humanitarian_assistance",                                      
                         "humanitarian_assistance_challenges", "market_safe_access",                    
                         "electricity_access", "shelter_types",                         
                         "access_employment",   "children_affected_violence",            
                         "children_affected_violence_types",  "children_separated_caregiver",          
                         "children_working",  "children_working_mistreated",           
                         "children_armed_groups", "services_specialneeds" ,                
                         "services_elderly", "children_change_of_behavior" ,          
                         "men_move_freely", "men_move_freely_why_not",               
                         "women_move_freely", "women_move_freely_why_not",             
                         "recent_arrivals", "reasons_left_previous_location",        
                         "recent_departures", "resons_leaving",                        
                         "IDP_like_to_return", "IDP_like_to_stay",                      
                         "families_left_syria", "community_explosive_hazards",           
                         "security_incidents_community", "community_structure_discussion",        
                         "community_structure_discussion_purpose", "community_structure_legitimate",        
                         "community_structure_representative",   "community_structure_useful",            
                         "community_structure_support"
                        
                        ## and now breakdown for multiple choice questions
                        )]

## Remove variable where we get only NA
data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
#names(data.single)

## Let's add again the label for the variable
data.single.label <- as.data.frame(names(data.single))
names(data.single.label)[1] <- "name"
datalabel.map <- datalabel[ ,c(7,15)]
names(datalabel.map)[1] <- "name"
data.single.label <- join (x=data.single.label, y=datalabel.map, by="name", type="left" )
attributes(data.single)$variable.labels <- data.single.label$label

## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(data.single), aes(y=data.single$Household_information.Family_Size)) + ylab("# of Ind") + scale_y_continuous(labels=format_si())
for (i in 3:34 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  title <- attributes(data.single)$variable.labels[i]
  rm(plot)
  plot <- p + 
     aes_string(x = names(data.single)[i]) +
   # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    # coord_flip()+
     xlab("") + 
    coord_flip() + 
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}


## Now trying ot facet by gvt 

p <- ggplot(data.frame(data.single), aes(y=data.single$Household_information.Family_Size)) + 
    ylab("# of Ind") + 
    scale_y_continuous(labels=format_si())
for (i in 3:34 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  title <- attributes(data.single)$variable.labels[i]
  rm(plot)
  plot <- p + 
    aes_string(x = names(data.single)[i]) +
    # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    facet_wrap(~ Location.Governorate , ncol=3) +
    # coord_flip()+
    xlab("") + 
    coord_flip() + 
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,"facet_gov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}


### Now let's create propoortion graphs --

