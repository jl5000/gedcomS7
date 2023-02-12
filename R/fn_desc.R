
desc_indi = function(x, xrefs){
  nms = character()
  for(xref in xrefs){
    nms = c(nms, pull_record(x,xref)@primary_name)
  }
  nms
}

life_story <- function(sex = NULL,
                       name = NULL,
                       dob = NULL,
                       pob = NULL,
                       mother_name = NULL,
                       father_name = NULL,
                       mother_dob = NULL,
                       mother_dod = NULL,
                       father_dob = NULL,
                       father_dod = NULL,
                       partner_name = NULL,
                       partner_sex = NULL,
                       married = NULL,
                       num_chil = NULL,
                       dom = NULL,
                       pom = NULL,
                       dod = NULL,
                       pod = NULL){
  
  "When {name} was born in {dob} in {pob}, 
  {sex} father, {father_name}, was {father_dob/father_dod} and
  {sex} mother, {mother_name} was {mother_dob/mother_dod}.
  
  {married} {sex} married {partner_name} in {dom} in {pom}.
  They/{sex} (have) had {num_chil} in {min(mother_dod, father_dod) - dom} years.
  
  {sex} died in {dod} in {pod} at the age of {dod - dob}."
  
  
}
