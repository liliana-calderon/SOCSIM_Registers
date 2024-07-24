#----------------------------------------------------------------------------------------------------
# Functions to get Reference Table and add birthyear and deathyear of ID's and refIDs
# Obtained from Martin Kolk
# Last (slightly) modified on the 22-07-2024
#----------------------------------------------------------------------------------------------------

getRefTable <- function(df , ref_TypeI){

  ## Parameters:
  #   df          : Data frame of population ("popdat") with following order of columns:
  #                 c("LopNr" ,"FoddAR", "Kon", "FodelselandGrp", "LopNrFar", "LopNrMor")
  #   ref_TypeI   : c("parent", "child", "sibling", "partner", "parsib", 
  #                 "cousin", "grand", "granchild", "sibchild",  "all")
  #
  #
  # Possible ref structures
  #
  #  ref_TypeI | ref_TypeII | ref_TypeIII  | ref_TypeIIII 
  # -----------|------------|--------------|--------------|
  #  parent    | father     |              |              |
  #     .      | mother     |              |              |
  # -----------|------------|------------- |--------------|
  # child      | son(1)     | partner.order|              |
  #     .      | daughter(2)|              |              |
  # -----------|------------|--------------|--------------|
  # sibling    | brother    | full         | full         |
  #     .      | sister     | half         | half.mother/father
  # ---------------------------------------|--------------|
  # partner    | married    | same.sex     | additional information|
  #     .      | sambo      | diff.sex     |              |
  # ---------------------------------------|--------------|
  # parsib     | aunt       | mothers side | full.sib     |
  #     .      | uncle      | fathers side | half.sib     |
  # ---------------------------------------|--------------|
  # cousin     | male       | moth.sis/bro | one shared grandparents
  #     .      | female     | fath.sis/bro | two shared grandparents
  # ---------------------------------------|--------------|
  # grand      | grandmother| mothers side |              |
  #     .      | grandfather| fathers side |              |
  # ---------------------------------------|--------------|
  # grandchild | grandson   | daughter     |              |
  #     .      | granddaughter|son         |              |
  # ---------------------------------------|--------------|
  # sibchild   | niece      | sister       | full         |          
  #     .      | nephew     | brother      | half         |
  # ---------------------------------------|--------------|
  
  reslist <- list()
  
  #### Create parent table
  if(ref_TypeI == "parent" | ref_TypeI == "parsib" | ref_TypeI == "cousin" | ref_TypeI == "grandparent" | ref_TypeI == "all"){
    
    parent_df <- 
      df %>% 
      select(LopNr, LopNrMor, LopNrFar) %>% 
      setDT() %>% 
      melt(id.vars = "LopNr") %>%
      select(ID = LopNr, refID = value, refTypeII = variable) %>% 
      mutate(refTypeI    = ifelse(is.na(refID), "no parent", "parent"),
             refTypeII   = ifelse(refTypeII == "LopNrMor", "mother", "father"), 
             refTypeII   = ifelse(is.na(refID), paste0("no ", refTypeII), refTypeII),
             refTypeIII  = NA,
             refTypeIIII = NA) %>%
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII)
    
    reslist[["parent_df"]] <- parent_df
    
  }
  
  #### Create child table
  if(ref_TypeI == "child" | ref_TypeI == "cousin" | ref_TypeI == "grandchild" | ref_TypeI == "sibchild" |  ref_TypeI == "all" ){

    ## Returns all IDs in the popdat LopNrMor and  LopNrFar collumns with refID = corresponding children.
    ## + remaining people in popdat (with IDs not in LopNrMor and  LopNrFar) with refID =  NA
    child_df <-
      df %>%
      select(LopNr, LopNrMor, LopNrFar) %>%
      setDT() %>%
      melt(id.vars = "LopNr") %>%
      select(ID = value, refID = LopNr) %>%
      filter(!is.na(ID)) %>%
      left_join(select(df, LopNr, Kon, FoddAr, LopNrMor, LopNrFar), by = c("refID" = "LopNr")) %>%
      mutate(refTypeI    = "child",
             refTypeII   = case_when(Kon == 1 ~ "son", Kon == 2 ~ "daughter", is.na(Kon) ~ "NAgender" ),
             refTypeIIII = NA,
             partner     = case_when(ID == LopNrMor ~ LopNrFar, ID == LopNrFar ~ LopNrMor)) %>%
      select(ID, refID, refTypeI, refTypeII, FoddAr, partner, refTypeIIII)

    # Original code changed to tidyverse language because of an error
    # child_df <- child_df[order(ID, FoddAr), i := rleid(partner), by = .(ID)]
    # Error in `[.data.frame`(child_df, order(ID, FoddAr), `:=`(i, rleid(partner)),  : 
    # unused argument (by = .(ID))
    
   child_df <- child_df %>%
     arrange(ID, FoddAr) %>%
     group_by(ID) %>%
     mutate(i = data.table::rleid(partner)) %>% 
     ungroup() %>% 
     select(ID, refID, refTypeI, refTypeII, refTypeIII = i, refTypeIIII) %>%
     full_join(select(df, LopNr), by = c("ID" = "LopNr"))   %>%
     mutate(refTypeI   = case_when(is.na(refID) ~ "no child", TRUE ~ refTypeI ),
             refTypeII  = case_when(is.na(refID) ~ "no child", TRUE ~ refTypeII ),
             refTypeIII = case_when(is.na(refID) ~ "no child", TRUE ~ as.character(refTypeIII)))

    reslist[["child_df"]] <- child_df

  }
  
  #### Create sibling table
  if(ref_TypeI == "sibling" | ref_TypeI == "parsib" | ref_TypeI == "cousin" | ref_TypeI == "sibchild" |  ref_TypeI == "all"){
    
    ## Table of siblings with same parents
    df_temp <- df %>% 
      filter(!(is.na(LopNrMor) & is.na(LopNrFar)))
    
    full.siblings <- 
      df_temp %>% 
      select(refID = LopNr, LopNrMor, LopNrFar, Kon) %>% 
      inner_join(select(df_temp, LopNr, LopNrMor, LopNrFar), 
                 by = c("LopNrMor" = "LopNrMor", "LopNrFar" = "LopNrFar"), 
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one sibling (and then multiple matches)
                 relationship = "many-to-many") %>%
      filter(refID != LopNr) %>%
      select(ID = LopNr, refID, refTypeII = Kon) %>%
      mutate(refTypeI    = "sibling",
             refTypeII   = case_when(is.na(refTypeII) ~ "NAgender", refTypeII == 1 ~ "brother", refTypeII == 2 ~ "sister"),
             refTypeIII  = "full",
             refTypeIIII = "full") %>%
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII) 
    
    
    ## Table of siblings with same mother different or NA fathers
    df_temp_mothers <- filter(df_temp, !is.na(LopNrMor))
    
    moth.siblings   <- df_temp_mothers %>% 
      select(refID = LopNr, LopNrMor, LopNrFar, Kon) %>% 
      inner_join(select(df_temp_mothers, LopNr, LopNrMor, LopNrFar),
                 by = c("LopNrMor" = "LopNrMor"), 
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one sibling (and then multiple matches)
                 relationship = "many-to-many") %>%
      filter(LopNrFar.x != LopNrFar.y | (is.na(LopNrFar.x) & is.na(LopNrFar.y))) %>% 
      select(ID = LopNr, refID, refTypeII = Kon) %>%
      filter(ID != refID) %>%
      mutate(refTypeI    = "sibling",
             refTypeII   = case_when(is.na(refTypeII) ~ "NAgender", refTypeII == 1 ~ "brother",  refTypeII == 2 ~ "sister"),
             refTypeIII  = "half",
             refTypeIIII = "half.mother") %>%
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII)
    
    
    ## Table of siblings with same father different or NA mothers
    df_temp_fathers <- filter(df_temp, !is.na(LopNrFar))
    
    fath.siblings   <- 
      df_temp_fathers %>% 
      select(refID = LopNr, LopNrMor, LopNrFar, Kon) %>% 
      inner_join(select(df_temp_fathers, LopNr, LopNrMor, LopNrFar), 
                 by = c("LopNrFar" = "LopNrFar"), 
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one sibling (and then multiple matches)
                 relationship = "many-to-many") %>%
      filter(LopNrMor.x != LopNrMor.y | (is.na(LopNrMor.x) & is.na(LopNrMor.y))) %>%
      select(ID = LopNr, refID, refTypeII = Kon) %>%
      filter(ID != refID) %>%
      mutate(refTypeI    = "sibling",
             refTypeII   = case_when(is.na(refTypeII) ~ "NAgender", refTypeII == 1 ~ "brother",  refTypeII == 2 ~ "sister"),
             refTypeIII  = "half",
             refTypeIIII = "half.father") %>%
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII)
    
    sibling_df  <- rbind(moth.siblings, fath.siblings, full.siblings) %>%
      distinct(ID,refID, .keep_all = TRUE)
    
    # Table of individuals with no siblings
    no.siblings <- data.frame(setdiff(unique(df$LopNr), unique(sibling_df$ID))) %>% 
      select(ID = 1) %>%
      mutate(refID = NA, 
             refTypeI = "no sibling", 
             refTypeII = "no sibling",
             refTypeIII = "no sibling",
             refTypeIIII = "no sibling")
    
    sibling_df <- rbind(sibling_df, no.siblings)
    
    reslist[["sibling_df"]] <- sibling_df
  }
  
  #### Get parent.siblings table
  if (ref_TypeI == "parsib" | ref_TypeI == "cousin" | ref_TypeI == "all"){
    
    # Get parent.siblings
    parsib_df <- parent_df %>% 
      filter(!is.na(refID)) %>% 
      select(ID, refID, refTypeII, refTypeIII) %>%
      inner_join(y = filter(sibling_df[, c(1:2,4,5)], !is.na(refID)), 
                 by = c("refID" = "ID"), 
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one sibling (and then multiple matches)
                 relationship = "many-to-many") %>%
      mutate(refID = refID.y, 
             refTypeI   = "parSib", 
             refTypeII  = ifelse(refTypeII.y == "brother", "uncle", "aunt"),
             refTypeIII = refTypeII.x,
             refTypeIIII= refTypeIII.y)  %>% 
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII) %>% 
      full_join(select(filter(df, !( unique(df$LopNr) %in% unique(.$ID))), ID = LopNr), by = c("ID" = "ID")) %>% 
      mutate(refTypeI   = ifelse(is.na(refID), "no parSib", refTypeI), 
             refTypeII  = ifelse(is.na(refID), "no parSib", refTypeII) ,
             refTypeIII = ifelse(is.na(refID), "no parSib", refTypeIII),
             refTypeIIII= ifelse(is.na(refID), "no parSib", refTypeIIII))
    
    reslist[["parsib_df"]] <- parsib_df
  }
  
  #### Get cousins table
  if (ref_TypeI == "cousin" | ref_TypeI == "all"){
    
    cousin_df <- parsib_df %>% 
      filter(!is.na(refID))  %>%
      inner_join(y = filter(child_df[, c(1:2,4)], !is.na(refID)), 
                 by = c("refID" = "ID"), 
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one sibling (and then multiple matches)
                 relationship = "many-to-many") %>%
      mutate(refID = refID.y, 
             refTypeI   = "cousin", 
             refTypeII  = ifelse(refTypeII.y == "son", "male", "female"),
             refTypeIII = case_when(refTypeII.x == "uncle" & refTypeIII == "mother" ~ "mother.brother",
                                    refTypeII.x == "aunt" & refTypeIII == "mother"~ "mother.sister",
                                    refTypeII.x == "uncle" & refTypeIII == "father" ~ "father.brother",
                                    refTypeII.x == "aunt" & refTypeIII == "father"~ "father.sister"),
             refTypeIIII = ifelse(refTypeIIII == "full", 2, 1 )) %>% 
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII) %>% 
      full_join(select(filter(df, !( unique(df$LopNr) %in% unique(.$ID))), ID = LopNr), by = c("ID" = "ID")) %>% 
      mutate(refTypeI   = ifelse(is.na(refID), "no cousin", refTypeI), 
             refTypeII  = ifelse(is.na(refID), "no cousin", refTypeII) ,
             refTypeIII = ifelse(is.na(refID), "no cousin", refTypeIII),
             refTypeIIII = ifelse(is.na(refID), "no cousin", refTypeIIII))
    
    reslist[["cousin_df"]] <- cousin_df
  }
  
  #### Get grandparent tables
  if (ref_TypeI == "grandparent" | ref_TypeI == "all"){
    
    grandparent_df <- parent_df %>% 
      filter(!is.na(refID)) %>% 
      select(ID, refID, refTypeII) %>% 
      inner_join(x = . , y = ., 
                 by = c("refID" = "ID"),
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID (parent) can have more than one parent (and then multiple matches)
                 relationship = "many-to-many") %>%
      mutate(refID = refID.y, 
             refTypeI   = "grandparent", 
             refTypeII  = ifelse(refTypeII.y == "mother", "grandmother", "grandfather"),
             refTypeIII = refTypeII.x) %>% 
      select(ID, refID, refTypeI, refTypeII, refTypeIII) %>% 
      full_join(select(filter(df, !( LopNr %in% .$ID)), ID = LopNr), 
                by = c("ID" = "ID")) %>% 
      mutate(refTypeI   = ifelse(is.na(refID), "no grandparent", refTypeI), 
             refTypeII  = ifelse(is.na(refID), "no grandparent", refTypeII) ,
             refTypeIII = ifelse(is.na(refID), "no grandparent", refTypeIII),
             refTypeIIII = NA)
    
    reslist[["grandparent_df"]] <- grandparent_df
  }
  
  #### Get grandchild tables
  if (ref_TypeI == "grandchild" | ref_TypeI == "all"){
    
    grandchild_df <- child_df %>% 
      filter(!is.na(refID)) %>% 
      select(ID, refID, refTypeII) %>%
      inner_join(x = ., y = ., 
                 by = c("refID" = "ID"),
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID can have more than one children (and then multiple matches)
                 relationship = "many-to-many") %>%
      mutate(refID = refID.y, 
             refTypeI   = "grandchild", 
             refTypeII  = ifelse(refTypeII.y == "daughter", "granddaughter", "grandson"),
             refTypeIII = refTypeII.x)%>% 
      select(ID, refID, refTypeI, refTypeII, refTypeIII) %>% 
      full_join(select(filter(df, !( LopNr %in% .$ID)), ID = LopNr), by = c("ID" = "ID")) %>% 
      mutate(refTypeI   = ifelse(is.na(refID), "no grandchild", refTypeI), 
             refTypeII  = ifelse(is.na(refID), "no grandchild", refTypeII) ,
             refTypeIII = ifelse(is.na(refID), "no grandchild", refTypeIII),
             refTypeIIII = NA)
    
    reslist[["grandchild_df"]] <- grandchild_df
    
  }
  
  #### Get sibling child table
  if (ref_TypeI == "sibchild" | ref_TypeI == "all"){
    
    sibchild_df <- sibling_df %>% 
      filter(!is.na(refID)) %>%
      inner_join(x = ., y = child_df, 
                 by = c("refID" = "ID"),
                 # To avoid the warning Detected an unexpected many-to-many relationship between `x` and `y`.
                 # Because a refID (sibling) can have more than one children (and then multiple matches)
                 relationship = "many-to-many") %>%
      mutate(refID       = refID.y, 
             refTypeI    = "sibchild", 
             refTypeII   = ifelse(refTypeII.y == "daughter", "niece", "nephew"),
             refTypeIII  = refTypeII.x,
             refTypeIIII = refTypeIII.x) %>% 
      select(ID, refID, refTypeI, refTypeII, refTypeIII, refTypeIIII) %>% 
      full_join(select(filter(df, !( df$LopNr %in% .$ID)), ID = LopNr), by = c("ID" = "ID")) %>% 
      mutate(refTypeI   = ifelse(is.na(refID), "no sibchild", refTypeI), 
             refTypeII  = ifelse(is.na(refID), "no sibchild", refTypeII) ,
             refTypeIII = ifelse(is.na(refID), "no sibchild", refTypeIII),
             refTypeIIII = ifelse(is.na(refID), "no sibchild", refTypeIIII))
    
    reslist[["sibchild_df"]] <- sibchild_df
    
  }
  
  
  return(reslist)
  
}

AddBirthsDeaths <- function(refTable, df){
  
  # Parameters
  # refTable : output from the getRefTable function
  #   df          : Data frame of population ("popdat") with following order of columns:
  #                 c("LopNr" ,"FoddAR", "Kon", "FodelselandGrp", "LopNrFar", "LopNrMor")
  
  # Add birthyear and deathyear of ID's and refIDs
  out <- left_join(refTable, df[, c("LopNr","FoddAr","deathyear")], by = c("ID" = "LopNr")) # Add ID birthyears and deathyears
  out <- left_join(out, df[, c("LopNr","FoddAr","deathyear")], by = c("refID" = "LopNr"))   # Add refID  birthyears and deathyears
  names(out)[7:10] <- c("IDbirthYear", "IDdeathYear", "refIDbirthYear","refIDdeathYear")
  
  return(out)
  
}