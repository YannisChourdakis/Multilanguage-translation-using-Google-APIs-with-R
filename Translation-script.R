packages <- c("googleLanguageR", "cld2", "textcat","tidyverse", "datasets")
install.packages(packages, repos = 'http://cran.us.r-project.org')
lapply(packages, require, character.only=TRUE)


# First step: Google Translation API authorisation
gl_auth("google_api_auth.json")

# Here we use the ```sentences``` dataset of the ```datasets``` package.
str(sentences)
head(sentences)


# In case the words were not capitalised, the following function capitalises the first letter of each sentence of the string data:
first_letter_upper=function(x) ifelse(is.na(x)==TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))


# Let's clean the string data to prepare it for the translation:
sentences_new <- sentences %>% 
  sapply(., str_trim) %>%
  sapply(., str_squish) %>%
  sapply(., first_letter_upper) %>%
  na.omit %>%
  data.frame(stringsAsFactors = FALSE)

# Third step: Kick-off by detecting the language of the existing data:
detected_language <- sentences_new %>% sapply(., map_chr, detect_language) %>% data.frame


# Let's annotate the ones that will be translated during our process in order to know which elements were translated by us:
#for (i in 1:ncol(sentences_new)){
  for (k in 1:nrow(sentences_new)){
    sentences_new[k,][detected_language[k,] =="en"] <- 
     paste("(Translated)", sentences_new[k,][detected_language[k,] =="en"], sep = " ")
  }
#}


# Ready to initiate the translation!
for (i in 1:ncol(sentences_new)){
  sentences_new[,i][detected_language[,i] =="en" & !is.na(detected_language[,i])] <- 
    data.frame(
      gl_translate(
        sentences_new[,i][detected_language[,i] =="en" & !is.na(detected_language[,i])], target = "it"
      )
    )[,1]
  
  sentences_new[,i][sentences_new[,i] %in% c("NA", "N/a", "N/A", "Na", "na", "n/a", "not applicable")] <- NA
}

head(sentences_new)


