The FIAS_FIRLS_models.RData contains three functions, FIAS_model_selection, FIRLS_model_selection, diet_pca
Both FIAS and FIRLS models need Diet PC1-6 derived from diet_pca
# example dataframes: diet_factors, FI_item_data 
# age_assess, age (days) at assessment of frailty 
# sex_bin, Female, "1"; Male, "0"
  library(dplyr)
  diet_df = diet_factors %>% # example to get diet PC1-6
    dplyr::select(carbo, protein, fat, fiber, ash, choline, va, 
                  vb1, vb2, vb3, vb5, vb6, vb7, vb9, vb12, vd, ve, vk) # need 18 factors from the nutrient factor of the diet
  diet_pcas = predict(diet_pca, newdata = diet_df) %>%
    `colnames<-`(paste0("diet_PC", 1:18)) %>%
     dplyr::select(paste0("diet_PC", 1:6)) %>% #'FIAS_model_selection' and 'FIRLS_model_selection' need paste0("diet_PC", 1:6)
     distinct()  
  fi_df = cbind(FI_item_data, df_diet %>% slice(rep(1, nrow(FI_item_data)))) # this example only use one diet, change accordingly if more than one
# example to use FIAS_model_selection
  data_fias = sapply(1:nrow(fi_df), function(x) {
    dat_for_fias = fi_df[x, ] %>%
      dplyr::select(age_assess, Alopecia, Loss_of_fur_colour, Loss_of_whiskers, Coat_condition, Tumours, Distended_abdomen, Kyphosis,
      Tail_stiffening, Gait_disorders, Tremor, Forelimb_grip_strength, Body_condition_score, Vestibular_disturbance, Eye_discharge_swelling,
      Microphthalmia, Vision_loss, Menace_reflex, Breathing_rate_depth, Piloerection, sex_bin, diet_PC1, diet_PC2, diet_PC3, diet_PC4, diet_PC5, diet_PC6)              "diet_PC6")
    age = FI_item_data[x, ]$age_assess
    sex = FI_item_data[x, ]$sex
    strain = FI_item_data[x, ]$strain
    model = FIAS_model_selection(age, sex, strain)
    fias = predict(model, dat_for_fias[, -1])
    return(fias)})    
# example to use FIRLS_model_selection
  data_firls = sapply(1:nrow(fi_df), function(x) {
    dat_for_firls = fi_df[x, ] %>%
      dplyr::select(Alopecia, Loss_of_fur_colour, Loss_of_whiskers, Coat_condition, Tumours, Distended_abdomen, Kyphosis,
      Tail_stiffening, Gait_disorders, Tremor, Forelimb_grip_strength, Body_condition_score, Vestibular_disturbance, Eye_discharge_swelling,
      Microphthalmia, Vision_loss, Menace_reflex, Breathing_rate_depth, Piloerection, age_assess, sex_bin, diet_PC1, diet_PC2, diet_PC3, diet_PC4, diet_PC5, diet_PC6)              "diet_PC6")
    strain = FI_item_data[x, ]$strain
    model = FIRLS_model_selection(strain)
    firls = predict(model, dat_for_firls)
    return(firls)})
