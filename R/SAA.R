
#' Deploy the SAA
#'
#' @param num_items
#' @param item_bank
#' @param demographics
#' @param demo
#' @param feedback
#' @param admin_password
#' @param SNR_test
#' @param get_range
#' @param absolute_url
#' @param examples
#' @param final_results
#' @param state
#' @param store_results_in_db
#' @param test_username
#' @param gold_msi
#' @param with_final_page
#' @param item_length
#' @param melody_sound
#' @param adjust_range
#' @param test_name
#'
#' @return
#' @export
#'
#' @examples
SAA <- function(num_items = list("long_tones" = 6L,
                                 "arrhythmic" = 10L,
                                 "rhythmic" = 10L),
                item_bank = itembankr::Berkowitz,
                demographics = TRUE,
                demo = FALSE,
                feedback = FALSE,
                admin_password = "demo",
                SNR_test = TRUE,
                get_range = TRUE,
                absolute_url,
                examples = 2,
                final_results = TRUE,
                state = "production",
                store_results_in_db = FALSE,
                test_username = NULL,
                gold_msi = TRUE,
                with_final_page = TRUE,
                item_length = c(3,15),
                melody_sound = "piano",
                adjust_range = TRUE,
                test_name = "Melody Singing Task") {

  if(demo) warning('Running SAA in demo mode!')


  timeline <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::join(

        psychTestR::module("SAA",
                           # introduction, same for all users
                           SAA_intro(demo,
                                     SNR_test,
                                     get_range, absolute_url = absolute_url,
                                     state = state,
                                     store_results_in_db = store_results_in_db,
                                     test_username = test_username,
                                     adjust_range = adjust_range),

                           # long tone trials
                           musicassessr::long_tone_trials(num_items$long_tones, num_examples = examples, feedback = feedback),

                           # arrhythmic
                           musicassessr::arrhythmic_melody_trials(itembankr::subset_item_bank(item_bank("main"), item_length = item_length),
                                                                  num_items = num_items$arrhythmic,
                                                                  num_examples = examples,
                                                                  feedback = feedback,
                                                                  sound = melody_sound),

                           # rhythmic
                           musicassessr::rhythmic_melody_trials(item_bank = itembankr::subset_item_bank(item_bank("phrases"), item_length),
                                                                num_items = num_items$rhythmic,
                                                                num_examples = examples,
                                                                feedback = feedback,
                                                                sound = melody_sound),

                           psychTestR::elt_save_results_to_disk(complete = TRUE),

                           if(final_results) musicassessr::final_results(test_name = test_name,
                                                                         url = absolute_url,
                                                                         num_items$long_tones,
                                                                         num_items$arrhythmic,
                                                                         num_items$rhythmic)

        )
      ),
      dict = SAA_dict
    ),
    if(gold_msi) psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
    musicassessr::deploy_demographics(demographics),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    musicassessr::final_page_or_continue_to_new_text(final = with_final_page, task_name = "Melody Singing Task")
  )

}






#' Deploy SAA as standalone test
#'
#' @param num_items
#' @param item_bank
#' @param demographics
#' @param demo
#' @param feedback
#' @param admin_password
#' @param SNR_test
#' @param get_range
#' @param absolute_url
#' @param examples
#' @param final_results
#' @param state
#' @param store_results_in_db
#' @param test_username
#' @param gold_msi
#' @param with_final_page
#' @param item_length
#' @param melody_sound
#' @param adjust_range
#' @param test_name
#'
#' @return
#' @export
#'
#' @examples
SAA_standalone <- function(num_items = list("long_tones" = 6L,
                                            "arrhythmic" = 10L,
                                            "rhythmic" = 10L),
                           item_bank = itembankr::Berkowitz,
                           demographics = TRUE,
                           demo = FALSE,
                           feedback = FALSE,
                           admin_password = "demo",
                           SNR_test = TRUE,
                           get_range = TRUE,
                           absolute_url,
                           examples = 2L,
                           final_results = TRUE,
                           state = "production",
                           store_results_in_db = FALSE,
                           test_username = NULL,
                           gold_msi = TRUE,
                           with_final_page = TRUE,
                           item_length = c(3,15),
                           melody_sound = "piano",
                           adjust_range = TRUE,
                           test_name = "Melody Singing Task") {

  timeline <- SAA(num_items,
                  item_bank,
                  demographics,
                  demo,
                  feedback,
                  admin_password,
                  SNR_test,
                  get_range,
                  absolute_url,
                  examples,
                  final_results,
                  state,
                  store_results_in_db,
                  test_username,
                  gold_msi,
                  with_final_page,
                  item_length,
                  melody_sound,
                  adjust_range,
                  test_name = test_name)

  # run the test
  psychTestR::make_test(
    elts = timeline,
    opt = psychTestR::test_options(title = "Melody Singing Task v2",
                                   admin_password = admin_password,
                                   display = psychTestR::display_options(
                                     left_margin = 1L,
                                     right_margin = 1L,
                                     css = system.file('www/css/style.css', package = "musicassessr")
                                   ),
                                   languages = c("en")
    ))
}


SAA_intro <- function(demo = FALSE,
                      SNR_test = TRUE,
                      get_range = TRUE,
                      absolute_url,
                      test_username = NULL,
                      store_results_in_db = FALSE,
                      state = "production",
                      adjust_range = TRUE) {

  c(
    musicassessr::musicassessr_init(test = "SAA", test_username = test_username, store_results_in_db),

    # introduction page
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("SAA_welcome")),
                                                       shiny::tags$img(src = 'custom-assets/img/intro.png', height = 100, width = 100),
                                                       shiny::tags$p(psychTestR::i18n("SAA_welcome_1")),
                                                       shiny::tags$p(psychTestR::i18n("SAA_welcome_2"))),
                                button_text = psychTestR::i18n("Next")),

    musicassessr::setup_pages(input = "microphone",
                              demo = demo,
                              get_instrument_range = get_range,
                              SNR_test = SNR_test,
                              absolute_url = absolute_url,
                              adjust_range = adjust_range),
    # instructions
    SAA_instructions()
  )

}

SAA_instructions <- function() {

  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("SAA_instructions1")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions2")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions3")),
                                                     shiny::tags$p(psychTestR::i18n("SAA_instructions4"))),
                              button_text = psychTestR::i18n("Next"))
}




.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "SAA") # path to resource in your package
  )
  # shiny::addResourcePath(
  #   prefix = "item_banks", # custom prefix that will be used to reference your directory
  #   directoryPath = system.file("item_banks", package = "itembankr") # path to resource in your package
  # )
}




