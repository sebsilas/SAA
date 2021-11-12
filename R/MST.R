.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "MST") # path to resource in your package
  )
  shiny::addResourcePath(
    prefix = "item_banks", # custom prefix that will be used to reference your directory
    directoryPath = system.file("item_banks", package = "itembankr") # path to resource in your package
  )
}



#' Deploy the MST
#'
#' @param aws_credentials
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
#'
#' @return
#' @export
#'
#' @examples
MST <- function(aws_credentials,
                num_items = list("long_tones" = 6L,
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
                final_results = TRUE) {

  if(demo) warning('Running MST2 in demo mode!')

  timeline <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::join(

        psychTestR::module("MST",
                           # introduction, same for all users
                           MST_intro(aws_credentials, demo, SNR_test, get_range, absolute_url = absolute_url),

                           # long tone trials
                           musicassessr::long_tone_trials(num_items$long_tones, num_examples = examples, feedback = feedback),

                           # arrhythmic
                           musicassessr::arrhythmic_melody_trials(item_bank = item_bank("main"),
                                                                  num_items = num_items$arrhythmic,
                                                                  num_examples = examples,
                                                                  feedback = feedback,
                                                                  sound = "voice_doo"),

                           # rhythmic
                           musicassessr::rhythmic_melody_trials(item_bank = item_bank("phrases"),
                                                                num_items = num_items$rhythmic,
                                                                num_examples = examples,
                                                                feedback = feedback,
                                                                sound = "voice_doo"),

                           psychTestR::elt_save_results_to_disk(complete = TRUE),

                           if(final_results) musicassessr::final_results(test_name = "Melody Singing Task",
                                                                         url = "https://adaptiveeartraining.com/MST")

        )
      ),
      dict = MST_dict
    ),
    psyquest::GMS(subscales = c("Musical Training", "Singing Abilities")),
    musicassessr::deploy_demographics(demographics),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page("You have completed the Melody Singing Task!")
  )

}

#' Deploy MST as standalone test
#'
#' @param aws_credentials
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
#'
#' @return
#' @export
#'
#' @examples
MST_standalone <- function(aws_credentials,
                           num_items = list("long_tones" = 6L,
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
                           final_results = TRUE) {

  timeline <- MST(aws_credentials,
                  num_items,
                  item_bank,
                  demographics,
                  demo,
                  feedback,
                  admin_password,
                  SNR_test,
                  get_range,
                  absolute_url,
                  examples,
                  final_results)

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


MST_intro <- function(aws_credentials = list("api_url" = "api url",
                                             "bucket_name" = "bucket name",
                                             "bucket_region" = "bucket region",
                                             "identity_pool_id" = "identity pool id",
                                             "destination_bucket" = "destination bucket"),
                      demo = FALSE,
                      SNR_test = TRUE,
                      get_range = TRUE,
                      absolute_url,
                      test_username = NULL,
                      store_results_in_db = FALSE) {

  musicassessr::make_aws_credentials_global(aws_credentials)

  c(
    musicassessr::musicassessr_init(test = "MST", test_username = test_username, store_results_in_db),

    # introduction page
    psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("mst_welcome")),
                                                       shiny::tags$img(src = 'custom-assets/img/intro.png', height = 100, width = 100),
                                                       shiny::tags$p(psychTestR::i18n("mst_welcome_1")),
                                                       shiny::tags$p(psychTestR::i18n("mst_welcome_2")),
                                                       musicassessr::musicassessr_js_scripts(api_url = aws_credentials$api_url,
                                                                                             bucket_name = aws_credentials$bucket_name,
                                                                                             bucket_region = aws_credentials$bucket_region,
                                                                                             identity_pool_id = aws_credentials$identity_pool_id,
                                                                                             destination_bucket = aws_credentials$destination_bucket)),
                                button_text = psychTestR::i18n("Next")),

    musicassessr::setup_pages(input = "microphone", demo = demo, get_instrument_range = get_range, SNR_test = SNR_test, absolute_url = absolute_url),
    # instructions
    MST_instructions()
  )

}

MST_instructions <- function() {

  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("mst_instructions1")),
                                                     shiny::tags$p(psychTestR::i18n("mst_instructions2")),
                                                     shiny::tags$p(psychTestR::i18n("mst_instructions3")),
                                                     shiny::tags$p(psychTestR::i18n("mst_instructions4"))),
                              button_text = psychTestR::i18n("Next"))
}




