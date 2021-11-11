


#' Deploy the MST2
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
MST2 <- function(aws_credentials,
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
