MST_intro <- function(aws_credentials = list("api_url" = "api url",
                                             "bucket_name" = "bucket name",
                                             "bucket_region" = "bucket region",
                                              "identity_pool_id" = "identity pool id",
                                             "destination_bucket" = "destination bucket"),
                      demo = FALSE,
                      SNR_test = TRUE,
                      get_range = TRUE) {

  musicassessr::make_aws_credentials_global(aws_credentials)

  c(

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

    musicassessr::setup_pages(demo = demo, get_instrument_range = get_range, SNR_test = SNR_test),
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

