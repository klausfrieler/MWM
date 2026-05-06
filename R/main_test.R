
get_eligible_first_items_MWM <- function(){
  lower_sd <- mean(MWM::MWM_item_bank$difficulty) - stats::sd(MWM::MWM_item_bank$difficulty)
  upper_sd <- mean(MWM::MWM_item_bank$difficulty) + stats::sd(MWM::MWM_item_bank$difficulty)
  which(MWM::MWM_item_bank$difficulty >= lower_sd  &
         MWM::MWM_item_bank$difficulty <= upper_sd) %>% sample(25) %>% sort()
}

main_test <- function(label,
                      num_items,
                      audio_dir,
                      dict = MWM::MWM_dict,
                      next_item.criterion,
                      next_item.estimator,
                      next_item.prior_dist,
                      next_item.prior_par,
                      final_ability.estimator,
                      constrain_answers,
                      autoplay = TRUE,
                      ...) {
  item_bank <- MWM::MWM_item_bank
  first_items <- get_eligible_first_items_MWM()
  #print(item_bank[first_items,])
  #print(first_items)
  psychTestRCAT::adapt_test(
    label = label,
    item_bank = item_bank,
    show_item = show_item(audio_dir, autoplay),
    stopping_rule = psychTestRCAT::stopping_rule.num_items(n = num_items),
    opt = MWM_options(
      next_item.criterion = next_item.criterion,
      next_item.estimator = next_item.estimator,
      next_item.prior_dist = next_item.prior_dist,
      next_item.prior_par = next_item.prior_par,
      final_ability.estimator = final_ability.estimator,
      constrain_answers = constrain_answers,
      eligible_first_items = first_items,
      item_bank = item_bank
    )
  )

}

get_prompt <- function(item_number, num_items, dict = MWM::MWM_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items))
                     "?" else
                       num_items)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

MWM_welcome_page <- function(dict = MWM::MWM_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "margin-left:20%;margin-right:20%;width:60%;display:block;text-align:justify")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

MWM_finished_page <- function(dict = MWM::MWM_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

MWM_final_page <- function(dict = MWM::MWM_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

show_item <- function(audio_dir, autoplay) {
  function(item, ...) {
    #stopifnot(is(item, "item"), nrow(item) == 1L)
    #print(item)
    item_number <- psychTestRCAT::get_item_number(item)
    num_items <- psychTestRCAT::get_num_items_in_test(item)
    messagef("Showing item #%d: %s, correct = %s",
             item_number,
             item$audio_file,
             item$answer)
    MWM_item(
      label = paste0("q", item_number),
      audio_file = item$audio_file,
      correct_answer = item$answer,
      prompt = get_prompt(item_number, num_items),
      audio_dir = audio_dir,
      save_answer = TRUE,
      get_answer = NULL,
      on_complete = NULL,
      instruction_page = FALSE,
      autoplay = autoplay
    )
  }
}
