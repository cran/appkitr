#' Example Entry Survey Data
#'
#' This is synthetic example data intended to allow users to test the package's functions.
#' The variables are from a study conducted by the GESIS AppKit Team on measuring emotionality in social interactions through momentary assessments.
#' This is the entry survey, which includes items from the affect intensity measure questionnaire.
#'
#' @format A data frame with `r nrow(appkit_survey_entry)` rows and `r ncol(appkit_survey_entry)` columns:
#' \describe{
#'   \item{`personalParticipantCode`}{PersonalParticipantCode}
#'   \item{`generalLoginCode`}{GeneralLoginCode}
#'   \item{`status`}{Participant Status}
#'   \item{`lastStatusChange`}{Last Status Change}
#'   \item{`note`}{Note}
#'   \item{`firstOpened`}{First Opened}
#'   \item{`committed`}{Committed}
#'   \item{`scheduled`}{Scheduled}
#'   \item{`expired`}{Expired}
#'   \item{`published`}{Published}
#'   \item{`exported`}{Exported}
#'   \item{`osVersion`}{OS Version}
#'   \item{`smartphoneType`}{SmartphoneType}
#'   \item{`pushNotificationAllowed`}{Push Notification Allowed}
#'   \item{`notificationType`}{Notification Type}
#'   \item{`notificationTitle`}{Notification Title}
#'   \item{`notificationText`}{Notification Text}
#'   \item{`age`}{How old are you (in years)?}
#'   \item{`sex`}{Which sex was assigned to you at birth?}
#'   \item{`gender`}{What is your gender?}
#'   \item{`aim01`}{When I feel happiness, it is a quite type of contentment.}
#'   \item{`aim02`}{When a person in a wheelchair can't get through a door, I have strong feelings of pity.}
#'   \item{`aim03`}{I get upset easily.}
#'   \item{`aim04`}{When I succeed at something, my reaction is calm contentment.}
#'   \item{`aim05`}{I get really happy or really unhappy.}
#'   \item{`aim06`}{I am a fairly quiet person.}
#'   \item{`aim07`}{When I'm happy, I feel fairly energetic.}
#'   \item{`aim08`}{Seeing a picture of some violent car accident in a newspaper makes me feel sick to my stomach.}
#'   \item{`aim09`}{When I'm happy, I feel like I'm bursting with joy.}
#'   \item{`aim10`}{I would be very upset, if I got a traffic ticket.}
#'   \item{`aim11`}{Looking at beautiful scenery really doesn't affect me much.}
#'   \item{`aim12`}{The weather doesn't affect my mood.}
#'   \item{`aim13`}{Others tend to get more excited than I do.}
#'   \item{`aim14`}{I am not an extremely enthusiastic individual.}
#'   \item{`aim15`}{Calm and cool could easily describe me.}
#'   \item{`aim16`}{When I'm feeling well it is easy for me to go from being in a good mood to being really joyful.}
#'   \item{`aim17`}{When I worry, it is so mild I hardly notice it.}
#'   \item{`aim18`}{I get overly enthusiastic.}
#'   \item{`aim19`}{My happy moods are so strong that I feel like I'm 'in heaven'.}
#'   \item{`aim20`}{When something bad happens, others tend to be more unhappy than I.}
#' }
"appkit_survey_entry"

#' Example ESM Data
#'
#' This is synthetic example data intended to allow users to test the package's functions.
#' The variables are from a study conducted by the GESIS AppKit Team on measuring emotionality in social interactions through momentary assessments.
#' This is the experience sampling survey, which captures the momentary assessments of emotions in social interactions.
#'
#' @format A data frame with `r nrow(appkit_survey_esm)` rows and `r ncol(appkit_survey_esm)` columns:
#' \describe{
#'   \item{`personalParticipantCode`}{PersonalParticipantCode}
#'   \item{`generalLoginCode`}{GeneralLoginCode}
#'   \item{`status`}{Participant Status}
#'   \item{`lastStatusChange`}{Last Status Change}
#'   \item{`note`}{Note}
#'   \item{`firstOpened`}{First Opened}
#'   \item{`committed`}{Committed}
#'   \item{`scheduled`}{Scheduled}
#'   \item{`expired`}{Expired}
#'   \item{`published`}{Published}
#'   \item{`exported`}{Exported}
#'   \item{`osVersion`}{OS Version}
#'   \item{`smartphoneType`}{SmartphoneType}
#'   \item{`pushNotificationAllowed`}{Push Notification Allowed}
#'   \item{`notificationType`}{Notification Type}
#'   \item{`notificationTitle`}{Notification Title}
#'   \item{`notificationText`}{Notification Text}
#'   \item{`screening`}{Have you been part of any social interaction in the last two hours?}
#'   \item{`description`}{Please briefly describe the type of social interaction.}
#'   \item{`delay`}{How many minutes ago did this interaction take place?}
#'   \item{`duration`}{How long did the interaction last?}
#'   \item{`groupsize`}{How many people took part in the interaction (you included)?}
#'   \item{`partner_gender`}{What gender do you ascribe to your (main) interaction partner?}
#'   \item{`partner_close`}{How close do you consider the relationship to your (main) interaction partner?}
#'   \item{`interaction_quality`}{Please indicate your perceived quality of the interaction.}
#'   \item{`interaction_intimacy`}{Please indicate the intimacy of the interaction.}
#'   \item{`interaction_control`}{Please indicate your perceived degree of control over the
#' interaction in comparison to your main interaction partner.}
#'   \item{`interaction_intensity`}{Please indicate the overall intensity of your emotional
#' experience during the interaction.}
#'   \item{`emo_happy`}{Please indicate how happy you felt during the interaction.}
#'   \item{`emo_sad`}{Please indicate how sad you felt during the interaction.}
#'   \item{`emo_nervous`}{Please indicate how nervous you felt during the interaction.}
#'   \item{`emo_surprise`}{Please indicate how surprised you felt during the interaction.}
#'   \item{`emo_angry`}{Please indicate how angry you felt during the interaction.}
#'   \item{`emo_embarrassed`}{Please indicate how embarrassed you felt during the interaction.}
#'   \item{`emo_ashamed`}{Please indicate how ashamed you felt during the interaction.}
#'   \item{`dataquality`}{Thank you very much for supporting our research.
#' For ensuring data quality, please indicate whether you answered the questions within this
#' survey conscientiously:}
#' }
"appkit_survey_esm"

#' Example Exit Survey Data
#'
#' This is synthetic example data intended to allow users to test the package's functions.
#' The variables are from a study conducted by the GESIS AppKit Team on measuring emotionality in social interactions through momentary assessments.
#' This is the exit survey, which includes items from the personal attributes questionnaire.
#'
#' @format A data frame with `r nrow(appkit_survey_exit)` rows and `r ncol(appkit_survey_exit)` columns:
#' \describe{
#'   \item{`personalParticipantCode`}{PersonalParticipantCode}
#'   \item{`generalLoginCode`}{GeneralLoginCode}
#'   \item{`status`}{Participant Status}
#'   \item{`lastStatusChange`}{Last Status Change}
#'   \item{`note`}{Note}
#'   \item{`firstOpened`}{First Opened}
#'   \item{`committed`}{Committed}
#'   \item{`scheduled`}{Scheduled}
#'   \item{`expired`}{Expired}
#'   \item{`published`}{Published}
#'   \item{`exported`}{Exported}
#'   \item{`osVersion`}{OS Version}
#'   \item{`smartphoneType`}{SmartphoneType}
#'   \item{`pushNotificationAllowed`}{Push Notification Allowed}
#'   \item{`notificationType`}{Notification Type}
#'   \item{`notificationTitle`}{Notification Title}
#'   \item{`notificationText`}{Notification Text}
#'   \item{`paqf1`}{I am emotional.}
#'   \item{`paqf2`}{I am able to devote myself completely to others.}
#'   \item{`paqf3`}{I am understanding.}
#'   \item{`paqf4`}{I am gentle.}
#'   \item{`paqf5`}{I am helpful to others.}
#'   \item{`paqf6`}{I am kind.}
#'   \item{`paqf7`}{I am unaware of the feelings of others.}
#'   \item{`paqf8`}{I am warm in relation to others.}
#'   \item{`paqm1`}{I am independent.}
#'   \item{`paqm2`}{I am a proactive person.}
#'   \item{`paqm3`}{I am competitive.}
#'   \item{`paqm4`}{I have difficulties making decisions.}
#'   \item{`paqm5`}{I give up easily.}
#'   \item{`paqm6`}{I feel superior.}
#'   \item{`paqm7`}{I am self-confident.}
#'   \item{`paqm8`}{I function well under pressure.}
#' }
"appkit_survey_exit"
