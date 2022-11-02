require(tidyverse)
require(janitor)
require(fastRhockey)
require(ggimage)

source('utils/team_colors.R')

df <- phf_pbp(game_id = 368719)

create_shot_race_plot <- function(game_df) {
  
  title <- paste0(unique(game_df$away_abbreviation), 
         ' (',
         stringr::str_trim(max(game_df$away_goals)), 
         ')',
         ' @ ', 
         unique(game_df$home_abbreviation),
         ' (',
         stringr::str_trim(max(game_df$home_goals)),
         ')')
  
  goals <- game_df %>%
    dplyr::filter(shot_result == 'made') %>%
    dplyr::mutate(minute_of_game = ceiling(sec_from_start / 60)) %>%
    dplyr::select(team, player_name_1, minute_of_game)
  
  shots <- game_df %>%
    # dplyr::mutate(min_of_game = minute_start + 
    #                 ifelse(period_id == 1, 0, 
    #                        ifelse(period_id == 2, 20, 
    #                               ifelse(period_id == 3, 40, NA)))) %>%
    dplyr::filter(! is.na(shot_result)) %>%
    dplyr::mutate(minute_of_game = ceiling(sec_from_start / 60),
                  is_shot = 1) %>%
    dplyr::select(team, minute_of_game, shot_result, is_shot) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(shots_to_point = cumsum(is_shot)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team, minute_of_game) %>%
    dplyr::filter(shots_to_point == max(shots_to_point, na.rm = TRUE)) %>%
    dplyr::select(team, minute_of_game, shots_to_point)
  
  comb <- shots %>%
    dplyr::left_join(goals, by = c("team", "minute_of_game"))
  
  comb %>%
    ggplot() +
    # geom_point(aes(x = minute_of_game, y = shots_to_point)) +
    geom_vline(aes(xintercept = 20), linetype = 'dashed', size = 0.66) +
    geom_vline(aes(xintercept = 40), linetype = 'dashed', size = 0.66) +
    # geom_vline(aes(xintercept = 20)) +
    geom_step(aes(x = minute_of_game, y = shots_to_point, color = team), size = 1) +
    geom_point(data = comb %>%
                 dplyr::filter(! is.na(player_name_1)),
               aes(x = minute_of_game, y = shots_to_point)) +
    # geom_point(aes(x = minute_of_game, y = shots_to_point, color = player_name_1)) +
    # geom_vline(aes(xintercept = minute_of_game, color = team), size = 1) +
    # scale_y_continuous(limits = c(0, max(shots_to_point)),
    #                    breaks = c(seq())) +
    PHF_TEAM_COLORS +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(size = 1),
          axis.text = element_text(size = 12))

}