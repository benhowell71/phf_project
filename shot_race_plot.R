require(tidyverse)
require(janitor)
require(fastRhockey)
require(ggimage)
require(ggtext)
require(rtweet)

source('utils/team_colors.R')

df <- phf_pbp(game_id = 368719)
asp_ratio <- 1.618

create_shot_race_plot <- function(game_df) {
  
  title <- paste0(unique(game_df$away_team), 
         ' (',
         stringr::str_trim(max(game_df$away_goals)), 
         ')',
         ' @ ', 
         unique(game_df$home_team),
         ' (',
         stringr::str_trim(max(game_df$home_goals)),
         ')')
  
  goals <- game_df %>%
    dplyr::filter(shot_result == 'made') %>%
    dplyr::mutate(minute_of_game = ceiling(sec_from_start / 60)) %>%
    dplyr::select(team, player_name_1, on_ice_situation, minute_of_game)
  
  goals$situation <- sapply(stringr::str_extract_all(goals$on_ice_situation, '[A-Z]+'), paste0, collapse = '')
  
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
    dplyr::left_join(goals, by = c("team", "minute_of_game")) %>%
    dplyr::mutate(shot = "https://images.vexels.com/media/users/3/238367/isolated/preview/b40ed847cff749bec2e1b151b2f46e08-moving-ice-hockey-puck.png")
    # dplyr::mutate(shot = "https://github.com/benhowell71/phf_project/blob/main/utils/puck.png")
  
  plt <- comb %>%
    ggplot() +
    # geom_point(aes(x = minute_of_game, y = shots_to_point)) +
    geom_vline(aes(xintercept = 20), linetype = 'dashed', size = 0.44) +
    geom_vline(aes(xintercept = 40), linetype = 'dashed', size = 0.44) +
    geom_vline(aes(xintercept = 60), linetype = 'dashed', size = 0.44) +
    # geom_vline(aes(xintercept = 20)) +
    geom_step(aes(x = minute_of_game, y = shots_to_point, color = team), size = 1) +
    # geom_point(data = comb %>%
    #              dplyr::filter(! is.na(player_name_1)),
    #            aes(x = minute_of_game, y = shots_to_point)) +
    # geom_richtext(data = comb %>%
    #                 dplyr::filter(! is.na(player_name_1)),
    #               aes(x = minute_of_game, y = shots_to_point, label = shot)) 
    geom_image(data = comb %>%
                 dplyr::filter(! is.na(player_name_1)),
               aes(x = minute_of_game, y = shots_to_point, image = shot), 
               # Set size, and aspect ratio
               size = 0.1, by = "width", asp = asp_ratio) +
    geom_segment(data = comb %>%
                dplyr::filter(! is.na(player_name_1)),
              aes(x = minute_of_game - 4, y = shots_to_point + 2,
                  xend = minute_of_game - 0.35, yend = shots_to_point + 0.5)) +
    geom_richtext(
      data = comb %>%
        dplyr::filter(! is.na(player_name_1)) %>%
        dplyr::mutate(lab = paste0(player_name_1, ' (', situation, ')')),
      aes(x = minute_of_game - 5, y = shots_to_point + 3, label = lab),
      fill = NA, label.color = NA, size = 3
    ) +
    labs(title = title,
         x = 'Minute of Game',
         y = 'Total Shots') +
    # geom_point(aes(x = minute_of_game, y = shots_to_point, color = player_name_1)) +
    # geom_vline(aes(xintercept = minute_of_game, color = team), size = 1) +
    # scale_y_continuous(limits = c(0, max(shots_to_point)),
    #                    breaks = c(seq())) +
    PHF_TEAM_COLORS +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(size = 1),
          axis.text = element_text(size = 12),
          aspect.ratio = 1 /asp_ratio,
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  
  ggsave(plt, file = paste0('plot_outputs/', unique(game_df$away_abbreviation), '_', unique(game_df$home_abbreviation), '_shot_race_plot.png'))

}