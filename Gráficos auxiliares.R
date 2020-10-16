
# Primera animación

library(tidyverse)

n_sim <- c(seq(10, 100, 10), seq(200, 1000, 100))

normal_list <- list()
for (i in 1:length(n_sim)) {
  normal_list[[i]] <- tibble(value = rnorm(n_sim[i]))
}

normal_list <- normal_list %>%
  bind_rows(.id = "n") %>%
  mutate(n = as.numeric(n),
         n = factor(n,
                    labels = n_sim))


histograma_sim <- normal_list %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    color = "black",
    fill = "grey"
  ) +
  labs(
    x = "Normal",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(
    text = element_text(
      size = 11,
      face="bold"),
    axis.text = element_text(
      size = 11,
      face="plain",
      colour="black"),
    panel.background = element_rect(fill = "#e9ebee",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#e9ebee"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#e9ebee"),
    plot.background = element_rect(fill = "#e9ebee")
  )


library(gganimate)
anim_histogram <- histograma_sim +
  transition_states(n,
                    transition_length = 2,
                    state_length = 1) +
  ggtitle('Tamaño de muestra: {closest_state}')


anim_save("img/normal_simulada1.gif", anim_histogram,
          height = 5, width = 6, units = "in", res = 200)
