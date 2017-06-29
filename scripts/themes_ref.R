default_theme <-   theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.5, colour = "gray90"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing.y = unit(1.5, "cm")
  )