# Function definition file for get_dmg_decay_fit()

library(dplyr)
library(tidyr)
library(ggplot2)


get_dmg_decay_fit <- function(df, orient = "fwd", pos = 30,
                              p_breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                              y_max = 0.7, y_min = -0.01) {
  df_dx_fwd <- df  |> 
    select(taxid, name, label, starts_with("fwdx")) |>
    select(-starts_with("fwdxConf")) |>
    pivot_longer(names_to = "type", values_to = "Dx_fwd",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("fwdx", "", type)) |>
    select(-type)

  df_dx_rev <- df |>
    select(taxid, name, label, starts_with("bwdx")) |>
    select(-starts_with("bwdxConf")) |>
    pivot_longer(names_to = "type", values_to = "Dx_rev",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("bwdx", "", type)) |>
    select(-type)

  df_dx_std_fwd <- df |>
    select(taxid, name, label, starts_with("fwdxConf")) |>
    pivot_longer(names_to = "type", values_to = "Dx_std_fwd",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("fwdxConf", "", type)) |>
    select(-type)

  df_dx_std_rev <- df |>
    select(taxid, name, label, starts_with("bwdxConf")) |>
    pivot_longer(names_to = "type", values_to = "Dx_std_rev",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("bwdxConf", "", type)) |>
    select(-type)

  df_fit_fwd <- df |>
    select(taxid, name, label, starts_with("fwf")) |>
    pivot_longer(names_to = "type", values_to = "f_fwd",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("fwf", "", type)) |>
    select(-type)

  df_fit_rev <- df |>
    select(taxid, name, label, starts_with("bwf")) |>
    pivot_longer(names_to = "type", values_to = "f_rev",
                 c(-taxid, -name, -label)) |>
    mutate(x = gsub("bwf", "", type)) |>
    select(-type)

  dat <- df_dx_fwd |>
    inner_join(df_dx_rev, by = c("taxid", "name", "label", "x")) |>
    inner_join(df_dx_std_fwd, by = c("taxid", "name", "label", "x")) |>
    inner_join(df_dx_std_rev, by = c("taxid", "name", "label", "x")) |>
    inner_join(df_fit_fwd, by = c("taxid", "name", "label", "x")) |>
    inner_join(df_fit_rev, by = c("taxid", "name", "label", "x")) |>
    mutate(x = as.numeric(x)) |>
    filter(x <= pos) |>
    rowwise() |>
    mutate(
      Dx_fwd_min = Dx_fwd - Dx_std_fwd,
      Dx_fwd_max = Dx_fwd + Dx_std_fwd,
      Dx_rev_min = Dx_rev - Dx_std_rev,
      Dx_rev_max = Dx_rev + Dx_std_rev
    )

  fwd_max <- dat |>
    group_by(as.character(x)) |>
    summarise(val = mean(Dx_std_fwd) + sd(Dx_std_fwd)) |>
    pull(val) |>
    max()

  fwd_min <- dat |>
    group_by(as.character(x)) |>
    summarise(val = mean(Dx_std_fwd) - sd(Dx_std_fwd)) |>
    pull(val) |>
    min()

  rev_max <- dat |>
    group_by(as.character(x)) |>
    summarise(val = mean(Dx_std_rev) + sd(Dx_std_rev)) |>
    pull(val) |>
    max()

  rev_min <- dat |>
    group_by(as.character(x)) |>
    summarise(val = mean(Dx_std_rev) - sd(Dx_std_rev)) |>
    pull(val) |>
    min()

  if (orient == "fwd") {
    ggplot() +
      geom_ribbon(data = dat,
                  aes(x, ymin = Dx_fwd_min, ymax = Dx_fwd_max,
                      group = interaction(name, taxid)),
                  alpha = 0.6, fill = "darkcyan") +
      geom_line(data = dat,
                aes(x, Dx_fwd, group = interaction(name, taxid)),
                color = "black") +
      geom_point(data = dat, aes(x, f_fwd), alpha = .50, size = 2,
                 fill = "black") +
      theme_test() +
      xlab("Position") +
      ylab("Frequency") +
      scale_y_continuous(limits = c(y_min, y_max), breaks = p_breaks) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      facet_wrap(~label, ncol = 1)
  } else {
    ggplot() +
      geom_ribbon(data = dat, aes(x, ymin = Dx_rev_min, ymax = Dx_rev_max,
                                  group = interaction(name, taxid)),
                  alpha = 0.6, fill = "orange") +
      geom_path(data = dat, aes(x, Dx_rev, group = interaction(name, taxid)),
                color = "black") +
      geom_point(data = dat, aes(x, f_rev), alpha = .50, size = 2,
                 fill = "black") +
      theme_test() +
      xlab("Position") +
      ylab("Frequency") +
      scale_x_continuous(trans = "reverse") +
      scale_y_continuous(limits = c(y_min, y_max), position = "right",
                         breaks = p_breaks) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      facet_wrap(~label, ncol = 1)
  }
}