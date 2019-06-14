#USING ggplot

# nvars <- regfit.full$np - 1 #number of variables 
subsets_plots <- function(reg.summary, nvars) {
    ggrss <- melt(reg.summary$rss) %>%
        ggplot(aes(x = seq_along(value), y = value), show) + 
        geom_line(color = "dodgerblue", alpha = 0.7) + 
        labs(x = "Number of variables", y = "rss", tag = "A") + 
        scale_x_continuous(breaks = seq_along(1:nvars))
    ggr2 <- melt(reg.summary$adjr2) %>%
        mutate(max_adjr2 = if_else(row_number() == which.max(reg.summary$adjr2), 
                                   TRUE, FALSE)) %>%
        ggplot(aes(x = seq_along(value), y = value, color = max_adjr2), show) + 
        geom_point() +
        geom_line(color = "dodgerblue", alpha = 0.7) + 
        labs(x = "Number of variables", y = "adjr2", tag = "B") + 
        scale_x_continuous(breaks = seq_along(1:nvars)) +
        guides(color = FALSE)
    ggcp <- melt(reg.summary$cp) %>%
        mutate(min_cp = if_else(row_number() == which.min(reg.summary$cp), 
                                TRUE, FALSE)) %>%
        ggplot(aes(x = seq_along(value), y = value, color = min_cp), show) + 
        geom_point() +
        geom_line(color = "dodgerblue", alpha = 0.7) + 
        labs(x = "Number of variables", y = "cp", tag = "C") + 
        scale_x_continuous(breaks = seq_along(1:nvars)) +
        guides(color = FALSE)
    ggbic <- melt(reg.summary$bic) %>%
        mutate(min_bic = if_else(row_number() == which.min(reg.summary$bic), 
                                 TRUE, FALSE)) %>%
        ggplot(aes(x = seq_along(value), y = value, color = min_bic), show) + 
        geom_point() +
        geom_line(color = "dodgerblue", alpha = 0.7) + 
        labs(x = "Number of variables", y = "bic", tag = "D") + 
        scale_x_continuous(breaks = seq_along(1:nvars)) +
        guides(color = FALSE)
    #grid.arrange(ggrss, ggr2, ggcp, ggbic, ncol = 2, widths = c(4, 4))
    grid.arrange(ggrss, ggr2, ggcp, ggbic, ncol = 2, nrow = 2)
}