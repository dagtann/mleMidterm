## --- Produce spaghetti plot for amelia results
## --- Setup simulation data -------------------------------
scen.data <- cbind(
  1, ELF_ethnic = mean(data[, 'ELF_ethnic']),
  polity2 = seq(4, 10, 1)
)

## --- Impute data & save coefficients ---------------------
N <- 100
set.seed(6886)
seeds <- sample(1000:10000, N) ## reproducible seeds for Amelia
seeds[1] <- 6886
coefs <- matrix(NA, nrow = 3, ncol = N, 
  dimnames = list(
    c('constant', all.vars(form)[2:3]),
    paste0('seed', seeds)
  )
)
for(i in 1:N){                ## Impute by seed and extract coefs
  set.seed(seeds[i])
  coefs[, i] <- ols(
    form, data = dataMiss, impute = TRUE
  )[['coefficients']][, 1]
}

## --- Predictions for each missing data regime -----------------
preds <- data.frame(scen.data, scen.data %*% coefs)
preds <- reshape2::melt(              ## imputed data predictions
  data = preds,
  id.vars = 'polity2',
  measure.vars = paste0('seed', seeds)
)
preds.true <- data.frame(          ## no missing data predictions
  scen.data, 
  yhat = predict(
    lm(formula = form, data = data), 
    newdata = data.frame(scen.data)
  )
)
preds.miss <- data.frame(             ## list wise deletion preds
  scen.data, 
  yhat = predict(
    lm(formula = form, data = dataMiss), 
    newdata = data.frame(scen.data)
  )
)

## --- Proceed to plotting --------------------------------------
p <- ggplot() +         ## empty plot, manual layering given data
geom_line(        ## NOTE: Linetype scale is generated on the fly
  data = preds.true, 
  aes(x = polity2, y = yhat, linetype = 'No missings')
) +
geom_line(
  data = preds.miss, 
  aes(x = polity2, y = yhat, linetype = 'Listwise deletion'), 
) + 
stat_summary(
  data = preds, fun.y = mean, 
  aes(x = polity2, y = value, linetype = 'Average Amelia'), 
  geom = 'line'
) +
geom_line(
  data = subset(preds, variable == 'seed6886'),
  aes(x = polity2, y = value, linetype = 'Seed 6886'), 
) +
geom_line(
  data = preds, 
  aes(x = polity2, y = value,  group = variable), 
  alpha = .1
) +                                  ## scale & label adjustments
scale_linetype_manual(values = c(2, 4, 1, 3)) +
scale_y_continuous(limits = c(-1, 1.2), breaks = seq(-1, 1.5, .5)) +
scale_x_continuous(limits = c(4, 10), breaks = 4:10) +
labs(
  x = 'Polity 2',
  y = 'Net income inequality\n(Z-standardized)',
  linetype = 'Missing data'
) +                                 ## finishing touches to theme
theme_bw() +
theme(
  text = element_text(family = 'serif'),
  legend.position = 'bottom',
  legend.direction = 'horizontal',
  legend.key = element_blank(),
  legend.background = element_rect(fill = 'transparent'),
  axis.ticks = element_blank(),
  panel.border = element_blank()
)
ggsave(plot = p, file = file.path(pathOut, 'spaghettiPolity.pdf'),
  width = 7, height = 7/1.618, dpi = 1200, family = 'serif'
)