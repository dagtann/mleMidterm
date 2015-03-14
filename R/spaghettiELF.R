## --- Setup scenario data --------------------------------------
scen.data <- cbind(
  1, 
  ELF_ethnic = seq(
    min(data[, 'ELF_ethnic']), 
    max(data[, 'ELF_ethnic']), 
    length.out = 15
  ),
  polity2 = mean(data[, 'polity2'])
)

## --- Impute data & save coefficients --------------------------
N <- 100
set.seed(6886)
seeds <- sample(1000:10000, N)
seeds[1] <- 6886
coefs <- matrix(NA, nrow = 3, ncol = N, 
  dimnames = list(
    c('constant', all.vars(form)[2:3]),
    paste0('seed', seeds)
  )
)
for(i in 1:N){
  set.seed(seeds[i])
  coefs[, i] <- ols(
    form, data = dataMiss, impute = TRUE
  )[['coefficients']][, 1]
}

## --- Predictions for each data structure ----------------------
preds <- data.frame(scen.data, scen.data %*% coefs)
preds <- reshape2::melt(
  data = preds,
  id.vars = 'ELF_ethnic',
  measure.vars = paste0('seed', seeds)
)
preds.true <- data.frame(
  scen.data, 
  yhat = predict(
    lm(formula = form, data = data), 
    newdata = data.frame(scen.data)
  )
)
preds.miss <- data.frame(
  scen.data, 
  yhat = predict(
    lm(formula = form, data = dataMiss), 
    newdata = data.frame(scen.data)
  )
)

## --- Proceed to plotting --------------------------------------
p <- ggplot() +
geom_line(
  data = preds.true, 
  aes(x = ELF_ethnic, y = yhat, linetype = 'No missings')
) +
geom_line(
  data = preds.miss, 
  aes(x = ELF_ethnic, y = yhat, linetype = 'Listwise deletion'), 
) + 
stat_summary(
  data = preds, fun.y = mean, 
  aes(x = ELF_ethnic, y = value, linetype = 'Average Amelia'), 
  geom = 'line'
) +
geom_line(
  data = subset(preds, variable == 'seed6886'),
  aes(x = ELF_ethnic, y = value, linetype = 'Seed 6886'), 
) +
geom_line(
  data = preds, 
  aes(x = ELF_ethnic, y = value,  group = variable), 
  alpha = .12
) +
scale_linetype_manual(values = c(2, 4, 1, 3)) +
scale_y_continuous(limits = c(-1, 1.2), breaks = seq(-1, 1.5, .5)) +
scale_x_continuous(limits = c(0.05, .9), breaks = seq(0.05, .9, .15)) +
labs(
  x = 'Ethnic fractionalization',
  y = 'Net income inequality\n(Z-standardized)',
  linetype = 'Missing data'
) +
guides(linetype = 'none') +
theme_bw() +
theme(
  text = element_text(family = 'serif'),
  legend.position = c(.43, 1.03),
  legend.direction = 'horizontal',
  legend.background = element_rect(fill = 'transparent'),
  legend.key = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank()
)
ggsave(plot = p, file = file.path(pathOut, 'spaghettiELF.pdf'),
  width = 7, height = 7/1.618, dpi = 1200, family = 'serif'
)
##END