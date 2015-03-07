## --- Polity coefficient Bias ----------------------------------
## --- data setup -----------------------------------------------
muELF <- mean(data[, 'ELF_ethnic'])
rangePolity2 <- min(data[, 'polity2']):max(data[, 'polity2'])
yPolity2 <- matrix(nrow = length(rangePolity2), ncol = 4)
dimnames(yPolity2) <- list(
  rangePolity2, c('Polity', 'true', 'ListDel', 'Amelia')
)
yPolity2[, 1] <- rangePolity2

## --- predict response -----------------------------------------
scenData <- data.frame(
  const = 1, ELF_ethnic = muELF, polity2 = rangePolity2
)
yPolity2[, 2] <- model[['coefficients']][, 1] %*% t(scenData)
yPolity2[, 3] <- modelListDel[['coefficients']][, 1] %*% t(scenData)
yPolity2[, 4] <- modelAmelia[['coefficients']][, 1] %*% t(scenData)

## --- Proceed to plotting --------------------------------------
yPolity2 <- as.data.frame(yPolity2)
yPolity2 <- reshape2::melt(
  data = yPolity2,
  id.vars = 1,
  mesure.vars = 2:4,
  value.name = 'yhat'
)

p <- ggplot(
  data = yPolity2, 
  aes(x = Polity, y = yhat, shape = variable)
) + 
geom_point() +
labs(
  x = 'Polity 2',
  y = 'Predicted Standardaized Net Income Inequality',
  linetype = 'Missing Data Treatment'
) +
scale_y_continuous(limits = c(-1, 2.09118), breaks = seq(-1, 2, 1)) +
scale_linetype_discrete(
  labels = c('None', 'Deletion', 
    'Imputation')
) +
guides(shape = 'none') + 
theme_bw() +
theme(
  text = element_text(family = 'serif'),
  axis.title.y = element_blank(),
  legend.position = 'bottom', 
  legend.direction = 'horizontal',
  legend.key = element_blank(),
  panel.border = element_blank()
)
ggsave(
  plot = p,
  file = file.path(pathOut, 'slopePlotPolity2.pdf'),
  width = 7, height = 7/1.618, dpi = 1200, family = 'serif'
)

rm(muELF, rangePolity2, yPolity2, scenData, p)
## END