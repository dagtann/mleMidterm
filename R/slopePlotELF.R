## --- Ethnic fractionalization bias ----------------------------
## --- data setup -----------------------------------------------
muPolity <- c(
  mean(data[, 'polity2']), 
  mean(dataMiss[, 'polity2'], na.rm = TRUE),
  mean(modelAmelia[['data']][, 2])
) 
rangeELF <- seq(
  min(data[, 'ELF_ethnic']), max(data[, 'ELF_ethnic']),
  length.out = 10
)
yELF <- matrix(nrow = length(rangeELF), ncol = 4)
dimnames(yELF) <- list(
  rangeELF, c('ELF', 'true', 'ListDel', 'Amelia')
)
yELF[, 1] <- rangeELF

## --- No missing model -----------------------------------------
scenData <- data.frame(
  const = 1, ELF_ethnic = rangeELF, polity2 = muPolity[1]
)
yELF[, 2] <- model[['coefficients']][, 1] %*% t(scenData)

## --- listwise deletion model ----------------------------------
scenData <- data.frame(
  const = 1, ELF_ethnic = rangeELF, polity2 = muPolity[2]
)
yELF[, 3] <- modelListDel[['coefficients']][, 1] %*% t(scenData)

## --- Amelia 2 single imputation model -------------------------
scenData <- data.frame(
  const = 1, ELF_ethnic = rangeELF, polity2 = muPolity[3]
)
yELF[, 4] <- modelAmelia[['coefficients']][, 1] %*% t(scenData)
yELF <- as.data.frame(yELF)

## --- Proceed to plotting --------------------------------------
yELF <- reshape2::melt(
  data = yELF,
  id.vars = 1,
  mesure.vars = 2:4,
  value.name = 'yhat'
)

p <- ggplot(data = yELF, 
  aes(x = ELF, y = yhat, shape = variable)
) + 
geom_point() +
labs(
  x = 'Ethnic Fractionalization',
  y = 'Predicted Standardaized Net Income Inequality',
  shape = 'Missing Data Treament'
) +
guides(shape = 'none') +
scale_y_continuous(limits = c(-1, 2.09118), breaks = seq(-1, 2, 1)) +
scale_linetype_discrete(
  labels = c('No missing data', 'Listwise deletion', 
    'Single Amelia 2 imputation')
) +
theme_bw() +
theme(
  text = element_text(family = 'serif'),
  legend.position = 'top', 
  legend.direction = 'horizontal',
  legend.key = element_blank(),
  panel.border = element_blank()
)
ggsave(
  plot = p,
  file = file.path(pathOut, 'slopePlotELF.pdf'),
  width = 7, height = 7/1.618, dpi = 1200, family = 'serif'
)
rm(p, muPolity, rangeELF, yELF, scenData)

## END