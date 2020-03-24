
# Set Working Directory to Repo
wd = paste0(Sys.getenv("USERPROFILE"),"/corona")
setwd(wd)

source('utils.R') # Remove Outliers, more utilities tbd


# Define and load necessary packages
packages = c("RMySQL", "vars", "stats", "Hmisc", 
             "data.table", "tseries", "quantmod", 
             "anytime", "xtable", "psd", "DescTools")

lapply(packages, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(packages, library, quietly = TRUE, character.only = TRUE)




###################################################### Define input data set #################################################

# Import data
firstwave.data = new.env()
secondwave.data = new.env()

# Demo Data
load("data/SOMEMATRIX.RData", envir = firstwave.data)
load("data/ANOTHERMATRIX.RData", envir = secondwave.data)

# Combine data input
dat = rbind(firstwave.data$NAME, secondwave.data$ANOTHERNAME)
setDT(dat) # 

# Clean up
rm(firstwave.data)
rm(secondwave.data)


# Initialize Environment for VARs
var.section = new.env()
adf.section = new.env()
acf.section = new.env()
descriptive.section = new.env()


### Descriptive Analysis

  # Autocorrelation
  acf.singles = apply(dat, 2, function(z){
    acf(z, ci = 0.95)
  })
  
  
  # Save Autocorrelation
  acfs.name = paste0(names(dat),"-acf")
  assign(acfs.name, acf.singles, envir = acf.section)
  
  # Correlation
  cors.pearson = cor(dat)
  cors.spearman = cor(dat, method = "spearman")
  
  # Save Correlations - Pearson
  cors.name.pearson = paste0(symbol,"-pearson.cor")
  assign(cors.name.pearson, cors.pearson, envir = descriptive.section)
  
  # Save Correlations - Spearman
  cors.name.spearman = paste0(symbol,"-spearman.cor")
  assign(cors.name.spearman, cors.spearman, envir = descriptive.section)
  
  
  # Summary Statistics
  summaries = sapply(dat, function(z) base::summary(z))
  
  # Save Summary Statistics
  summary.name = paste0(symbol,"-summary")
  assign(summary.name, summaries, envir = descriptive.section)
  
  

### End Descriptive Analysis




criterion = "AIC" # Akaike Information Criterion
varlag = 15 # max allowed lag
main_var = vars::VAR(dat, ic = criterion, type = "none", lag.max = varlag)


# Save VAR in according environment
assign("SOMEVARNAME", main_var, envir = var.section) # seriously put it in the according environment so that we can just
# run the test over all elements in the env


# ADF TEST / AUGMENTED DICKEY FULLER
adflag = 12
ur.df(VARNAME, type = "none", lags = adflag)


################################################### Section Start: Vector Autoregression EVALUATION ###############################################

# Save results in new environment
print("Relevant objects are stored in the new environment var.section")
print(ls(var.section))
lapply(var.section, summary) # summaries

################################################### Section End: Vector Autoregression ###############################################



################################################### Section Start: Check Stability ###############################################


# New Section: Check if VAR is stable by examining the modulus 
# of the eigenvalues of the characteristic polynomial

root.list = lapply(var.section, roots)
root = unlist(root.list)
if(any(root > 1))
{
  root.location = which(root > 1)
  root.message = paste0("Root > 1 detected at ", root.location)
  print(root.message)
} else
{
  print("Congratulations. All VARs are stable.")  
}

################################################### Section End: Check Stability ###############################################



################################################### Section Start: Analyzing Residuals / Portmanteau Test ###############################################

# Environment: portmanteau
portmanteau.section = new.env()

# use VAR objects from var.section
portmanteau.section$test = lapply(var.section, function(z)
{
  s.es = serial.test(z, type = "BG") # or ES, BG = BREUSCH-PAGAN
  s.pt = serial.test(z, type = "PT.asymptotic", lags.pt = 20) # some nice asymptotics #FeelZeBernd
  
  return(list(s.es, s.pt))
})

# Save results in new environment
print("Relevant objects are stored in the new environment portmanteau.section")
print(ls(portmanteau.section))
portmanteau.section$test # Output


# Exclude serially correlated error models from further analysis
################################################### Section End: Analyzing Residuals / Portmanteau Test ###############################################



################################################### Section Start: Analyzing Residuals / Normality Test ###############################################


# New Environment
normality.section = new.env()

# Again use VAR objects from var.section
normality.section$test = lapply(var.section, function(z)
{
  n = normality.test(z)
  return(n)
})


# Save results in new environment
print("Relevant objects are stored in the new environment normality.section")
print(ls(normality.section))
normality.section$test # Output




################################################### Section End: Analyzing Residuals / Normality Test ###############################################




################################################### Section Start: Granger Causality ###############################################

# New Environment
granger.section = new.env()

# Again use VAR objects from var.section
granger.section$test = lapply(var.section, function(z)
{
  # returns are always first column
  cause.column.one = colnames(z$y)[1] # choose two colnames here that you want to test for granger-causality
  cause.column.two = colnames(z$y)[2] 
  
  c.one = causality(z, cause = c(cause.column.two)) # VAR1 is cause
  c.two = causality(z, cause = c(cause.column.one)) # VAR2 is cause
  
  out.list = list(c.one, c.two)
  
  return(out.list)
})


# Save results in new environment
print("Relevant objects are stored in the new environment granger.section")
print(ls(granger.section))
granger.section$test # Output





## TO BE DONE!!! 
# This is an example
# Forecast Error Variance Decomposition
#
# v = var.section$voi.btc
#
# f = fevd(v)
# data <- ldply(f, data.frame)
# data$Steps = rep(1:10, 2)
#
# p <- plot_ly(data, x = ~Steps[.id == "voi"], y = ~voi[.id == "voi"], color = I("red"),type = 'bar', name = 'Order Imbalance', height = 300, legendgroup = "one") %>%
#   add_trace(y = ~ret[.id == "voi"], name = 'Returns', color = I("black"), legendgroup = "one") %>%
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'), barmode = 'stack', title = "FEVD for Order Imbalance")
#
# p2 <- plot_ly(data, x = ~Steps[.id == "ret"], y = ~voi[.id == "ret"], color = I("red"), type = 'bar', name = 'Order Imbalance', height = 300, legendgroup = "one", showlegend = FALSE) %>%
#   add_trace(y = ~ret[.id == "ret"], name = 'Returns', color = I("black"), showlegend = FALSE) %>%
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'), barmode = 'stack', title = "FEVD for Returns")
#
#
# subplot(p, p2, nrows = 2, shareX = TRUE, shareY = TRUE)
#
#
# # FEVD Function
# 
# gen.fevd.plot = function(varmodel, cryptoname){
# 
# 
# v = varmodel
# 
# f = fevd(v)
# data <- ldply(f, data.frame)
# data$Steps = rep(1:10, 2)
# 
# p <-  plot_ly(data, x = ~Steps[.id == "voi"], y = ~voi[.id == "voi"], color = I("red"),type = 'bar', name = 'Order Imbalance') %>%
#       add_trace(y = ~ret[.id == "voi"], name = 'Returns', color = I("black")) %>%
#   
#       layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'),
#       barmode = 'stack', title = paste0(cryptoname, " - FEVD for Order Imbalance"), showlegend = TRUE)
# 
# p2 <- plot_ly(data, x = ~Steps[.id == "ret"], y = ~voi[.id == "ret"], color = I("red"), type = 'bar', name = 'Order Imbalance') %>%
#   add_trace(y = ~ret[.id == "ret"], name = 'Returns', color = I("black")) %>%
#   
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'),
#          barmode = 'stack', title = paste0(cryptoname, " - FEVD for Returns"),showlegend = FALSE)
# 
# 
# p3 = subplot(p, p2, nrows = 2, which_layout = 1, heights = c(0.3, 0.3), margin = 0.1)
# p3
# 
# 
# 
# p.comb = subplot(p, p2, heights = 0.5) %>%
#   layout(annotations = list(
#   list(x = 0.2 , y = 1.05, text = "AA", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.8 , y = 1.05, text = "BB", showarrow = F, xref='paper', yref='paper')))
# 
# 
# p.comb = subplot(p, p2, nrows = 1,heights = c(0.5))
# # p.comb = subplot(p, p2, nrows = 2, heights = c(0.5, 0.5))
# 
# return(p.out)
# 
# }
# 
#  gen.fevd.plot(var.section$voi.btc, "Bitcoin")
#
# # 
# # 
# # # Litecoin
# # 
# # ltc.voi.irf = irf(var.section$voi.ltc, response = "ret", boot = TRUE)
# # ltc.voi = data.frame(ltc.voi.irf$irf$voi, ltc.voi.irf$irf$voi2, 1:11)
# # names(ltc.voi) = c("VOI", "VOI2", "Steps")
# # 
# # p2 = plot_ly(ltc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers", yaxis = "Returns") %>% 
# #   add_trace(y = ~VOI2, name = "VOI2 --> Returns",mode = "lines+markers") %>%
# #   layout(title = "Litecoin IRF: VOI & VOI2 --> Returns",
# #          yaxis = list("title" = "Returns"),
# #          xaxis = list("title" = "Steps"))
# # 
# 
# plotly::export(p2, "LTC-IRF-VOI.png")
# 
# # Bitcoin
# 
# btc.voi.irf = irf(var.section$voi.btc, response = "ret", boot = TRUE)
# btc.voi = data.frame(btc.voi.irf$irf$voi, btc.voi.irf$irf$voi2, 1:11)
# names(btc.voi) = c("VOI", "VOI2", "Steps")
# 
# p3 = plot_ly(btc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~VOI2, name = "VOI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Bitcoin IRF: VOI & VOI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p3, "BTC-IRF-VOI.png")
# 
# 
# btc.oi.irf = irf(var.section$oi.btc, response = "ret", boot = TRUE)
# btc.oi = data.frame(btc.oi.irf$irf$oi, btc.oi.irf$irf$oi2, 1:11)
# names(btc.oi) = c("OI", "OI2", "Steps")
# 
# p4 = plot_ly(btc.oi, x = ~Steps, y = ~OI, name = "OI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~OI2, name = "OI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Bitcoin IRF: OI & OI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p4, "BTC-IRF-OI.png")
# 
# 
# # Etherium
# 
# etc.voi.irf = irf(var.section$voi.etc, response = "ret", boot = TRUE, n.ahead = 10, cumulative = TRUE)
# etc.voi = data.frame(etc.voi.irf$irf$voi, etc.voi.irf$irf$voi2, 1:11)
# names(etc.voi) = c("VOI", "VOI2", "Steps")
# 
# p5 = plot_ly(etc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~VOI2, name = "VOI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Etherium IRF: VOI & VOI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p5, "ETC-IRF-VOI.png")
# 
# 
# etc.oi.irf = irf(var.section$oi.etc, response = "ret", boot = TRUE)
# etc.oi = data.frame(etc.oi.irf$irf$oi, etc.oi.irf$irf$oi2, 1:11)
# names(etc.oi) = c("OI", "OI2", "Steps")
# 
# p6 = plot_ly(etc.oi, x = ~Steps, y = ~OI, name = "OI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~OI2, name = "OI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Etherium IRF: OI & OI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p6, "ETC-IRF-OI.png")
# setwd(wd)
# 
