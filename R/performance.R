postResampleSpectro <- function(pred, obs){
  # If preds and obs are void
  if (length(obs) + length(pred) == 0) {
    out <- rep(NA, 2)
  } else {
    if (length(unique(pred)) < 2 || length(unique(obs)) < 2) {
      resamplCor <- NA
    }
    else {
      # Compute preds-obs correlation
      resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)
      # Manage error
      if (class(resamplCor) == "try-error") 
        resamplCor <- NA
    }
    
    n <- length(obs)
    
    # Compute R2
    r2 <- resamplCor^2
    
    # Compute MSE/SEP2
    mse <- mean((pred - obs)^2)
    
    # Standard error of prediction
    # (SEP/RMSEP)
    rmsep <- sqrt(mse)
    
    # Bias
    bias <- mean(pred) - mean(obs)
    
    # Standard error
    #     se <- sd(pred - obs)
    sse <- sum((pred - obs)^2)
    se <- sqrt(sse/(n - 1))
    
    # Residual  variance
    #     sep2c <- sqrt(sum(((pred - bias - obs)^2) / n))
    
    # ratio of performance to deviation
    rpd <- sd(obs)/rmsep
    
    # Ratio of performance to interquartile distance
    qs <- quantile(obs, probs = seq(0, 1, 0.25), names = FALSE)
    iq <- qs[4] - qs[2]
    rpiq <- iq/rmsep
    
    # Lin's CCC
    ccc <- as.numeric(epi.ccc(obs, pred)$rho.c[1])
    
    out <- c(rmsep, r2, rpd, rpiq, ccc, bias, se)
  }
  
  # Manage var names
  names(out) <- c("RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SE")
  
  # Manage NAs
  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out  
}

spectroSummary <- function (data, lev = NULL, model = NULL) {
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  postResampleSpectro(data[, "pred"], data[, "obs"])
}