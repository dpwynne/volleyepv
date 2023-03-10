## Just verifying approximately normal distribution with 100 attempts
Hmisc::rMultinom(matrix(c(0.5, 0.2, 0.3), nrow = 1000, ncol = 3, byrow = TRUE), 100) -> a
apply(a, 1, function(x) (sum(x == 1) - sum(x == 2))/length(x)) -> EFF
plot(EFF)

# Approx N(pK - pE, sqrt((pK*(1-pK+pE) + pE*(1 - pE + pK))/n))


# Hard part is estimating pK and pE, here we use plug-in estimates with Dirichlet(0, 0, 0) improper prior
pEff <- function(line1, line2, alpha = c(0, 0, 0), plot_eff = FALSE){
  # line1 and line2 are of the form c(Kills, Errors + Blocks, Total Attempts)

  n <- c(line1[3] + sum(alpha), line2[3] + sum(alpha))
  pK <- c((line1[1] + alpha[1])/n[1], (line2[1] + alpha[1])/n[2]) # Posterior mean of P(Kill)
  pE <- c((line1[2] + alpha[2])/n[1], (line2[2] + alpha[2])/n[2]) # Posterior mean of P(Error/Block)

  # Note: this default uses Dirichlet(0) prior
  # But in avgEff (below) we use a weakly informative prior based on overall efficiency in the dataset

  # Get parameters of posterior distribution of efficiency
  Eff_mean <- pK - pE
  Eff_Var <- (pK * (1 - pK + pE) + pE * (1 - pE + pK))/n

  # Difference in efficiency is approximately normally distributed
  p1 <- pnorm(0, diff(Eff_mean), sqrt(sum(Eff_Var))) # Estimated P(Eff2 < Eff1) = P(Eff1 > Eff2)

  if(plot_eff){
    x <- seq(-1, 1, by = 0.01)
    plot(x, dnorm(x, mean = Eff_mean[1], sd = sqrt(Eff_Var[1])), type = "l", xlim = c(-1, 1),
         xlab = "Estimated Posterior Distribution of Efficiency", ylab = "Density")
    lines(x, dnorm(x, mean = Eff_mean[2], sd = sqrt(Eff_Var[2])), col = "red")


  }

  return(p1)
}

avgEff <- function(df, use_prior = TRUE){

  n <- nrow(df)
  Eff_diff_matrix <- matrix(0.5, nrow = n, ncol = n)

  if(use_prior){
    # Extremely weakly informative prior - essentially assumes everyone hits avg. efficiency over full dataset
    K <- sum(df$Kills)
    E <- sum(df$Errors)
    N <- sum(df$Attempts)
    prior <- c(K/N, E/N, (N - K - E)/N)
  } else {
    prior <- c(0, 0, 0) # noninformative Dirichlet(0) prior
  }

  for(i in 1:(n-1)){
    for(j in 2:n){
      Eff1 <- c(df$Kills[i], df$Errors[i], df$Attempts[i])
      Eff2 <- c(df$Kills[j], df$Errors[j], df$Attempts[j])
      pEff_ij <- pEff(Eff1, Eff2, alpha = prior)
      Eff_diff_matrix[i,j] <- pEff_ij
      Eff_diff_matrix[j,i] <- 1 - pEff_ij
    }
  }

  avg_better_eff <- (apply(Eff_diff_matrix, 1, sum) - 0.5)/(n-1)

  return(list(
    Eff = (df$Kills - df$Errors)/df$Attempts,
    p_better_eff = Eff_diff_matrix,
    avg_p_better = avg_better_eff))
}

#### EXAMPLE
toy_df <- data.frame(Kills = c(5, 3, 7, 9, 11),
                     Errors = c(2, 0, 1, 3, 2),
                     Attempts = c(10, 8, 16, 16, 20))
# Player 1 hitting .300, Players 2 and 3 hitting .375 on 12 attempts, Player 4 hitting .450
avgEff(toy_df, use_prior = F)
avgEff(toy_df, use_prior = T)
# because use_prior = TRUE, pools information from all players in dataset, so Player 2 and Player 3 are not exactly 50/50 anymore


toy_df2 <- data.frame(Kills = c(4, 40, 50, 100, 120),
                      Errors = c(2, 20, 25, 50, 30),
                      Attempts = c(10, 100, 100, 200, 300))
avgEff(toy_df2)
# default is use_prior = T, here there are so many attacks it doesn't really matter for comparing between players 2-5
# but it matters quite a bit for comparing to player 1
