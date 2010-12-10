gx.runs <-
function(n1, n2, u)
{
     # Function to compute a runs, Wald-Wolfowitz, test for anomalies along a
     # traverse.
     #
     # This can be used in conjunction with function gx.hypergeom, for which
     # see Stanley, C.R., 2003. Statistical evaluation of anomaly recognition 
     # performance. Geochemistry: Exploration, Environment, Analysis. 3(1):3-12.
     #
     # n1 = sites <Threshold along traverse
     # n2 = sites >Threshold along traverse 
     # u = number of runs of > and < threshold sites along traverse
     #
     m <- 2 * n1 * n2
     n <- n1 + n2
     Eu <- 1 + m/n
     Varu <- (m * (m - n))/(n * n * (n - 1))
     z <- (u - Eu)/sqrt(Varu)
     pz <- 100 * round((pnorm(z)), 3)
     cat("  Traverse length is", n, "sites with", n2, "anomalous sites in", u,
          "runs\n  E(u) =", round(Eu, 2), ", Var(u) =", round(Varu, 2), "and z =", 
         signif(z, 4), paste("\n  Probability that this is due to 'chance' is ", 
         pz, "%\n",sep = ""))
     invisible()
}

