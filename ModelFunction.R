leish <- function(t, state, parameters){
  with(as.list(c(state, parameters)),
       {
         #Vector
         Hn <- Hs + He + Hi
         Rn <- Rs + Ri
         
         K1 <- a1 * Rn + b1 * Hn
         V1 <- V1s + V1i
         
         K2 <- a2 * Rn + b2 * Hn
         V2 <- V2s + V2i
         
         beta.h1 <- - n1 * log(1 - epsilon)
         beta.h2 <- - n2 * log(1 - epsilon)
         
         dV1s <- r1*V1 * (1-V1/K1) - beta.1 * p1 *V1s * Ri/(p1*Rn + (1-p1)*Hn) - mu *V1s
         dV1i <- beta.1 *p1 *V1s * Ri/(p1*Rn + (1-p1)*Hn) - mu * V1i
         
         dV2s <- r2*V2 * (1-V2/K2) - beta.2 * p2* V2s * Ri/(p2*Rn + (1-p2)*Hn) - mu *V2s
         dV2i <- beta.2 *p2 *V2s * Ri/(p2*Rn + (1 - p2)*Hn) - mu *V2i
         
         dV1s <- ifelse(V1s <= 0 & V1i <= 0, 0, dV1s)
         dV2s <- ifelse(V2s <= 0 & V2i <= 0, 0, dV2s)
         
         #Reservorio
         
         transR1 <- ifelse(V1 == 0, 0, beta.r * p1 * V1i/V1 * Rs)
         transR2 <- ifelse(V2 == 0, 0, beta.r * p2 * V2i/V2 * Rs)
         
         dRs <- r.r * Rn *(1 - Rn/Kr) - transR1 - transR2 - mu.r * Rs
         dRi <-  transR1 + transR2 - mu.r * Ri
         
         #Humanos
         
         transH1 <- ifelse(V1 == 0, 0, beta.h1 * (1 - p1)*V1i/V1)
         transH2 <- ifelse(V2 == 0, 0, beta.h2 * (1 - p2)*V2i/V2)
         
         dHs <- r.h * Hn - (transH1 + transH2)*Hs + omega * Hi - mu.h * Hs
         dHe <- (transH1 + transH2)*Hs - sigma * He - mu.h * Hi
         dHi <- sigma * He - omega * Hi - mu.h * Hi
         
         list(c(dV1s, dV1i, dV2s, dV2i, dRs, dRi, dHs, dHe, dHi))
       }
  )
}
