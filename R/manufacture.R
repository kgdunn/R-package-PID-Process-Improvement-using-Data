# (c) Kevin Dunn, 2015.

manufacture <- function(P=0.75, T=325){
  # Simulates a manufacturing facility where there are 2 factors that affect the outcome.
  #
  #   P = selling price of the product, measured in dollars and cents
  #   T = throughput (production rate) of the process, measured in parts per hour
  # 
  # Typical values for p = $0.75 and T = 325 parts per hour.
  # 
  # The outcome is: profit made per hour [dollars].
  
  if ((length(P)>1)|(length(T)>1)){
    stop('Running the manufacturing experiments in parallel is (intentionally) not allowed.')
  }
  if (!all(is.finite(P)) | !all(is.finite(T))){
    stop('All function inputs must be finite numbers.')
  } else if(P < 0){
    stop('Please provide a positive sales price, P.')
  } else if(T < 0){
    stop('The throughput must be a positive value.')
    
  } else{
  
    pCoded = (P - 1.5) / 1.0
    tCoded = (T - 320.0) / 20.0
    y = (18.0*tCoded + 10*pCoded - 5*tCoded*pCoded - 7*tCoded*tCoded - 24*pCoded*pCoded + 50)*12 + 2*sin(T) + 2*cos(P) + rnorm(1)*2
    y = round(y)
  }
  return(y)
}
manufacture(c(400,040))