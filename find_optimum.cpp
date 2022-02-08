#include <Rcpp.h>
#include <map>

// [[Rcpp::export]]
Rcpp::List find_optimum(Rcpp::NumericVector price_supply,
                        Rcpp::NumericVector quant_supply,
                        Rcpp::NumericVector price_demand,
                        Rcpp::NumericVector quant_demand) {
  
  std::map<double, double> supply;
  std::map<double, double> demand;
  
  // fill the maps -- just copies the arrays into key-value pairs
  for (int i = 0; i < price_supply.size(); ++i) {
    supply[price_supply[i]] += quant_supply[i];
  }
  for (int i = 0; i < price_demand.size(); ++i) {
    demand[price_demand[i]] += quant_demand[i];
  }
  
  if (supply.empty() || demand.empty()) 
    return Rcpp::List::create(Rcpp::Named("price") = 0, Rcpp::Named("quantity") = 0);
  
  auto sIt = supply.begin(), nextS = std::next(sIt, 1);
  const auto endS = supply.end();
  auto dIt = demand.rbegin(), nextD = std::next(dIt, 1);
  const auto endD = demand.rend();
  
  // quantity and prices at either side
  double pS = sIt->first, pD = dIt->first;
  double qS = 0, qD = 0;
  
  // next prices
  double nextPS = nextS->first, nextPD = nextD->first;
  if (pD < pS) 
    return Rcpp::List::create(Rcpp::Named("price") = 0, Rcpp::Named("quantity") = 0);
  
  // add the best price from each side!
  qS += sIt->second;
  qD += dIt->second;
  
  while (pS < pD) {
    if (nextS == endS && nextD == endD) {
      pD = qD < qS ? pS : pD;
      break;
    }
    
    while (qS <= qD && sIt != endS && nextS->first <= pD) {
      ++sIt;
      ++nextS;
      pS = sIt->first;
      qS += sIt->second;
    }
    if (sIt == endS) break;
    
    if (nextD->first < pS) {
      pD = qD < qS ? pS : pD;
      break;
    }
    
    while (qD < qS && dIt != endD && nextD->first >= pS) {
      ++dIt;
      ++nextD;
      pD = dIt->first;
      qD += dIt->second;
    }
    if (dIt == endD) break;
  }
  
  double price = pD;
  double vol = qS < qD ? qS : qD;
  
  return Rcpp::List::create(Rcpp::Named("price") = price, 
                            Rcpp::Named("quantity") = vol);
}