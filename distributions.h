#include <iostream>
#include <string>
#include <random>
#include <chrono>
#include <math>


////////////////////////////////////////////////////////////////////////////////
////*******               NORMAL DISTRIBUTION                       *******/////
////////////////////////////////////////////////////////////////////////////////

//****** PDF ******//
double normal_pdf(double x, double mu, double sigma){
  return 1.0 / (std::sqrt(2.0 * M_PI) * sigma) * exp(-pow(x - mu, 2.0) / (2 * pow(sigma, 2.0)));
}

//****** Log PDF ******//
double normal_lpdf(double x, double mu, double sigma){
  return -0.5 * log(2 * M_PI) - log(sigma) - pow(x - mu, 2.0) / (2 * pow(sigma, 2.0));
}

//****** GENERATE RANDOM SAMPLES ******//
// unseeded
std::vector<double> normal_rng(int n, double mu, double sigma){
  // set seed according to clock, alternatively set number
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  std::default_random_engine generator(seed);
  std::normal_distribution<double> distribution(mu, sigma);
  std::vector<double> samples(n);
  for(int i = 0; i < n; i++)
    samples[i] = distribution(generator);
  return samples;
}
// seeded
std::vector<double> normal_rng(int n, double mu, double sigma, unsigned seed){
  std::default_random_engine generator(seed);
  std::normal_distribution<double> distribution(mu, sigma);
  std::vector<double> samples(n);
  for(int i = 0; i < n; i++)
    samples[i] = distribution(generator);
  return samples;
}

////////////////////////////////////////////////////////////////////////////////
////*******              END NORMAL DISTRIBUTION                    *******/////
////////////////////////////////////////////////////////////////////////////////
