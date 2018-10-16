// normal_distribution
#include <iostream>
#include <string>
#include <random>
#include <chrono>
#include <math.h>

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
std::vector<double> normal_rng(int n, double mu, double sigma, int seed){
  std::default_random_engine generator(seed);
  std::normal_distribution<double> distribution(mu, sigma);
  std::vector<double> samples(n);
  for(int i = 0; i < n; i++)
    samples[i] = distribution(generator);
  return samples;
}

int main()
{
  // parameter values
  double mu = 0.0;
  double sigma = 1.0;

  // Example x value
  double x = 0.0;

  // Compute normal pdf at x = 0
  double a_pdf = normal_pdf(x, mu, sigma);
  std::cout << "normal_pdf(x| mu, sigma) = " << a_pdf << std::endl;

  // Compute log normal pdf at x = 0
  double a_lpdf = normal_lpdf(x, mu, sigma);
  std::cout << "normal_lpdf(x| mu, sigma) = " << a_lpdf << std::endl;

  // Generate n random samples
  int n = 1000;
  std::vector<double> samples = normal_rng(n, mu, sigma);
  // properties of sample
  float mean = accumulate(samples.begin(), samples.end(), 0.0) / samples.size();
  double sq_sum = std::inner_product(samples.begin(), samples.end(), samples.begin(), 0.0);
  std::cout << "The sample mean is " << mean << std::endl;
  std::cout << "The standard deviation is " << std::sqrt(sq_sum / samples.size() - pow(mean, 2)) << std::endl;
  return 0;
}
