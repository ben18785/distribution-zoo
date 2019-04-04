fCpluspluscode <- function(input){
  if(input$dist=="Normal"){
    text.common.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;cassert&gt;\n",
      "#include &lt;cmath&gt;\n",
      "\n",
      "class NormalDistributionPdf {\n",
      "private:\n",
      "    // Params\n",
      "    double mMean;\n",
      "    double mStdDev;\n",
      "\n",
      "    // Cached constants for Pdf & LogPdf\n",
      "    double m2SigSq;\n",
      "    double mPrefactor;\n",
      "    double mLogPrefactor;\n",
      "\n",
      "public:\n",
      "    explicit NormalDistributionPdf(double mean = 0.0, double std_dev = 1.0)\n",
      "            : mMean(mean), mStdDev(std_dev) {\n",
      "\n",
      "        // Standard deviation must be positive\n",
      "        assert(mStdDev &gt; 0.0);\n",
      "\n",
      "        m2SigSq = 2.0 * mStdDev * mStdDev;\n",
      "        mPrefactor = 1.0 / std::sqrt(M_PI * m2SigSq);\n",
      "        mLogPrefactor = -0.5 * std::log(M_PI * m2SigSq);\n",
      "    }\n",
      "\n",
      "    double Pdf(const double x) {\n",
      "        return mPrefactor * std::exp(-(x - mMean) * (x - mMean) / m2SigSq);\n",
      "    }\n",
      "\n",
      "    double LogPdf(const double x) {\n",
      "        return mLogPrefactor - (x - mMean) * (x - mMean) / m2SigSq;\n",
      "    }\n",
      "};\n",
      ""
    )
    text.pdf <- paste0(
      "const double mean = ", eval(parse(text=input$normal_mu)), ";\n",
      "const double std_dev = ", eval(parse(text=input$normal_sigma)), ";\n",
      "NormalDistributionPdf pdf{mean, std_dev};\n",
      "\n",
      "// Sample from the pdf at any given x-value:\n",
      "pdf.Pdf(0.5);\n",
      ""
    )
    text.logpdf <- paste0(
      "const double mean = ", eval(parse(text=input$normal_mu)), ";\n",
      "const double std_dev = ", eval(parse(text=input$normal_sigma)), ";\n",
      "NormalDistributionPdf pdf{mean, std_dev};\n",
      "\n",
      "// Sample from the pdf at any given x-value:\n",
      "pdf.LogPdf(0.5);\n",
      ""
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0(
      "// Seed a Mersenne twister with a 'true' random seed from random_device\n",
      "std::random_device rd{};\n",
      "std::mt19937 gen{rd()};\n",
      "\n",
      "// Params\n",
      "const std::size_t n = 100'000;\n",
      "const double mean = ", eval(parse(text=input$normal_mu)), ";\n",
      "const double std_dev = ", eval(parse(text=input$normal_sigma)), ";\n",
      "\n",
      "// Create distribution\n",
      "std::normal_distribution&lt;double&gt; dist{mean, std_dev};\n",
      "\n",
      "// Create and fill the vector\n",
      "std::vector&lt;double&gt; vec(n);\n",
      "for (double &x : vec) {\n",
      "    x = dist(gen);\n",
      "}\n",
      ""
    )
  } else if(input$dist=="Uniform"){
    text.common.dep <- paste0("Uniform coming soon"
    )
    text.pdf <- paste0("Uniform coming soon"
    )
    text.logpdf <- paste0("Uniform coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("Uniform coming soon"
    )
  } else if(input$dist=="LogNormal"){
    text.common.dep <- paste0("LogNormal coming soon"
    )
    text.pdf <- paste0("LogNormal coming soon"
    )
    text.logpdf <- paste0("LogNormal coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("LogNormal coming soon"
    )
  } else if(input$dist=="Exponential"){
    text.common.dep <- paste0("Exponential coming soon"
    )
    text.pdf <- paste0("Exponential coming soon"
    )
    text.logpdf <- paste0("Exponential coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("Exponential coming soon"
    )
  } else if(input$dist=="Gamma"){
    text.common.dep <- paste0("Gamma coming soon"
    )
    text.pdf <- paste0("Gamma coming soon"
    )
    text.logpdf <- paste0("Gamma coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("Gamma coming soon"
    )
  } else if(input$dist=="t"){
    text.common.dep <- paste0("t coming soon"
    )
    text.pdf <- paste0("t coming soon"
    )
    text.logpdf <- paste0("t coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("t coming soon"
    )
  } else if(input$dist=="Beta"){
    text.common.dep <- paste0("Beta coming soon"
    )
    text.pdf <- paste0("Beta coming soon"
    )
    text.logpdf <- paste0("Beta coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("Beta coming soon"
    )
  } else if(input$dist=="Cauchy"){
    text.common.dep <- paste0("Cauchy coming soon"
    )
    text.pdf <- paste0("Cauchy coming soon"
    )
    text.logpdf <- paste0("Cauchy coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("Cauchy coming soon"
    )
  } else if(input$dist=="HalfCauchy"){
    text.common.dep <- paste0("HalfCauchy coming soon"
    )
    text.pdf <- paste0("HalfCauchy coming soon"
    )
    text.logpdf <- paste0("HalfCauchy coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("HalfCauchy coming soon"
    )
  } else if(input$dist=="InverseGamma"){
    text.common.dep <- paste0("InverseGamma coming soon"
    )
    text.pdf <- paste0("InverseGamma coming soon"
    )
    text.logpdf <- paste0("InverseGamma coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("InverseGamma coming soon"
    )
  } else if(input$dist=="InverseChiSquared"){
    text.common.dep <- paste0("InverseChiSquared coming soon"
    )
    text.pdf <- paste0("InverseChiSquared coming soon"
    )
    text.logpdf <- paste0("InverseChiSquared coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("InverseChiSquared coming soon"
    )
  } else if(input$dist=="LogitNormal"){
    text.common.dep <- paste0("LogitNormal coming soon"
    )
    text.pdf <- paste0("LogitNormal coming soon"
    )
    text.logpdf <- paste0("LogitNormal coming soon"
    )
    text.samples.dep <- paste0(
      "// The code above depends on the following, which you can copy directly into your application\n",
      "// Note that C++11 (or later) is required\n",
      "\n",
      "#include &lt;random&gt;\n",
      "#include &lt;vector&gt;\n",
      ""
    )
    text.samples <- paste0("LogitNormal coming soon"
    )
  } else {
    text.common.dep <- paste0("Not done yet!"
    )
    text.pdf <- paste0("Not done yet!"
    )
    text.logpdf <- paste0("Not done yet!"
    )
    text.samples.dep <- paste0("Not done yet!"
    )
    text.samples <- paste0("Not done yet!"
    )
  }

  text.to.display.main <-
  if(input$property=="pdf") {
    text.pdf
  } else if(input$property=="log_pdf") {
    text.logpdf
  } else if(input$property=="random") {
    text.samples
  }
  
  text.to.display.dep <-
  if(input$property=="random") {
    text.samples.dep
  } else {
    text.common.dep
  }
  
  return(tagList(
      prismCodeBlock(text.to.display.main, language = "cpp"),
      prismCodeBlock(text.to.display.dep, language = "cpp")
    )
  )
}
