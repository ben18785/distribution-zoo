fCpluspluscode <- function(input){
  text <- 
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      paste("#include &lt;math&gt",
              paste("double normal_pdf(int n, double mu, double sigma)",
                           "{",
                           paste0("return 1.0 / (std::sqrt(2.0 * M_PI) * ",
                                  eval(parse(text=input$normal_sigma)),
                                  ") * exp(-pow(x - ",
                                  eval(parse(text=input$normal_mu)),
                                  ", 2.0) / (2 * pow(",
                                  input$normal_sigma, ", 2.0)));"),
                           "}",
                           sep="\n"),
              sep="\n")
    else if(input$property=="log_pdf")
      paste("#include &lt;math&gt",
                paste("double normal_lpdf(int n, double mu, double sigma)",
                           "{",
                           paste0("return -0.5 * log(2 * M_PI) - log(",
                                  eval(parse(text=input$normal_sigma)),
                                  ") - pow(x - ",
                                  eval(parse(text=input$normal_mu)),
                                  ", 2.0) / (2 * pow(",
                                  input$normal_sigma, ", 2.0)));"),
                           "}",
                           sep="\n"), sep="\n")
    else if(input$property=="random")
      paste("#include &lt;random&gt;",
                           "#include &lt;vector&gt",
                           "#include &lt;math&gt",
                           "#include &lt;chrono&gt;",
              "// unseeded",
              paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma)", "\n",
                            "{",
                            "\n",
                            "unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();", "\n",
                            "std::normal_distribution&lt;double&gt; distribution(",
                            eval(parse(text=input$normal_mu)), ", ", eval(parse(text=input$normal_sigma)), ");", "\n",
                            "std::vector&lt;double&gt; samples(n);", "\n",
                            "for(int i = 0; i < n; i++)", "\n",
                            "&emsp;", "samples[i] = distribution(generator);", "\n",
                            "return samples;", "\n",
                            "}"),
              "// seeded",
              paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma, unsigned seed)", "\n",
                            "{",
                            "\n",
                            "std::normal_distribution&lt;double&gt; distribution(",
                            eval(parse(text=input$normal_mu)), ", ", eval(parse(text=input$normal_sigma)), ");", "\n",
                            "std::vector&lt;double&gt; samples(n);", "\n",
                            "for(int i = 0; i < n; i++)", "\n",
                            "&emsp;", "samples[i] = distribution(generator);", "\n",
                            "return samples;", "\n",
                            "}"),
              sep = "\n")
  }
  return(prismCodeBlock(text, language = "cpp"))
}
