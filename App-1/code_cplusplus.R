fCpluspluscode <- function(input){
  
  if(input$dist=="Normal"){
    if(input$property=="pdf")
      tagList(p(HTML(paste("#include &lt;math&gt",
                           sep="<br/>")), style="color:#d95f02"),
              p(HTML(paste("double normal_pdf(int n, double mu, double sigma)",
                           "{",
                           paste0("return 1.0 / (std::sqrt(2.0 * M_PI) * ",
                                  eval(parse(text=input$sigma)),
                                  ") * exp(-pow(x - ",
                                  eval(parse(text=input$mu)),
                                  ", 2.0) / (2 * pow(",
                                  input$sigma, ", 2.0)));"),
                           "}",
                           sep="<br/>")), style="color:#d95f02"))
    else if(input$property=="log_pdf")
      tagList(p(HTML(paste("#include &lt;math&gt",
                           sep="<br/>")), style="color:#d95f02"),
              p(HTML(paste("double normal_lpdf(int n, double mu, double sigma)",
                           "{",
                           paste0("return -0.5 * log(2 * M_PI) - log(",
                                  eval(parse(text=input$sigma)),
                                  ") - pow(x - ",
                                  eval(parse(text=input$mu)),
                                  ", 2.0) / (2 * pow(",
                                  input$sigma, ", 2.0)));"),
                           "}",
                           sep="<br/>")), style="color:#d95f02"))
    else if(input$property=="random")
      tagList(p(HTML(paste("#include &lt;random&gt;",
                           "#include &lt;vector&gt",
                           "#include &lt;math&gt",
                           "#include &lt;chrono&gt;",
                           sep="<br/>")), style="color:#d95f02"),
              p(HTML("// unseeded"), style="color:#d95f02"),
              p(HTML(paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma)", "<br/>",
                            "{", "<br/>",
                            "unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();", "<br/>",
                            "std::normal_distribution&lt;double&gt; distribution(",
                            eval(parse(text=input$mu)), ", ", eval(parse(text=input$sigma)), ");", "<br/>",
                            "std::vector&lt;double&gt; samples(n);", "<br/>",
                            "for(int i = 0; i < n; i++)", "<br/>",
                            "&emsp;", "samples[i] = distribution(generator);", "<br/>",
                            "return samples;", "<br/>",
                            "}")), style="color:#d95f02"),
              p(HTML("// seeded"), style="color:#d95f02"),
              p(HTML(paste0("std::vector&lt;double&gt; normal_rng(int n, double mu, double sigma, unsigned seed)", "<br/>",
                            "{", "<br/>",
                            "std::normal_distribution&lt;double&gt; distribution(",
                            eval(parse(text=input$mu)), ", ", eval(parse(text=input$sigma)), ");", "<br/>",
                            "std::vector&lt;double&gt; samples(n);", "<br/>",
                            "for(int i = 0; i < n; i++)", "<br/>",
                            "&emsp;", "samples[i] = distribution(generator);", "<br/>",
                            "return samples;", "<br/>",
                            "}")), style="color:#d95f02"))
  }
}