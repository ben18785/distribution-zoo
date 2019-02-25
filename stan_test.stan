functions{
  int discreteuniform_rng(int lower, int upper){
    int diff;
    int cat;
    int d = 0;
    if(upper < lower)
      return(not_a_number());
    diff = upper - lower + 1;
    cat = categorical_rng(rep_vector(1.0 / diff, diff));
    if(lower <= 0)
      d = 1 - lower;
    else if(lower > 1)
      d = -(lower - 1);
    return(cat - d);
  }
}

data{
  int x;
}

generated quantities{
  // calling function

  int test[10];
  for(i in 1:10)
    test[i] = discreteuniform_rng(2, 5);
  
}