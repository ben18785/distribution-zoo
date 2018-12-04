#!/bin/sh

changeName()
{
  grep -rl "input\$$1[^a-zA-Z]" ./App-1/*.R | xargs sed -ie "s/input\$$1\([^a-zA-Z]\)/input\$$2\1/"
}

changeName mu normal_mu
changeName sigma normal_sigma

rm ./App-1/*.Re