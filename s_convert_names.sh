grep -rl 'input\$mu[^a-zA-Z]' ./App-1/*.R | xargs sed -ie 's/input\$mu\([^a-zA-Z]\)/input\$normal_mu\1/'

rm ./App-1/*.Re