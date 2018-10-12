# Reference : Lewandowski D, Kurowicka D and Joe H (2009).
# Generating random correlation matrices based on vines and extended
# Onion method. J. Multivariate Analysis, to appear.
#============================================================

# C code

# cvine
cvine: cvine.c rbeta2.c
	gcc -DMAIN -o cvine cvine.c rbeta2.c -lm

# cvine < rcor.in > cvine.out

onion: onion.c rbeta2.c 
	gcc -DMAIN -o onion onion.c rbeta2.c -lm
# onion < rcor.in > onion.out

#============================================================

# R code 
# The following works in Unix and produces Rplots.ps in older versions of R
# For the latest version of R, Rplots.pdf is produced.
rcvine:
	R -q --no-save --slave < rcvine.r
	mv Rplots.ps rcvine.ps

ronion:
	R -q --no-save --slave < ronion.r
	mv Rplots.ps ronion.ps

#============================================================


zipfile:
	zip -l rcor-R-C.zip Readme.txt Makefile *.c *.in *.out *.r
