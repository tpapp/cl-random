gcc -Wall -I/usr/local/include -c gsl-rng-dump.c
gcc -L/usr/local/lib gsl-rng-dump.o -lgsl -lm -o gsl-rng-dump
