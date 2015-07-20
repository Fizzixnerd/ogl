# A simple Makefile
# Author: Matthew Walker <matt.g.d.walker@gmail.com>
# Date: July 5, 2015
# License: GNU GPL 3+

#################################
# YOUR SHITTY VARIABLES GO HERE #
#################################

EXECUTABLE_NAMES=most-illustrious-of-cubes
SRC_SUFFIX=.cpp
RM=trash

YOUR_CFLAGS=
YOUR_CXXFLAGS=
YOUR_CPPFLAGS=
YOUR_LDFLAGS=-L.
YOUR_LDLIBS=-lglut -lGL -lGLEW -lm

#########################
# DON'T TOUCH THIS SHIT #
#########################

CC=gcc
CXX=g++

CFLAGS=$(YOUR_CFLAGS)
CXXFLAGS=-Wall -std=c++11 $(YOUR_CXXFLAGS)
CPPFLAGS=$(YOUR_CPPFLAGS)
LDFLAGS=$(YOUR_LDFLAGS)
LDLIBS=$(YOUR_LDLIBS)

SRCS=$(wildcard *$(SRC_SUFFIX))
OBJS=$(subst $(SRC_SUFFIX),.o,$(SRCS))

##############
# MAKE RULES #
##############

all: $(EXECUTABLE_NAMES)

most-illustrious-of-cubes: $(OBJS)
	$(CXX) $(LDFLAGS) -o most-illustrious-of-cubes $(OBJS) $(LDLIBS)

####################################################
# YOU NEED TO TELL ME HOW TO MAKE YOUR EXECUTABLE(S)
# FOR EXAMPLE, YOU COULD DO:
#
# tool: $(OBJS)
#	$(CXX) $(LDFLAGS) -o tool $(OBJS) $(LDLIBS)
# 
# this: $(OBJS)
#	$(CXX) $(LDFLAGS) -o this $(OBJS) $(LDLIBS)
####################################################

depend: .depend

.depend: $(SRCS)
	$(RM) ./.depend
	$(CXX) $(CPPFLAGS) -MM $^>>./.depend;

clean:
	$(RM) $(OBJS)

dist-clean: clean
	$(RM) *~ .depend

include .depend
