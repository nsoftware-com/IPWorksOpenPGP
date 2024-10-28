#
# IPWorks OpenPGP 2024 C++ Edition - Sample Project
#
# This sample project demonstrates the usage of IPWorks OpenPGP in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.nsoftware.com/ipworksopenpgp
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lipworksopenpgp.24.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o pfilemailer pfilemailer.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/ipworksopenpgp.o
	g++ $(OS_CFLAGS) -o pfilemailer ../../src/ipworksopenpgp.o pfilemailer.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/ipworksopenpgp.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/ipworksopenpgp.o ../../src/ipworksopenpgp.cpp

else # LINUX
CPU=$(shell uname -m)
# Assumed that a product is always built for Windows-x64 platform
ifeq ($(CPU), $(filter $(CPU), i386 i686))
  LIB=lib
endif
ifeq ($(LIB),)
  LIB=lib64
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lipworksopenpgp  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX  -o pfilemailer pfilemailer.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH)

src: ../../src/ipworksopenpgp.o
	g++ -D UNIX -o pfilemailer ../../src/ipworksopenpgp.o pfilemailer.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/ipworksopenpgp.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/ipworksopenpgp.o ../../src/ipworksopenpgp.cpp

clean:
	rm -f ../../src/*.o
	rm -f pfilemailer *.o
endif