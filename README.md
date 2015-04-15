 A preliminary port of ANU SQ to GAP4
 
 to install, you need to 
   * build ve binaries from https://github.com/dimpase/ve
   * adjust the path in the makefile to point to these binaries
   * build Sq binary
 

to load anusq into GAP4, just cd to the appropriate directory, start GAP, and do

       gap> Read("init.g");
