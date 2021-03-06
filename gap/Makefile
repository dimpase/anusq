## do not edit this definitions #############################################
VRSN="1.0, March 1994"
SHELL=/bin/sh
EFILE=Sq
MAKEFILE=../gap/Makefile
GAP_PATH=..
PKG_PATH=${GAP_PATH}/pkg
SRCDISTFILE=anusq
EXT=.ext
ME=`pwd`/../../ve/bin/me

## show makefile options ####################################################
unknown:
	@echo "usage: 'make <target> EXT=<ext>'  where <target> is one of"
	@echo "'bsd-gcc'    for Berkeley UNIX with GNU cc 2"
	@echo "'bsd-cc'     for Berkeley UNIX with cc"
	@echo "'usg-gcc'    for System V UNIX with cc"
	@echo "'usg-cc'     for System V UNIX with cc"
	@echo "'clean'      remove all created files"
	@echo ""
	@echo "   where <ext> should be a sensible extension, i.e.,"
	@echo "   'EXT=-sun-sparc-sunos' for SUN 4 or 'EXT=' if the SQ only"
	@echo "   runs on a single architecture"
	@echo ""
	@echo "   additional C compiler and linker flags can be passed with"
	@echo "   'make <target> COPTS=<compiler-opts> LOPTS=<linker-opts>',"
	@echo "   i.e., 'make bsd-cc COPTS=\"-DTAILS -DCOLLECT\"', see the"
	@echo "   README file for details on TAILS and COLLECT."
	@echo ""
	@echo "   set ME if the vector enumerator is not started with the "
	@echo "   command '`pwd`/../ve/bin/me',"
	@echo "   i.e., 'make bsd-cc ME=/home/ve/bin/me'."

bsd-gcc:
	@( cd src ; $(MAKE) -f $(MAKEFILE) $(EFILE) CC=gcc \
	CFLAGS="$(COPTS) -O -DSYS_BSD -DCHAT=0" \
	EXT=$(EXT) \
	ME=$(ME) )

bsd-cc:
	@( cd src ; $(MAKE) -f $(MAKEFILE) $(EFILE) CC=cc \
	CFLAGS="$(COPTS) -O -DSYS_BSD -DCHAT=0" \
	EXT=$(EXT) \
	ME=$(ME) )

usg-gcc:
	@( cd src ; $(MAKE) -f $(MAKEFILE) $(EFILE) CC=gcc \
	CFLAGS="$(COPTS) -O -DSYS_USG -DCHAT=0" \
	EXT=$(EXT) \
	ME=$(ME) )

usg-cc:
	@( cd src ; $(MAKE) -f $(MAKEFILE) $(EFILE) CC=cc \
	CFLAGS="$(COPTS) -O -DSYS_USG -DCHAT=0" \
	EXT=$(EXT) \
	ME=$(ME) )

## special rules,  we need to set ME ########################################
veinter.o: veinter.c
	$(CC) $(CFLAGS) -DME='"$(ME)"' -c veinter.c

sq.o: sq.c
	$(CC) $(CFLAGS) -DVERSION='$(VRSN)' -c sq.c

## NEVER DELETE/ALTER THESE LINES BELOW - see "objectlist" and "depend" #####

#@OBJ@#
OBJECTS=\
	arith.o                        	consis.o                      \
	eapquot.o                      	mem.o                         \
	modifypres.o                   	modmem.o                      \
	pcp.o                          	pcparith.o                    \
	presentation.o                 	sq.o                          \
	storepc.o                      	system.o                      \
	updatepres.o                   	veinter.o

#@ENDOBJ@#

## clean me up ##############################################################
clean:
	(cd src ; rm -f $(OBJECTS) core* $(EFILE) ../bin/$(EFILE)$(EXT))

## link together the executable #############################################
$(EFILE):  $(OBJECTS)
	$(CC) -o $(EFILE) $(OBJECTS) $(LOPTS) $(LDLIBS)
	mv $(EFILE) ../bin/$(EFILE)$(EXT)

## create object list for the Makefile ######################################
objectlist:
	@( cd src ; ls -1 *.c ) | \
	sed s/\\.c$$/\\.o/ | \
	( echo "OBJECTS=\\"; \
	  awk '{printf( "\t%-30s%s", $$0, (i++ % 2)?"\\\n":" ")}'; \
	  echo ""; echo "" ) > /tmp/object.tmp
	@gap/replace OBJ gap/Makefile /tmp/object.tmp > /tmp/Makefile
	@mv gap/Makefile gap/Makefile.bak
	@mv /tmp/Makefile gap
	@rm /tmp/object.tmp
	@echo "check 'gap/Makefile',  backup is in 'gap/Makefile.bak'"

## create dependency list for the Makefile ##################################
depend:
	@(cd src ; gcc -MM -CHAT=0 *.c > /tmp/depend.tmp )
	@gap/replace DEP gap/Makefile /tmp/depend.tmp > /tmp/Makefile
	@mv gap/Makefile gap/Makefile.bak
	@mv /tmp/Makefile gap
	@rm /tmp/depend.tmp
	@echo "check 'gap/Makefile',  backup is in 'gap/Makefile.bak'"

## create distribution package ##############################################
src_dist:
	@zoo ah ${SRCDISTFILE}.zoo \
		${PKG_PATH}/anusq/README \
		${PKG_PATH}/anusq/Makefile \
		${PKG_PATH}/anusq/init.g \
		${PKG_PATH}/anusq/bin/Sq.sh \
		`find ${PKG_PATH}/anusq/doc -name "*.ps" -print` \
		`find ${PKG_PATH}/anusq/examples -print` \
		`find ${PKG_PATH}/anusq/gap -print ` \
		`find ${PKG_PATH}/anusq/src -name "*.[ch]" -print` \
		${PKG_PATH}/anusq/src/Makefile
	@zoo PE ${SRCDISTFILE}.zoo

bin_dist:
	@( test \! -r ${PKG_PATH}/anusq/bin.orig || ( echo \
	 "*** WARNING: 'anusq/bin.orig' exists ***";  exit 1 ) )
	( cd ${PKG_PATH}/anusq ; mv bin bin.orig ; mkdir bin ; \
	  cp bin.orig/Sq.sh bin/Sq.sh ; cp bin.orig/Sq-${EXT} bin/Sq )
	@zoo ah ${SRCDISTFILE}-${EXT}.zoo \
		${PKG_PATH}/anusq/bin/Sq \
		${PKG_PATH}/anusq/bin/Sq.sh 
	@zoo PE ${SRCDISTFILE}-${EXT}.zoo
	( cd ${PKG_PATH}/anusq ; rm -rf bin.old ; mv bin bin.old ; \
	  mv bin.orig bin )

#@DEP@#
arith.o : arith.c pres.h sq.h modmem.h 
consis.o : consis.c sq.h modmem.h arith.h pcparith.h 
eapquot.o : eapquot.c pres.h sq.h modmem.h pcparith.h 
mem.o : mem.c pres.h 
modifypres.o : modifypres.c pres.h sq.h modmem.h arith.h 
modmem.o : modmem.c pres.h sq.h pcparith.h 
pcp.o : pcp.c pres.h sq.h 
pcparith.o : pcparith.c pres.h sq.h modmem.h arith.h 
presentation.o : presentation.c pres.h 
sq.o : sq.c sys.h pres.h sq.h arith.h modmem.h pcparith.h veinter.h eapquot.h \
  updatepres.h 
storepc.o : storepc.c pres.h sq.h modmem.h 
system.o : system.c 
updatepres.o : updatepres.c pres.h sq.h modmem.h pcparith.h veinter.h 
veinter.o : veinter.c pres.h sq.h modmem.h 
#@ENDDEP@#
