# -- VARIABLES --
# (locations)
SRC=src
TEST_SRC=test/src
LIB=etc
BUILD=bin
TEST_BUILD=test/bin
DIST=dist
TMP=tmp
# (classpath)
#JAVANLP=${JAVANLP_HOME}/projects/core/classes:${JAVANLP_HOME}/projects/research/classes
JAVANLP=${LIB}/stanford-corenlp-2012-07-09.jar
CP=${LIB}/lib.jar:${LIB}/scala-compiler.jar:${LIB}/scala-library.jar:${LIB}/postgresql.jar:${JAVANLP}:${LIB}/xom.jar:${LIB}/joda-time.jar:${LIB}/joda-convert.jar


# -- PROGRAM --
${DIST}/time-all.jar:
	cp ${DIST}/time.jar ${DIST}/time-all.jar
	#((scala-library))
	rm -rf ${TMP}/scala-library
	unzip ${SCALA_HOME}/lib/scala-library.jar -d ${TMP}/scala-library > /dev/null
	rm -r ${TMP}/scala-library/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/scala-library/ .
	rm -rf ${TMP}/scala-library
	#((scala-compiler))
	rm -rf ${TMP}/scala-compiler
	unzip ${SCALA_HOME}/lib/scala-compiler.jar -d ${TMP}/scala-compiler > /dev/null
	rm -r ${TMP}/scala-compiler/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/scala-compiler/ .
	rm -rf ${TMP}/scala-compiler
	#((joda-time))
	rm -rf ${TMP}/joda-time
	unzip ${LIB}/joda-time.jar -d ${TMP}/joda-time > /dev/null
	rm -r ${TMP}/joda-time/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/joda-time/ .
	rm -rf ${TMP}/joda-time
	#((joda-convert))
	rm -rf ${TMP}/joda-convert
	unzip ${LIB}/joda-convert.jar -d ${TMP}/joda-convert > /dev/null
	rm -r ${TMP}/joda-convert/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/joda-convert/ .
	rm -rf ${TMP}/joda-convert
	#((xom))
	rm -rf ${TMP}/xom
	unzip ${LIB}/xom.jar -d ${TMP}/xom > /dev/null
	rm -r ${TMP}/xom/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/xom/ .
	rm -rf ${TMP}/xom
	#((corenlp))
	rm -rf ${TMP}/stanford-corenlp-2012-07-09
	unzip ${LIB}/stanford-corenlp-2012-07-09.jar -d ${TMP}/stanford-corenlp-2012-07-09 > /dev/null
	rm -r ${TMP}/stanford-corenlp-2012-07-09/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/stanford-corenlp-2012-07-09/ .
	rm -rf ${TMP}/stanford-corenlp-2012-07-09
	#((jeli))
	rm -rf ${TMP}/lib
	unzip ${LIB}/lib.jar -d ${TMP}/lib > /dev/null
	rm -r ${TMP}/lib/META-INF
	jar uf ${DIST}/time-all.jar -C ${TMP}/lib/ .
	rm -rf ${TMP}/lib

${DIST}/time.jar: ${BUILD}/time/Entry.class
	mkdir -p ${DIST}
	jar cf ${DIST}/time.jar -C $(BUILD) .
	jar uf ${DIST}/time.jar -C $(SRC) .

${BUILD}/time/JITime.class: src/time/JITime.java ${BUILD}/time/Entry.class
	javac -d $(BUILD) -cp $(CP) src/time/O.java src/time/CoreMaps.java

${BUILD}/time/Entry.class: ${BUILD}/time/O.class $(wildcard src/time/*.scala)
	${SCALA_HOME}/bin/scalac -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`


${BUILD}/time/O.class: src/time/O.java src/time/CoreMaps.java
	mkdir -p ${BUILD}
	javac -d $(BUILD) -cp $(CP) src/time/O.java src/time/CoreMaps.java

# -- TESTING --
${DIST}/test.jar: $(wildcard test/src/time/*.java) $(wildcard test/src/time/*.scala) ${DIST}/time.jar
	mkdir -p ${TEST_BUILD}
	mkdir -p ${DIST}
	etc/fsc -deprecation -d ${TEST_BUILD} -cp ${CP}:$(DIST)/time.jar:$(LIB)/scalatest.jar `find $(TEST_SRC) -name "*.scala"` `find ${TEST_SRC} -name "*.java"`
	jar cf ${DIST}/test.jar -C $(TEST_BUILD) .
	jar uf ${DIST}/test.jar -C $(TEST_SRC) .
	

# -- TARGETS --
default: ${DIST}/time.jar 
test: ${DIST}/test.jar

clean:
	rm -rf ${BUILD}
	rm -rf ${TEST_BUILD}
	rm ${DIST}/time.jar
	rm -f java.hprof.txt
