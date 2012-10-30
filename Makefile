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
${DIST}/time.jar: ${BUILD}/time/Entry.class
	mkdir -p ${DIST}
	jar cf ${DIST}/time.jar -C $(BUILD) .
	jar uf ${DIST}/time.jar -C $(SRC) .

${BUILD}/time/JITime.class: src/time/JITime.java ${BUILD}/time/Entry.class
	javac -d $(BUILD) -cp $(CP) src/time/O.java src/time/CoreMaps.java

${BUILD}/time/Entry.class: ${BUILD}/time/O.class $(wildcard src/time/*.scala)
	scalac -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`


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
