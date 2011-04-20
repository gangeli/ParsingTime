SRC=src
TEST_SRC=test/src
LIB=etc
BUILD=bin
TEST_BUILD=test/bin
DIST=dist
TMP=tmp

JAVANLP=${JAVANLP_HOME}/projects/core/classes:${JAVANLP_HOME}/projects/research/classes
CP=${LIB}/lib.jar:${LIB}/scala-library.jar:${LIB}/scala-compiler.jar:${LIB}/postgresql.jar:${JAVANLP}:${LIB}/jdom.jar:${LIB}/joda-time.jar

default: dist

${DIST}/time.jar: $(wildcard src/time/*.java)	$(wildcard src/time/*.scala)
	mkdir -p ${BUILD}
	mkdir -p ${DIST}
	javac -d $(BUILD) -cp $(CP) `find $(SRC) -name "*.java"`
	fsc -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`
	jar cf ${DIST}/time.jar -C $(BUILD) .
	jar uf ${DIST}/time.jar -C $(SRC) .

${DIST}/test.jar: $(wildcard test/src/time/*.java) $(wildcard test/src/time/*.scala) ${DIST}/time.jar
	mkdir -p ${TEST_BUILD}
	mkdir -p ${DIST}
#	javac -d $(TEST_BUILD) -cp $(CP):$(LIB)/scalatest.jar `find $(TEST_SRC) -name "*.java"`
	fsc -deprecation -d ${TEST_BUILD} -cp ${CP}:$(DIST)/time.jar:$(LIB)/scalatest.jar `find $(TEST_SRC) -name "*.scala"` `find ${TEST_SRC} -name "*.java"`
	jar cf ${DIST}/test.jar -C $(TEST_BUILD) .
	jar uf ${DIST}/test.jar -C $(TEST_SRC) .
	

dist: ${DIST}/time.jar ${DIST}/test.jar

clean:
#	rm -rf ${TMP}
	rm -rf ${BUILD}
	rm -rf ${TEST_BUILD}
	rm -rf ${DIST}
	rm -f java.hprof.txt
