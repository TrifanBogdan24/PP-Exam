SCALA_ARCHIVE = ~/Downloads/Exam-Scala.zip
HASKELL_ARCHIVE = ~/Downloads/Exam-Haskell.zip


default-code:
	rm -rf exam-haskell exam-scala
	unzip skel.zip
	echo "Write your solution in the following files:"
	echo "skel/exam-haskell/Exam.hs"
	echo "skel/exam-scala/src/main/Exam.scala"


open-in-IDEs:
	code exam-haskell ; \
	intellij-idea-ultimate exam-scala/ &> /dev/null &


test:
	cd exam-scala ; \
	sbt test ; \
	cd ../exam-haskell ; \
	runhaskell test.hs ; \
	cd ../

archive:
	rm -rf $(SCALA_ARCHIVE) $(HASKELL_ARCHIVE)
	zip $(SCALA_ARCHIVE) *
	zip $(HASKELL_ARCHIVE) *

