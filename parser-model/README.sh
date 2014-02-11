#!/bin/bash
# This directory is for the model file used to treebank-parsing.
#
# You can download the file from:
# http://opennlp.sourceforge.net/models-1.5/

# OpenNLP data
wget http://opennlp.sourceforge.net/models-1.5/en-ner-person.bin
wget http://opennlp.sourceforge.net/models-1.5/en-parser-chunking.bin
wget http://opennlp.sourceforge.net/models-1.5/en-token.bin
wget http://opennlp.sourceforge.net/models-1.5/en-sent.bin

# Berkeley parser
# http://nlp.cs.berkeley.edu/
wget http://berkeleyparser.googlecode.com/files/BerkeleyParser-1.7.jar
# add it to the local maven repository
mvn install:install-file -Dfile=BerkeleyParser-1.7.jar -DgroupId=edu.berkeley.nlp -DartifactId=parser -Dversion=1.7 -Dpackaging=jar
# English parser data
wget http://berkeleyparser.googlecode.com/files/eng_sm6.gr


# WordNet
wget http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz
tar xfzpv wn3.1.dict.tar.gz
