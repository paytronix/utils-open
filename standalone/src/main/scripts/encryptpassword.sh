#! /bin/bash

java -classpath "$(find lib -maxdepth 1 -name \*.jar -print0 | tr "\0" ":" | sed -e 's,:$,,')" com.paytronix.utils.standalone.EncryptDatabasePassword "$@"
