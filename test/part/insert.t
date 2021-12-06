tests on part.insert
  $ part.make rwx.idx
  $ part.insert rwx.idx test 42
  $ part.insert rwx.idx test 42
  insert: "test" already exists into rwx.idx.
  [1]
  $ part.insert idx.idx test 42
  insert: idx.idx (or its socket) does not exist.
  Usage: insert [OPTION]... ARG ARG ARG
  Try `insert --help' for more information.
  [124]
  $ rm rwx.idx.socket rwx.idx-truncate.socket
  $ part.insert rwx.idx test 42
  insert: rwx.idx (or its socket) does not exist.
  Usage: insert [OPTION]... ARG ARG ARG
  Try `insert --help' for more information.
  [124]
  $ rm rwx.idx
  $ part.insert rwx.idx test 42
  insert: rwx.idx (or its socket) does not exist.
  Usage: insert [OPTION]... ARG ARG ARG
  Try `insert --help' for more information.
  [124]
  $ part.make rwx.idx
  $ part.find rwx.idx test
  find: "test" does not exists.
  [1]
  $ part.insert rwx.idx test 42
  $ part.find rwx.idx test
  42
