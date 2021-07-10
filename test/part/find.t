tests on part.find
  $ part.make index.idx
  $ part.find index.idx test
  find: "test" does not exists.
  [1]
  $ part.find not-found.idx test
  find: not-found.idx does not exist.
  Usage: find [OPTION]... ARG ARG
  Try `find --help' for more information.
  [124]
  $ part.find --fmt "%" index.idx test
  find: option `--fmt': Invalid format: "%"
  Usage: find [OPTION]... ARG ARG
  Try `find --help' for more information.
  [124]
  $ part.find --fmt "%s" index.idx test
  find: option `--fmt': Invalid format: "%s"
  Usage: find [OPTION]... ARG ARG
  Try `find --help' for more information.
  [124]
  $ part.find --fmt "%d" index.idx test
  find: "test" does not exists.
  [1]
  $ part.find --fmt "%x" index.idx test
  find: "test" does not exists.
  [1]
  $ part.insert index.idx test 42
  $ part.find index.idx test
  42
  $ part.find index.idx --fmt "%x" test
  2a
