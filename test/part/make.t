test the maker
  $ part.make index-000.idx
  $ test -f index-000.idx
  $ test -p index-000.idx.socket
  $ part.make index-000.idx
  make: The index (and its socket) index-000.idx already exists.
  Usage: make [OPTION]... ARG
  Try `make --help' for more information.
  [124]
  $ rm index-000.idx
  $ part.make index-000.idx
  make: The index (and its socket) index-000.idx already exists.
  Usage: make [OPTION]... ARG
  Try `make --help' for more information.
  [124]
  $ rm index-000.idx.socket
  $ part.make -vvv index-000.idx
  [DEBUG][persistent]: alloc[0]
  [DEBUG][application]: brk:0000000000000010, allocate 2080 byte(s)
  [DEBUG][application]: brk:0000000000000830
  $ test -f index-000.idx
  $ test -p index-000.idx.socket
  $ rm index-000.idx index-000.idx.socket
  $ part.make $PWD/index-000.idx
  $ rm index-000.idx index-000.idx.socket
  $ part.make $PWD/../index-000.idx
  $ test -f ../index-000.idx
  $ test -p ../index-000.idx.socket
  $ rm ../index-000.idx ../index-000.idx.socket
