[![Clojars Project](https://img.shields.io/clojars/v/gloss.svg)](https://clojars.org/org.clj-commons/gloss)
[![cljdoc badge](https://cljdoc.org/badge/gloss/gloss)](https://cljdoc.org/d/org.clj-commons/gloss)
[![CircleCI](https://circleci.com/gh/clj-commons/gloss.svg?style=svg)](https://circleci.com/gh/clj-commons/gloss)

Gloss is a byte-format DSL. It can turn complicated byte formats into Clojure data structures, allowing
for easy use of custom network protocols and C libraries.  It can also turn Clojure data structures into
compact byte representations, allowing for efficient use of bandwidth and disk.

Read more about it in [the wiki](https://github.com/clj-commons/gloss/wiki).

## usage

To add Gloss as a dependency:

Leiningen:
```clojure
[org.clj-commons/gloss "0.3.0"]
```

deps.edn:
```clojure
org.clj-commons/gloss {:mvn/version "0.3.0"}
```

## documentation

The full API documentation can be found [here](https://cljdoc.org/d/org.clj-commons/gloss).

## license

Copyright © 2014 Zach Tellman

Distributed under the Eclipse Public License, which is also used by Clojure.
