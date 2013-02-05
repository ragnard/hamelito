# hamelito

> As a younger, distant cousin of [HAML](http://haml.info/), Hamelito
> looks up to his handsome relative, but has different goals in life.

hamelito is a [Clojure](http://www.clojure.org) library implementing a
very restricted subset of HAML.

The main goal is to allow HAML documents to be used as templates for
[enlive](http://github.com/cgrand/enlive).

## Status

**NOT USABLE. YET.**

**5/2/2013**: 
- Parser works ok.
- Hacky conversion to Hiccup to allow some `HAML->HTML` [tests](https://github.com/ragnard/hamelito/blob/master/test/hamelito/rendering_test.clj).
- Data representions still fluctuating heavily.

 
## Usage

`lein test` and watch lots of garbage scroll by.

## License

Copyright © 2013 Ragnar Dahlen

Distributed under the Eclipse Public License, the same as Clojure.
