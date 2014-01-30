# sem-logic

Implementing logical nlp techniques, most notably DRS. This is early work in progress. Look at the unit-tests to see what is extractable. There is no support for temporal information at the moment.
Incorporates lexical lookup in wiktionary or wikipedia.


## Usage

We use the Berkeley parser to create the syntax-tree of sentences. Have a look at [README.sh](parser-model/README.sh) and execute it if you have a bash available or follow the steps by hand. All parser data is supposed to be in `parser-model`.
Currently the DRS rules are only for English.


Recommended for exploration on the (Insta)REPL is [LightTable](http://www.chris-granger.com/lighttable/)

## License

Copyright © 2013 Christian Weilbach

Distributed under the Affero General Public License either version 3.0 or (at
your option) any later version.
