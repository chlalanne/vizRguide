#! /usr/bin/env sh

grep -o -E --color=never "tt  [a-z.]*" vizRguide.idx | sed 's/tt  //g' > terms.txt

