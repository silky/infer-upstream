infer-upstream
==============

Takes a repo name, and looks up the upstream repository. If there is an
upstream repository, writes it to standard out. Otherwise writes nothing.

Installation

This package is available on [hackage](http://hackage.haskell.org/package/infer-upstream)
so can be installed with

````
cabal install infer-upstream
````

Alternatively, you can clone this repo and install with cabal

````
git clone https://github.com/silky/infer-upstream.git
cd infer-upstream
cabal install
````

Usage:

````
noon@dev> infer-upstream -r scirate3 -u silky
git@github.com:draftable/scirate3.git
````

Another usage:

````
noon@dev> cd scirate3
noon@dev> git remote add upstream `infer-upstream --using-cwd`
````

A more interesting usage (and the reason I wrote this) is to use the
`upstream_everything.sh` script.  It performs the following task.

For all folders in a given directory:

  * go into each one,
  * if it is a github repo,
  * look up the upstream repo,
  * if we find it,
  * set it as a new remote.

Usage:

````
noon@~> cd dev
noon@dev> git clone git@github.com:silky/infer-upstream
noon@dev> infer-upstream/upstream_everything.sh
...
````

With `upstream` set on your repos, you can then run `fetch_upstreams.sh`, if
you like, which will bring down any incoming changes and give print out a
short summary.

Example:

````
noon@dev>infer-upstream/fetch_upstreams.sh 
fetching upstream for Javascript-Voronoi ...
 1 file changed, 2 insertions(+), 2 deletions(-)
````

Notes:

  * This script assumes every folder in the ~/dev directory is the clone of a
  Github repository.
  * Github currently limits you to 60 unauthenticated API requests *per hour*;
  this approach uses one API request per folder.


