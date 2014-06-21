infer-upstream
==============

Takes a repo name, and looks up the upstream repository. If there is an
upstream repository, writes it to standard out. Otherwise writes nothing.

Usage:

````
> infer-upstream -r scirate3 -u silky
git@github.com:draftable/scirate3.git
````

Another usage:

````
cd scirate3
git remote add upstream `infer-upstream -r scirate3 -u silky`
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
> cd ~/dev
> git clone git@github.com:silky/infer-upstream
> infer-upstream/upstream_everything.sh
...
````

Notes:

  * This script assumes every folder in the ~/dev directory is the clone of a
  Github repository.
  * Github currently limits you to 60 unauthenticated API requests *per hour*;
  this approach uses one API request per folder.


