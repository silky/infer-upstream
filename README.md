infer-upstream
==============

Takes a repo name, and looks up the upstream repository. If there is an
upstream repository, writes it to standard out. Otherwise writes nothing.

Usage:

````
> infer-upstream scirate3 -u silky
git@github.com:draftable/scirate3.git
````

Another usage

````
cd scirate3
git remote add upstream `infer-upstream scirate3 -u silky`
````
