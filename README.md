infer-upsteam
=============

Takes a directory, and looks up the upstream repository. When found, it
creates this as a remote in the given repository.

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
