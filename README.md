hbrew
=====

   cabal-install wrapper inspired by hellno(https://github.com/db81/hellno).

### different from hellno

* use prefix to install.(if link, program which use share directory(ex. pandoc, haddock) not work.)
* more powerful dependency solver.(check descendant.)

require
----

* ghc/ghc-pkg/haddock
* cabal
* hscolour

installation
----

1. install binary

   ```bash
   $ git clone https://github.com/philopon/hbrew.git
   $ cd hbrew
   $ cabal configure
   $ cabal build
   $ cabal install
   ```

2. cleaning haskell package
   
   ```bash
   rm -r $HOME/.ghc/* # or backup these folder
   ghc-pkg recache --user
   ```

usage
----

hbrew COMMAND [Args]

### COMMAND

* install: install package
* setup: install package with --only-dependences
* reset: pull all hbrew packages
* haddock: generate haddock
         
### OPTIONS

* --dry-run     dry-run

example
----

```bash
$ ghc-pkg list --user
/Users/$USER/.ghc/x86_64-darwin-7.6.2/package.conf.d

$ hbrew install void --dry-run
[INSTALL] nats-0.1
[INSTALL] semigroups-0.9.1
[INSTALL] text-0.11.2.3
[INSTALL] hashable-1.2.0.5
[INSTALL] void-0.6
    
$ hbrew install void
( install messages )

$ ghc-pkg list --user  ## package installed
/Users/$USER/.ghc/x86_64-darwin-7.6.2/package.conf.d
   hashable-1.2.0.5
   nats-0.1
   semigroups-0.9.1
   text-0.11.2.3
   void-0.6

$ hbrew reset

$ ghc-pkg list --user  ## package removed
/Users/$USER/.ghc/x86_64-darwin-7.6.2/package.conf.d

$ hbrew install void --dry-run ## reuse packages
[PUSH]    hashable-1.2.0.5-6369bb0e7f86eaf80fdec96a830fe508
[PUSH]    nats-0.1-34f672487bb5ca5f99abffc5b9521800
[PUSH]    semigroups-0.9.1-0befef5b7b43bd4847b39dbb41d4235e
[PUSH]    text-0.11.2.3-0c1a3e2e69b48d5b34227ad938016982
[PUSH]    void-0.6-6145dc21c21b7b017d1effb01b56359c

$ hbrew install void bytestring-0.9.2.1 --dry-run ## reuse if it can
[PUSH]    nats-0.1-34f672487bb5ca5f99abffc5b9521800
[PUSH]    semigroups-0.9.1-0befef5b7b43bd4847b39dbb41d4235e
[INSTALL] bytestring-0.9.2.1
[INSTALL] text-0.11.2.3
[INSTALL] hashable-1.2.0.5
[INSTALL] void-0.6

$ hbrew remove nats # remove package and its descendant.
nats-0.1-34f672487bb5ca5f99abffc5b9521800
semigroups-0.9.1-0befef5b7b43bd4847b39dbb41d4235e
void-0.6-6145dc21c21b7b017d1effb01b56359c
delete 3 packages. continue? (y/n)

$ hbrew haddock # generate document index to $HOME/.cabal/hbrew/doc/ghc-7.6.2-x86_64/index.html
```









