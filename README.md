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
$ for l in \`ghc-pkg list --user | awk 'NR > 1 {print $0}'\`; do ghc-pkg unregister $l --force; done
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
[INSTALL] semigroups-0.9
[INSTALL] text-0.11.2.3
[INSTALL] hashable-1.2.0.5
[INSTALL] void-0.6
    
$ hbrew install void
( install messages )

$ ghc-pkg list --user
/Users/$USER/.ghc/x86_64-darwin-7.6.2/package.conf.d
   hashable-1.2.0.5
   nats-0.1
   semigroups-0.9
   text-0.11.2.3
   void-0.6

$ hbrew reset

$ ghc-pkg list --user
/Users/$USER/.ghc/x86_64-darwin-7.6.2/package.conf.d

$ hbrew install void --dry-run
[PUSH]        hashable-1.2.0.5-6369bb0e7f86eaf80fdec96a830fe508
[PUSH]        nats-0.1-34f672487bb5ca5f99abffc5b9521800
[PUSH]        semigroups-0.9-2947823d7c0a6064bf9c08818332909a
[PUSH]        text-0.11.2.3-0c1a3e2e69b48d5b34227ad938016982
[PUSH]        void-0.6-eed3f9ea0eec23a6ac3a25c6f7bad927

$ hbrew install void bytestring-0.9.2.1 --dry-run
[PUSH]        nats-0.1-34f672487bb5ca5f99abffc5b9521800
[PUSH]        semigroups-0.9-2947823d7c0a6064bf9c08818332909a
[INSTALL] bytestring-0.9.2.1
[INSTALL] text-0.11.2.3
[INSTALL] hashable-1.2.0.5
[INSTALL] void-0.6

$ hbrew install conduit --dry-run
[PUSH]        hashable-1.2.0.5-6369bb0e7f86eaf80fdec96a830fe508
[PUSH]        nats-0.1-34f672487bb5ca5f99abffc5b9521800
[PUSH]        semigroups-0.9-2947823d7c0a6064bf9c08818332909a
[PUSH]        text-0.11.2.3-0c1a3e2e69b48d5b34227ad938016982
[PUSH]        void-0.6-eed3f9ea0eec23a6ac3a25c6f7bad927
[INSTALL] base-unicode-symbols-0.2.2.4
[INSTALL] transformers-0.3.0.0
[INSTALL] mmorph-1.0.0
[INSTALL] mtl-2.1.2
[INSTALL] transformers-base-0.4.1
[INSTALL] monad-control-0.3.2
[INSTALL] lifted-base-0.2.0.3
[INSTALL] resourcet-0.4.7
[INSTALL] conduit-1.0.4.2

$ hbrew haddock
(generate document index to $HOME/.cabal/hbrew/doc/ghc-7.6.2-x86_64/index.html)
```









