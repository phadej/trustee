# trustee

*a tool to automate (and parallelise) some Hackage Trustee tasks*

Read a short use-case tutorial in my [blog post](http://oleg.fi/gists/posts/2018-01-08-haskell-package-qa.html#s:6).

**Disclaimer** the tool is very immature.

---

## Notes:

- `trustee` expectes to find `ghc-7.0.4`, `ghc-7.2.2` up to `ghc-8.2.2` in the `$PATH`.
  Consider using https://launchpad.net/~hvr/+archive/ubuntu/ghc
- You can limit GHC range with `-g`.

---

From a comment https://github.com/feuerbach/tasty/pull/201#issuecomment-356514371 (about `bounds --lower --verify`):

I wouldn't use it in CI in general, and not even for `tasty` (a package with small dependency footprint).  If you want to use it as a library author I'd suggest you do occasional checks, e.g. before releases with more changes.

There are various small unimplemented features I'd like to have (like setting failure exit code), but most importantly: **it's slow**, and I don't know how to fix that (it has to rebuild a lot of stuff). For `tasty` it's not very bad, around one minute CPU time per compiler (clocked with GHC-7.6), given the dependencies are cached. For `tasty-quickcheck` (for two compilers at once):

- without `--verify`: `user    1m53.632s`. But this always succeeds, you have to check manually if there's something suspicious.
- cold cache: `user    9m46.768s`
- warm cache: `user    2m12.192s`

Also I don't know how to find right lower bound otherwise than by doing linear scan from the bottom which is slow for newer compilers, as it have to check many versions. On the other hand, binary search won't do it, as e.g. https://matrix.hackage.haskell.org/package/tagged has weird structure, and there are many packages with similar "not diagonal strip" matrices. One idea would be to try major versions first `==x.y.*` and there is an install plan, do a linear scan on that range (cabal will probably pick greatest in that group, we want least).
