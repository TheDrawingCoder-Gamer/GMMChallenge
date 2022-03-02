# Haskell: cligmm

This is the haskell flavor of cligmm. 

This app is not fully stable; it errors ungracefully, however it's much more "safe"
than the C flavor as it won't innappropiately access memory. 

This app is also not a fully sane implementation. Functional languages like haskell aren't
really meant for heavy IO work, which this app is. OOP/Procedural apps get the job done much better.

This app has all of the features mentioned in the top level readme.

It also adds: 
- The `--ignore-deps` flag, which skips install of dependencies. Note that this must come after the normal arguments or it will fail to parse.