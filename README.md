A Haskell binding to libcurl.
------


### Why forked?

This is a fork repo from [haskell curl binding](https://github.com/GaloisInc/curl)

It has been maintained for very long time ( 14yrs ago for the first commit ) and well develped.

Unfortunatedly , it is not in active development anymore, here is a plan I'm tring to keep up

* replace build tool chain with `Stack`
* house cleaning to make internal implmentation consistant
** ie. `curlGet` will use HTTPS with insecure seeting ,while `curlPost` isn't.
* keep up to CURL version 7.81

