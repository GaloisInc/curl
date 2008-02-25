AC_INIT([Haskell curl package], [1.1], [dons@galois.com], [curl])

AC_TRY_CPP([#include <curl/curl.h>],,[no_curl=yes])

# Build the package if we found curl stuff
if test "$no_curl" = yes; then
  AC_MSG_FAILURE([curl libraries not found, so curl package cannot be built])
else
  CURL_BUILD_PACKAGE=yes
  BUILD_PACKAGE_BOOL=True
fi
AC_SUBST([CURL_BUILD_PACKAGE])
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([curl.buildinfo])
AC_OUTPUT
