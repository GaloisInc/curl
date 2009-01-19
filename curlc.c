/*
 * Haskell FFI friendly wrappers to curl_easy_* functions
 * for setting/getting option values. Could import these into
 * .hs without too much trouble, but calling out to 'typed'
 * versions saves the C compiler from issuing warnings.
 *
 * (c) 2007-2009, Galois, Inc.
 *
 */
#include <curl/curl.h>

int curl_easy_getinfo_long(void *curl, long tg, long *pl)
{
    return curl_easy_getinfo(curl, CURLINFO_LONG+tg, pl);
}

int curl_easy_getinfo_string(void *curl, long tg, char **s)
{
    return curl_easy_getinfo(curl, CURLINFO_STRING+tg, s);
}

int curl_easy_getinfo_double(void *curl, long tg, double *d)
{
    return curl_easy_getinfo(curl, CURLINFO_DOUBLE+tg, d);
}

int curl_easy_getinfo_slist(void *curl, long tg, char ***s)
{
    return curl_easy_getinfo(curl, CURLINFO_SLIST+tg, s);
}


int curl_easy_setopt_long(void *curl, int i, long x)
{ return curl_easy_setopt(curl,i,x); }

int curl_easy_setopt_longlong(void *curl, int i, long long x)
{ return curl_easy_setopt(curl,i,x); }

int curl_easy_setopt_string(void *curl, int i, char *x)
{ return curl_easy_setopt(curl,i,x); }

int curl_easy_setopt_ptr(void *curl, int i, void *x)
{ return curl_easy_setopt(curl,i,x); }

/*
 * Function curl_version_str()
 *
 * Returns the libcurl version number as a "MAJOR.MINOR.PATCH" string.
 *
 * Note: a static string, so no free()ing required (or asked for! :-)
 */
char*
curl_version_str() {
  return LIBCURL_VERSION;
}

/*
 * Function curl_version_num()
 *
 * Returns the libcurl version number in 3-byte format 0xXXYYZZ,
 * representing major,minor and patch levels. Encoded in a (host)
 * 'int' value, making for easy comparisons.
 *
 * See curlver.h for complete story.
 */
int
curl_version_num() {
  return LIBCURL_VERSION_NUM;
}
