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
