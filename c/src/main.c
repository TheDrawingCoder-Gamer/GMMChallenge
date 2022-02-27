#include <stdio.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include "json.h"

#define STRING_EQUAL(s1, s2) strcmp(s1, s2) == 0
struct memory {
   char *response;
   size_t size;
 };
 
static size_t cb(void *data, size_t size, size_t nmemb, void *userp)
 {
   size_t realsize = size * nmemb;
   struct memory *mem = (struct memory *)userp;
 
   char *ptr = realloc(mem->response, mem->size + realsize + 1);
   if(ptr == NULL)
     return 0;  /* out of memory! */
 
   mem->response = ptr;
   memcpy(&(mem->response[mem->size]), data, realsize);
   mem->size += realsize;
   mem->response[mem->size] = 0;
 
   return realsize;
 }
void setup_curl(CURL *curl, const char* url, struct memory* chunk) {
  curl_easy_setopt(curl, CURLOPT_URL, url);
  // don't cache
  // curl_easy_setopt(curl, CURLOPT_ALTSVC, "altsvc.txt");
  curl_easy_setopt(curl, CURLOPT_ALTSVC_CTRL, (long) CURLALTSVC_H1|CURLALTSVC_H2|CURLALTSVC_H3);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, cb);

  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)chunk);
}
const char* fetch_path(char* path) {
  FILE* file = fopen("gtag_path.txt", "r");
  if (file == NULL) {
    printf("error: unknown gtag path\nrun %s setup [gtag path]");
    exit(1);
    return NULL;
  }
  fgets(path, 1000, file);
  fclose(file);
  // i want death
  // TODO: this is such a shitty way of reading it
  // make it so it doesn't require 1000 bytes :sob:
  return path;
}
const char* mangle_name(char* name) {
  for (int i = 0; i < strlen(name); i++) {
    char c = name[i];
    if ((c >= 'a' && c <= 'z') || c == '-') {
      name[i] = c;
    } else if (c >= 'A' && c <= 'Z') {
      name[i] = tolower(c);
    } else if (c == '\0') {
      name[i] = '\0';
      break;
    } else if (c == ' ') {
      name[i] = '-';
    } else {
      name[i] = c;
    }
  }
  
  return name;
}
// modname expected to be mangled
struct json_object* find_mod(array_list* modlist, const char* modname) {
  for (int i = 0; i < modlist->length; i++) {
    json_object* mod = modlist->array[i];
    const char* name = mangle_name((char *) json_object_get_string(json_object_object_get(mod, "name")));
    if (STRING_EQUAL(name, modname)) 
      return mod;
  }
  return NULL;
} 
int main(int argc, char *argv[])
{
    if (argc < 2) {
        // TODO: help message
        // 22: invalid argument
        printf("usage: %s [arg]\n", argv[0]);
        return 22;
    }
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    struct memory chunk = {0};
    if (curl) {
        setup_curl(curl, "https://raw.githubusercontent.com/DeadlyKitten/MonkeModInfo/master/modinfo.json", &chunk);
        
        

        

        res = curl_easy_perform(curl);

        if (res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
        curl_easy_cleanup(curl);
        char* cmdname = argv[1];
        json_object* modinforaw = json_tokener_parse(chunk.response);
        
        if (STRING_EQUAL(cmdname,"list")) {
          printf("modinfos:\n");
          
          array_list* modinfo = json_object_get_array(modinforaw);
          for (int i = 0; i < modinfo->length; i++) {
            json_object* obj = (struct json_object*) modinfo->array[i];
            const char* name = mangle_name(json_object_get_string(json_object_object_get(obj, "name")));
            const char* version = json_object_get_string(json_object_object_get(obj, "version"));
            printf("%s %s\n", name, version);
          }
        } else if (STRING_EQUAL(cmdname, "setup")) {
          if (argc < 3) {
            printf("usage: %s setup [path to gtag]\n", argv[0]);
            return 22;
          }
          // write only
          FILE* file = fopen("gtag_path.txt", "w");
          fputs(argv[2], file);
          fclose(file);
        } else if (STRING_EQUAL(cmdname, "install")) {
          if (argc < 3) {
            printf("usage: %s install [mod name]\n", argv[0]);
            return 22;
          }
          char* modname = malloc(strlen(argv[2]) + 1);
          strcpy(modname, argv[2]);
          mangle_name(modname);
          array_list* modinfo = json_object_get_array(modinforaw);
          json_object* mod = find_mod(modinfo, modname);
          
        }
    } else {
        // idfk
        return 1;
    }
    
    
    return 0;
}

#undef STRING_EQUAL