#include <stdio.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include "json.h"
#include <zip.h>
#include <sys/stat.h>
#include <unistd.h>

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
  // i'm going to kill someone
  // it didn't work because redirect
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION,1);
}
void setup_saved_curl(CURL *curl, const char* url, FILE* file) {
  curl_easy_setopt(curl, CURLOPT_URL, url);
  // don't cache
  // curl_easy_setopt(curl, CURLOPT_ALTSVC, "altsvc.txt");
  curl_easy_setopt(curl, CURLOPT_ALTSVC_CTRL, (long) CURLALTSVC_H1|CURLALTSVC_H2|CURLALTSVC_H3);

  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)file);
  // i'm tired
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, fwrite);
  // i'm going to kill someone
  // it didn't work because redirect
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION,1);
}
const char* fetch_path(char* path) {
  FILE* file = fopen("gtag_path.txt", "r");
  if (file == NULL) {
    printf("error: unknown gtag path\nrun cligmm setup [gtag path]");
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

const char * path_directory(const char* path) {
  char * path2 = malloc(strlen(path) + 1);
  strcpy(path2, path);
  int i = strlen(path) - 1;
  while (path[i] != '/') {
    i--;
  }
  path2[i] = '\0';
  return (const char*) path2;
}
void ensure_has_folder(const char* path) {
  
  if (access(path, F_OK) == 0) {
    return;
  } else {
    const char* parent = path_directory(path);
    ensure_has_folder(parent);
    printf("Creating directory %s\n", path);
    mkdir(path, ACCESSPERMS);
    free((void *) parent);
    
  }
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

        if (res != CURLE_OK) {
          fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
          return 1;
        }
            
        
        char* cmdname = argv[1];
        json_object* modinforaw = json_tokener_parse(chunk.response);
        curl_easy_cleanup(curl);
        if (STRING_EQUAL(cmdname,"list")) {
          printf("modinfos:\n");
          
          array_list* modinfo = json_object_get_array(modinforaw);
          for (int i = 0; i < modinfo->length; i++) {
            json_object* obj = (struct json_object*) modinfo->array[i];
            const char* name = mangle_name((char *) json_object_get_string(json_object_object_get(obj, "name")));
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
          const char* version = json_object_get_string(json_object_object_get(mod, "version"));
          const char* author = json_object_get_string(json_object_object_get(mod, "author"));
          printf("Installing %s:\n    version: %s\n    author: %s\nProceed? (y/n)", modname, version, author);
          char response = getc(stdin);
          printf("\n");
          if (response != 'y') {
            printf("denied... aborting\n");
            return 0;
          }
          free(modname);
          const char* url = json_object_get_string(json_object_object_get(mod, "download_url"));
          CURL* zipcurl = curl_easy_init();
          char path[1000]; 
          fetch_path(path);
          strcat(path, "temp.zip");

          FILE* zipfile = fopen(path, "wb");
          setup_saved_curl(zipcurl, url, zipfile);
          
          // reusing this... i'm tired
          res = curl_easy_perform(zipcurl);
          // close it AFTER it's been saved
          fclose(zipfile);
          if (res != CURLE_OK) {  
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
            return 1;
          }
          
         

          int error = 0;
          zip_t* archive = zip_open(path, ZIP_RDONLY, &error);
          if (error != 0) {
            printf("failed: %d\n", error);
            return 1;
          }
          int numentries = zip_get_num_entries(archive, 0);
          for (int i = 0; i < numentries; i++) {
            zip_file_t* entry = zip_fopen_index(archive, i, 0);
            zip_stat_t entryinfo = {0};
            zip_stat_init(&entryinfo);
            zip_stat_index(archive, i, 0, &entryinfo);
            if ((entryinfo.valid & ZIP_STAT_NAME) == 0 || (entryinfo.valid & ZIP_STAT_SIZE) == 0) {
              printf("invalid entry");
              return 1;
            }
            char path2[1000];
            fetch_path(path2);
            strcat(path2, entryinfo.name);
            if (entryinfo.size == 0) {
              // directory
              if (access(path2, F_OK) != 0) {
                printf("Creating directory %s\n", entryinfo.name);
                ensure_has_folder(path2);
                
              }
              continue;
            }
            char* directory = (char*) path_directory(path2);
            ensure_has_folder(directory);
            free(directory);
            printf("Saving %s\n", entryinfo.name);
            // this shit better free when it loops
            
            // this will crash somewhere. too bad!
            void* buffer = malloc(entryinfo.size);
            // pray
            zip_fread(entry, buffer, entryinfo.size);
            FILE* savedfile = fopen(path2, "wb");
            fwrite((const void*) buffer, 1, entryinfo.size, savedfile);
            fclose(savedfile);
            free(buffer);
          }
          // cleanup
          unlink(path);
          printf("Done!\n");
          return 0;
        }
        
    } else {
        // idfk
        return 1;
    }
    
    
    return 0;
}

#undef STRING_EQUAL