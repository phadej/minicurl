#ifndef HS_MINICURL_H
#define HS_MINICURL_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <curl/curl.h>

/*************************************************************************
 * declarations
 *************************************************************************/

static inline int hs_minicurl_global_init();
static inline void hs_minicurl_global_cleanup();

static inline CURL *hs_minicurl_init();
static inline void hs_minicurl_cleanup(CURL *curl);

static inline int hs_minicurl_perform(CURL *curl, const char *url, uint8_t *ptr, size_t size);
static inline int hs_minicurl_response_code(CURL *curl);

/*************************************************************************
 * implementations
 *************************************************************************/

static inline int hs_minicurl_global_init() {
    return curl_global_init(CURL_GLOBAL_ALL);
}

static inline void hs_minicurl_global_cleanup() {
    return curl_global_cleanup();
}

static inline CURL *hs_minicurl_init() {
    CURL *curl = curl_easy_init();
    if (curl == NULL) return NULL;

    /* Set common options */
    curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1L);
    curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1L);
    /* CURLcode curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L); */

    return curl;
}

static inline void hs_minicurl_cleanup(CURL *curl) {
    curl_easy_cleanup(curl);
}

struct hs_minicurl_buffer {
    uint8_t *data;
    size_t off;
    size_t len;
};

static inline size_t write_data(uint8_t *read_buf, size_t always_one, size_t n, struct hs_minicurl_buffer *write_buf) {
    /* printf("write_data %p %lu %lu %p %lu %lu\n", read_buf, always_one, n, write_buf->data, write_buf->off, write_buf->len); */

    if (write_buf->off >= write_buf->len) {
        return 0;
    }

    size_t p = write_buf->len - write_buf->off;
    size_t m = n > p ? p : n; /* MIN(n, p) */

    memcpy(write_buf->data + write_buf->off, read_buf, m);
    write_buf->off += m;

    return m;
}

static inline int hs_minicurl_perform(CURL *curl, const char *url, uint8_t *ptr, size_t size) {
    CURLcode res = CURLE_OK;

    if (curl == NULL) return CURLE_FAILED_INIT;
  
    struct hs_minicurl_buffer buf = { .data = ptr, .off = 0, .len = size };

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buf);

    return curl_easy_perform(curl);
}

static inline int hs_minicurl_response_code(CURL *curl) {
    long codep = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &codep);
    return codep;
}

#endif
