/*
 * Copyright (c) 2017 Steve Leski and contributors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */


/*
 * Apache mod_cjson module - companion library.
 *
 * Author: Steve Leski - https://github.com/steveleski
 * Date: July 27, 2017
 *
 * This code exposes some functions to allow the user defined shareable library
 * to interact with mod_cjson. mod_cjson is an Apache module that can invoke a 
 * user defined function to process a http request using JSON.
 *
 * To use mod_cjson, set up a <Location> block in your configuration file like 
 * so:
 *
 *    LoadModule cjson_module modules/mod_cjson.so 
 *    <IfModule mod_cjson.c>
 *       <Location /example>
 *          SetHandler cjson-handler
 *          cJSONContentHandler /usr/lib/example/user_code.so
 *       </Location>
 *       ... other location directives
 *    </IfModule>
 *
 * user_code is a shareable library that exposes a single function call with the
 * signature:
 *     int APACHE_CJSON_ENTRY_POINT(apache_cjson *context, cJSON *request, cJSON **response)
 * The request and response objects use cJSON https://github.com/DaveGamble/cJSON
 * to marshall/unmarshall the http request and response bodies.
 * 
 * 
 * IMPORTANT NOTES
 * ===============
 *
 * XXX TO DO XXX
 * =============
 *
 * Maybe I need to use a different license?
 */

#include "apache_cjson.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Add headers to be passed back in the response
 */
void cjson_add_response_header(apache_cjson *context, char *name, char *value) {
    response_header_list *tmp = (response_header_list*)malloc(sizeof(response_header_list));
    if (context->response_headers != NULL) {
        tmp->next = context->response_headers;
    } else {
        tmp->next = NULL;
    }
    tmp->name = name;
    tmp->value  = value;
    context->response_headers = tmp;

    return;
}


/*
 * Allocate memory from a pool. mod_cjson will free this memory
 * automatically after a request/response completes.
 */
void *cjson_alloc_mem(apache_cjson *context, size_t size) {
    cjson_pool_t *tmp = (cjson_pool_t*)malloc(sizeof(cjson_pool_t));
    if (context->pool != NULL) {
        tmp->next = context->pool;
    } else {
        tmp->next = NULL;
    }
    context->pool = tmp;
    tmp->value = (void *)malloc(size);

    return tmp->value;
}


/*
 * Deallocate all memory allocated from the pool. 
 *
 * This should only be called from test code. It will automatically be
 * called by mod_cjson to clean up after a request/response completes.
 */
void cjson_free_mem(apache_cjson *context) {
    cjson_pool_t *tmp, *pool = context->pool;
    do {
        tmp = pool->next;
        free(pool);
        pool = tmp;
    } while  (pool != NULL);
    context->pool = NULL;
}

#ifdef __cplusplus
}
#endif
