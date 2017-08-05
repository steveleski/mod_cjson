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
 * Apache mod_cjson module - companion header.
 *
 * Author: Steve Leski - https://github.com/steveleski
 * Date: July 27, 2017
 *
 * This header file is to be #include'd in the user defined shareable library 
 * code. It also exposes some functions to allow that code to interact with
 * mod_cjson. mod_cjson is an Apache module that can invoke a user defined
 * function to process a http request using JSON.
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
 */
#ifndef _APACHE_CJSON_H_INCLUDED
#define _APACHE_CJSON_H_INCLUDED

#include "cJSON.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

enum http_method {GET, POST, PUT, DELETE};

/* 
 * Response header list that can be set in the apache_cjson context object.
 *
 * This is used by the mod_cjson framework to allow user code to set
 * response headers. This comes in handy after a POST where you might
 * want to return a Location header pointing to the newly added resource.
 * 
 * Headers are posted in the opposite order they are added. It's just easier 
 * to code it this way so deal with it. It's not like the order matters all 
 * that much anyway.
 */
typedef struct response_header {
    char *name;
    char *value;
    struct response_header *next;
} response_header_list;

/*
 * List of pointers malloc'ed by the framework that can be freed by mod_cjson
 * after it no longer needs the data.
 */
typedef struct cjson_pool {
    void *value;
    struct cjson_pool *next;
} cjson_pool_t;

/*
 * Context object for the mod_cjson api. It provides a way to pass useful
 * nuggets between Apache and user code. This object gets passed as the first
 * parameter to the user-defined entry point.
 */
typedef struct apache_cjson {
    enum http_method method;  //http method in request
    const char *scheme;       //"http" or "https"
    const char *hostname;     //Host name. Ex. abc.mycompany.com
    const char *port;         //port as a string
    const char *path;         //path
    const char *query_args;   //query args (unparsed)
    int formatted_output;     //Make the JSON response pretty? true/false
    response_header_list *response_headers; //Used internally
    cjson_pool_t *pool;      //memory pool. Also used internally.
} apache_cjson;

//Framework api prototypes.
void cjson_add_response_header(apache_cjson *context, char *name, char *value);
void *cjson_alloc_mem(apache_cjson *context, size_t size);
void cjson_free_mem(apache_cjson *context);

#ifdef __cplusplus
}
#endif

#endif                          /*_APACHE_CJSON_H_INCLUDED */
