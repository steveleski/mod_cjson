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
 * Apache mod_cjson module.
 *
 * Author: Steve Leski - https://github.com/steveleski
 * Date: July 27, 2017
 *
 * This Apache module provides capability to build http services using JSON
 * with C.
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
 * IMPORTANT NOTES
 * ===============
 *
 * XXX TO DO XXX
 * =============
 *
 * How do I pass back other success response codes (like 201 CREATED) without
 * causing Apache to choke.
 *
 *
 */
#ifdef __VMS
#define _USE_STD_STAT 1
#endif

#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "http_protocol.h"
#include "http_request.h"
#include "apr_uri.h"
#include "apr_strings.h"
#include "cJSON.h"
#include "apache_cjson.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Call signature to handle the request/response */
typedef int (*cjson_entry_point)(apache_cjson *context, cJSON *request, cJSON **response);

#define CONFIG_MODE_SERVER 1
#define CONFIG_MODE_DIRECTORY 2
#define CONFIG_MODE_COMBO 3 /* Shouldn't ever happen. */

/* Name of the main entry point for the shareable object library */
#define APACHE_HTTPSERVER_ENTRY_POINT "APACHE_CJSON_ENTRY_POINT"

#define BLOCKSIZE 8192

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Data declarations.                                                       */
/*                                                                          */
/* Here are the static cells and structure declarations private to our      */
/* module.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Configuration record.  Used for both per-directory and per-server
 * configuration data.
 *
 * It's perfectly reasonable to have two different structures for the two
 * different environments.  The same command handlers will be called for
 * both, though, so the handlers need to be able to tell them apart.  One
 * possibility is for both structures to start with an int which is 0 for
 * one and 1 for the other.
 *
 * Note that while the per-directory and per-server configuration records are
 * available to most of the module handlers, they should be treated as
 * READ-ONLY by all except the command and merge handlers.  Sometimes handlers
 * are handed a record that applies to the current location by implication or
 * inheritance, and modifying it will change the rules for other locations.
 *
 * This config record contains a reference to a shareable object library
 * that will be called to process a request/response interaction.
 */
typedef struct cjson_cfg_s {
    apr_pool_t *pool;   /* APR memory pool */
    int cmode;          /* Environment to which record applies */
    const char *shrlib_path; /* File path to the share library */
#ifdef _WIN32            /* handle to the share library */
    HMODULE shrlib;
#else
    void *shrlib;
#endif
    cjson_entry_point entry_point; /* entry point to call in the library */
    int isLoaded;       /* Libraries are loaded at first request */
    char *loc;          /* Location to which this record applies. */
} cjson_cfg;

/*
 * Declare ourselves so the configuration routines can find and know us.
 * We'll fill it in at the end of the module.
 */
module AP_MODULE_DECLARE_DATA cjson_module;


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* These routines are strictly internal to this module, and support its     */
/* operation.  They are not referenced by any external portion of the       */
/* server.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Used for trace messages printed in the error log.
 */
static void log_message(char *msg, apr_pool_t *p) {
    apr_file_t *out = NULL;
    apr_file_open_stderr(&out, p);
    apr_file_puts(msg, out);
    apr_file_puts("\n", out);
}


/*
 * Create an empty config object.
 */
static cjson_cfg *cjson_create_cfg(apr_pool_t *p)
{
    cjson_cfg *cfg = (cjson_cfg *)apr_pcalloc(p, sizeof(cjson_cfg));
    cfg->pool = p;
    cfg->cmode = CONFIG_MODE_DIRECTORY;
    cfg->shrlib_path = NULL;
    cfg->shrlib = NULL;
    cfg->entry_point = NULL;
    cfg->isLoaded = 0;
    cfg->loc = NULL;
    return cfg;
}


/*
 * Get a handle to a module local pool (if needed)
 * CURRENTLY NOT USED
 */
static apr_pool_t *local_pool = NULL;
static apr_pool_t *cjson_get_local_pool() {
    if (NULL == local_pool) {
        apr_pool_create_ex(&local_pool, NULL, NULL, NULL);
    }
    return local_pool;
}


/*
 * Open the shareable object that will handle the json processing
 */
static const char *cjson_dlopen(cjson_cfg *cfg, apr_pool_t *tmp_pool)
{
    const char *err_text = NULL;

#ifdef _WIN32
    cfg->shrlib = LoadLibrary(cfg->shrlib_path);
    if (cfg->shrlib == NULL) {
        LPVOID lpMsgBuf;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, 
                      NULL, 
                      GetLastError(), 
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                      (LPTSTR) & lpMsgBuf, 
                      0, 
                      NULL);
        err_text = apr_psprintf(NULL == tmp_pool ? cfg->pool : tmp_pool,
                                "Load of %s failed with %s", 
                                cfg->shrlib_path,
                                lpMsgBuf);
        LocalFree(lpMsgBuf);
    }
#else
    cfg->shrlib = (void *)dlopen(cfg->shrlib_path, RTLD_LAZY);
    if (cfg->shrlib == NULL) {
        char *dlerr = dlerror();
        err_text = apr_psprintf(NULL == tmp_pool ? cfg->pool : tmp_pool,
                                "Load of %s failed with %s", 
                                cfg->shrlib_path, 
                                dlerr);
    }
#endif
    return err_text;
}


/*
 * Load the shareable object that will handle the json processing. This call
 * completes the configuration.
 */
static const char *cjson_load_shrlib(cjson_cfg *cfg, request_rec *r)
{
    void *pfun = NULL;
    const char *err_text = NULL;

    err_text = cjson_dlopen(cfg, r->pool);
    if (err_text != NULL) return err_text;

#ifdef _WIN32
    pfun = (void *)GetProcAddress(cfg->shrlib, APACHE_HTTPSERVER_ENTRY_POINT);
#else
    pfun = (void *)dlsym(cfg->shrlib, APACHE_HTTPSERVER_ENTRY_POINT);
#endif
    if (pfun == NULL) {
        err_text = apr_psprintf(r->pool,
                                "Load of library \"%s\" successful, but failed to find the \"%s\" entry point",
                                cfg->shrlib_path,
                                APACHE_HTTPSERVER_ENTRY_POINT);
        return err_text;
    }

    cfg->entry_point = (cjson_entry_point) pfun;
    cfg->isLoaded = 1;

    return err_text;
}


/*
 * Read the request body and return it as a string
 */
static int cjson_read_request_body(request_rec *r, char **body) {
    long length_to_read, rpos = 0;
    int length_read, rsize;
    int stat;
    char buffer[BLOCKSIZE];

    /* Setup to read the request body */
    stat = ap_setup_client_block(r, REQUEST_CHUNKED_ERROR);
    if (stat != OK) { //Client doesn't contain a body ??
        return stat;
    }

    if (!ap_should_client_block(r)) {
        return REQUEST_NO_BODY;
    }

    /* Allocate buffers */
    length_to_read = r->remaining;
    *body = (char*)apr_pcalloc(r->pool, length_to_read + 1);

    /* Read loop - read the request into a string */
    /* SHOULD THERE BE AN UPPER LIMIT TO THE REQUEST SIZE ???? */
    while ((length_read = ap_get_client_block(r, buffer, BLOCKSIZE)) > 0) {
        if ((rpos + length_read) > length_to_read) {
            rsize = length_to_read - rpos;
        } else {
            rsize = length_read;
        }
        memcpy(*body + rpos, buffer, rsize);
        rpos += rsize;
    }

    return HTTP_OK;
}


/*
 * Set response headers provided by the shareable object library.
 */
static void cjson_set_response_headers(request_rec *r, apache_cjson *context) {
    response_header_list *tmp, *elt = context->response_headers;
    do {
        apr_table_add(r->headers_out, apr_pstrdup(r->pool, elt->name), apr_pstrdup(r->pool, elt->value));
        tmp = elt->next;
        free(elt);
        elt = tmp;
    } while (elt != NULL);
}


/*--------------------------------------------------------------------------*/
/* We prototyped the various syntax for command handlers (routines that     */
/* are called when the configuration parser detects a directive declared    */
/* by our module) earlier.  Now we actually declare a "real" routine that   */
/* will be invoked by the parser when our "real" directive is               */
/* encountered.                                                             */
/*                                                                          */
/* If a command handler encounters a problem processing the directive, it   */
/* signals this fact by returning a non-NULL pointer to a string            */
/* describing the problem.                                                  */
/*                                                                          */
/* The magic return value DECLINE_CMD is used to deal with directives       */
/* that might be declared by multiple modules.  If the command handler      */
/* returns NULL, the directive was processed; if it returns DECLINE_CMD,    */
/* the next module (if any) that declares the directive is given a chance   */
/* at it.  If it returns any other value, it's treated as the text of an    */
/* error message.                                                           */
/*--------------------------------------------------------------------------*/
/*
 * Command handler for the cJSONContentHandler directive.  All we do is set
 * the argument containing the path to the shareable object library.
 */
static const char *cmd_cjson_handler(cmd_parms *cmd, void *config, const char *path)
{
    cjson_cfg *cfg = (cjson_cfg *)config;
    cfg->shrlib_path = apr_pstrdup(cfg->pool, path);

    /* Should I check if path is null and report an error ?? */
    return NULL;
}


/*
 * This function gets called to create a per-directory configuration
 * record.  This will be called for the "default" server environment, and for
 * each directory for which the parser finds any of our directives applicable.
 * If a directory doesn't have any of our directives involved (i.e., they
 * aren't in the .htaccess file, or a <Location>, <Directory>, or related
 * block), this routine will *not* be called - the configuration for the
 * closest ancestor is used.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *cjson_create_dir_config(apr_pool_t *p, char *dirspec)
{
    /* Create an empty config object */
    cjson_cfg *cfg = cjson_create_cfg(p);

    cfg->loc = apr_pstrdup(p, dirspec);
    cfg->cmode = CONFIG_MODE_DIRECTORY;

    return (void *)cfg;
}


/*
 * This function gets called to create a per-server configuration
 * record.  It will always be called for the "default" server.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *cjson_create_server_config(apr_pool_t *p, server_rec *s)
{
    /* Create an empty config object */
    cjson_cfg *cfg = cjson_create_cfg(p);

    cfg->loc = apr_pstrdup(p, s->server_hostname);
    cfg->cmode = CONFIG_MODE_SERVER;

    return (void *)cfg;
}


/*--------------------------------------------------------------------------*
 *                                                                          *
 * Now let's declare routines for each of the callback hooks in order.      *
 * (That's the order in which they're listed in the callback list, *not     *
 * the order in which the server calls them!  See the command_rec           *
 * declaration near the bottom of this file.)  Note that these may be       *
 * called for situations that don't relate primarily to our function - in   *
 * other words, the fixup handler shouldn't assume that the request has     *
 * to do with "cjson" stuff.                                        *
 *                                                                          *
 * With the exception of the content handler, all of our routines will be   *
 * called for each request, unless an earlier handler from another module   *
 * aborted the sequence.                                                    *
 *                                                                          *
 * There are three types of hooks (see include/ap_config.h):                *
 *                                                                          *
 * VOID      : No return code, run all handlers declared by any module      *
 * RUN_FIRST : Run all handlers until one returns something other           *
 *             than DECLINED. Hook runner result is result of last callback *
 * RUN_ALL   : Run all handlers until one returns something other than OK   *
 *             or DECLINED. The hook runner returns that other value. If    *
 *             all hooks run, the hook runner returns OK.                   *
 *                                                                          *
 * Handlers that are declared as "int" can return the following:            *
 *                                                                          *
 *  OK          Handler accepted the request and did its thing with it.     *
 *  DECLINED    Handler took no action.                                     *
 *  HTTP_mumble Handler looked at request and found it wanting.             *
 *                                                                          *
 * See include/httpd.h for a list of HTTP_mumble status codes.  Handlers    *
 * that are not declared as int return a valid pointer, or NULL if they     *
 * DECLINE to handle their phase for that specific request.  Exceptions, if *
 * any, are noted with each routine.                                        *
 *--------------------------------------------------------------------------*/

/*
 * This routine is called after the server finishes the configuration
 * process.  At this point the module may review and adjust its configuration
 * settings in relation to one another and report any problems.  On restart,
 * this routine will be called only once, in the running server process.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 */
static int cjson_post_config(apr_pool_t *pconf, apr_pool_t *plog,
                          apr_pool_t *ptemp, server_rec *s)
{
    ap_log_error(APLOG_MARK, APLOG_INFO, 0, s, "mod_cjson: Initialized");
    return OK;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Now we declare our content handlers, which are invoked when the server   */
/* encounters a document which our module is supposed to have a chance to   */
/* see.  (See mod_mime's SetHandler and AddHandler directives, and the      */
/* mod_info and mod_status examples, for more details.)                     */
/*                                                                          */
/* Since content handlers are dumping data directly into the connection     */
/* (using the r*() routines, such as rputs() and rprintf()) without         */
/* intervention by other parts of the server, they need to make             */
/* sure any accumulated HTTP headers are sent first.  This is done by       */
/* calling send_http_header().  Otherwise, no header will be sent at all,   */
/* and the output sent to the client will actually be HTTP-uncompliant.     */
/*--------------------------------------------------------------------------*/
/*
 * cJSON content handler.  This handler parses the JSON request body, calls
 * the entry point of the shareable object library, and parses the response
 * before passing it back in the response body.
 *
 * This routine gets called for every request, unless another handler earlier
 * in the callback chain has already handled the request. It is up to us to
 * test the request_rec->handler field and see whether we are meant to handle
 * this request.
 *
 * The content handler gets to write directly to the client using calls like
 * ap_rputs() and ap_rprintf()
 *
 * This is a RUN_FIRST hook.
 */
static int cjson_handler(request_rec *r)
{
    int verb = 0;
    cjson_cfg *cfg;
    const char *err_text = NULL;
    char *body;
    cJSON *reqObject = NULL;
    cJSON *respObject = NULL;
    apache_cjson context;
    char *respBuf;
    int stat;

    /* If it's not for us, get out as soon as possible. */
    if (!r->handler || strcmp(r->handler, "cjson-handler")) {
        return DECLINED;
    }

    /* Determine method to pass to content handler */
    switch(r->method_number) {
        case M_GET:
            context.method = GET;
            break;
        case M_POST:
            context.method = POST;
            break;
        case M_PUT:
            context.method = PUT;
            break;
        case M_DELETE:
            context.method = DELETE;
            break;
        default:
            ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_cjson: Unsupported http method %s", r->method);
            return HTTP_METHOD_NOT_ALLOWED;
    }

    /* Get the configuration object for this request path */
    cfg = (cjson_cfg *)ap_get_module_config(r->per_dir_config, &cjson_module);

    /*
     * Set the Content-type header for the response. Note that we do not
     * actually have to send the headers: this is done by the http core.
     */
    ap_set_content_type(r, "application/json");
    
    /* Do I need to do this ? */
    ap_update_mtime(r, r->request_time);
    ap_set_last_modified(r);

    /* Load shareable object library if not already loaded */
    if (!cfg->isLoaded) {
        err_text = cjson_load_shrlib(cfg, r);
        if (err_text != NULL) {
            ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_cjson: %s", err_text);
            return HTTP_INTERNAL_SERVER_ERROR;
        }
    }

    /* Parse request body into a cJSON object */
    stat = cjson_read_request_body(r, &body);
    if (stat == HTTP_OK) {
        if (strcmp(r->content_type, "application/json")) {
            ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, 
                "mod_cjson: Invalid Content-Type header - %s", r->content_type);
            return HTTP_UNSUPPORTED_MEDIA_TYPE;
        }
        reqObject = cJSON_Parse(body);
        if (reqObject == NULL) {
            ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_cjson: Invalid JSON syntax - %s", r->the_request);
            return HTTP_BAD_REQUEST;
        }
    } else {
        if (stat != REQUEST_NO_BODY) return stat;
    }

    /* Fill in the context parameter */
    apr_uri_t uri = r->parsed_uri;
    context.scheme = apr_pstrdup(r->pool, uri.scheme? uri.scheme : ap_http_scheme(r));
    context.hostname = apr_pstrdup(r->pool, r->hostname);
    context.port = apr_pstrdup(r->pool, uri.port_str);
    context.path = apr_pstrdup(r->pool, uri.path);
    context.query_args = apr_pstrdup(r->pool, uri.query);
    context.formatted_output = 0;
    context.response_headers = NULL;
    context.pool = NULL;

    /* Call the entry point in the shareable object library */
    stat = (cfg->entry_point) (&context, reqObject, &respObject);

    /* User code can pass back response headers */
    if (context.response_headers != NULL) {
        cjson_set_response_headers(r, &context);
    }

    /* Convert the cJSON object returned into text and send the response */
    if (respObject) {
        if (context.formatted_output)
            respBuf = cJSON_Print(respObject);
        else
            respBuf = cJSON_PrintUnformatted(respObject);
	    apr_pool_cleanup_register(r->pool, respBuf, (int(*)(void*))free, apr_pool_cleanup_null) ;
        ap_rputs(respBuf, r);
        cJSON_Delete(respObject);
    }

    /* Cleanup */
    if (reqObject) cJSON_Delete(reqObject);
    if (context.pool) cjson_free_mem(&context);

    /*
     * We did what we wanted to do, so tell the rest of the server we
     * succeeded.
     */
    if (stat == HTTP_OK) stat = OK;
    return stat;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Which functions are responsible for which hooks in the server.           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * Each function our module provides to handle a particular hook is
 * specified here.  The functions are registered using
 * ap_hook_foo(name, predecessors, successors, position)
 * where foo is the name of the hook.
 *
 * The args are as follows:
 * name         -> the name of the function to call.
 * predecessors -> a list of modules whose calls to this hook must be
 *                 invoked before this module.
 * successors   -> a list of modules whose calls to this hook must be
 *                 invoked after this module.
 * position     -> The relative position of this module.  One of
 *                 APR_HOOK_FIRST, APR_HOOK_MIDDLE, or APR_HOOK_LAST.
 *                 Most modules will use APR_HOOK_MIDDLE.  If multiple
 *                 modules use the same relative position, Apache will
 *                 determine which to call first.
 *                 If your module relies on another module to run first,
 *                 or another module running after yours, use the
 *                 predecessors and/or successors.
 *
 * The number in brackets indicates the order in which the routine is called
 * during request processing.  Note that not all routines are necessarily
 * called (such as if a resource doesn't have access restrictions).
 * The actual delivery of content to the browser [9] is not handled by
 * a hook; see the handler declarations below.
 */
static void cjson_register_hooks(apr_pool_t *p)
{
    ap_hook_post_config(cjson_post_config, NULL, NULL, APR_HOOK_MIDDLE);
    ap_hook_handler(cjson_handler, NULL, NULL, APR_HOOK_LAST);
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* All of the routines have been declared now.  Here's the list of          */
/* directives specific to our module, and information about where they      */
/* may appear and how the command parser should pass them to us for         */
/* processing.  Note that care must be taken to ensure that there are NO    */
/* collisions of directive names between modules.                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * List of directives specific to our module.
 */
typedef const char *(*command_function_interface) ();

static const command_rec cjson_cmds[] =
{
    AP_INIT_TAKE1("cJSONContentHandler",    /* directive name */
                  (command_function_interface) cmd_cjson_handler, /* config action routine */
                  NULL,         /* argument to include in call */
                  ACCESS_CONF,  /* where available */
                  /* directive description */
                  "CJSON dynamically loaded sharable library that will handle cjson content. - 1 argument (path)"
        ),
    {NULL}
};


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Finally, the list of callback routines and data structures that provide  */
/* the static hooks into our module from the other parts of the server.     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
 * Module definition for configuration.  If a particular callback is not
 * needed, replace its routine name below with the word NULL.
 */
module AP_MODULE_DECLARE_DATA cjson_module = 
{
    STANDARD20_MODULE_STUFF,
    cjson_create_dir_config,    /* per-directory config creator */
    NULL,                       /* dir config merger */
    cjson_create_server_config, /* server config creator */
    NULL,                       /* server config merger */
    cjson_cmds,                 /* command table */
    cjson_register_hooks,       /* set up other request processing hooks */
};

#ifdef __cplusplus
}
#endif
