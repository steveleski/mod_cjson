# mod_cjson
Apache module for JSON http web services.

## Table of contents
* [License](#license)
* [Usage](#usage)
* [Welcome to mod_cjson](#welcome-to-mod_cjson)
* [Building](#building)
* [Usage](#usage)
* [Writing a Service](#writing-a-service)
* [Caveats](#caveats)
* [Enjoy mod_cjson!](#enjoy-mod_cjson)

## License

MIT License

>  Copyright (c) 2017 Steve Leski and contributors
>
>  Permission is hereby granted, free of charge, to any person obtaining a copy
>  of this software and associated documentation files (the "Software"), to deal
>  in the Software without restriction, including without limitation the rights
>  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
>  copies of the Software, and to permit persons to whom the Software is
>  furnished to do so, subject to the following conditions:
>
>  The above copyright notice and this permission notice shall be included in
>  all copies or substantial portions of the Software.
>
>  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
>  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
>  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
>  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
>  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
>  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
>  THE SOFTWARE.

## Welcome to mod_cjson.

mod_cjson is an Apache module that provides the ability to build JSON/http
web services in C. It uses the cJSON framework written by Dave Gamble 
https://github.com/DaveGamble/cJSON for marshalling and unmarshalling requests
and responses.

To use it, you develop a shareable library with a single entry point using a
specific call signature. If a body is present in a request, it is parsed into
a cJSON object and passed to the shareable. You can then do whatever with it and
create an optional cJSON object to print to the response.

The shareable should expose a single function whose prototype is:
```c
    #include "apache_cjson.h"
    int APACHE_CJSON_ENTRY_POINT(apache_cjson *context, cJSON *in, cJSON **out);
```
The http method, URL components and other useful nuggets will be provided via 
the context parameter.

The configuration will be a location directive using this format:
```
  LoadModule cjson_module modules/mod_cjson.so
  <IfModule mod_cjson.c>
    <Location /some/path/>
      SetHandler cjson-handler
      cJSONContentHandler /path/to/shareable/shrlib.1.so
    </Location>
  </IfModule>
```

JSON is best described here: http://www.json.org/.
It is a lightweight data format similar in purpose to xml. It's a popular format
for use in REST protocols as it is less verbose than xml. You'll see it used in
everything from config files, big data products like Avro, and REST APIs.

This module aims to keep development as simple as possible. It is written in
ANSI C and should be buildable on many platforms including Windows, Linux and
even OpenVMS. There's even support for REST service development using OpenVMS 
COBOL.

Potential need at my workplace inspired me to write mod_cjson. I based the
design on the mod_gsoap plugin from Genivia https://www.genivia.com/.
 
## Building

### Linux
On linux, you build and deploy this way <i>assuming that you have the Apache 
headers and libraries installed</i>:
```
    $ sudo /usr/bin/apxs -i -a -c mod_cjson.c apache_cjson.c cJSON.c
```
Optionally, you can build an archive to link user code with:
```
    $ cc -c apache_cjson.c cJSON.c
    $ ar cr libapache_cjson.a apache_cjson.o cJSON.o
```
#### Makefile
Not yet.

#### CMake
Not yet.


### OpenVMS
On OpenVMS, you build this way:
```
    $ cc/share/pointer=32/prefix=all/names=(as_is,short) mod_cjson.c+apache$common:[include]apache$library.tlb/lib
    $ cc/pointer=32/prefix=all/names=(as_is,short)/warn=noinformationals cjson.c
    $ cc/pointer=32/prefix=all/names=(as_is,short) apache_cjson.c
    $ cc/pointer=32/prefix=all/names=(as_is,short) cjson_vms.c
    $!
    $ lib/create/object cjson.olb cjson, apache_cjson, cjson_vms
    $!
    $ link /share=mod_cjson.exe mod_cjson.obj, cjson.obj, apache_cjson.obj, sys$input:/option
    IDENTIFICATION="V1.0"
    GSMATCH=LEQUAL,1,0
    CASE_SENSITIVE=YES
    SYMBOL_VECTOR=(cjson_module=DATA)
    APACHE$HTTPD_SHR/SHARE
    APACHE$APR_SHR/SHARE
    APACHE$APU_SHR/SHARE
```
To build the COBOL-friendly object library:
```
    $ cc/pointer=32/prefix=all/names=(as_is,short)/warn=noinformationals cjson.c
    $ cc/pointer=32/prefix=all/names=(as_is,short) apache_cjson.c
    $ cc/pointer=32/prefix=all/names=(as_is,short) cjson_vms.c
    $ library/create/object cJSON.olb cJSON, apache_cjson, cJSON_VMS
```

#### Deploy the module
Just copy mod_cjson.exe to `APACHE$COMMON:[MODULES]`

#### MMS
Not yet. 

### Windows
I'm working on instructions for building on Windows. It should compile with Visual Studio, but I haven't tried
it yet. More to come...

## Usage:
To configure Apache to use mod_cjson, start by copying the module into Apache's
module directory under `apache_root/modules`. Then add a configuration similar
to this in the configuration file (typically in httpd.conf). Where the
configuration is located and how it's organized varies by platform. It's usually
located at `apache_root/conf`.

```
LoadModule cjson_module modules/mod_cjson.so
<IfModule mod_cjson.c>
        <Location /example>
                SetHandler cjson-handler
                cJSONContentHandler /path/to/share/test_module.so
        </Location>
        <Location /example/path>
                SetHandler cjson-handler
                cJSONContentHandler /path/to/share/test_module2.so
        </Location>
</IfModule>
```
The uri defined in the Location directive will select which shareable to invoke.
On OpenVMS systems, the file path specified by the cJSONContentHandler directive
can be an OpenVMS-style filespec.



## Writing a Service

### The Call Signature

#### In C:

```c
    #include "apache_cjson.h"
    int APACHE_CJSON_ENTRY_POINT(apache_cjson *context, cJSON *in, cJSON **out);
```

* `context` - `#included` from apache_cjson.h
* `in` - cJSON object passed in from the request (or null if none)
* `out` - cJSON object passed back from the shareable.

#### In COBOL:
```
PROGRAM-ID. APACHE_CJSON_ENTRY_POINT.
...
LINKAGE SECTION.
COPY 'APACHE_CJSON.CPY'.
01  LS_CJSON_IN                         USAGE POINTER.
01  LS_CJSON_OUT                        USAGE POINTER.
```

NOTE: `LS_CJSON_IN` isn't really a pointer. Because LINKAGE SECTION data is de-referenced,
it will be the actual cJSON struct. However, when you pass `LS_CJSON_IN` to the various
CJSON$ calls by reference, the struct is re-referenced and everything should work OK.

### The Context Object

This struct defines an object to hold state and pass useful information to the user code
shareable. For right now, it passes the http method and the parsed components of the
invoking URL. It also provides a boolean the user code can set if you want pretty JSON
in the response. The remaining fields are used internally.

```c
enum http_method {GET, POST, PUT, DELETE};
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
```

There's a COBOL copybook defining the same for use with OpenVMS COBOL.

```
01  CJSON$APACHE_CJSON.
    05  CJSON$METHOD                    PIC S9(09) COMP.
        88  CJSON$_GET                  VALUE 0.
        88  CJSON$_POST                 VALUE 1.
        88  CJSON$_PUT                  VALUE 2.
        88  CJSON$_DELETE               VALUE 3.
    05  CJSON$PARSED_URL.
        10  CJSON$SCHEME                USAGE POINTER.
        10  CJSON$HOST                  USAGE POINTER.
        10  CJSON$PORT                  USAGE POINTER.
        10  CJSON$PATH                  USAGE POINTER.
        10  CJSON$QUERY_ARGS            USAGE POINTER.
    05  CJSON$OUTPUT_FORMAT             PIC S9(09) COMP.
        88  CJSON$_COLLAPSED_OUTPUT     VALUE 0.
        88  CJSON$_PRETTY_OUTPUT        VALUE 1.
    05  FILLER                          PIC X(08).
```

### Sample C program to implement a REST-like service

```c
int APACHE_CJSON_ENTRY_POINT(apache_cjson *context, cJSON *request, cJSON **response) {
    cJSON *root;
    cJSON *fmt;
    fprintf(stderr, "Method is: %d\n", context->method);
    fprintf(stderr, "Scheme: %s\n", context->scheme);
    fprintf(stderr, "Hostname: %s\n", context->hostname);
    fprintf(stderr, "Port: %s\n", context->port);
    fprintf(stderr, "Path: %s\n", context->path);
    fprintf(stderr, "Query params: %s\n", context->query_args);

    fprintf(stderr, "Parsed Request: \n%s\n", cJSON_Print(request));

    root = cJSON_CreateObject();
    cJSON_AddItemToObject(root, "name", cJSON_CreateString("Jack (\"Bee\") Nimble"));
    cJSON_AddItemToObject(root, "format", fmt = cJSON_CreateObject());
    cJSON_AddStringToObject(fmt, "type", "rect");
    cJSON_AddNumberToObject(fmt, "width", 1920);
    cJSON_AddNumberToObject(fmt, "height", 1080);
    cJSON_AddFalseToObject (fmt, "interlace");
    cJSON_AddNumberToObject(fmt, "frame rate", 24);
    *response = root;

    cjson_add_response_header(context, "Location", "http://localhost/example/00440A");
    return 200; // OK to return 0 - equates to 200 OK
}
```

#### Build the shareable
```
$ gcc -c -fPIC -o test_module.o test_module.c
$ gcc -shared -fPIC -o test_module.so test_module.o -lc
```

#### Configure it in Apache httpd.conf

```
LoadModule cjson_module modules/mod_cjson.so
<IfModule mod_cjson.c>
    <Location /example>
        SetHandler cjson-handler
        cJSONContentHandler /usr/local/etc/test_module.so
    </Location>
</IfModule>
```

### Sample OpenVMS COBOL program to implement a REST-like service

```
IDENTIFICATION DIVISION.
PROGRAM-ID. APACHE_CJSON_ENTRY_POINT.
DATA DIVISION.
WORKING-STORAGE SECTION.

*   HTTP return code to pass back to the caller.
*   This should be placed in a copybook somewhere.
01  HTTP_RETURN_CODE                    PIC S9(09) COMP.
    88  HTTP_OK                         VALUE 200.
    88  HTTP_CREATED                    VALUE 201.
    88  HTTP_NO_CONTENT                 VALUE 204.
    88  HTTP_NOT_FOUND                  VALUE 404.
    88  HTTP_NOT_ALLOWED                VALUE 405.
    88  HTTP_CONFLICT                   VALUE 409.
    88  HTTP_SERVER_FAILURE             VALUE 500.

01  WS_STATUS                           PIC S9(09) COMP.
01  WS_STRING_VARS.
    05  WS_ID                           PIC X(55).
    05  WS_SCHEME                       PIC X(55).
    05  WS_HOST                         PIC X(55).
    05  WS_PATH                         PIC X(55).
    05  WS_PORT                         PIC X(55).
    05  WS_ARGS                         PIC X(55).

LINKAGE SECTION.
COPY 'APACHE_CJSON.CPY'.
01  LS_CJSON_IN                         USAGE POINTER.
01  LS_CJSON_OUT                        USAGE POINTER.

PROCEDURE DIVISION
    USING
        CJSON$APACHE_CJSON,
        LS_CJSON_IN,
        LS_CJSON_OUT
    GIVING
        HTTP_RETURN_CODE.

A1000_MAIN.
*   Extract the components of the calling URL. For REST services, 
*   you'll be interested in the path and query args.
    CALL 'CJSON$NTS_TO_SPS' USING CJSON$SCHEME, DESCRIPTOR WS_SCHEME.
    CALL 'CJSON$NTS_TO_SPS' USING CJSON$HOST, DESCRIPTOR WS_HOST.
    CALL 'CJSON$NTS_TO_SPS' USING CJSON$PATH, DESCRIPTOR WS_PATH.
    CALL 'CJSON$NTS_TO_SPS' USING CJSON$PORT, DESCRIPTOR WS_PORT.
    CALL 'CJSON$NTS_TO_SPS' USING CJSON$QUERY_ARGS, DESCRIPTOR WS_ARGS.

*   Get a field named "_id" from the cJSON object record passed in.
    CALL 'CJSON$GET_STRING_ITEM' USING LS_CJSON_IN,
        DESCRIPTOR "_id", WS_ID
        GIVING WS_STATUS.
    IF  NOT WS_STATUS SUCCESS
        DISPLAY "Incoming record does not have a field called _id"
    END-IF.

*   Create a cJSON object record to return
    CALL 'CJSON$CREATE_OBJECT' USING LS_CJSON_OUT

*   Add a field to it
    CALL 'CJSON$ADD_STRING_TO_OBJECT' USING VALUE LS_CJSON_OUT,
        DESCRIPTOR "out_id", "12345678".

    DISPLAY "Method:     ", CJSON$METHOD CONVERSION.
    DISPLAY "Scheme:     ", WS_SCHEME.
    DISPLAY "Hostname:   ", WS_HOST.
    DISPLAY "Path:       ", WS_PATH.
    DISPLAY "Port:       ", WS_PORT.
    DISPLAY "Query Args  ", WS_QUERY_ARGS.
    DISPLAY "ID:         ", WS_ID.

    EXIT PROGRAM.
```

#### Build script for the shareable

```
$! Compile the test shareable
$  cobol test_service.cob
$!
$! Link the user code into a shareable library
$ link/share=test_service.exe test_service, cjson.olb/lib, -
    sys$input:/option
IDENTIFICATION="V1.0"
GSMATCH=LEQUAL,1,0
SYMBOL_VECTOR=(APACHE_CJSON_ENTRY_POINT=PROCEDURE)
$ exit
```

#### Configure it in Apache httpd.conf

```
LoadModule cjson_module modules/mod_cjson.so
<IfModule mod_cjson.c>
    <Location /example>
        SetHandler cjson-handler
        cJSONContentHandler DISK:[DIR.PATH]TEST_SERVICE.EXE
    </Location>
</IfModule>
```

### cJSON wrapper for COBOL

The cJSON wrapper for COBOL is a series of subprogram calls to provide an OpenVMS
COBOL-friendly API to interact with cJSON and mod_cjson. The wrapper can also be
used with other OpenVMS-supported products like gSOAP and CURL to call other REST/JSON
services.

More on this later. You can look at the source file `cjson_vms.c` for usage for now.

## Caveats

### cJSON

mod_cjson uses cJSON for marshalling JSON as it is passed in a request/response body. See 
[cJSON](https://github.com/DaveGamble/cJSON) for details and limitations on how JSON is handled.

### C Standard

Much like cJSON, mod_cjson is written in ANSI C (specifically C89/90 in order to be buildable on OpenVMS). 
Make sure your compiler supports the C runtime and syntax as defined by the standard. 

NOTE: This code has not been compiled and tested using a C++ compiler.

### Floating Point Numbers

Only ANSI/IEEE 754-1985 double precision floating point numbers are supported. If compiling on Alpha, 
make sure you use the /FLOAT=IEEE switch. On Itanium and x86, this will be the default.

### Thread Safety

This code should be thread safe, however there are limitations in cJSON pertaining to thread safety. See
[cJSON](https://github.com/DaveGamble/cJSON) for details.

## Enjoy mod_cjson!

- Steve Leski, Jul 2017
- [mod_cjson contributors](CONTRIBUTORS.md)
