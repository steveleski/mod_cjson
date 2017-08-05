*
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
*
* Apache mod_cjson module - companion copybook for OpenVMS COBOL.
*
* Author: Steve Leski - https://github.com/steveleski
* Date: July 27, 2017
*
* This copybook file is to be COPY'd as the first parameter in the linkage
* section of a COBOL program that implements a mod_cjson shareable image.
* mod_cjson is an Apache module that can invoke a user defined function to 
* process a http request using JSON.
*
* To use mod_cjson, set up a <Location> block in your configuration file like 
* so:
*
*    LoadModule cjson_module modules/mod_cjson.so 
*    <IfModule mod_cjson.c>
*       <Location /example>
*          SetHandler cjson-handler
*          cJSONContentHandler disk:[dir1.dir2]user_code.exe
*       </Location>
*       ... other location directives
*    </IfModule>
*
* user_code is a shareable image that exposes a single function call with the
* signature:
*     PROGRAM-ID. APACHE_CJSON_ENTRY_POINT.
*     LINKAGE SECTION.
*     COPY 'APACHE_CJSON.CPY'.
*     01  INPUT_CJSON USAGE POINTER.
*     01  OUTPUT_CJSON USAGE POINTER.
*     PROCEDURE DIVISION USING CJSON$APACHE_CJSON, INPUT_CJSON, OUTPUT_CJSON
*                        GIVING HTTP_ERROR_CODE.
*
* The request and response objects use cJSON https://github.com/DaveGamble/cJSON
* to marshall/unmarshall the http request and response bodies. See cJSON_VMS.c
* for the api that COBOL can use to implement a mod_cjson service.
* 
* 
* IMPORTANT NOTES
* ===============
*
* XXX TO DO XXX
* =============
*
*
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
    05  FILLER                          USAGE POINTER.
    05  FILLER                          USAGE POINTER.
