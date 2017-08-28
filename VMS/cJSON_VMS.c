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
 * OpenVMS COBOL-friendly framework for use with cJSON as well as the mod_cjson
 * Apache module.
 *
 * Author: Steve Leski - https://github.com/steveleski
 * Date: July 27, 2017
 *
 * cJSON provides an api for serializing and deserializing JSON using C. It
 * compiles clean on OpenVMS. That's because the source code sticks to the ANSI
 * C standard (thanks to Dave Gamble). The only two sources needed to make this
 * work is cJSON.h and cJSON.c. This file provides a wrapper to cJSON to make
 * it easy to use in OpenVMS COBOL programs.
 *
 * cJSON is provided by Dave Gamble - https://github.com/DaveGamble/cJSON
 * 
 * IMPORTANT NOTES
 * ===============
 *
 * XXX TO DO XXX
 * =============
 *
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <descrip.h>
#include <dscdef.h>
#include <ssdef.h>
#include <libdef.h>
#include <lib$routines.h>

#include "cJSON.h"
#include "apache_cjson.h"

#define failure(stat) (!(stat & 1))
#define success(stat) (stat & 1)

//Private routine to convert space-padded strings to null-terminated.
static char *to_nts(struct dsc$descriptor_s *string) {
    int i, len = string->dsc$w_length;
    char *str = malloc(len + 1);
    memcpy(str, string->dsc$a_pointer, len);
    str[len] = '\0';

    for (i = len - 1; i >= 0 && str[i] == ' '; i--) {
        str[i] = '\0';
    } // One off error here? Possibly?
    return str;
}


/*
 * Test for null.
 *
 * Parameters passed to a COBOL program are de-referenced because COBOL assumes
 * pass-by-reference. This is a way of testing the address of a de-referenced 
 * parameter passed in the LINKAGE SECTION. Returns SS$_NORMAL if the reference
 * is NULL, otherwise, 0 is returned.
 *
 * Usage:
 * LINKAGE SECTION.
 * 01  _parameter USAGE/PIC whatever.
 * CALL 'CJSON$TEST_FOR_NULL' USING
 *    REFERENCE _parameter,
 *    GIVING _result.
 */
extern unsigned long CJSON$TEST_FOR_NULL(void *ptr) {
    return (ptr == NULL)? SS$_NORMAL : 0L;
}


/*
 * Test for null.
 *
 * Parameters passed to a COBOL program are de-referenced because COBOL assumes
 * pass-by-reference. If a param is passed BY VALUE to a COBOL program, the 
 * value becomes inaccessible and if you try to touch it, you'll get an access
 * violation. This routine returns the value of a parameter passed by value. It
 * only works for 32 bit integers.
 *
 * Usage:
 * LINKAGE SECTION.
 * 01  _parameter USAGE/PIC whatever.
 * 01  _value PIC S9(09) COMP.
 * CALL 'CJSON$GET_BY_VALUE_PARAM' USING
 *    REFERENCE _parameter,
 *    REFERENCE _value,
 *    GIVING _status.
 */
extern unsigned long CJSON$GET_BY_VALUE_PARAM(unsigned long *ptr, unsigned long *value) {
    if (ptr == NULL) {
        *value = 0;
        return SS$_BADPARAM;
    }
    *value = *ptr;
    return SS$_NORMAL; 
}


/*
 * Convert a space-padded string to a null-terminated. Requires apache_cjson
 * (part of the mod_cjson Apache plug-in). The string that's returned is 
 * dynamically allocated. mod_cjson will atomatically clean it up after an http
 * request/response completes.
 *
 * Usage:
 * 01  _cjson USAGE POINTER.
 * 01  _text PIC X(whatever).
 * 01  null_terminated_string USAGE POINTER.
 * CALL 'CJSON$SPS_TO_NTS' USING
 *    REFERENCE _cjson,
 *    DESCRIPTOR _text, 
 *    REFERENCE null_terminated_string.
 */
extern unsigned long CJSON$SPS_TO_NTS(apache_cjson *context, struct dsc$descriptor_s *sps, char **nts) {
    int len;
    char *temp;

    //trim trailing spaces
    for (len = sps->dsc$w_length - 1; len >=0; len--) {
        if (sps->dsc$a_pointer[len] > ' ') break;
    }
    len++;
    
    if (len < 1) {
        //Allocate empty string if all blanks
        temp = cjson_alloc_mem(context, 1);
        temp[0] = 0x00;
    } else {
        temp = cjson_alloc_mem(context, len + 1);
        memcpy(temp, sps->dsc$a_pointer, len);
        temp[len] = 0x00;
    }
    if (temp == NULL) return LIB$_INSVIRMEM;
    *nts = temp;
    return SS$_NORMAL;
}


/*
 * Convert a null terminated string to a space-padded string. Returns LIB$_STRTRU
 * if the receiving string is too short and will truncate the result.
 *
 * Usage:
 * 01  null_terminated_string USAGE POINTER.
 * 01  _text PIC X(whatever).
 * CALL 'CJSON$NTS_TO_SPS' USING
 *    REFERENCE null_terminated_string,
 *    DESCRIPTOR _text.
 */
extern unsigned long CJSON$NTS_TO_SPS(char **nts, struct dsc$descriptor_s *sps) {
    unsigned short nts_len;
    unsigned long stat;

    nts_len = (*nts == NULL)? 0 : strlen(*nts);
    lib$movc5(&nts_len, *nts, " ", &(sps->dsc$w_length), sps->dsc$a_pointer);
    if (nts_len > sps->dsc$w_length) return LIB$_STRTRU;
    return SS$_NORMAL;
}


/*
 * Adds a http header field to the response.
 *
 * Usage:
 * 01  _cjson USAGE POINTER.
 * 01  _name PIC X(whatever).
 * 01  _value PIC X(whatever).
 * CALL 'CJSON$ADD_HTTP_HEADER' USING
 *    REFERENCE _cjson,
 *    DESCRIPTOR _name,
 *    DESCRIPTOR _value.
 */
extern unsigned long CJSON$ADD_RESPONSE_HEADER(apache_cjson *context, 
                                               struct dsc$descriptor_s *name,
                                               struct dsc$descriptor_s *value)
{
    char *n = NULL; 
    char *v = NULL;
    unsigned long stat;
    
    stat = CJSON$SPS_TO_NTS(context, name, &n);
    if (failure(stat)) return stat;
    stat = CJSON$SPS_TO_NTS(context, value, &v);
    if (failure(stat)) return stat;
    cjson_add_response_header(context, n, v);
    return SS$_NORMAL;
}


/*
 * Free up memory allocated from calling CJSON$SPS_TO_NTS.
 *
 * This should only be used in a dummy main program for testing and 
 * debugging the shareable library user code. It should never be
 * called from the shareable.
 *
 * mod_cjson will call this automatically after the response has been
 * returned. 
 *
 * Usage:
 * COPY 'APACHE_CJSON.CPY'.
 * CALL 'CJSON$DONE' USING CJSON$APACHE_CJSON.
 */
extern void CJSON$DONE(apache_cjson *context) {
    cjson_free_mem(context);
}

/*
 * Wraps cJSON_Parse to an OpenVMS COBOL friendly call signature.
 *
 * Converts JSON text to a cJSON object.
 *
 * Usage:
 * 01  root USAGE POINTER.
 * 01  json_text PIC X(whatever).
 * CALL 'CJSON$PARSE' USING DESCRIPTOR json_text, REFERENCE root.
 */
extern void CJSON$PARSE(struct dsc$descriptor_s *json, cJSON **root) {
    char *string = to_nts(json);
    *root = cJSON_Parse(string);
    free(string);
}


/*
 * Wraps cJSON_PrintUnformatted to an OpenVMS COBOL friendly call signature.
 *
 * Converts a cJSON object to text without making it pretty. Returns LIB$_STRTRU
 * if the receiving string is too short.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * 01  json_text PIC X(whatever).
 * 01  true_length PIC S9(09) COMP.
 * CALL 'CJSON$PRINT_UNFORMATTED' USING
 *     VALUE      cJSON_Object,
 *     DESCRIPTOR json_text,
 *     REFERENCE true_length.
 */
extern int CJSON$PRINT_UNFORMATTED(cJSON *object, struct dsc$descriptor_s *json, int *length) {
    int stat = SS$_NORMAL;
    char *string = cJSON_PrintUnformatted(object);
    int len = strlen(string);

    if (json->dsc$w_length < len) {
        len = json->dsc$w_length;
        stat = LIB$_STRTRU;
    }

    memcpy(json->dsc$a_pointer, string, len);
    *length = len;
    free(string);
    return stat;
}


/*
 * Wraps cJSON_Print to an OpenVMS COBOL friendly call signature.
 *
 * Converts a cJSON object to formatted pretty text. Returns LIB$_STRTRU
 * if the receiving string is too short.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * 01  json_text PIC X(whatever).
 * 01  true_length PIC S9(09) COMP.
 * CALL 'CJSON$PRINT' USING
 *     VALUE      cJSON_Object,
 *     DESCRIPTOR json_text,
 *     REFERENCE true_length.
 */
extern int CJSON$PRINT(cJSON *object, struct dsc$descriptor_s *json, int *length) {
    int stat = SS$_NORMAL;
    char *string = cJSON_Print(object);
    int len = strlen(string);

    if (json->dsc$w_length < len) {
        len = json->dsc$w_length;
        stat = LIB$_STRTRU;
    }

    memcpy(json->dsc$a_pointer, string, len);
    *length = len;
    free(string);
    return stat;
}


/*
 * Wraps cJSON_Delete to an OpenVMS COBOL friendly call signature.
 *
 * Deletes a cJSON object from memory. If you are executing code using the
 * mod_cjson Apache module, it will automatically call this for you to prevent
 * memory leaks.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$DELETE' USING cJSON_Object.
 */
extern void CJSON$DELETE(cJSON **c) {
    cJSON_Delete(*c);
    *c = NULL;
}


/*
 * Wraps cJSON_GetArraySize to an OpenVMS COBOL friendly call signature.
 *
 * Gets the size of a cJSON object that represents an array.
 *
 * Usage:
 * 01  cJSON_Array_Object USAGE POINTER.
 * 01  array_size PIC S9(09) COMP.
 * CALL 'CJSON$GET_ARRAY_SIZE' USING 
 *    VALUE cJSON_Array_Object, 
 *    REFERENCE array_size.
 */
extern void CJSON$GET_ARRAY_SIZE(cJSON *array, int *size) {
    *size = cJSON_GetArraySize(array);
}


/*
 * Wraps cJSON_GetArrayItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets an element in an array via index. Returns 0 if the index is out of
 * bounds.
 *
 * Usage:
 * 01  cJSON_Array_Object USAGE POINTER.
 * 01  _index PIC S9(09) COMP.
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$GET_ARRAY_ITEM' USING 
 *   VALUE     cJSON_Array_Object, 
 *   REFERENCE _index, 
 *   REFERENCE cJSON_Object.
 */
extern int  CJSON$GET_ARRAY_ITEM(cJSON *array, int *item, cJSON **object) {
    cJSON *tmp = cJSON_GetArrayItem(array, *item);
    if (!tmp) return 0;
    *object = tmp;
    return SS$_NORMAL;
}


/*
 * Wraps cJSON_GetObjectItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets a cJSON object field contained in another cJSON object. 
 * Returns 0 if not found.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  cJSON_Object_Field USAGE POINTER.
 * CALL 'CJSON$GET_OBJECT_ITEM' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name, 
 *    REFERENCE  cJSON_Object_Field.
 */
extern int CJSON$GET_OBJECT_ITEM(cJSON *object, struct dsc$descriptor_s *name, cJSON **item) {
    char *string = to_nts(name);
    cJSON *tmp = cJSON_GetObjectItem(object, string);
    free(string);
    if (!tmp) return 0;
    *item = tmp;
    return SS$_NORMAL;
}


/*
 * Wraps cJSON_GetObjectItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets an IEEE double field contained in a cJSON record object.
 * Returns 0 if not found or the object does not represent a number.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  _number USAGE COMP-2.
 * CALL 'CJSON$GET_NUMBER_ITEM' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name, 
 *    REFERENCE  _number.
 */
extern int CJSON$GET_NUMBER_ITEM(cJSON *object, struct dsc$descriptor_s *name, double *number) {
    char *string = to_nts(name);
    cJSON *tmp = cJSON_GetObjectItem(object, string);
    free(string);
    if (!tmp) return 0;
    if (!(cJSON_Number == tmp->type)) return 0;

    *number = tmp->valuedouble;
    return SS$_NORMAL;
}


/*
 * Wraps cJSON_GetObjectItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets an integer field from a cJSON record object. 
 * Returns 0 if not found or the field does not represent a number.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  _number PIC S9(09) COMP.
 * CALL 'CJSON$GET_INTEGER_ITEM' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name, 
 *    REFERENCE  _number.
 */
extern int CJSON$GET_INTEGER_ITEM(cJSON *object, struct dsc$descriptor_s *name, int *number) {
    char *string = to_nts(name);
    cJSON *tmp = cJSON_GetObjectItem(object, string);
    free(string);
    if (!tmp) return 0;
    if (!(cJSON_Number == tmp->type)) return 0;

    *number = tmp->valueint;
    return SS$_NORMAL;
}


/*
 * Wraps cJSON_GetObjectItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets a text field contained in a cJSON record object.
 * Returns 0 if not found.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  _text PIC X(whatever).
 * CALL 'CJSON$GET_STRING_ITEM' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name, 
 *    DESCRIPTOR _text.
 */
extern int CJSON$GET_STRING_ITEM(cJSON *object, struct dsc$descriptor_s *name, struct dsc$descriptor_s *value) {
    int srclen, dstlen, i;
    char *string = to_nts(name);
    cJSON *tmp = cJSON_GetObjectItem(object, string);
    free(string);
    if (!tmp) return 0;
    srclen = strlen(tmp->valuestring);
    dstlen = value->dsc$w_length;

    if (srclen > dstlen) srclen = dstlen;

    memcpy(value->dsc$a_pointer, tmp->valuestring, srclen);
    for (i = srclen; i < dstlen; i++) {
        value->dsc$a_pointer[i] = ' '; // Pad with spaces
    }

    return SS$_NORMAL;
}


/*
 * Wraps cJSON_GetObjectItem to an OpenVMS COBOL friendly call signature.
 *
 * Gets a true/false field contained in a cJSON record object.
 * Returns 0 if not found.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  true_false PIC S9(09) COMP. 
 *     88  VALUE_FALSE VALUE 0.
 *     88  VALUE_TRUE VALUE 1.
 * CALL 'CJSON$GET_BOOL_ITEM' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name, 
 *    REFERENCE  true_false.
 */
extern int CJSON$GET_BOOL_ITEM(cJSON *object, struct dsc$descriptor_s *name, int *value) {
    char *string = to_nts(name);
    cJSON *tmp = cJSON_GetObjectItem(object, string);
    free(string);
    if (!tmp) return 0;

    *value = tmp->valueint;
    return SS$_NORMAL;
}


/*
 * Wraps cJSON_CreateNull to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing NULL.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$CREATE_NULL' USING cJSON_Object.
 */
extern void CJSON$CREATE_NULL(cJSON **object) {
    *object = cJSON_CreateNull();
}


/*
 * Wraps cJSON_CreateTrue to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing a boolean true.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$CREATE_TRUE' USING cJSON_Object.
 */
extern void CJSON$CREATE_TRUE(cJSON **object) {
    *object = cJSON_CreateTrue();
}


/*
 * Wraps cJSON_CreateFalse to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing a boolean false.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$CREATE_FALSE' USING cJSON_Object.
 */
extern void CJSON$CREATE_FALSE(cJSON **object) {
    *object = cJSON_CreateFalse();
}


/*
 * Wraps cJSON_CreateBool to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing a boolean.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * 01  true_false PIC S9(09) COMP.
 *     88  VALUE_FALSE VALUE 0.
 *     88  VALUE_TRUE VALUE 1.
 * CALL 'CJSON$CREATE_BOOL' USING cJSON_Object, true_false.
 */
extern void CJSON$CREATE_BOOL(cJSON **object, int *bool) {
    *object = cJSON_CreateBool(*bool);
}


/*
 * Wraps cJSON_CreateNumber to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing an IEEE double.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * 01  _number USAGE COMP-2.
 * CALL 'CJSON$CREATE_NUMBER' USING cJSON_Object, _number.
 */
extern void CJSON$CREATE_NUMBER(cJSON **object, double *number) {
    *object = cJSON_CreateNumber(*number);
}


/*
 * Wraps cJSON_CreateString to an OpenVMS COBOL friendly call signature.
 *
 * Create a record field item representing a string. Trailing spaces are 
 * trimmed.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * 01  _text PIC X(whatever).
 * CALL 'CJSON$CREATE_STRING' USING cJSON_Object, _text.
 */
extern void CJSON$CREATE_STRING(cJSON **object, struct dsc$descriptor_s *string) {
    char* str = to_nts(string);
    *object = cJSON_CreateString(str);
    free(str);
}


/*
 * Wraps cJSON_CreateArray to an OpenVMS COBOL friendly call signature.
 *
 * Create an array object.
 *
 * Usage:
 * 01  cJSON_Array_Object USAGE POINTER.
 * CALL 'CJSON$CREATE_ARRAY' USING cJSON_Array_Object.
 */
extern void CJSON$CREATE_ARRAY(cJSON **array) {
    *array = cJSON_CreateArray();
}


/*
 * Wraps cJSON_CreateObject to an OpenVMS COBOL friendly call signature.
 *
 * Create an object that represents a record.
 *
 * Usage:
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$CREATE_OBJECT' USING cJSON_Object.
 */
extern void CJSON$CREATE_OBJECT(cJSON **object) {
    *object = cJSON_CreateObject();
}


/*
 * Wraps cJSON_AddItemToArray to an OpenVMS COBOL friendly call signature.
 *
 * Add a cJSON object to a cJSON array.
 *
 * Usage:
 * 01  cJSON_Array_Object USAGE POINTER.
 * 01  cJSON_Object USAGE POINTER.
 * CALL 'CJSON$ADD_ITEM_TO_ARRAY' USING 
 *    VALUE     cJSON_Array_Object, 
 *    VALUE     cJSON_Object.
 */
extern void CJSON$ADD_ITEM_TO_ARRAY(cJSON *array, cJSON *item) {
    cJSON_AddItemToArray(array, item);
}


/*
 * Wraps cJSON_AddItemToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a cJSON object into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  cJSON_Field_Object USAGE POINTER.
 * CALL 'CJSON$ADD_ITEM_TO_OBJECT' USING 
 *    VALUE     cJSON_Record_Object, 
 *    DESCRIPTOR field_name,
 *    VALUE     cJSON_Field_Object.
 */
extern void CJSON$ADD_ITEM_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name, cJSON *item) {
    char *string = to_nts(name);
    cJSON_AddItemToObject(object, string, item);
    free(string);
}


/*
 * Wraps cJSON_AddNullToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a NULL into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * CALL 'CJSON$ADD_NULL_TO_OBJECT' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name.
 */
extern void CJSON$ADD_NULL_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name) {
    char *string = to_nts(name);
    cJSON_AddNullToObject(object, string);
    free(string);
}


/*
 * Wraps cJSON_AddTrueToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a boolean set to true into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * CALL 'CJSON$ADD_TRUE_TO_OBJECT(' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name.
 */
extern void CJSON$ADD_TRUE_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name) {
    char *string = to_nts(name);
    cJSON_AddTrueToObject(object, string);
    free(string);
}


/*
 * Wraps cJSON_AddFalseToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a boolean set to false into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * CALL 'CJSON$ADD_FALSE_TO_OBJECT(' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name.
 */
extern void CJSON$ADD_FALSE_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name) {
    char *string = to_nts(name);
    cJSON_AddFalseToObject(object, string);
    free(string);
}

/*
 * Wraps cJSON_AddBoolToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a boolean into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  true_false PIC S9(09) COMP.
 *     88  VALUE_FALSE VALUE 0.
 *     88  VALUE_TRUE VALUE 1.
 * CALL 'CJSON$ADD_BOOL_TO_OBJECT' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name,
 *    REFERENCE true_false.
 */
extern void CJSON$ADD_BOOL_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name, int *bool) {
    char *string = to_nts(name);
    cJSON_AddBoolToObject(object, string, *bool);
    free(string);
}


/*
 * Wraps cJSON_AddNumberToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add a number (IEEE double) into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  _number USAGE COMP-2.
 *     88  VALUE_FALSE VALUE 0.
 *     88  VALUE_TRUE VALUE 1.
 * CALL 'CJSON$ADD_NUMBER_TO_OBJECT' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name,
 *    REFERENCE _number.
 */
extern void CJSON$ADD_NUMBER_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name, double *number) {
    char *string = to_nts(name);
    cJSON_AddNumberToObject(object, string, *number);
    free(string);
}


/*
 * Wraps cJSON_AddStringToObject to an OpenVMS COBOL friendly call signature.
 *
 * Add text into a cJSON record object as a new field.
 *
 * Usage:
 * 01  cJSON_Record_Object USAGE POINTER.
 * 01  field_name PIC X(whatever).
 * 01  _text PIC X(whatever).
 *     88  VALUE_FALSE VALUE 0.
 *     88  VALUE_TRUE VALUE 1.
 * CALL 'CJSON$ADD_STRING_TO_OBJECT' USING 
 *    VALUE      cJSON_Record_Object, 
 *    DESCRIPTOR field_name,
 *    DESCRIPTOR _text.
 */
extern void CJSON$ADD_STRING_TO_OBJECT(cJSON *object, struct dsc$descriptor_s *name, struct dsc$descriptor_s *value) {

    char *nm = to_nts(name);
    char *vl = to_nts(value);

    cJSON_AddStringToObject(object, nm, vl);

    free(nm);
    free(vl);
}
