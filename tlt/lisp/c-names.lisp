(in-package "TLI")

;;;; Module C-NAMES

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: Jim Allard






;;;; C Namespaces




;;; A part of translating is determining the C identifiers to be used to
;;; represent Lisp symbols within the translated files.  A C namespace structure
;;; will be used to represent each of the layers of C namespaces; global scope,
;;; file scope, and function scope.  C namespaces keep hashtables relating C
;;; identifiers delcared within a scope to short descriptions.  The descriptions
;;; will be lists whose first value is one of the symbols function, variable,
;;; macro, or reserved.  For variables and functions, there may be a second
;;; element in the list which is a string containing the appropriate extern
;;; declaration to declare the type of that identifier.  If there is no second
;;; element, then it is externed by default in all translated files.

(defstruct (c-namespace
             (:constructor make-c-namespace (surrounding-c-namespace?)))
  (local-identifiers (make-hash-table :test #'equal :size 1000))
  (surrounding-c-namespace? nil))




;;; There will be a global C namespace that contains all built in identifiers
;;; and reserved words of C.  This set will be determined by numbly entering all
;;; identifiers listed in the index on pages 385 through 392 of Harbison and
;;; Steele (3rd), all identifers built-in to TL in the hand-written C code, and any
;;; POSIX identifiers we use.  Omissions in this list will show up as multiple
;;; definitions or improper links only if Lisp symbols are made that happen to
;;; collide with the C versions.  In all cases, errors of this form should be
;;; able to be fixed up by adding identifiers to the global namespace.

;;; The function `c-namespace-get-identifier' takes a string and returns the
;;; data associated with that identifier.  It may be used with setf.  Note that
;;; getting an identifier will search through surrounding namespaces, but
;;; setting will directly set the given namespace.

(defun c-namespace-get-identifier (c-namespace identifier-string)
  (loop with name-entry?
	for namespace = c-namespace
		      then (c-namespace-surrounding-c-namespace? namespace)
        while namespace
	when (setq name-entry? 
		   (gethash identifier-string
			    (c-namespace-local-identifiers namespace)))
	return name-entry?))

(defmacro set-c-namespace-get-identifier
    (c-namespace identifier-string declaration-info)
  `(setf (gethash ,identifier-string
		  (c-namespace-local-identifiers ,c-namespace))
	 ,declaration-info))

(defsetf c-namespace-get-identifier set-c-namespace-get-identifier)




;;; As translation proceeds, all external symbols of each translated file should
;;; be added to the global namespace.  When a namespace is made for a file, it
;;; should not inherit symbols from the global namespace, except as they are
;;; imported through extern statements.  When a namespace is made for a
;;; function, it should inherit from the file namespace.

;;; Note that no identifier within any of these namespaces should exceed 32
;;; characters in length, since that is the limit for the VAX linker.

;;; The function `populate-c-namespace' takes a namespace and a list of strings
;;; dotted with decls.  They strings are used as keys to the declarations and
;;; entered into the namespace.  The namespace is returned.

(defun populate-c-namespace (namespace identifiers-and-decls)
  (loop for (string . decl) in identifiers-and-decls do
    (unless (memq (cons-car decl) '(function variable type reserved macro))
      (warn "C name ~s has bad decl ~s" string decl))
    (setf (c-namespace-get-identifier namespace string)
	  decl))
  namespace)




;;; The global variable `*global-c-namespace*' is bound in translate system to a
;;; namespace inheriting from the reserved namespace.

(defvar *global-c-namespace* nil)




;;; The global parameter `*reserved-c-namespace*' contains a c-namespace holding
;;; all of the reserved words for the "Clean C" dilect described in _C, A
;;; Reference Manual, 4th Edition_ (CaRM4), by Harbison and Steele.  This set
;;; was compiled from CaRM4 reserved words, p. 23; CaRM4 C++ reserved words,
;;; p. 37, and the index from CaRM3.  This namespace should also hold all
;;; identifiers defined within hand-written C code we include into TLT output
;;; images.

(defparameter *reserved-c-namespace*
  (populate-c-namespace
    (make-c-namespace nil)
    '(;; TL library defined identifiers
      ("Values_count" variable)
      ("Values_buffer" variable)
      ("Unbound" variable)
      ("THROW_STACK_MAX" variable)
      ("Throw_stack" variable)
      ("Throw_stack_top" variable)
      ("Current_throw" variable)
      ("store_values_on_stack" function)
      ("retrieve_values_from_stack" function)
      ("throw_towards_catch_tag" function)
      ("bind_global" function)
      ("unbind_global" function)
      ("get_binding_addr" function)
      ("get_binding_value" function)
      ("set_binding_value" function)
      
      ("malloc_block_into_region" function)
      ("region_number_bytes_size" function)
      ("region_number_bytes_used" function)
      ("region_number_bytes_available" function)

      ("alloc_cons" function)
      ("hook_up_cdrs" function)
      ("alloc_list" function)
      ("alloc_simple_vector" function)
      ("alloc_string" function)
      ("alloc_uint16_array" function)
      ("alloc_sint16_array" function)
      ("alloc_double_array" function)
      ("alloc_ldouble" function)
      ("alloc_mdouble" function)
      ("alloc_symbol" function)
      ("T" variable)
      ("alloc_package" function)
      ("alloc_string_strm" function)
      ("alloc_file_strm" function)
      ("alloc_struct" function)
      ("notify" function)
      ("warn" function)
      ("error" function)
      ("type_cast_error" function)
      ("fatal_error" function)
      ("write_fixnum_into_str" function)
      ("write_double_into_str" function)
      ("current_time" function)
      ("get_platform_code" function)
      ("delete_named_file" function)

      ;; Types defined in tl.h
      ("uint8" type)
      ("sint8" type)
      ("uint16" type)
      ("sint16" type)
      ("uint32" type)
      ("sint32" type)
      ("Hdr" type)
      ("Obj" type)
      ("Cons" type)
      ("Sv" type)
      ("Str" type)
      ("Sa_uint8" type)
      ("Sa_uint16" type)
      ("Sa_sint16" type)
      ("Sa_double" type)
      ("Ldouble" type)
      ("Mdouble" type)
      ("Sym" type)
      ("Func" type)
      ("Func_0" type)
      ("Func_1" type)
      ("Func_2" type)
      ("Func_3" type)
      ("Func_4" type)
      ("Func_5" type)
      ("Func_6" type)
      ("Func_7" type)
      ("Func_8" type)
      ("Func_9" type)
      ("Func_10" type)
      ("Func_11" type)
      ("Func_12" type)
      ("Func_13" type)
      ("Func_14" type)
      ("Func_15" type)
      ("Func_16" type)
      ("Func_17" type)
      ("Func_18" type)
      ("Func_19" type)
      ("Func_20" type)
      ("Pkg" type)
      ("String_strm" type)
      ("File_strm" type)
      ("Class_hdr" type)

      ;; Macros in tl.h
      ("CAR" macro)
      ("CDR" macro)
      ("BOXFIX" macro)
      ("UNBOXFIX" macro)
      ("BOXCHAR" macro)
      ("UNBOXCHAR" macro)
      ("StrHDR" macro)
      ("ObjStrHDR" macro)
      ("SvHDR" macro)
      ("ObjSvHDR" macro)
      ("CLASS_HDR_TAG" macro)
      ("TYPE_TAG" macro)
      ("IMMED_TAG" macro)
      ("STD_TAG" macro)
      ("EXTENDED_TAG" macro)
      
      ;; Oddball reserved words
      ("j0" function)			; Bessel functions in math.h
      ("j1" function)
      ("jn" function)
      ("y0" function)
      ("y1" function)
      ("yn" function)
      ("sigfpe" variable)		; Signal name.

      ;; C and C++ defined identifiers
      ("_IOFBF" variable)
      ("_IOFBF" variable)
      ("_IONBF" variable)
      ("abort" function)
      ("abs" function)
      ("acos" function)
      ("alarm" function)
      ("asctime" function)
      ("asin" function)
      ("asm" reserved)
      ("assert" function)
      ("atan" function)
      ("atan2" function)
      ("atexit" function)
      ("atof" function)
      ("atoi" function)
      ("atol" function)
      ("auto" reserved)
      ("bcmp" function)
      ("bcpy" function)
      ("bool" type)
      ("break" function)
      ("bsearch" function)
      ("BUFSIZ" variable)
      ("bzero" function)
      ("calloc" function)
      ("case" function)
      ("__cplusplus" reserved)
      ("catch" reserved)
      ("ceil" function)
      ("cfree" function)
      ("char" type)
      ("clalloc" function)
      ("class" reserved)
      ("clearerr" function)
      ("clock" function)
      ("clock_t" function)
      ("CLOCKS_PER_SEC" variable)
      ("const" reserved)
      ("const_cast" reserved)
      ("continue" function)
      ("cos" function)
      ("cosh" function)
      ("ctermid" function)
      ("ctime" function)
      ("cuserid" function)
      ("__DATE__" variable)
      ("default" reserved)
      ("defined" function)
      ("delete" reserved)
      ("difftime" function)
      ("div" function)
      ("do" function)
      ("double" type)
      ("dynamic_cast" reserved)
      ("EDOM" variable)
      ("else" reserved)
      ("entry" reserved)
      ("enum" type)
      ("EOF" variable)
      ("ERANGE" variable)
      ("errno" variable)
      ("exec" function)
      ("exit" function)
      ("exp" function)
      ("expt" function)
      ("extern" reserved)
      ("fabs" function)
      ("false" reserved)
      ("fclose" function)
      ("feof" function)
      ("ferror" function)
      ("fflush" function)
      ("fgetc" function)
      ("fgetpos" function)
      ("fgets" function)
      ("FILE" type)
      ("__FILE__" variable)
      ("FILENAME_MAX" variable)
      ("float" type)
      ("floor" function)
      ("fmod" function)
      ("fopen" function)
      ("FOPEN_MAX" variable)
      ("for" function)
      ("fortran" reserved)
      ("fpos_t" type)
      ("fprintf" function)
      ("fputc" function)
      ("fputs" function)
      ("fread" function)
      ("free" function)
      ("freopen" function)
      ("frexp" function)
      ("friend" reserved)
      ("fscanf" function)
      ("fseek" function)
      ("fsetpos" function)
      ("ftell" function)
      ("fwrite" function)
      ("getc" function)
      ("getchar" function)
      ("getcwd" function)
      ("getenv" function)
      ("getlogin" function)
      ("getopt" function)
      ("gets" function)
      ("getwd" function)
      ("gmtime" function)
      ("goto" reserved)
      ("gsignal" function)
      ("HUGE" variable)
      ("HUGE_VAL" variable)
      ("if" reserved)
      ("ifdef" reserved)
      ("ifndef" reserved)
      ("include" reserved)
      ("inline" reserved)
      ("instr" function)
      ("int" type)
      ("iscsymf" function)
      ("islower" function)
      ("isodigit" function)
      ("isprint" function)
      ("ispunct" function)
      ("isupper" function)
      ("iswhite" function)
      ("isxdigit" function)
      ("jmp_buf" variable)
      ("L_tmpnam" function)
      ("labs" function)
      ("lconv" type)
      ("ldexp" function)
      ("ldiv" function)
      ("lenstr" function)
      ("ln" function)
      ("localeconv" function)
      ("localtime" function)
      ("log" function)
      ("log10" function)
      ("long" type)
      ("float" type)
      ("longjmp" function)
      ("main" function)
      ("malloc" function)
      ("matherr" function)
      ("mblen" function)
      ("mbstowcs" function)
      ("mbtowc" function)
      ("memccpy" function)
      ("memchr" function)
      ("memcmp" function)
      ("memmove" function)
      ("memset" function)
      ("mktemp" function)
      ("mktime" function)
      ("mlalloc" function)
      ("modf" function)
      ("mutable" reserved)
      ("namespace" reserved)
      ("new" reserved)
      ("NDEBUG" variable)
      ("notstr" function)
      ("NULL" variable)
      ("offsetof" function)
      ("onexit" function)
      ("onexit_t" function)
      ("operator" reserved)
      ("perror" function)
      ("pow" function)
      ("printf" function)
      ("private" reserved)
      ("protected" reserved)
      ("psignal" function)
      ("ptrdiff_t" function)
      ("public" reserved)
      ("putc" function)
      ("putchar" function)
      ("putenv" function)
      ("puts" function)
      ("qsort" function)
      ("raise" function)
      ("rand" function)
      ("RAND_MAX" variable)
      ("realloc" function)
      ("register" reserved)
      ("reinterpret_cast" reserved)
      ("relalloc" function)
      ("remainder" function)
      ("remove" function)
      ("rename" function)
      ("return" reserved)
      ("rewind" function)
      ("scanf" function)
      ("SEEK_CUR" variable)
      ("SEEK_END" variable)
      ("SEEK_SET" variable)
      ("setbuf" function)
      ("setjmp" function)
      ("setlocale" function)
      ("setvbuf" function)
      ("short" type)
      ("signal" function)
      ("sin" function)
      ("signed" type)
      ("sinh" function)
      ("size_t" type)
      ("sizeof" function)
      ("sleep" function)
      ("sprintf" function)
      ("sqrt" function)
      ("srand" function)
      ("sscanf" function)
      ("ssignal" function)
      ("static" reserved)
      ("static_cast" reserved)
      ("__STDC__" variable)
      ("stderr" variable)
      ("stdin" variable)
      ("stdout" variable)
      ("strcat" function)
      ("strchr" function)
      ("strcmp" function)
      ("strcoll" function)
      ("strcpy" function)
      ("strcspm" function)
      ("strerror" variable)
      ("strftime" function)
      ("strlen" function)
      ("strncat" function)
      ("strncmp" function)
      ("strncpy" function)
      ("strpbrk" function)
      ("strpos" function)
      ("strrchr" function)
      ("strrpbrk" function)
      ("strrpos" function)
      ("strspn" function)
      ("strstr" function)
      ("strtod" function)
      ("strtok" function)
      ("strtol" function)
      ("strtoul" function)
      ("struct" type)
      ("strxfrm" function)
      ("switch" reserved)
      ("sys_errlist" variable)
      ("system" function)
      ("tan" function)
      ("tanh" function)
      ("template" reserved)
      ("this" reserved)
      ("throw" reserved)
      ("time" function)
      ("__TIME__" variable)
      ("time_t" type)
      ("times" function)
      ("tm" type)
      ("TMP_MAX" variable)
      ("tmpfile" function)
      ("tmpnam" function)
      ("toupper" function)
      ("try" reserved)
      ("typedef" reserved)
      ("typeid" reserved)
      ("ungetc" function)
      ("union" type)
      ("unix" function)
      ("unsigned" type)
      ("using" reserved)
      ("vax" function)
      ("vfprintf" function)
      ("virtual" reserved)
      ("void" type)
      ("volatile" type)
      ("vprintf" function)
      ("wchar_t" type)
      ("wcstombs" function)
      ("wctomb" function)
      ("while" reserved)
      )))






;;;; C Names for Lisp Symbols




;;; The following section of code implements translations of Lisp symbols to C
;;; identifier names.  It works by first determining the base name of the C
;;; identifier, a string that is no more than 30 characters long and that
;;; contains only legal characters for C identifiers.  Then a search is made
;;; through the given C namespace to determine whether or not the base name is
;;; unique.  If so, the base name is returned.  Otherwise, the integers 1
;;; through 9 are attempted to be appended to the string, searching for a unique
;;; name.  If one cannot be found through 9 attempts, the string is truncated to
;;; no more than 29 characters, and the process continues starting with 10.

;;; The base name is determined from the Lisp symbol as follows.  First all
;;; characters in a copy of the symbol-name are transformed into a downcased
;;; version, containing only valid characters for C identifiers.  If the
;;; resulting string is no longer than 30 characters, it is returned.  If not,
;;; the string is cut up into its constituent words, using hyphens (actually
;;; underscores at this point) as delimiters.  While the entire string is too
;;; long, attempt to replace each constituent word with its mnemonic, starting
;;; with the first word.  After all possible replacements have been performed,
;;; concatenate the constituents back together again, and then begin removing
;;; vowels, starting from the back of the identifier, until the string is short
;;; enough.  If all vowels are removed and the string is still too long,
;;; truncate it, begin truncating alphabetic characters from the tail of the
;;; string until it is short enough.

;;; A special case is included for uninterned symbols with names that begin with
;;; G and then only contain numeric characters (i.e. symbols returned from
;;; gensym).  The base name for this is always "g".

;;; The macro `valid-c-identifier-char-p' takes a character and returns whether
;;; or not that character may be used within the C identifier.  The valid
;;; characters are the alphabetic, numeric and underscore characters.

(defmacro valid-c-identifier-char-p (character)
  `(char-bit-on-p valid-c-identifier-chars ,character))

(defparameter valid-c-identifier-chars
  (make-char-bit-vector
    '((#\a #\z)
      (#\A #\Z)
      (#\0 #\9)
      #\_)))




;;; The constant `maximum-c-identifier-length' holds the longest string length
;;; that should be allowed to be emitted.  CaRM3, p. 15 states that ANSI C
;;; requires at least 31 significant characters.  In practice, the VAX linker
;;; seems to be the low program on the totem pole, using 32 characters.  We'll
;;; stick with the ANSI limit, 31.  Note that generated base names must be one
;;; character shorter than this, since we assume that we will have up to 9
;;; collisions in common cases.

;;; Since the VAX is less and less of an issue, we'll up this to a value more in
;;; keeping with Lisp coding style, 63.  If this is a problem, then we'll have
;;; to dump it back down again.  -jallard 10/31/99

(defconstant maximum-c-identifier-length 63)




;;; The function `legalize-as-c-identifier' takes a string and modifies
;;; individual characters within it so that all characters are valid for use as
;;; C identifiers.  There are defined translations for some invalid characters,
;;; such as hyphen to underscore and question-mark to p.  For all characters not
;;; having defined translations, a capital Z will be used.  The argument string
;;; is always returned.

(defun legalize-as-c-identifier (string)
  (unless (simple-string-p string)
    (error "Identifier string wasn't simple."))
  (loop with char 
	with length fixnum = (length string)
	for index fixnum from 0
	while (< index length)
	do
    (setq char (schar string index))
    (unless (valid-c-identifier-char-p char)
      (case (the character char)
	((#\-)
	 (setf (schar string index) #\_))
	((#\?)
	 (setf (schar string index) #\P))
	((#\*)
	 (setf (schar string index) #\S))
	((#\< #\>)
	 (setq string 
	       (if (and (< (1+ index) length)
			(char= (schar string (1+ index)) #\=))
		   (replace-chars-at-index
		     string index 2 (if (char= char #\<) "LE" "GE"))
		   (replace-chars-at-index
		     string index 1 (if (char= char #\<) "LT" "GT"))))
	 (incf index)
	 (setq length (length string)))
	((#\=)
	 (setq string (replace-chars-at-index string index 1 "EQ"))
	 (incf index)
	 (setq length (length string)))
	(t
	 (setf (schar string index) #\Z)))))
  string)

(defun replace-chars-at-index (old-string index number-to-replace replacement)
  (concatenate
    'string
    (subseq old-string 0 index)
    replacement
    (subseq old-string (+ index number-to-replace))))




;;; The parameters `c-name-mnemonic-alist' and `c-name-mnemonic-hash' are used
;;; to store mnemonics from long words used in Lisp symbols to short words that
;;; should be used in C identifiers.  The input Lisp words should not contain
;;; any hyphens and should be all lowercase.

;;; The hash parameter is used to store a hash table that is generated when the
;;; first use is made of this facility.  This is done to delay the generation of
;;; the hash table until the last moment, since it is a well known bug in Lucid
;;; that hash tables cannot be stored in world saves.

(defparameter c-name-mnemonic-alist
  '(("procedure" .                      "proc")
    ("expression" .                     "expr")
    ("designation" .                    "desgn")
    ("specification" .                  "spec")
    ("string" .                         "str")
    ("fixnum" .                         "fix")
    ("system" .                         "sys")
    ("revision" .                       "rev")
    ("release" .                        "rel")
    ("version" .                        "ver")
    ("value" .                          "val")
    ("quality" .                        "qual")
    ("control" .                        "ctrl")
    ("escape" .                         "esc")
    ("package" .                        "pkg")

    ("gensym" .                         "gsym")
    ("character" .                      "char")
    ("symbol" .                         "sym")
    ("conversion" .                     "cnvsn")
    ("function" .                       "func")
    ("current" .                        "crnt")
    
    ))

(defparameter c-name-mnemonic-hash nil)

    


;;; The function `shorten-c-identifier-using-mnemonics' takes a string and
;;; returns a new string which has been shortened by substituting mnemonics for
;;; words within the string delineated by underscores.  Note that this scheme is
;;; somewhat less flexible than the one used to generate 6 character filename
;;; mnemonics since it requires word delineation.  I made this choice since I
;;; expect that there will be hundreds of mnemonics here, not a dozen, and since
;;; this operation will need to run tens of thousands of times during a
;;; translation, not a couple hundred.

;; Note that the routine to cut the identifier up into words may lose a trailing
;; underscore.  -jra 9/7/95

(defun shorten-c-identifier-using-mnemonics (string)
  (unless c-name-mnemonic-hash
    (setq c-name-mnemonic-hash (make-hash-table :test #'equal))
    (loop for (long . short) in c-name-mnemonic-alist do
      (setf (gethash long c-name-mnemonic-hash) short)))
  (let ((name-list
	  (loop with next-break?
		with length = (length string)
		for start = 0 then (1+ next-break?)
		while (< start length)
		collect
		(if (setq next-break?
			  (loop for index from start below length
				when (char= (schar string index) #\_)
				return index))
		    (subseq string start next-break?)
		    (subseq string start))
		while next-break?)))
    (loop with mnemonic?
	  for name-cons on name-list
	  while (> (+ (length (car name-list))
		      (loop for string in (cdr name-list)
			    sum (1+ (length string))))
		   (- maximum-c-identifier-length 2))
	  do
      (setq mnemonic? (gethash (car name-cons) c-name-mnemonic-hash))
      (when mnemonic?
	(setf (car name-cons) mnemonic?)))
    (loop with result
	    = (make-string
		(+ (loop for string in name-list sum (length string))
		   (1- (length name-list))))
	  with index fixnum = 0
	  for first-string? = t then nil
	  for string in name-list
	  for string-length = (length string)
	  do
      (unless first-string?
	(setf (schar result index) #\_)
	(incf index))
      (loop for string-index from 0 below string-length do
	(setf (schar result (+ index string-index))
	      (schar string string-index)))
      (incf index string-length)
	  finally (return result))))




;;; The function `shorten-c-identifier-by-removing-chars' takes a string that is
;;; too long to be used as a C identifier.  It returns a string that is short
;;; enough by removing vowels and underscores from the string starting at the
;;; end.  If that isn't good enough, it removes non-numeric characters from the
;;; tail of the string.

(defparameter vowel-chars
  (make-char-bit-vector '("aeiou")))

(defparameter removable-chars
  (make-char-bit-vector '((#\a #\z) #\_)))

(defun remove-c-identifier-chars (reverse-char-list char-bit-vector)
  (let ((length (length reverse-char-list)))
    (loop while (and (> length (- maximum-c-identifier-length 2))
		     (char-bit-on-p char-bit-vector
				    (car reverse-char-list)))
	  do
      (setq reverse-char-list (cdr reverse-char-list))
      (decf length))
    (loop with last-cons = reverse-char-list
	  for this-cons = (cdr last-cons)
	  while (and (> length (- maximum-c-identifier-length 2))
		     this-cons)
	  do
      (cond ((char-bit-on-p char-bit-vector (car this-cons))
	     (setq this-cons (cdr this-cons))
	     (setf (cdr last-cons) this-cons)
	     (decf length))
	    (t
	     (setq last-cons this-cons))))
    reverse-char-list))

(defun shorten-c-identifier-by-removing-chars (string)
  (let ((reverse-char-list nil)
	(length (length string)))
    (loop for index from 0 below length do
      (push (schar string index) reverse-char-list))
    (setq reverse-char-list
	  (remove-c-identifier-chars reverse-char-list vowel-chars))
    (when (> (length reverse-char-list) (- maximum-c-identifier-length 2))
      (setq reverse-char-list
	    (remove-c-identifier-chars reverse-char-list removable-chars)))
    ;; Never end with a trailing underscore.
    (when (char= (cons-car reverse-char-list) #\_)
      (setq reverse-char-list (cons-cdr reverse-char-list)))
    ;; Never begin with an underscore.
    (let ((char-list (nreverse reverse-char-list)))
      (loop while (char= (cons-car char-list) #\_) do
	(setq char-list (cons-cdr char-list)))
      ;; Reconstruct a string result.
      (setq length (length char-list))
      (loop with result = (make-string length)
	    for index fixnum from 0 below length
	    for char in char-list
	    do
	(setf (schar result index) char)
	    finally (return result)))))
	  
				   
	      


;;; The macro `c-base-string' computes and returns a base string for the given
;;; symbol.  Note that if this symbol has previously had a base string computed
;;; for it, then the previously computed string is returned.  If none currently
;;; exists, then compute-new-c-base-string is called.

(defmacro c-base-string (symbol)
  (if (symbolp symbol)
      `(or (get ,symbol 'c-base-string)
	   (compute-new-c-base-string ,symbol))
      (let ((sym (gensym)))
	`(let ((,sym ,symbol))
	   (or (get ,sym 'c-base-string)
	       (compute-new-c-base-string ,sym))))))

(defun compute-new-c-base-string (symbol)
  (setf (get symbol 'c-base-string)
	(compute-new-c-base-string-1 symbol)))

(defparameter alphabetic-chars
  (make-char-bit-vector '((#\a #\z) (#\A #\Z))))

(defmacro alphabetic-char-p (character)
  `(char-bit-on-p alphabetic-chars ,character))

(defun compute-new-c-base-string-1 (symbol)
  (let* ((symbol-name (symbol-name symbol))
	 (new-name symbol-name))
    (cond
      ((and (null (symbol-package symbol))
	    (char= (schar symbol-name 0) #\G)
	    (loop for index from 1 below (length symbol-name)
		  always (digit-char-p (schar symbol-name index))))
       (setq new-name "g"))
      (t
       ;; Downcase the string.  Note that string-downcase could return its
       ;; argument in some cases, so ensure that new-name isn't eq to the
       ;; symbol-name.
       (setq new-name (string-downcase symbol-name))
       (when (eq new-name symbol-name)
	 (setq new-name (copy-seq new-name)))
       ;; Prefix if the first character isn't alphabetic.
       (when (digit-char-p (schar new-name 0))
	 (setq new-name (concatenate 'string "N" new-name)))
       ;; Change non-C identifier characters.
       (setq new-name (legalize-as-c-identifier new-name))
       ;; When the string is too long, attempt to shorten it by applying
       ;; mnemonics.
       (when (> (length new-name) (- maximum-c-identifier-length 2))
	 (setq new-name (shorten-c-identifier-using-mnemonics new-name)))
       ;; When the string is still too long, shorten it by removing vowels and
       ;; underscores and by truncating.  This function is guaranteed to return
       ;; a string that is short enough.
       (when (> (length new-name) (- maximum-c-identifier-length 2))
	 (setq new-name (shorten-c-identifier-by-removing-chars new-name)))))
    ;; Ensure that the result is a valid C identifier.
    (unless (and (> (length new-name) 0)
		 (alphabetic-char-p (schar new-name 0)))
      (error "Produced illegal C identifier as base name:~%  ~s from ~s"
	     new-name symbol))
    new-name))




;;; The function `c-identifier-for-string' takes a string, an identifier, a
;;; short description, a namespace into which to insert the identifier, and
;;; another namespace in which the identifier must be unique.  The second
;;; namespace must be either EQ to or inherit from the first namespace.

;;; When emitting large constants, the function c-identifier-for-string can be
;;; called very often with the strings representing names for cons, string, and
;;; simple-vector constants.  In these cases, c-identifier-for-string keeps
;;; track of the last index for that name that was handed out, and it begins its
;;; search at the next index.  In cases where there were hundreds or thousands
;;; of constants in a single file, this function takes on polynomial order
;;; execution times searching for an unused name.  This optimization turns this
;;; operation back into one that is typically constant.

;;; The short description should be a list whose first element is a symbol
;;; describing the identified thing, either function, variable, macro, or
;;; reserved.  The macro `c-identifier-for-symbol' performs the same operation
;;; after fetching the c-base-string for the given symbol.

(defmacro c-identifier-for-symbol
    (symbol description id-namespace referencing-namespace)
  `(c-identifier-for-string
     (c-base-string ,symbol) ,description ,id-namespace ,referencing-namespace))

(defvar last-colliding-namespace nil)

(defvar last-index-for-string-alist nil)

(defun c-identifier-for-string
    (string description id-namespace referencing-namespace)
  (cond
    ((null (c-namespace-get-identifier referencing-namespace string))
     (setf (c-namespace-get-identifier id-namespace string)
	   description)
     string)
    (t
     (loop with length = (length string)
	   with last-index-description?
	     = (when (eql (search "_const" string) (- length 6))
		 (when (not (eq id-namespace last-colliding-namespace))
		   (setq last-colliding-namespace id-namespace)
		   (setq last-index-for-string-alist nil))
		 (or (assoc string last-index-for-string-alist :test #'string=)
		     (let ((new (cons string 0)))
		       (push new last-index-for-string-alist)
		       new)))
	   with index = (if last-index-description?
			    (+ (cdr last-index-description?) 1)
			    1)
	   for digits from (ceiling (log (+ index 1) 10))
	   for index-limit = (expt 10 digits)
	   for base-string
	       = (if (<= (+ length digits 1) maximum-c-identifier-length)
		     string
		     (subseq string 0 (- maximum-c-identifier-length digits 1)))
	   do
       (loop while (< index index-limit)
	     do
	 (let ((new-string (format nil "~a_~d" base-string index)))
	 (when (null (c-namespace-get-identifier
		       referencing-namespace new-string))
	   (setf (c-namespace-get-identifier id-namespace new-string)
		 description)
	   (when last-index-description?
	     (setf (cdr last-index-description?) index))
	   (return-from c-identifier-for-string new-string))
	 (incf index)))))))




;;; The function `c-identifier-for-function' takes a Lisp symbol, a C namespace
;;; for the identifier and a C namespace from which the reference is being made.
;;; It returns a C identifier string.  An attempt is first made to look up an
;;; existing identifier for the function named by the given symbol.  If none can
;;; be found, a new one is generated and proclaimed as the identifier for this
;;; function.  Note that the declaration function-c-identifier is used to cache
;;; this value.

;; Note that there is an interesting potential here for somewhat weird names to
;; be generated within this function.  If the referencing namespace already has
;; in it an identifier that would be the natural C name for this function, then
;; an X_1 style name will be generated to disambiguate.  For example, this could
;; happen within a function that bound a variable foo and then made a forward
;; referencing call to the function foo.  The identifier for the function would
;; be foo_1, since the identifier foo was already used within the referencing
;; namespace, even though it was free within the global namespace.  This should
;; be rare.  -jra 11/16/95

(defun c-identifier-for-function
    (lisp-function id-namespace referencing-namespace)
  (or (function-decl lisp-function 'function-c-identifier)
      (let ((foreign-identifier?
	      (function-decl lisp-function 'foreign-c-identifier)))
	(cond
	  (foreign-identifier?
	   (c-identifier-for-foreign-function
	     lisp-function foreign-identifier?)
	   foreign-identifier?)
	  (t
	   (let ((new-identifier
		   (c-identifier-for-symbol
		     lisp-function (list 'function lisp-function)
		     id-namespace referencing-namespace)))
	     (tl:proclaim
	       (list 'function-c-identifier lisp-function new-identifier))
	     new-identifier))))))




;;; The function `c-identifier-for-class' takes a Lisp symbol and returns a
;;; string naming the C identifier for the typedef for this type.

(defun c-identifier-for-class (lisp-class id-namespace referencing-namespace)
  (let* ((info (or (class-info lisp-class)
		   (translation-error "Undefined structure or class ~s." 
				      lisp-class)))
	 (c-name? (struct-c-type-name info)))
    (unless c-name?
      (setq c-name? (c-identifier-for-symbol lisp-class (list 'class lisp-class)
					     id-namespace referencing-namespace))
      (setf (struct-c-type-name info) c-name?))
    c-name?))




;;; The function `c-identifier-for-struct-slot' takes a Lisp symbol
;;; and returns a string naming the C identifier that should be used
;;; as the member name for a struct slot.

(defun c-identifier-for-struct-slot (slot id-namespace referencing-namespace)
  (c-identifier-for-symbol 
    slot (list 'slot slot) id-namespace referencing-namespace))




;;; The function `c-identifier-for-foreign-function' is called when registering
;;; a foreign function name in the C namespaces.  While translating, this
;;; function should ensure that the given string is registered as the
;;; function-c-identifier for the given Lisp symbol, and that the C name is
;;; reserved in the global-c-namespace.

(defun c-identifier-for-foreign-function (lisp-name c-name)
  (when (eval-feature :translator)
    (let* ((current-name? (function-decl lisp-name 'function-c-identifier))
	   (name-match? (and current-name? (string= current-name? c-name)))
	   (current-description?
	     (c-namespace-get-identifier *global-c-namespace* c-name)))
      (unless name-match?
	(when current-name?
	  (translation-warning "Redefining ~s from ~s to foreign function ~s"
			       lisp-name current-name? c-name))
	(tl:proclaim (list 'function-c-identifier lisp-name c-name))
	(unless current-description?
	  (setf (c-namespace-get-identifier *global-c-namespace* c-name)
		(list 'function lisp-name))))))
  nil)




;;; The function `c-identifier-for-variable' takes a Lisp symbol naming a global
;;; variable, a namespace for any new C identifier, and a referencing namespace.
;;; It returns a C identifier string.  It first attempts to look up an existing
;;; identifier for this variable, but if none has been made it generates a new
;;; one, proclaims it as the identifier for the symbol, and returns the new one.
;;; Note that the declaration variable-c-identifier is used to cache this value.

(defun c-identifier-for-variable
    (lisp-variable id-namespace referencing-namespace)
  (or (variable-decl lisp-variable 'variable-c-identifier)
      (let ((new-identifier
	      (c-identifier-for-symbol
		lisp-variable '(variable) id-namespace referencing-namespace)))
	(tl:proclaim (list 'variable-c-identifier lisp-variable new-identifier))
	new-identifier)))




;;; The function `clear-c-name-declarations' is called to clear out the global
;;; namespace of any cached c-identifiers-for-variable and
;;; c-identifiers-for-function stored within the variable-c-identifier and
;;; function-c-identifier declarations.  It also clears any cached c-identifiers
;;; for class typedefs.  This is called at the end of each translate.

(defun clear-c-name-declarations ()
  (let* ((new-decls nil)
	 (current-decl 'variable-c-identifier)
	 (mapper #'(lambda (symbol c-name-string)
		     (when c-name-string
		       (setq new-decls
			     (cons (list current-decl symbol nil)
				   new-decls)))
		     nil)))
    (map-over-declarations mapper :variable 'variable-c-identifier)
    (setq current-decl 'function-c-identifier)
    (map-over-declarations mapper :function 'function-c-identifier)
    ;; Assert these one at a time, since collect-environment-augmentations is
    ;; order polynomial on the number of declarations.  -jallard 11/11/97
    (loop for decl in new-decls do
      (proclaim-decl-list (list decl)))
    ;; Clear class names.
    (loop for class in *all-classes*
	  for info = (class-info class)
	  do
      (setf (struct-c-type-name info) nil))
    nil))




;;; The function `reserve-foreign-function-identifiers' is called when first
;;; starting a translation.  It will reserve in the C namespace any identifiers
;;; which are declared as foreign functions so that new translations will not
;;; collide with them.

(defun reserve-foreign-function-identifiers ()
  (map-over-declarations
    #'(lambda (lisp-symbol c-identifier-string)
	(c-identifier-for-foreign-function lisp-symbol c-identifier-string))
    :function
    'foreign-c-identifier))



;;; The function `reserve-global-identifiers' will iterate over all functions,
;;; variables, structures, and classes defined within the given system and it
;;; will reserve the identifiers for those symbols.  Note that during
;;; translations we can allocate function names appropriately within scopes that
;;; already define C identifiers that would clash with the natural C name (in
;;; these cases the function gets a "_1" postpended name) but this approach
;;; risks variable shadowing warnings within other functions that have already
;;; used the name that some function will eventually get.  By getting all such
;;; names now, we prevent all varieties of this kind of warning.

;;; Note that the names of the structure and class slot accessors are also
;;; reserved.

(defun reserve-global-identifiers (system-name)
  (let ((lisp-class-symbols nil)
	(lisp-function-symbols nil)
	(lisp-variable-symbols nil))

    ;; Classes.
    (map-over-declarations
      #'(lambda (lisp-symbol home)
	  (when (eq (cons-car home) system-name)
	    (push lisp-symbol lisp-class-symbols)))
      :function
      'class-home)
    (setq lisp-class-symbols
      (sort lisp-class-symbols #'string< :key #'symbol-name))
    (loop for class in lisp-class-symbols do
      (compute-c-type-for-class class))

    ;; Functions
    (map-over-declarations
      #'(lambda (lisp-symbol home)
	  (when (eq (cons-car home) system-name)
	    (push lisp-symbol lisp-function-symbols)))
      :function
      'function-home)
    (setq lisp-function-symbols
	  (sort lisp-function-symbols #'string< :key #'symbol-name))
    (loop for lisp-symbol in lisp-function-symbols do
      (c-identifier-for-function
	lisp-symbol *global-c-namespace* *global-c-namespace*))
    (setq lisp-function-symbols nil)

    ;; Variables.
    (map-over-declarations
      #'(lambda (lisp-symbol home)
	  (when (eq (cons-car home) system-name)
	    (push lisp-symbol lisp-variable-symbols)))
      :variable
      'variable-home)
    (setq lisp-variable-symbols
	  (sort lisp-variable-symbols #'string< :key #'symbol-name))
    (loop for lisp-symbol in lisp-variable-symbols do
      (c-identifier-for-variable
	lisp-symbol *global-c-namespace* *global-c-namespace*))

    nil))

	
	      
