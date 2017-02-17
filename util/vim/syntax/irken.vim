" Vim syntax file
" Language:     irken
" Last Change:  2015 Mar 28
" Maintainer:   Mike Burr <mburr@unintuitive.org>
" Based upon scheme.vim written by:      Dirk van Deun <dirk@igwe.vub.ac.be>

" Initializing:

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

let s:cpoptions_save = &cpoptions
set cpoptions&vim

syn case ignore

syn match       irkenAssignment ![a-z!$%&*/:<>?^_~+@#%-]*=[a-z!$%&*/:<>?^_~+@#%-]*!
syn match       irkenBoolean   "#[tf]"

syn match       irkenError     ,[a-z!$%&*/:<>?^_~+@%-][-a=z!$%&*/:<>?^_~0-9+.@%]*[^-a-z!'$%&*/:<=>?^_~0-9+.@ \t\[\]{}()#";]\+[^ \t\[\]{}()";]*,
syn match       irkenError     ![^ \t{}()\[\]";]*!
syn match       irkenError     "[^}]?)"


" Quoted and backquoted stuff

syn region irkenQuoted matchgroup=Delimiter start="['`]" end=![ \t{}()\[\]";]!me=e-1 contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenQuoted matchgroup=Delimiter start="['`]#(" matchgroup=Delimiter end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenStrucRestricted matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenStrucRestricted matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenStrucRestricted matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenStrucRestricted matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenUnquote matchgroup=Delimiter start="," end=![ \t\[\]{}()";]!me=e-1 contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenUnquote matchgroup=Delimiter start=",@" end=![ \t\[\]{}()";]!me=e-1 contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenUnquote matchgroup=Delimiter start=",(" end=")" contains=ALL
syn region irkenUnquote matchgroup=Delimiter start=",@(" end=")" contains=ALL

syn region irkenUnquote matchgroup=Delimiter start=",#(" end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenUnquote matchgroup=Delimiter start=",@#(" end=")" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

syn region irkenUnquote matchgroup=Delimiter start=",\[" end="\]" contains=ALL
syn region irkenUnquote matchgroup=Delimiter start=",@\[" end="\]" contains=ALL

syn region irkenUnquote matchgroup=Delimiter start=",#\[" end="\]" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc
syn region irkenUnquote matchgroup=Delimiter start=",@#\[" end="\]" contains=ALLBUT,irkenStruc,irkenSyntax,irkenFunc

" R5RS irken Functions and Syntax:

if version < 600
  set iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
else
  setlocal iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
endif

syn keyword irkenSyntax lambda and or if cond case define let let* letrec
syn keyword irkenSyntax begin do delay set! else =>
syn keyword irkenSyntax quote quasiquote unquote unquote-splicing
syn keyword irkenSyntax define-syntax let-syntax letrec-syntax syntax-rules
" R6RS
syn keyword irkenSyntax define-record-type fields protocol

syn keyword irkenFunc not boolean? eq? eqv? equal? pair? cons car cdr set-car!
syn keyword irkenFunc set-cdr! caar cadr cdar cddr caaar caadr cadar caddr
syn keyword irkenFunc cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr
syn keyword irkenFunc cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
syn keyword irkenFunc cddaar cddadr cdddar cddddr null? list? list length
syn keyword irkenFunc append reverse list-ref memq memv member assq assv assoc
syn keyword irkenFunc symbol? symbol->string string->symbol number? complex?
syn keyword irkenFunc real? rational? integer? exact? inexact? = < > <= >=
syn keyword irkenFunc zero? positive? negative? odd? even? max min + * - / abs
syn keyword irkenFunc quotient remainder modulo gcd lcm numerator denominator
syn keyword irkenFunc floor ceiling truncate round rationalize exp log sin cos
syn keyword irkenFunc tan asin acos atan sqrt expt make-rectangular make-polar
syn keyword irkenFunc real-part imag-part magnitude angle exact->inexact
syn keyword irkenFunc inexact->exact number->string string->number char=?
syn keyword irkenFunc char-ci=? char<? char-ci<? char>? char-ci>? char<=?
syn keyword irkenFunc char-ci<=? char>=? char-ci>=? char-alphabetic? char?
syn keyword irkenFunc char-numeric? char-whitespace? char-upper-case?
syn keyword irkenFunc char-lower-case?
syn keyword irkenFunc char->integer integer->char char-upcase char-downcase
syn keyword irkenFunc string? make-string string string-length string-ref
syn keyword irkenFunc string-set! string=? string-ci=? string<? string-ci<?
syn keyword irkenFunc string>? string-ci>? string<=? string-ci<=? string>=?
syn keyword irkenFunc string-ci>=? substring string-append vector? make-vector
syn keyword irkenFunc vector vector-length vector-ref vector-set! procedure?
syn keyword irkenFunc apply map for-each call-with-current-continuation
syn keyword irkenFunc call-with-input-file call-with-output-file input-port?
syn keyword irkenFunc output-port? current-input-port current-output-port
syn keyword irkenFunc open-input-file open-output-file close-input-port
syn keyword irkenFunc close-output-port eof-object? read read-char peek-char
syn keyword irkenFunc write display newline write-char call/cc
syn keyword irkenFunc list-tail string->list list->string string-copy
syn keyword irkenFunc string-fill! vector->list list->vector vector-fill!
syn keyword irkenFunc force with-input-from-file with-output-to-file
syn keyword irkenFunc char-ready? load transcript-on transcript-off eval
syn keyword irkenFunc dynamic-wind port? values call-with-values
syn keyword irkenFunc irken-report-environment null-environment
syn keyword irkenFunc interaction-environment
" R6RS
syn keyword irkenFunc make-eq-hashtable make-eqv-hashtable make-hashtable
syn keyword irkenFunc hashtable? hashtable-size hashtable-ref hashtable-set!
syn keyword irkenFunc hashtable-delete! hashtable-contains? hashtable-update!
syn keyword irkenFunc hashtable-copy hashtable-clear! hashtable-keys
syn keyword irkenFunc hashtable-entries hashtable-equivalence-function hashtable-hash-function
syn keyword irkenFunc hashtable-mutable? equal-hash string-hash string-ci-hash symbol-hash
syn keyword irkenFunc find for-all exists filter partition fold-left fold-right
syn keyword irkenFunc remp remove remv remq memp assp cons*

" ... so that a single + or -, inside a quoted context, would not be
" interpreted as a number (outside such contexts, it's a irkenFunc)


syn match       irkenDelimiter !\.[ \t\[\]{}()";]!me=e-1
syn match       irkenDelimiter !\.$!
" ... and a single dot is not a number but a delimiter

syn match       irkenDelimiter ![ '(\t]*->[ )\t]*!

" This keeps all other stuff unhighlighted, except *stuff* and <stuff>:

syn match       irkenOther     ,[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,

syn match       irkenOther     "\.\.\."
syn match       irkenError     !\.\.\.[^ \t\[\]{}()";]\+!
" ... a special identifier

syn match       irkenConstant  ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]\+\*[ \t\[\]{}()";],me=e-1
syn match       irkenConstant  ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]\+\*$,
syn match       irkenError     ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]{}()";]\+[^ \t\[\]{}()";]*,

syn match       irkenConstant  ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[ \t\[\]{}()";],me=e-1
syn match       irkenConstant  ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>$,
syn match       irkenError     ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]{}()";]\+[^ \t\[\]{}()";]*,

syn region irkenRecord matchgroup=Delimiter start="{" matchgroup=Delimiter end="}" contains=ALL

" Non-quoted lists, and strings:

syn region irkenStruc matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
syn region irkenStruc matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALL

syn region irkenStruc matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region irkenStruc matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALL

" Simple literals:
syn region irkenString start=+\%(\\\)\@<!"+ skip=+\\[\\"]+ end=+"+ contains=@Spell

" Comments:

syn match       irkenComment   ";.*$" contains=@Spell


" Writing out the complete description of irken numerals without
" using variables is a day's work for a trained secretary...

syn match       irkenOther         ![+-][ \t\[\]{}()";]!me=e-1
syn match       irkenOther         ![+-]$!

syn match       irkenNumber    "[-#+.]\=[0-9][-#+/0-9a-f@i.boxesfdl]*"

syn match       irkenUndefined "#u"
syn match       irkenError     !#[tf][^ \t\[\]{}()";]\+!

syn match       irkenError     !#\\.[^ \t\[\]{}()";]\+!
syn match       irkenError     !#\\space[^ \t\[\]{}()";]\+!
syn match       irkenError     !#\\newline[^ \t\[\]{}()";]\+!

" R6RS
syn match       irkenCharacter "#\\x[0-9a-fA-F]\+"
syn match       irkenCharacter "#\\return"
syn match       irkenCharacter "#\\[a-z%0-9^\.<>%]*"
syn match       irkenCharacter "#\\space"
syn match       irkenCharacter "#\\newline"
syn match       irkenCharacter "#\\\%(return\|tab\)"


if exists("b:is_mzirken") || exists("is_mzirken")
    " Mzirken extensions
    " multiline comment
    syn region  irkenComment start="#|" end="|#" contains=@Spell

    " #%xxx are the special Mzirken identifiers
    syn match irkenOther "#%[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
    " anything limited by |'s is identifier
    syn match irkenOther "|[^|]\+|"

    syn match   irkenCharacter "#\\\%(return\|tab\)"

    " Modules require stmt
    syn keyword irkenExtSyntax module require dynamic-require lib prefix all-except prefix-all-except rename
    " modules provide stmt
    syn keyword irkenExtSyntax provide struct all-from all-from-except all-defined all-defined-except
    " Other from Mzirken
    syn keyword irkenExtSyntax with-handlers when unless instantiate define-struct case-lambda syntax-case
    syn keyword irkenExtSyntax free-identifier=? bound-identifier=? module-identifier=? syntax-object->datum
    syn keyword irkenExtSyntax datum->syntax-object
    syn keyword irkenExtSyntax let-values let*-values letrec-values set!-values fluid-let parameterize begin0
    syn keyword irkenExtSyntax error raise opt-lambda define-values unit unit/sig define-signature
    syn keyword irkenExtSyntax invoke-unit/sig define-values/invoke-unit/sig compound-unit/sig import export
    syn keyword irkenExtSyntax link syntax quasisyntax unsyntax with-syntax

    syn keyword irkenExtFunc format system-type current-extension-compiler current-extension-linker
    syn keyword irkenExtFunc use-standard-linker use-standard-compiler
    syn keyword irkenExtFunc find-executable-path append-object-suffix append-extension-suffix
    syn keyword irkenExtFunc current-library-collection-paths current-extension-compiler-flags make-parameter
    syn keyword irkenExtFunc current-directory build-path normalize-path current-extension-linker-flags
    syn keyword irkenExtFunc file-exists? directory-exists? delete-directory/files delete-directory delete-file
    syn keyword irkenExtFunc system compile-file system-library-subpath getenv putenv current-standard-link-libraries
    syn keyword irkenExtFunc remove* file-size find-files fold-files directory-list shell-execute split-path
    syn keyword irkenExtFunc current-error-port process/ports process printf fprintf open-input-string open-output-string
    syn keyword irkenExtFunc get-output-string
    " exceptions
    syn keyword irkenExtFunc exn exn:application:arity exn:application:continuation exn:application:fprintf:mismatch
    syn keyword irkenExtFunc exn:application:mismatch exn:application:type exn:application:mismatch exn:break exn:i/o:filesystem exn:i/o:port
    syn keyword irkenExtFunc exn:i/o:port:closed exn:i/o:tcp exn:i/o:udp exn:misc exn:misc:application exn:misc:unsupported exn:module exn:read
    syn keyword irkenExtFunc exn:read:non-char exn:special-comment exn:syntax exn:thread exn:user exn:variable exn:application:mismatch
    syn keyword irkenExtFunc exn? exn:application:arity? exn:application:continuation? exn:application:fprintf:mismatch? exn:application:mismatch?
    syn keyword irkenExtFunc exn:application:type? exn:application:mismatch? exn:break? exn:i/o:filesystem? exn:i/o:port? exn:i/o:port:closed?
    syn keyword irkenExtFunc exn:i/o:tcp? exn:i/o:udp? exn:misc? exn:misc:application? exn:misc:unsupported? exn:module? exn:read? exn:read:non-char?
    syn keyword irkenExtFunc exn:special-comment? exn:syntax? exn:thread? exn:user? exn:variable? exn:application:mismatch?
    " Command-line parsing
    syn keyword irkenExtFunc command-line current-command-line-arguments once-any help-labels multi once-each

    " syntax quoting, unquoting and quasiquotation
    syn region irkenUnquote matchgroup=Delimiter start="#," end=![ \t\[\]{}()";]!me=e-1 contains=ALL
    syn region irkenUnquote matchgroup=Delimiter start="#,@" end=![ \t\[\]{}()";]!me=e-1 contains=ALL
    syn region irkenUnquote matchgroup=Delimiter start="#,(" end=")" contains=ALL
    syn region irkenUnquote matchgroup=Delimiter start="#,@(" end=")" contains=ALL
    syn region irkenUnquote matchgroup=Delimiter start="#,\[" end="\]" contains=ALL
    syn region irkenUnquote matchgroup=Delimiter start="#,@\[" end="\]" contains=ALL
    syn region irkenQuoted matchgroup=Delimiter start="#['`]" end=![ \t{}()\[\]";]!me=e-1 contains=ALL
    syn region irkenQuoted matchgroup=Delimiter start="#['`](" matchgroup=Delimiter end=")" contains=ALL
endif


if exists("b:is_chicken") || exists("is_chicken")
    " multiline comment
    syntax region irkenMultilineComment start=/#|/ end=/|#/ contains=@Spell,irkenMultilineComment

    syn match irkenOther "##[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
    syn match irkenExtSyntax "#:[-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"

    syn keyword irkenExtSyntax unit uses declare hide foreign-declare foreign-parse foreign-parse/spec
    syn keyword irkenExtSyntax foreign-lambda foreign-lambda* define-external define-macro load-library
    syn keyword irkenExtSyntax let-values let*-values letrec-values ->string require-extension
    syn keyword irkenExtSyntax let-optionals let-optionals* define-foreign-variable define-record
    syn keyword irkenExtSyntax pointer tag-pointer tagged-pointer? define-foreign-type
    syn keyword irkenExtSyntax require require-for-syntax cond-expand and-let* receive argc+argv
    syn keyword irkenExtSyntax fixnum? fx= fx> fx< fx>= fx<= fxmin fxmax
    syn keyword irkenExtFunc ##core#inline ##sys#error ##sys#update-errno

    " here-string
    syn region irkenString start=+#<<\s*\z(.*\)+ end=+^\z1$+ contains=@Spell

    if filereadable(expand("<sfile>:p:h")."/cpp.vim")
        unlet! b:current_syntax
        syn include @ChickenC <sfile>:p:h/cpp.vim
        syn region ChickenC matchgroup=irkenOther start=+(\@<=foreign-declare "+ end=+")\@=+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+foreign-declare\s*#<<\z(.*\)$+hs=s+15 end=+^\z1$+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenOther start=+(\@<=foreign-parse "+ end=+")\@=+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+foreign-parse\s*#<<\z(.*\)$+hs=s+13 end=+^\z1$+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenOther start=+(\@<=foreign-parse/spec "+ end=+")\@=+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+foreign-parse/spec\s*#<<\z(.*\)$+hs=s+18 end=+^\z1$+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+#>+ end=+<#+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+#>?+ end=+<#+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+#>!+ end=+<#+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+#>\$+ end=+<#+ contains=@ChickenC
        syn region ChickenC matchgroup=irkenComment start=+#>%+ end=+<#+ contains=@ChickenC
    endif

    " suggested by Alex Queiroz
    syn match irkenExtSyntax "#![-a-z!$%&*/:<=>?^_~0-9+.@#%]\+"
    syn region irkenString start=+#<#\s*\z(.*\)+ end=+^\z1$+ contains=@Spell
endif

" Synchronization and the wrapping up...

syn sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_irken_syntax_inits")
  if version < 508
    let did_irken_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink irkenSyntax           Statement
  HiLink irkenFunc             Function

  HiLink irkenString           String
  HiLink irkenCharacter        Character
  HiLink irkenNumber           Number
  HiLink irkenBoolean          Boolean

  HiLink irkenUndefined        Constant
  HiLink irkenAssignment       Operator

  HiLink irkenDelimiter        Delimiter
  HiLink irkenConstant         Constant

  HiLink irkenComment          Comment
  HiLink irkenMultilineComment Comment
  HiLink irkenError            Error

  HiLink irkenExtSyntax        Type
  HiLink irkenExtFunc          PreProc
  HiLink irkenExtIdentifers    Statement


  delcommand HiLink
endif

let b:current_syntax = "irken"

let &cpoptions = s:cpoptions_save
unlet s:cpoptions_save
