/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> LIN
%token <Support.Error.info> UN
%token <Support.Error.info> SPLIT
%token <Support.Error.info> AS
%token <Support.Error.info> IN
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> BOOL

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
   Syntax.command list.
*/

%start toplevel
%type < Syntax.command list > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { [] }
  | Command SEMI toplevel
      { let cmd = $1 in
          let cmds = $3 in
          cmd::cmds }

/* A top-level command */
Command :
    IMPORT STRINGV { (Import($2.v)) }
  | Term 
      { (let t = $1 in Eval(t, tmInfo t)) }

Term :
  | LCID
      { TmVar(Var $1.v, $1.i) }
  | LIN Boolean
      { TmBool(QLinear, $2, $1) }
  | UN Boolean
      { TmBool(QUnrestricted, $2, $1) }
  | IF Term THEN Term ELSE Term
      { TmIf($2, $4, $6, $1) }
  | LIN LT Term COMMA Term GT
      { TmPair(QLinear, $3, $5, $1) }
  | UN LT Term COMMA Term GT
      { TmPair(QUnrestricted, $3, $5, $1) }
  | SPLIT Term AS LCID COMMA LCID IN Term
      { TmSplit($2, Var $4.v, Var $6.v, $8, $1) }
  | LIN LAMBDA LCID COLON Type DOT Term
      { TmAbs(QLinear, Var $3.v, $5, $7, $1) }
  | UN LAMBDA LCID COLON Type DOT Term
      { TmAbs(QUnrestricted, Var $3.v, $5, $7, $1) }
  | Term Term
      { TmApp($1, $2, tmInfo $1) }

Boolean :
    TRUE
      { BTrue }
  | FALSE
      { BFalse }

Type :
    LIN PreType
      { TyQual(QLinear, $2) }
  | UN PreType
      { TyQual(QUnrestricted, $2) }

PreType :
    BOOL
      { PBool }
  | Type STAR Type
      { PPair($1, $3) }
  | Type ARROW Type
      { PFunc($1, $3) }

/*   */
