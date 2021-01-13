/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_ASIN_H_INCLUDED
# define YY_YY_ASIN_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    FALSE_ = 258,
    TRUE_ = 259,
    BOOL_ = 260,
    INT_ = 261,
    IF_ = 262,
    ELSE_ = 263,
    FOR_ = 264,
    READ_ = 265,
    PRINT_ = 266,
    RETURN_ = 267,
    ASIG_ = 268,
    SUMA_ = 269,
    SUMA2_ = 270,
    RESTA_ = 271,
    RESTA2_ = 272,
    MULT_ = 273,
    DIV_ = 274,
    NOTIGUAL_ = 275,
    AND_ = 276,
    OR_ = 277,
    NOT_ = 278,
    MENOR_ = 279,
    MAYOR_ = 280,
    IGUAL2_ = 281,
    MENORIG_ = 282,
    MAYORIG_ = 283,
    LLAVE1_ = 284,
    LLAVE2_ = 285,
    COR1_ = 286,
    COR2_ = 287,
    PAR1_ = 288,
    PAR2_ = 289,
    PCOMA_ = 290,
    COMA_ = 291,
    ID_ = 292,
    CTE_ = 293
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 12 "src/asin.y"

    char *ident;      /* Para el terminal "identificador" */
    int aux;          /* Para los no-terminales con atributo simple */
    EXPR expr;        /* Para los no terminales con expresion */
    ARGU arg;         /* Para los argumentos" */
    LISTA list;       /* Para GCI */

#line 104 "asin.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_ASIN_H_INCLUDED  */
