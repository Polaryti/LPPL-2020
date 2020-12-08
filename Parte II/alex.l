
%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define  retornar(x) {if (verbosidad) ECHO; return(x);}
%}

%option noinput
%option nounput
%option yylineno

delimitador [ \t\n]+
minus		[a-z]
mayus		[A-Z]
letra		{mayus}|{minus}
digito      [0-9]
id		    {minus}({letra}|{digito}|"_")*
entero      {digito}+
real1		{digito}*\.{digito}+
real2       {digito}+\.{digito}*
real        {real1}|{real2}
cte		    {entero}|{real}
comentario	"//".*

%%

{delimitador}   {if (verbosidad) ECHO ; }
"+"		        { retornar (MAS_) ; }
"++"		    { retornar (DMAS_) ; }
"-"             { retornar (MENOS_) ; }
"--"		    { retornar (DMENOS_) ; }
"*"             { retornar (POR_) ; }
"/"             { retornar (DIV_) ; }
"("             { retornar (APAR_) ; }
")"             { retornar (CPAR_) ; }
"{"             { retornar (ALLAVE_) ; }
"}"             { retornar (CLLAVE_) ; }
"["             { retornar (ACLAU_) ; }
"]"             { retornar (CCLAU_) ; }
"="             { retornar (IGUAL_) ; }
"=="            { retornar (DIGUAL_) ; }
"!="            { retornar (DIF_) ; }
"!"             { retornar (NEG_) ; }
"&&"            { retornar (AND_) ; }
"||"            { retornar (OR_) ; }
";"             { retornar (PCOMA_) ; }
","             { retornar (COMA_) ; }
">"             { retornar (MAY_) ; }
"<"             { retornar (MEN_) ; }
">="             { retornar (MAYIG_) ; }
"<="             { retornar (MENIG_) ; }
"for"           { retornar (FOR_) ; }
"if"            { retornar (IF_) ; }
"else"          { retornar (ELSE_) ; }
"read"          { retornar (READ_) ; }
"print"         { retornar (PRINT_) ; }
"return"    { retornar (RETURN_) ; }
"int"         { retornar (INT_) ; }
"bool"         { retornar (BOOL_) ; }
"true"         { retornar (TRUE_) ; }
"false"         { retornar (FALSE_) ; }
{id}            { retornar (ID_) ; }
{cte}        	{ retornar (CTE_) ; }
{comentario}	{ if (verbosidad) ECHO ; }
.		        { yyerror("Caracter desconocido") ;}
%%
