/*****************************************************************************/
/**  Joaquin Martinez Peiro                                                 **/
/**  V. 20                                                                  **/
/*****************************************************************************/

%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define retornar(x) {if (verbosidad) ECHO; return x;}
%}
/*----------------------------------------------------------------------
  Dado que las funciones "input()" y "unput(c)" son costosas y no las
  utilizaremos, evitaremos que se generen automaticamente sus rutinas
  correspondientes desactivandolas mediante las siguientes opciones:
----------------------------------------------------------------------*/
%option noinput
%option nounput
/*----------------------------------------------------------------------
  Para mantener el numero de linea actual en la variable global yylineno
----------------------------------------------------------------------*/
%option yylineno

delimitador     [ \t\v\f]+
comentario      \/\/.*\n
numero          [0-9]+("."[0-9]*)?|[0-9]*"."[0-9]+
identificador   [a-zA-Z][_a-zA-Z0-9]*


%%

{delimitador}   {if (verbosidad) ECHO; }      
"\n"            {if (verbosidad) ECHO; }   
{comentario}    {if (verbosidad) ECHO; }  
"="             { retornar(ASIG_); }
"+"             { retornar(SUMA_); }
"++"            { retornar(SUMA2_); }
"--"            { retornar(RESTA2_); }
"-"             { retornar(RESTA_); }
"*"             { retornar(MULT_); }
"/"             { retornar(DIV_); }
"<"             { retornar(MENOR_); }
">"             { retornar(MAYOR_); }
"<="            { retornar(MENORIG_); }
">="            { retornar(MAYORIG_); }
"=="            { retornar(IGUAL2_); }
"!="            { retornar(NOTIGUAL_); }
"!"             { retornar(NOT_); }
"&&"            { retornar(AND_); }
"||"            { retornar(OR_); }
"{"             { retornar(LLAVE1_); }
"}"             { retornar(LLAVE2_); }
"["             { retornar(COR1_); }
"]"             { retornar(COR2_); }
"("             { retornar(PAR1_); }
")"             { retornar(PAR2_); }
";"             { retornar(PCOMA_); }
","             { retornar(COMA_); }
bool            { retornar(BOOL_); }
int             { retornar(INT_); }
read            { retornar(READ_); }
print           { retornar(PRINT_); }
if              { retornar(IF_); }
else            { retornar(ELSE_); }
for             { retornar(FOR_); }
return          { retornar(RETURN_); }
false           { retornar(FALSE_); }
true            { retornar(TRUE_); }
{numero}        {yylval.aux  = atoi(yytext);   retornar(CTE_);}
{identificador} {yylval.ident = strdup(yytext); retornar(ID_);}
.               { printf("Error, caracter desconocido"); }
%%
/*****************************************************************************/
