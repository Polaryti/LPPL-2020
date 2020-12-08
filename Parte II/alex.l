%{
#include <stdio.h>
#include <string.h>
#include "header.h"
#include "asin.h"

#define retornar(x) {if (verbosidad) ECHO; return(x);}
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

letra           	[a-zA-Z]
digito          	[0-9]
delimitador     	[ \t\n]
comentario      	\/\/.*
entero          	{digito}+
constante       	{digito}+(\.{digito}+)?
identificador  	 	{letra}({letra}|{digito}|_)*

%%

{delimitador} 		{ if (verbosidad) ECHO; }
{comentario}		{ if (verbosidad) ECHO; }
"+"					{ retornar(OPSUMA_); }
"-"					{ retornar(OPRESTA_); }
"*"					{ retornar(OPMULT_); }
"/"					{ retornar(OPDIV_); }
"&&"				{ retornar(OPAND_); }
"||"				{ retornar(OPOR_); }
"!"					{ retornar(OPNOT_); }
"++"				{ retornar(OPINCREMENTO_); }
"--"				{ retornar(OPDECREMENTO_); }

">"					{ retornar(COMPMAYOR_); }
"<"					{ retornar(COMPMENOR_); }
">="				{ retornar(COMPMAYORIG_); }
"<="				{ retornar(COMPMENORIG_); }
"=="				{ retornar(OPIGUAL_); }
"!="				{ retornar(OPNOTIGUAL_); }
"="					{ retornar(IGUAL_); }

"for"				{ retornar(FOR_); }
"if"				{ retornar(IF_); }
"else"				{ retornar(ELSE_); }

"int"				{ retornar(INT_); }
"bool"				{ retornar(BOOL_); }

"read"				{ retornar(READ_); }
"print"				{ retornar(PRINT_); }
"return"			{ retornar(RETURN_); }

{constante}			{ retornar(CTE_); }
{identificador}		{ retornar(ID_); }

"true"				{ retornar(TRUE_); }
"false"				{ retornar(FALSE_); }

"{"					{ retornar(LLAVE1_); }
"}"					{ retornar(LLAVE2_); }
"("					{ retornar(PARENTESIS1_); }
")"					{ retornar(PARENTESIS2_); }
"["					{ retornar(CORCHETE1_); }
"]"					{ retornar(CORCHETE2_); }
";"					{ retornar(SEMICOLON_); }
","					{ retornar(COMA_); }

. 					{ yyerror("Caracter desconocido") ;}
%%
