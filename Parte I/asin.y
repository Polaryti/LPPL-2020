%{
	#include <stdio.h>
	#include "header.h"
	
%}

%token OPSUMA_ OPRESTA_ OPMULT_ OPDIV_ OPAND_ OPOR_ OPNOT_ OPINCREMENTO_ OPDECREMENTO_
%token COMPMAYOR_ COMPMENOR_ COMPMAYORIG_ COMPMENORIG_ OPIGUAL_ OPNOTIGUAL_ IGUAL_
%token FOR_		IF_		ELSE_
%token INT_     BOOL_
%token READ_    PRINT_	RETURN_
%token CTE_     ID_     TRUE_   FALSE_
%token LLAVE1_  LLAVE2_ PARENTESIS1_ PARENTESIS2_ CORCHETE1_ CORCHETE2_ SEMICOLON_ COMA_

%%

programa
    : lista_declaraciones
    ;

lista_declaraciones
    : declaracion
    | lista_declaraciones declaracion
    ;

declaracion
    : declaracion_variable
    | declaracion_funcion
    ;

declaracion_variable
    : tipo_simple ID_ SEMICOLON_
    | tipo_simple ID_ CORCHETE1_ CTE_ CORCHETE2_ SEMICOLON_
    ;

tipo_simple
    : INT_
    | BOOL_
    ;
	
declaracion_funcion
	: cabecera_funcion bloque
	;
	
cabecera_funcion
	: tipo_simple ID_ PARENTESIS1_ parametros_formales PARENTESIS2_
	;
	
parametros_formales
	:
	| lista_parametros_formales
	;

lista_parametros_formales
	: tipo_simple ID_
	| tipo_simple ID_ COMA_ lista_parametros_formales
	;
	
bloque
	: LLAVE1_ declaracion_variable_local lista_instrucciones RETURN_ expresion SEMICOLON_ LLAVE2_
	;
	
declaracion_variable_local
	: declaracion_variable_local declaracion_variable
	|
	;

lista_instrucciones
	: lista_instrucciones instruccion
	|
	;

instruccion
    : LLAVE1_ lista_instrucciones LLAVE2_
    | instruccion_asignacion
    | instruccion_seleccion
    | instruccion_entrada_salida
	| instruccion_iteracion
    ;

instruccion_asignacion
	: ID_ IGUAL_ expresion SEMICOLON_
	| ID_ CORCHETE1_ expresion CORCHETE2_ IGUAL_ expresion SEMICOLON_
	;
	
instruccion_entrada_salida
	: READ_ PARENTESIS1_ ID_ PARENTESIS2_ SEMICOLON_
	| PRINT_ PARENTESIS1_ expresion PARENTESIS2_ SEMICOLON_
	;
	
instruccion_seleccion
	: IF_ PARENTESIS1_ expresion PARENTESIS2_ instruccion ELSE_ instruccion
	;
	
instruccion_iteracion
	: FOR_ PARENTESIS1_ expresion_opcional SEMICOLON_ expresion SEMICOLON_ expresion_opcional PARENTESIS2_ instruccion
	;
	
expresion_opcional
	: expresion
	| ID_ IGUAL_ expresion
	|
	;

expresion
    : expresion_igualdad
    | expresion operador_logico expresion_igualdad
    ;
	
expresion_igualdad
	: expresion_relacional
	| expresion_igualdad operador_igualdad expresion_relacional
	;
	
expresion_relacional
	: expresion_aditiva
	| expresion_relacional operador_relacional expresion_aditiva
	;
	
expresion_aditiva
	: expresion_multiplicativa
	| expresion_aditiva operador_aditivo expresion_multiplicativa
	;
	
expresion_multiplicativa
	: expresion_unaria
	| expresion_multiplicativa operador_multiplicativo expresion_unaria
	;
	
expresion_unaria
	: expresion_sufija
	| operador_unario expresion_unaria
	| operador_incremento ID_
	;
	
expresion_sufija
	: PARENTESIS1_ expresion PARENTESIS2_
	| ID_ operador_incremento
	| ID_ CORCHETE1_ expresion CORCHETE2_
	| ID_ PARENTESIS1_ parametros_actuales PARENTESIS2_
	| ID_
	| constante
	;
	
parametros_actuales
	: lista_parametros_actuales
	|
	;
	
lista_parametros_actuales
	: expresion
	| expresion COMA_ lista_parametros_actuales
	;
	
constante
	: CTE_
	| TRUE_
	| FALSE_
	;
	
operador_logico
	: OPAND_
	| OPOR_
	;
	
operador_igualdad
	: OPIGUAL_
	| OPNOTIGUAL_
	;
	
operador_relacional
	: COMPMAYOR_
	| COMPMENOR_
	| COMPMAYORIG_
	| COMPMENORIG_
	;
	
operador_aditivo
	: OPSUMA_
	| OPRESTA_
	;
	
operador_multiplicativo
	: OPMULT_
	| OPDIV_
	;
	
operador_unario
	: OPSUMA_
	| OPRESTA_
	| OPNOT_
	;
	
operador_incremento
	: OPINCREMENTO_
	| OPDECREMENTO_
	;


%%
