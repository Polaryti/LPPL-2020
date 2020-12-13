%{
	#include <stdio.h>
	#include <string.h>
	#include "header.h"
	#include "libtds.h" 
%}

%union{
	int cent;
	char *ident;
	Lista lista;
	Expresion texp;
}
%token MAS_ DMAS_ MENOS_ DMENOS_ POR_ DIV_ IGUAL_ INT_ BOOL_
%token TRUE_ FALSE_ MAY_ MEN_ MAYIG_ MENIG_ DIGUAL_ DIF_ NEG_ AND_ OR_
%token APAR_ CPAR_ ALLAVE_ CLLAVE_ ACLAU_ CCLAU_ PCOMA_ COMA_
%token PRINT_ RETURN_ FOR_ IF_ ELSE_ READ_ 

%token <cent>  CTE_
%token <ident> ID_
%type  <lista> listaParametrosFormales parametrosFormales
%type  <cent>  tSimple operadorIncremento operadorUnario operadorMultiplicativo
			   operadorAditivo operadorRelacional operadorIgualdad  operadorLogico 
			   listaDeclaraciones declaracion declaracionFuncion cabeceraFuncion

%type  <texp>  expresionOpcional expresion expresionIgualdad expresionRelacional 
			   expresionAditiva expresionMultiplicativa expresionUnaria expresionSufija
               constante

%%
programa 
	: { dvar=0; niv = 0; cargaContexto(niv); }
	listaDeclaraciones { if($2 != -1); yyerror("No hay main");} 
	{ if(verTdS) mostrarTdS(); }
    ;

listaDeclaraciones    
	: declaracion { $$ = $1; }
	|listaDeclaraciones declaracion { $$ = $1 + $2; }
    ;

declaracion   
	: declaracionVariable { $$ = 0; }
	| declaracionFuncion { $$ = $1; }
    ;

declaracionVariable    
	: tSimple ID_ PCOMA_ 
	{ 
		if (!insTdS($2, VARIABLE, $1, niv, dvar, -1))
            yyerror("Ya existe una variable con el mismo nombre");
        else
            dvar += TALLA_TIPO_SIMPLE; 
	}
    | tSimple ID_ ACLAU_ CTE_ CCLAU_ PCOMA_
	{ 
        if ($4 <= 0) {
            yyerror("El tamaño del array no es valido");
        } else {
			int ref = insTdA($1, $4);
			if (!insTdS($2, VARIABLE, T_ARRAY, niv, dvar, ref))
				yyerror("Ya existe una array con el mismo nombre");
			else
				dvar += $4 * TALLA_TIPO_SIMPLE; 
		}
        
	}
    ;

tSimple
	: INT_  { $$ = T_ENTERO; }
    | BOOL_ { $$ = T_LOGICO; }
	;

declaracionFuncion
	: cabeceraFuncion { $<cent>$ = dvar; dvar = 0; } bloque
		{
			if (verTdS) {
                mostrarTdS(); 
            }
			descargaContexto(niv); 
			niv = 0; 
			dvar = $<cent>2;
			$$ = $1;
		}
	;

cabeceraFuncion
	: tSimple ID_ { niv=1; cargaContexto(niv); } APAR_ parametrosFormales CPAR_
		{
			if (!insTdS($2, FUNCION, $1, 0, -1, $5.ref)) {
                yyerror("Desclaración repetida");
            }
			if (strcmp($2, "main\0") == 0) { 
                $$ = -1;
            } else {
                $$ = 0;
            }
		}
	;

parametrosFormales
	: listaParametrosFormales
		{
			$$.ref = $1.ref;
			$$.talla = $1.talla;
		}
	| {
		$$.ref = insTdD(-1, T_VACIO);
		$$.talla = 0;
	  }
	;

listaParametrosFormales
	: tSimple ID_
	{
			$$.ref = insTdD(-1, $1);
			$$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
			if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1)) yyerror("Variable ya declarada");
        
	}
	| tSimple ID_ COMA_ listaParametrosFormales
	{
		$$.ref = insTdD($4.ref, $1);
		$$.talla = $4.talla + TALLA_TIPO_SIMPLE;
        if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1))  yyerror("Variable ya declarada");
	}
	;

bloque
	: ALLAVE_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PCOMA_ CLLAVE_
		{ 
			INF inf = obtTdD(-1);
			if(inf.t != T_ERROR)
				if (inf.t != $5.t)  yyerror("Error con los tipos");        
			
		}
	;

declaracionVariableLocal
	: declaracionVariableLocal declaracionVariable
	|
	;

listaInstrucciones
	: listaInstrucciones instruccion
	|
	;

instruccion
	: ALLAVE_ listaInstrucciones CLLAVE_
	| instruccionAsignacion
	| instruccionSeleccion
	| instruccionEntradaSalida
	| instruccionIteracion
	;

instruccionAsignacion
	: ID_ IGUAL_ expresion PCOMA_ 
		{
            SIMB sim = obtTdS($1);
			if($3.t != T_ERROR){   
				if (sim.t == T_ERROR) {
					yyerror("Objeto no declarado");
				} else if (! ((sim.t == $3.t && sim.t == T_ENTERO) || (sim.t == $3.t && sim.t == T_LOGICO))) {
					yyerror("Error de tipos en la instrucción de asignación");
				}
			}
		}

	| ID_ ACLAU_ expresion CCLAU_ IGUAL_ expresion PCOMA_
		{
			SIMB sim = obtTdS($1); DIM dim;
			
            if (sim.t != T_ARRAY) {
                yyerror("No es de tipo array");
            } else {
                dim = obtTdA(sim.ref);
            }
            
			if($3.t != T_ERROR && $6.t != T_ERROR){                    
                if (sim.t == T_ERROR) {
                    yyerror("Error en la vairable");
                } else if (! ($3.t == T_ENTERO)) {
                    yyerror("El numero de elemento del array no es un entero");
                } else if (! ($6.t == dim.telem)) { 
                    yyerror("Error en el tipo de la expresion"); 
                }                      

            }
		}
	;

instruccionEntradaSalida
	: READ_ APAR_ ID_ CPAR_ PCOMA_
		{
			SIMB sim = obtTdS($3);
			if (sim.t != T_ENTERO) yyerror("El argumento de la función "read" debe ser de tipo entero.");
		}
	| PRINT_ APAR_ expresion CPAR_ PCOMA_
		{
			if ($3.t != T_ERROR && $3.t != T_ENTERO) yyerror("El argumento de la función "print" debe ser de tipo entero.");
		}
	;

instruccionSeleccion
	: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
		{
			if ($3.t != T_ERROR)
				if ($3.t != T_LOGICO) yyerror("La expresión de evaluación del "if" debe ser de tipo lógico.");
		}
	;

instruccionIteracion
	: FOR_ APAR_ expresionOpcional PCOMA_ expresion PCOMA_ expresionOpcional 
			{
				if ($5.t != T_ERROR)
					if ($5.t != T_LOGICO) yyerror("La expresión de evaluación del "for" debe ser de tipo lógico.");
			}
	  CPAR_ instruccion
	;

expresionOpcional 
	: expresion { $$.t = $1.t; }
	| ID_ IGUAL_ expresion 
		{
            $$.t = T_ERROR;
            SIMB sim = obtTdS($1);

            if (sim.t != T_ERROR && $3.t != T_ERROR){
                if (sim.t == $3.t) {
                    $$.t = sim.t;
                } else {
                    yyerror("Erro en el tipo de la expresión.");
                }
            }
		}
	| { $$.t = T_VACIO; }
	;

expresion 
	: expresionIgualdad  { $$.t = $1.t; }
	| expresion operadorLogico expresionIgualdad
		{
			$$.t = T_ERROR;
			if ($1.t != T_ERROR || $3.t != T_ERROR) {
				if (!($1.t == $3.t && $1.t == T_LOGICO)) {
					yyerror("Error en el tipo de la expresion. ");
				} else {
					$$.t = T_LOGICO;
				}
			}
		}
	;

expresionIgualdad 
	: expresionRelacional { $$.t = $1.t; }
	| expresionIgualdad operadorIgualdad expresionRelacional
		{	
			$$.t = T_ERROR;
			
            if ($1.t != T_ERROR && $3.t != T_ERROR) {
                if ($1.t != $3.t) {
                    yyerror("Error en el tipo de la expresion igualdad. ");
                } else if ($3.t != T_LOGICO || $3.t != T_ENTERO) { 
                    yyerror("No se puede aplicar el operador de igualdad. ");
                }  else {
                    $$.t = T_LOGICO;
                }
            } 
		}
	;

expresionRelacional 
	: expresionAditiva {$$.t = $1.t;}
	| expresionRelacional operadorRelacional expresionAditiva
		{
            $$.t = T_ERROR;
			if ($1.t != T_ERROR && $3.t != T_ERROR){
				if (!($1.t == $3.t && $1.t == T_ENTERO)) {
					yyerror("Error en el tipo de la expresion relacional. ");
				} else {
					$$.t = T_LOGICO;
				}
			}
		}
	;

expresionAditiva 
	: expresionMultiplicativa { $$.t = $1.t; }
	| expresionAditiva operadorAditivo expresionMultiplicativa
	{
        $$.t = T_ERROR;
		if ($1.t != T_ERROR && $3.t != T_ERROR) {
			if (!($1.t == $3.t && $1.t == T_ENTERO)) {
				yyerror("Error en el tipo de la expresion aditiva. ");
			} else {
				$$.t = T_ENTERO;
			}
		}
	}
	;

expresionMultiplicativa 
	: expresionUnaria {$$.t = $1.t;}
	| expresionMultiplicativa operadorMultiplicativo expresionUnaria
		{
            $$.t = T_ERROR;
			if ($1.t != T_ERROR && $3.t != T_ERROR) {
				if (!($1.t == $3.t && $1.t == T_ENTERO)) {
					yyerror("Error en el tipo de la expresion multiplicativa. ");
				} else {
					$$.t = T_ENTERO;
				} 
			}
		}
	;

expresionUnaria 
	: expresionSufija {$$.t = $1.t;}
	| operadorUnario expresionUnaria
	{  
        $$.t = T_ERROR;
        if ($2.t != T_ERROR) {
            if ($2.t == T_ENTERO) {                                                                         
                if ($1 == OP_NOT) {
					yyerror("No se puede negar un número");
				} else { 
					$$.t = T_ENTERO; 
				}
            } else if ($2.t == T_LOGICO) {                                                                  
                if ($1 == OP_SUMA || $1 == OP_RESTA) {
					yyerror("Operación no valida para un booleano");
				} else { 
					$$.t = T_LOGICO;
				}
            } else {
				yyerror("Error en el tipo de la expresion unaria. ");
			}                                                               
        } 
    }
	| operadorIncremento ID_
	{
		SIMB sim = obtTdS($2);
		
		$$.t = T_ERROR;

		if (sim.t == T_ERROR) {
			yyerror("La variable no esta declarada");
		}
		else if (sim.t != T_ENTERO) {
			yyerror("El operador incremento solo se puede aplicar a variables enteras");
		}
		else {
			$$.t = sim.t;
		}
	}
	;

expresionSufija
	: APAR_ expresion CPAR_ {$$.t = $2.t;}
	| ID_ operadorIncremento
		{
			SIMB sim = obtTdS($1);
			
			$$.t = T_ERROR;
		
			if (sim.t == T_ERROR) {
				yyerror("Objeto no declarado");
			} else if (sim.t == T_ENTERO) {
				$$.t = sim.t;
			} else {
				yyerror("Error en el tipo de la expresion sufija. ");
			}
		}
	| ID_ ACLAU_ expresion CCLAU_
		{
			SIMB sim = obtTdS($1);
			
			$$.t = T_ERROR;
		
			if (sim.t == T_ERROR) {
				yyerror("Objeto no declarado");
			} else if ($3.t != T_ENTERO) {
				yyerror("Indicador de posición no válido");
			} else { 
				DIM dim = obtTdA(sim.ref);
				$$.t = dim.telem;
			}
		}
	| ID_ APAR_ parametrosActuales CPAR_
		{
			SIMB sim = obtTdS($1);

			$$.t = T_ERROR;
			
			if (sim.t == T_ERROR) { 
				yyerror("La variable no existe"); 
			}
			INF inf = obtTdD(sim.ref);
			if (inf.t == T_ERROR) { 
				yyerror("Funcion no definida"); 
			} else {
				$$.t = inf.t;
			}
		}
	| ID_ 
		{
			SIMB sim = obtTdS($1);
			$$.t = T_ERROR;

		 	if (sim.t == T_ERROR) {
				 yyerror("Objeto no declarado");
			 } else { 
				 $$.t = sim.t;
			 }
		}
	| constante {$$.t = $1.t;}
	;

parametrosActuales
	: listaParametrosActuales
	|
	;


listaParametrosActuales
	: expresion
	| expresion COMA_ listaParametrosActuales
	;

constante
	: CTE_   {$$.t = T_ENTERO;}
	| TRUE_  {$$.t = T_LOGICO;}
	| FALSE_ {$$.t = T_LOGICO;}
	;

operadorLogico
	: AND_		{$$ = OP_AND;}
	| OR_		{$$ = OP_OR;}
	;

operadorIgualdad
	: DIGUAL_	{$$ = OP_IGUAL;}
	| DIF_		{$$ = OP_NOTIGUAL;}
	;

operadorRelacional
	: MAY_		{$$ = OP_MAYOR;}
	| MEN_ 		{$$ = OP_MENOR;}
	| MAYIG_	{$$ = OP_MAYORIG;}
	| MENIG_	{$$ = OP_MENORIG;}
	;

operadorAditivo
	: MAS_		{$$ = OP_SUMA;}
	| MENOS_	{$$ = OP_RESTA;}
	;

operadorMultiplicativo
	: POR_		{$$ = OP_MULT;}
	| DIV_		{$$ = OP_DIV;}
	;

operadorUnario 
	: MAS_		{$$ = OP_SUMA;} 
	| MENOS_ 	{$$ = OP_RESTA;}
	| NEG_ 		{$$ = OP_NOT;}
	;

operadorIncremento
	: DMAS_ 	{$$ = OP_INCR;}
	| DMENOS_	{$$ = OP_DECR;}
	;
%%