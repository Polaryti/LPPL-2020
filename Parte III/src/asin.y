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
%type  <cent>  tipoSimple operadorIncremento operadorUnario operadorMultiplicativo
			   operadorAditivo operadorRelacional operadorIgualdad  operadorLogico 
			   listaDeclaraciones declaracion declaracionFuncion cabeceraFuncion

%type  <texp>  expresionOpcional expresion expresionIgualdad expresionRelacional 
			   expresionAditiva expresionMultiplicativa expresionUnaria expresionSufija
               constante

%%
programa 
	: { dvar=0; niv = 0; cargaContexto(niv); }
	listaDeclaraciones 
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
	: tipoSimple ID_ PCOMA_ 
	{ 
		if (!insTdS($2, VARIABLE, $1, niv, dvar, -1))
            yyerror("Ya existe una variable con el mismo identificador.");
        else
            dvar += TALLA_TIPO_SIMPLE; 
	}
    | tipoSimple ID_ ACLAU_ CTE_ CCLAU_ PCOMA_
	{ 
        if ($4 <= 0) {
            yyerror("El indice de inicialización de los vectores tiene que ser un entero positivo.");
        } else {
			int ref = insTdA($1, $4);
			if (!insTdS($2, VARIABLE, T_ARRAY, niv, dvar, ref))
				yyerror("Ya existe un vector con el mismo identificador.");
			else
				dvar += $4 * TALLA_TIPO_SIMPLE; 
		}
        
	}
    ;

tipoSimple
	: INT_  { $$ = T_ENTERO; }
    | BOOL_ { $$ = T_LOGICO; }
	;

declaracionFuncion
	: cabeceraFuncion { $<cent>$ = dvar; dvar = 0; } bloque
		{
			descargaContexto(niv); 
			niv = 0; 
			dvar = $<cent>2;
			$$ = $1;
		}
	;

cabeceraFuncion
	: tipoSimple ID_ { niv = 1; cargaContexto(niv); } APAR_ parametrosFormales CPAR_
		{
			if (!insTdS($2, FUNCION, $1, 0, -1, $5.ref)) {
                yyerror("Ya existe una función con el mismo nombre y el mismo numero y tipo de parametros.");
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
	: tipoSimple ID_
	{
			$$.ref = insTdD(-1, $1);
			$$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
			if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1)) yyerror("Ya existe un parametro con el mismo identificador.");
        
	}
	| tipoSimple ID_ COMA_ listaParametrosFormales
	{
		$$.ref = insTdD($4.ref, $1);
		$$.talla = $4.talla + TALLA_TIPO_SIMPLE;
        if(!insTdS($2, PARAMETRO, $1, niv, -$$.talla, -1))  yyerror("Ya existe un parametro con el mismo identificador.");
	}
	;

bloque
	: ALLAVE_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PCOMA_ CLLAVE_
		{ 
			INF inf = obtTdD(-1);
			if (inf.t != T_ERROR) {
				if (inf.t != $5.t) {
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes."); 
				}     
			}
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
					yyerror("No existe ninguna variable con ese identificador.");
				} else if (! ((sim.t == $3.t && sim.t == T_ENTERO) || (sim.t == $3.t && sim.t == T_LOGICO))) {
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
				}
			}
			emite(EASIG, crArgPos($3.pos), crArgNul(), crArgPos(sim.d));
		}

	| ID_ ACLAU_ expresion CCLAU_ IGUAL_ expresion PCOMA_
		{
			SIMB sim = obtTdS($1); DIM dim;
			
            if (sim.t != T_ARRAY) {
                yyerror("La variable no es un vector, no se puede acceder mediante indices.");
            } else {
                dim = obtTdA(sim.ref);
            }
            
			if ($3.t != T_ERROR && $6.t != T_ERROR) {                    
                if (sim.t == T_ERROR) {
                    yyerror("No existe ninguna variable con ese identificador.");
                } else if (! ($3.t == T_ENTERO)) {
                    yyerror("El indice para acceder a un vector debe ser un entero 0 o positivo.");
                } else if (! ($6.t == dim.telem)) { 
                    yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes."); 
                }                      
            }
		}
	;

instruccionEntradaSalida
	: READ_ APAR_ ID_ CPAR_ PCOMA_
		{
			SIMB sim = obtTdS($3);
			if (sim.t != T_ENTERO) {
				yyerror("El argumento de la funcion read() debe ser de tipo entero.");
			}
			emite(EREAD, crArgNul(), crArgNul(), crArgPos(sim.d));
		}
	| PRINT_ APAR_ expresion CPAR_ PCOMA_
		{
			if ($3.t != T_ERROR && $3.t != T_ENTERO) {
				yyerror("El argumento de la funcion print() debe ser de tipo entero.");
			}
			emite(EWRITE, crArgNul(), crArgNul(), crArgPos($3.pos));
		}
	;

instruccionSeleccion
	: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
		{
			if ($3.t != T_ERROR)
				if ($3.t != T_LOGICO) yyerror("La expresion de evaluacion del \"if\" debe ser de tipo logico.");
		}
	;

instruccionIteracion
	: FOR_ APAR_ expresionOpcional PCOMA_ expresion PCOMA_ expresionOpcional 
			{
				if ($5.t != T_ERROR)
					if ($5.t != T_LOGICO) yyerror("La expresion de evaluacion del \"for\" debe ser de tipo logico.");
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
                    yyerror("No existe ninguna variable con ese identificador.");
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
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
				} else {
					$$.t = T_LOGICO;
				}
			}
			$$.pos=creaVarTemp();
			if($2==EASIG) emite($2,crArgPos($3.pos),crArgNul(),crArgPos(sim.desp));
			else emite($2,crArgPos(sim.desp),crArgPos($3.pos),crArgPos(sim.desp));
			emite(EASIG,crArgPos(sim.desp),crArgNul(),crArgPos($$.pos));
		}
	;

expresionIgualdad 
	: expresionRelacional { $$.t = $1.t; }
	| expresionIgualdad operadorIgualdad expresionRelacional
		{	
			$$.t = T_ERROR;
			
            if ($1.t != T_ERROR && $3.t != T_ERROR) {
                if ($1.t != $3.t) {
                    yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
                } else if ($3.t != T_LOGICO || $3.t != T_ENTERO) { 
                    yyerror("Incompatibilidad de tipos, deben ser expresiones logicas o de enteros.");
                }  else {
                    $$.t = T_LOGICO;
                }
            } 
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos($$.pos));
			emite($2,crArgPos($1.pos),crArgPos($3.pos),crArgEtq(si+2));
			emite(EASIG,crArgEnt(FALSE),crArgNul(),crArgPos($$.pos));
		}
	;

expresionRelacional 
	: expresionAditiva {$$.t = $1.t;}
	| expresionRelacional operadorRelacional expresionAditiva
		{
            $$.t = T_ERROR;
			if ($1.t != T_ERROR && $3.t != T_ERROR){
				if (!($1.t == $3.t && $1.t == T_ENTERO)) {
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
				} else {
					$$.t = T_LOGICO;
				}
			}
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos($$.pos));
			emite($2,crArgPos($1.pos),crArgPos($3.pos),crArgEtq(si+2));
			emite(EASIG,crArgEnt(FALSE),crArgNul(),crArgPos($$.pos));
		}
	;

expresionAditiva 
	: expresionMultiplicativa { $$.t = $1.t;  }
	| expresionAditiva operadorAditivo expresionMultiplicativa
	{
        $$.t = T_ERROR;
		if ($1.t != T_ERROR && $3.t != T_ERROR) {
			if (!($1.t == $3.t && $1.t == T_ENTERO)) {
				yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
			} else {
				$$.t = T_ENTERO;
			}
		}
		$$.pos = creaVarTemp();
		emite($2, crArgPos($1.pos),crArgPos($3.pos), crArgPos($$.pos));
	}
	;

expresionMultiplicativa 
	: expresionUnaria {$$.t = $1.t; }
	| expresionMultiplicativa operadorMultiplicativo expresionUnaria
		{
            $$.t = T_ERROR;
			if ($1.t != T_ERROR && $3.t != T_ERROR) {
				if (!($1.t == $3.t && $1.t == T_ENTERO)) {
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes. ");
				} else {
					$$.t = T_ENTERO;
				} 
			}
		$$.pos = creaVarTemp();
		emite($2, crArgPos($1.pos),crArgPos($3.pos), crArgPos($$.pos));
		}
	;

expresionUnaria 
	: expresionSufija {$$.t = $1.t; $$.pos = $1.pos; }
	| operadorUnario expresionUnaria
	{  
        $$.t = T_ERROR;
        if ($2.t != T_ERROR) {
            if ($2.t == T_ENTERO) {                                                                         
                if ($1 == EDIST) {
					yyerror("Incompatibilidad de tipos, no se puede negar un entero.");
				} else { 
					$$.t = T_ENTERO; 
				}
            } else if ($2.t == T_LOGICO) {                                                                  
                if ($1 == ESUM || $1 == EDIF) {
					yyerror("Incompatibilidad de tipos, solo se puede aplicar el operador unario \"+\" o \"-\" a una expresion entera.");
				} else { 
					$$.t = T_LOGICO;
				}
            } else {
				yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
			}                                                               
        } 
    }
	| operadorIncremento ID_
	{
		SIMB sim = obtTdS($2);
		
		$$.t = T_ERROR;

		if (sim.t == T_ERROR) {
			yyerror("No existe ninguna variable con ese identificador.");
		}
		else if (sim.t != T_ENTERO) {
			yyerror("Incompatibilidad de tipos, solo se puede aplicar el operador \"++\" o \"--\" a una expresion entera.");
		}
		else {
			$$.t = sim.t;
		}
		$$.pos=creaVarTemp();
		emite($1,crArgPos(sim.desp),crArgEnt(1),crArgPos(sim.desp));
		emite(EASIG,crArgPossim.desp),crArgNul(),crArgPos($$.pos));
	}
	;

expresionSufija
	: APAR_ expresion CPAR_ {$$.t = $2.t; $$.pos = $2.pos; }
	| ID_ operadorIncremento
		{
			SIMB sim = obtTdS($1);
			
			$$.t = T_ERROR;
		
			if (sim.t == T_ERROR) {
				yyerror("No existe ninguna variable con ese identificador.");
			} else if (sim.t == T_ENTERO) {
				$$.t = sim.t;
			} else {
				yyerror("Incompatibilidad de tipos, solo se puede aplicar el operador \"++\" o \"--\" a una expresion entera.");
			}
			$$.pos=creaVarTemp();
			emite(EASIG,crArgPos(sim.desp),crArgNul(),crArgPos($$.pos));
			emite($2,crArgPos(sim.desp),crArgEnt(1),crArgPos(sim.desp));
		}
	| ID_ ACLAU_ expresion CCLAU_
		{
			SIMB sim = obtTdS($1);
			
			$$.t = T_ERROR;
		
			if (sim.t == T_ERROR) {
				yyerror("No existe ninguna variable con ese identificador.");
			} else if ($3.t != T_ENTERO) {
				yyerror("El indice para acceder a un vector debe ser un entero 0 o positivo.");
			} else { 
				DIM dim = obtTdA(sim.ref);
				$$.t = dim.telem;
			}
			emite(EMULT,crArgPos($3.pos),crArgEnt(TALLA_TIPO_SIMPLE),crArgPos($3.pos));
			$$.pos=creaVarTemp();
			emite(EAV,crArgPos(sim.desp),crArgPos($3.pos),crArgPos($$.pos));
		}
	| ID_ APAR_ parametrosActuales CPAR_
		{
			SIMB sim = obtTdS($1);

			$$.t = T_ERROR;
			
			if (sim.t == T_ERROR) { 
				yyerror("No existe ninguna variable con ese identificador."); 
			}
			INF inf = obtTdD(sim.ref);
			if (inf.t == T_ERROR) { 
				yyerror("No existe ninguna funcion con ese identificador."); 
			} else {
				$$.t = inf.t;
			}
		}
	| ID_ 
		{
			SIMB sim = obtTdS($1);
			$$.t = T_ERROR;

		 	if (sim.t == T_ERROR) {
				 yyerror("No existe ninguna variable con ese identificador.");
			 } else { 
				 $$.t = sim.t;
			 }
			$$.pos=creaVarTemp();
			emite(EASIG,crArgPos(sim.desp),crArgNul(),crArgPos($$.pos));
		}
	| constante {$$.t = $1.t;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgPos($1.pos),crArgNul(),crArgPos($$.pos));}
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
	: CTE_   {$$.t = T_ENTERO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt($1.lexval),crArgNul(),crArgPos($$.pos));}
	| TRUE_  {$$.t = T_LOGICO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(1),crArgNul(),crArgPos($$.pos));}
	| FALSE_ {$$.t = T_LOGICO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(0),crArgNul(),crArgPos($$.pos));}
	;

operadorLogico
	: AND_		{$$ = EMULT;}
	| OR_		{$$ = ESUM;}
	;

operadorIgualdad
	: DIGUAL_	{$$ = EIGUAL;}
	| DIF_		{$$ = EDIST;}
	;

operadorRelacional
	: MAY_		{$$ = EMAY;}
	| MEN_ 		{$$ = EMEN;}
	| MAYIG_	{$$ = EMAYEQ;}
	| MENIG_	{$$ = EMENEQ;}
	;

operadorAditivo
	: MAS_		{$$ = ESUM;}
	| MENOS_	{$$ = EDIF;}
	;

operadorMultiplicativo
	: POR_		{$$ = EMULT;}
	| DIV_		{$$ = EDIVI;}
	;

operadorUnario 
	: MAS_		{$$ = ESUM;} 
	| MENOS_ 	{$$ = EDIF;}
	| NEG_ 		{$$ = EDIST;}
	;

operadorIncremento
	: DMAS_ 	{$$ = $$=ESUM;}
	| DMENOS_	{$$ = $$=EDIF;}
	;
%%