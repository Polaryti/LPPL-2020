%{
	#include <stdio.h>
	#include <string.h>
	#include "header.h"
	#include "libtds.h" 
	#include "libgci.h"
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
	: { dvar=0; niv = 0; cargaContexto(); }
	listaDeclaraciones
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
	: cabeceraFuncion 
		{ 
			$<cent>$ = dvar; dvar = 0;
		}
		{
			emite(PUSHFP, crArgNul(), crArgNul(), crArgNul()); 
			emite(FPTOP, crArgNul(), crArgNul(), crArgNul()); 
			$<cent>$ = creaLans(si);
			emite(INCTOP, crArgNul(), crArgNul(), crArgEtq(-1)); 
		} 
		bloque
		{
			descargaContexto(niv); 
			niv = 0; 
			dvar = $<cent>2;
			$$ = $1;
			completaLans($<cent>3, crArgEnt(dvar));
			emite(TOPFP, crArgNul(), crArgNul(), crArgNul()); 
			emite(FPPOP, crArgNul(), crArgNul(), crArgNul()); 
			emite(RET, crArgNul(), crArgNul(), crArgNul()); 
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
			emite(EASIG, crArgPos(niv, $3.pos), crArgNul(), crArgPos(niv, sim.d));
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
			emite(EMULT,crArgPos(niv, $3.pos),crArgEnt(TALLA_TIPO_SIMPLE),crArgPos(niv, $3.pos));
			emite(EVA,crArgPos(niv, sim.d),crArgPos(niv, $3.pos),crArgPos(niv,$6.pos));
		}
	;

instruccionEntradaSalida
	: READ_ APAR_ ID_ CPAR_ PCOMA_
		{
			SIMB sim = obtTdS($3);
			if (sim.t != T_ENTERO) {
				yyerror("El argumento de la funcion read() debe ser de tipo entero.");
			}
			emite(EREAD, crArgNul(), crArgNul(), crArgPos(niv, sim.d));
		}
	| PRINT_ APAR_ expresion CPAR_ PCOMA_
		{
			if ($3.t != T_ERROR && $3.t != T_ENTERO) {
				yyerror("El argumento de la funcion print() debe ser de tipo entero.");
			}
			emite(EWRITE, crArgNul(), crArgNul(), crArgPos(niv, $3.pos));
		}
	;

instruccionSeleccion
	: IF_ APAR_ expresion CPAR_ 
	{
		if ($3.t != T_ERROR)
				if ($3.t != T_LOGICO) yyerror("La expresion de evaluacion del \"if\" debe ser de tipo logico.");
		$<cent>$ = creaLans(si); 
		emite(EIGUAL, crArgEnt($3.pos), crArgEnt(FALSE), crArgEtq(-1));
	} 
	instruccion 
	{
		$<cent>$ = creaLans(si); 
		emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1)); 
		completaLans($<cent>5, crArgEtq(si));
	}
	ELSE_ instruccion
		{
			completaLans($<cent>7, crArgEtq(si));
		}
	;

instruccionIteracion
	: FOR_ APAR_ expresionOpcional PCOMA_
		{
			$<cent>$ = si;
		}
		expresion PCOMA_ 
		{
			$<cent>$ = creaLans(si);
			emite(EIGUAL, crArgPos(niv, $6.pos), crArgEnt(0), crArgEtq(-1));
		
		}
		{
			$<cent>$ = creaLans(si);
			emite(GOTOS, crArgNul(),crArgNul(),crArgEtq(-1));
		}
		{
			$<cent>$ = si;
		}
		expresionOpcional 
		{
			if ($6.t != T_ERROR)
				if ($6.t != T_LOGICO) yyerror("La expresion de evaluacion del \"for\" debe ser de tipo logico.");
			emite(GOTOS, crArgNul(),crArgNul(),crArgEtq($<cent>5));
			completaLans($<cent>9, crArgEtq(si));
		}
	  CPAR_ instruccion 
	  	{
		  	emite(GOTOS, crArgNul(),crArgNul(),crArgEtq($<cent>10));
			completaLans($<cent>8, crArgEtq(si));
	  	}
	;

expresionOpcional 
	: expresion { { $$ = $1; }}
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
			emite(EASIG, crArgPos(niv, $3.pos), crArgNul(), crArgPos(niv, sim.d));
			emite(EASIG,crArgPos(niv, sim.d),crArgNul(),crArgPos(niv, $$.pos));
		}
	| { $$.t = T_VACIO; }
	;

expresion 
	: expresionIgualdad  { { $$ = $1; }}
	| expresion operadorLogico expresionIgualdad
		{
			$$.t = T_ERROR;
			if ($1.t != T_ERROR && $3.t != T_ERROR) {
				if (!($1.t == $3.t && $1.t == T_LOGICO)) {
					yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
				} else {
					$$.t = T_LOGICO;
				}
			}
			$$.pos=creaVarTemp();
			emite($2,crArgPos(niv, $1.pos),crArgPos(niv, $3.pos),crArgPos(niv, $$.pos));
			if ($2 == ESUM) {
				emite(EMENEQ,crArgPos(niv, $$.pos),crArgEnt(1),crArgEtq(si+2));
				emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos(niv, $$.pos));
			}
		}
	;

expresionIgualdad 
	: expresionRelacional { $$ = $1; }
	| expresionIgualdad operadorIgualdad expresionRelacional
		{	
			$$.t = T_ERROR;
			
            if ($1.t != T_ERROR && $3.t != T_ERROR) {
                if ($1.t != $3.t) {
                    yyerror("Incompatibilidad de tipos, no son el mismo tipo o no son equivalentes.");
                } else if ($3.t != T_LOGICO && $3.t != T_ENTERO) { 
                    yyerror("Incompatibilidad de tipos, deben ser expresiones logicas o de enteros.");
                }  else {
                    $$.t = T_LOGICO;
                }
            } 
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos(niv, $$.pos));
			emite($2,crArgPos(niv, $1.pos),crArgPos(niv, $3.pos),crArgEtq(si+2));
			emite(EASIG,crArgEnt(FALSE),crArgNul(),crArgPos(niv, $$.pos));
		}
	;

expresionRelacional 
	: expresionAditiva { { $$ = $1; }}
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
			emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos(niv, $$.pos));
			emite($2,crArgPos(niv, $1.pos),crArgPos(niv, $3.pos),crArgEtq(si+2));
			emite(EASIG,crArgEnt(FALSE),crArgNul(),crArgPos(niv, $$.pos));
		}
	;

expresionAditiva 
	: expresionMultiplicativa {  { $$ = $1; }}
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
		emite($2, crArgPos(niv, $1.pos),crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
	}
	;

expresionMultiplicativa 
	: expresionUnaria { { $$ = $1; }}
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
		emite($2, crArgPos(niv, $1.pos),crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
		}
	;

expresionUnaria 
	: expresionSufija {  { $$ = $1; } }
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
		$$.pos=creaVarTemp();
		if($1==EDIST){
			emite(EDIF,crArgEnt(1),crArgPos(niv, $2.pos),crArgPos(niv, $$.pos));
		}
		else if($1==EDIF){
			emite(ESIG,crArgPos(niv, $2.pos),crArgNul(),crArgPos(niv, $$.pos));
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
		emite($1,crArgPos(niv, sim.d),crArgEnt(1),crArgPos(niv, sim.d));
		emite(EASIG,crArgPos(niv, sim.d),crArgNul(),crArgPos(niv, $$.pos));
	}
	;

expresionSufija
	: APAR_ expresion CPAR_ {  { $$ = $2; } }
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
			emite(EASIG,crArgPos(niv, sim.d),crArgNul(),crArgPos(niv, $$.pos));
			emite($2,crArgPos(niv, sim.d),crArgEnt(1),crArgPos(niv, sim.d));
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
			emite(EMULT,crArgPos(niv, $3.pos),crArgEnt(TALLA_TIPO_SIMPLE),crArgPos(niv, $3.pos));
			$$.pos=creaVarTemp();
			emite(EAV,crArgPos(niv, sim.d),crArgPos(niv, $3.pos),crArgPos(niv, $$.pos));
		}
	| ID_
		{
			$<cent>$ = creaVarTemp();
		}
		{
			
			SIMB sim = obtTdS($1);

			$<cent>$ = T_ERROR;
			
			if (sim.t == T_ERROR) { 
				yyerror("No existe ninguna variable con ese identificador."); 
			}
			INF inf = obtTdD(sim.ref);
			if (inf.t == T_ERROR) { 
				yyerror("No existe ninguna funcion con ese identificador."); 
			} else {
				$<cent>$ = inf.t;
			}
			if (sim.t != T_VACIO){
				emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE));
			} else {
				emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(0));
			}
		}
	 APAR_ parametrosActuales CPAR_
		{   
			SIMB sim = obtTdS($1);
			INF inf = obtTdD(sim.ref);
			emite(EPUSH, crArgNul(), crArgNul(), crArgEnt(dvar));
			emite(CALL, crArgNul(), crArgNul(), crArgEnt(sim.d));
			emite(EPOP, crArgNul(), crArgNul(), crArgEnt(dvar));
			emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-inf.tsp));
			$$.pos = $<cent>2;
			$$.t = $<cent>3;
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
			emite(EASIG,crArgPos(niv, sim.d),crArgNul(),crArgPos(niv, $$.pos));
		}
	| constante {$$.t = $1.t;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgPos(niv, $1.pos),crArgNul(),crArgPos(niv, $$.pos));}
	;

parametrosActuales
	: listaParametrosActuales
	|
	;


listaParametrosActuales
	: expresion {emite(EPUSH, crArgNul(), crArgNul(), crArgEnt($1.pos));}
	| expresion COMA_ listaParametrosActuales {emite(EPUSH, crArgNul(), crArgNul(), crArgEnt($1.pos));}
	;

constante
	: CTE_   {$$.t = T_ENTERO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt($1),crArgNul(),crArgPos(niv, $$.pos));}
	| TRUE_  {$$.t = T_LOGICO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(TRUE),crArgNul(),crArgPos(niv, $$.pos));}
	| FALSE_ {$$.t = T_LOGICO;
			$$.pos=creaVarTemp();
			emite(EASIG,crArgEnt(FALSE),crArgNul(),crArgPos(niv, $$.pos));}
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
	: DMAS_ 	{$$=ESUM;}
	| DMENOS_	{$$=EDIF;}
	;
%%