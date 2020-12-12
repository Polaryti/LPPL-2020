%{
#include <stdio.h>
#include <string.h>
#include "header.h"
%}

%union{
	int cent;
	char *ident;
	Lista lista;
}
%token MAS_ DMAS_ MENOS_ DMENOS_ POR_ DIV_ IGUAL_ 
%token TRUE_ FALSE_ MAY_ MEN_ MAYIG_ MENIG_ DIGUAL_ DIF_ NEG_ AND_ OR_
%token APAR_ CPAR_ ALLAVE_ CLLAVE_ ACLAU_ CCLAU_ PCOMA_ COMA_
%token PRINT_ RETURN_ FOR_ IF_ ELSE_ READ_ 

%token <cent>  CTE_ BOOL_ INT_
%token <ident> ID_
%type  <cent>  tipoSimple
%type  <lista> listaParametrosFormales parametrosFormales listaparametrosActuales parametrosActuales
%type  <cent>  operadorUnario
%type  <cent>  expresionOpcional expresion expresionIgualdad expresionRelacional 
%type  <cent>  expresionAditiva expresionMultiplicativa expresionUnitaria expresionSufija
%type  <cent>  constante

%%
programa 
	: { dvar=0; niv = 0; cargarContexto(niv); }
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
		if (!insertarTDS($2, $1, dvar, -1))
            yyerror("Ya existe una variable con el mismo nombre");
        else
            dvar += TALLA_TIPO_SIMPLE; 
	}
    | tipoSimple ID_ ACLAU_ CTE_ CCLAU_ PCOMA_
	{ 
        if ($4 < 0) {
            yyerror("El tamaño del array no es valido");
        } else {
			int ref = insertaTdA($1, $4);
			if (!insertarTDS($2, T_ARRAY, niv, dvar, ref))
				yyerror("Ya existe una variable con el mismo nombre");
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
	: cabeceraFuncion bloque
	;

cabeceraFuncion
	: tipoSimple ID_ APAR_ parametrosFormales CPAR_
		{
			if (!insTdS($2,FUNCION,$1,0,-1,$4)) yyerror("Desclaración repetida")
			if (strcmp($2,"main\0"==0)) $$ = -1;
			else $$ = 0;
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
		$$.talla = TALLA_TIPO_SIMPLE;
	  }
	;

listaParametrosFormales
	: tipoSimple ID_
	{
			$$.ref = insTdD(-1, $1);
			$$.talla = TALLA_TIPO_SIMPLE;
	}
	| tipoSimple ID_ COMA_ listaParametrosFormales
	{
		INF inf = obtTdD($4) 
		if(inf.tipo == T_ERROR){
			yyerror("Error en los parámetros formales");
		}
		else{
			$$.ref = $4.ref;
			$$.talla = $4.talla + TALLA_TIPO_SIMPLE;
		}

	}
	;

bloque
	: ALLAVE_ declaracionVariableLocal listaInstrucciones RETURN_ expresion PCOMA_ CLLAVE_
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
		{SIMB sim = obtTDS($1);
		
		 if (sim.t == T_ERROR) yyerror("Objeto no declarado");
		 else if (! ((sim.tipo == $3 == T_ENTERO) || (sim.t == $3 == T_LOGICO)))
		 	yyerror("Error de tipos en la instrucción de asignación");
		}

	| ID_ ACLAU_ expresion CCLAU_ IGUAL_ expresion PCOMA_
		{
			SIMB sim = obtTDS($1);
			DIM dim = obtTdA(sim.ref);

			if (dim.telem == T_ERROR) yyerror("Array no declarada");
			else if ($3 != T_ENTERO) yyerror("El indice debe ser un entero positivo");
			else if (!((dim.telem == $6 == T_ENTERO) || (dim.telem == $6 == T_LOGICO))) 
				yyerror("Error de tipos en la instrucción de asignación de la array");
		}
	;

instruccionEntradaSalida
	: READ_ APAR_ ID_ CPAR_ PCOMA_
	{
		SIMB sim = obtTDS($3);

		if (sim.t == T_ERROR) {
			yyerror("Variable no declarada");
		} 
	}
	| PRINT_ APAR_ expresion CPAR_ PCOMA_
	{
		if ($3 == T_ERROR) {
			yyerror("Expresion no valida");
		} 
	}
	;

instruccionSeleccion
	: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
	{
		if ($3 != T_LOGICO) {
			yyerror("La expresion no es valida");
		}
	}
	;

instruccionIteracion
	: FOR_ APAR_ expresionOpcional PCOMA_ expresion PCOMA_ expresionOpcional CPAR_ instruccion
		{
			if ($5 != T_LOGICO) yyerror("Expresión de evaluación inválida");
		}
	;

expresionOpcional 
	: expresion {$$ = $1;}
	| ID_ IGUAL_ expresion 
		{SIMB sim = obtTDS($1);
		
		 if (sim.t == T_ERROR) yyerror("Objeto no declarado");
		 else if (sim.t == $3 == T_ENTERO || sim.t == $3 == T_LOGICO) $$ = sim.t;
		 else yyerror("Tipo de la variable inadecuado");
		}
	|
	;

expresion 
	: expresionIgualdad
	| expresion operadorLogico expresionIgualdad
	{
		if (!($1 == $3 == T_LOGICO)) {
			yyerror("Tipo de expresión no válido");
		} else {
			$$ = T_LOGICO;
		}
	}
	;

expresionIgualdad 
	: expresionRelacional {$$ = $1;}
	| expresionIgualdad operadorIgualdad expresionRelacional
		{
			if (!($1==$3==T_ENTERO || $1==$3==T_LOGICO)) yyerror("Tipo de expresión no válido");
			else $$=$1;
		}
	;

expresionRelacional 
	: expresionAditiva {$$.t = $1.t;}
	| expresionRelacional operadorRelacional expresionAditiva
		{
			if (!($1.t == $3.t == T_ENTERO)) {
				yyerror("Tipo de expresión no válido");
			} else {
				$$.t = T_ENTERO;
			}
		}
	;

expresionAditiva 
	: expresionMultiplicativa { $$.t = $1.t; }
	| expresionAditiva operadorAditivo expresionMultiplicativa
	{
		if (!($1.t == $3.t == T_ENTERO)) {
			yyerror("Tipo de expresión no válido");
		} else {
			$$.t = T_ENTERO;
		}
	}
	;

expresionMultiplicativa 
	: expresionUnaria {$$.t = $1.t;}
	| expresionMultiplicativa operadorMultiplicativo expresionUnaria
		{
			if (!($1.t == $3.t == T_ENTERO)) {
				yyerror("Tipo de expresión no válida");
			} else {
				$$.t = T_ENTERO;
			} 
		}
	;

expresionUnario 
	: expresionSufija {$$.t = $1.t;}
	| operadorUnario expresionUnaria
	{  
        if ($2.t != T_ERROR) {
            if ($2.t == T_ENTERO) {                                                                         
                if ($1.t == OP_NOT) {
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
				yyerror(ERROR_DE_TIPO);
			}                                                               
        } 
    }
	| operadorIncremento ID_
	{
		SIMB sim = obtTDS($2);

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
			SIMB sim = obtTDS($1);
		
			if (sim.t == T_ERROR) {
				yyerror("Objeto no declarado");
			} else if (sim.t == T_ENTERO) {
				$$ = sim.t;
			} else {
				yyerror("Tipo de la variable inadecuado");
			}
		}
	| ID_ ACLAU_ expresion CCLAU_
		{
			SIMB sim = obtTDS($1);
		
			if (sim.t == T_ERROR) {
				yyerror("Objeto no declarado");
			} else if (expresion != T_ENTERO) {
				yyerror("Indicador de posición no válido");
			} else { 
				DIM dim = obtTDA(sim.ref);
				$$ = dim.telem;
			}
		}
	| ID_ APAR_ parametrosActuales CPAR_
		{
			SIMB sim = obtTdS($1);

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
			SIMB sim = obtTDS($1);

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
		// {
		// 		$$.ref = insTdD(-1, $1);
		// 		$$.talla = TALLA_TIPO_SIMPLE;
		// }
	| expresion COMA_ listaParametrosActuales
		// {
		// 	INF inf = obtTdD($3) 
		// 	if(inf.tipo == T_ERROR){
		// 		yyerror("Error en los parámetros actuales");
		// 	}
		// 	else{
		// 		$$.ref = $3.ref;
		// 		$$.talla = $3.talla + TALLA_TIPO_SIMPLE;
		// 	}

		// }
	;

constante
	: CTE_   {$$ = T_ENTERO;}
	| TRUE_  {$$ = T_LOGICO;}
	| FALSE_ {$$ = T_LOGICO;}
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

/*****************************************************************************/
int verbosidad = FALSE;                  /* Flag si se desea una traza       */

/*****************************************************************************/
void yyerror(const char *msg)
/*  Tratamiento de errores.                                                  */
{  fprintf(stderr, "\nError en la linea %d: %s\n", yylineno, msg); }

/*****************************************************************************/
int main (int argc, char **argv) 
/* Gestiona la linea de comandos e invoca al analizador sintactico-semantico.*/
{ int i, n=1 ;

  for (i=1; i<argc; ++i)
    if (strcmp(argv[i], "-v")==0) { verbosidad = TRUE; n++; }
  if (argc == n+1)
    if ((yyin = fopen (argv[n], "r")) == NULL) {
      fprintf (stderr, "El fichero '%s' no es valido\n", argv[n]) ;     
      fprintf (stderr, "Uso: cmc [-v] fichero\n");
    } 
  else yyparse();
  else fprintf (stderr, "Uso: cmc [-v] fichero\n");

  return (0);
}
/*****************************************************************************/