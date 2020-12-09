%{
#include <stdio.h>
#include <string.h>
#include "header.h"
%}

%union{
	int cent;
	char *ident;
}
%token MAS_ DMAS_ MENOS_ DMENOS_ POR_ DIV_ IGUAL_ 
%token TRUE_ FALSE_ MAY_ MEN_ MAYIG_ MENIG_ DIGUAL_ DIF_ NEG_ AND_ OR_
%token APAR_ CPAR_ ALLAVE_ CLLAVE_ ACLAU_ CCLAU_ PCOMA_ COMA_
%token PRINT_ RETURN_ FOR_ IF_ ELSE_ READ_ 

%token <cent>  CTE_ BOOL_ INT_
%token <ident> ID_
%type  <cent>  tipoSimple
%type  <cent>  operadorLogico operadorIgualdad operadorRelacional operadorAditivo
%type  <cent>  operadorMultiplicativo operadorUnitario operadorIncremento
%type <cent> expresionOpcional expresion expresionIgualdad expresionRelacional 
%type <cent> expresionAditiva expresionMultiplicativa expresionUnitaria expresionSufija
%type <cent> constante

%%

programa 
	: listaDeclaraciones
        ;
listaDeclaraciones    
	: declaracion
	|listaDeclaraciones declaracion
        ;
declaracion   
	: declaracionVariable
	| declaracionFuncion
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
		int numelem = $4; int ref;
        if (numelem <= 0) {
            yyerror("El tamaño del array no es valido");
            numelem = 0;
        }
        ref = insertaTDArray($1, numelem);
        if (!insertarTDS($2, T_ARRAY, dvar, ref))
            yyerror("Ya existe una variable con el mismo nombre");
        else
            dvar += numelem * TALLA_TIPO_SIMPLE; 
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
		}
	;
parametrosFormales
	: listaParametrosFormales
	|
	;
listaParametrosFormales
	: tipoSimple ID_
	| tipoSimple ID_ COMA_ listaParametrosFormales
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
	: expresionAditiva {$$ = $1;}
	| expresionRelacional operadorRelacional expresionAditiva
		{
			if (!($1==$3==T_ENTERO)) yyerror("Tipo de expresión no válido");
			else $$=T_ENTERO;
		}
	;
expresionAditiva 
	: expresionMultiplicativa
	| expresionAditiva operadorAditivo expresionMultiplicativa
	{
		if (!($1 == $3 == T_ENTERO)) {
			yyerror("Tipo de expresión no válido");
		} else {
			$$=T_ENTERO;
		}
	}
	;
expresionMultiplicativa 
	: expresionUnitaria {$$ = $1;}
	| expresionMultiplicativa operadorMultiplicativo expresionUnitaria
		{
			if (!($1==$3==T_ENTERO)) yyerror("Tipo de expresión no válido");
			else $$=T_ENTERO;
		}
	;
expresionUnitaria 
	: expresionSufija
	| operadorUnitario expresionUnitaria
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
			$$ = sim.t;
		}
	}
	;
expresionSufija
	: APAR_ expresion CPAR_ {$$ = $2;}
	| ID_ operadorIncremento
		{SIMB sim = obtTDS($1);
		
		 if (sim.t == T_ERROR) yyerror("Objeto no declarado");
		 else if (sim.t == T_ENTERO) $$ = sim.t;
		 else yyerror("Tipo de la variable inadecuado");
		}
	| ID_ ACLAU_ expresion CCLAU_
		{SIMB sim = obtTDS($1);
		
		 if (sim.t == T_ERROR) yyerror("Objeto no declarado");
		 else if (expresion != T_ENTERO) yyerror("Indicador de posición no válido");
		 else { 
		 	DIM dim = obtTDA(sim.ref);
			$$ = dim.telem;
		 };
		}
	| ID_ APAR_ parametrosActuales CPAR_
	| ID_ 
		{SIMB sim = obtTDS($1);
		
		 if (sim.t == T_ERROR) yyerror("Objeto no declarado");
		 else $$ = sim.t;
		}
	| constante {$$ = $1;}
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
	: CTE_   {$$ = T_ENTERO;}
	| TRUE_  {$$ = T_LOGICO;}
	| FALSE_ {$$ = T_LOGICO;}
	;
operadorLogico
	: AND_
	| OR_
	;
operadorIgualdad
	: DIGUAL_
	| DIF_
	;
operadorRelacional
	: MAY_
	| MEN_ 
	| MAYIG_
	| MENIG_
	;
operadorAditivo
	: MAS_
	| MENOS_
	;
operadorMultiplicativo
	: POR_
	| DIV_
	;
operadorUnitario 
	: MAS_ 
	| MENOS_ 
	| NEG_ 
	;
operadorIncremento
	: DMAS_ 
	| DMENOS_
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