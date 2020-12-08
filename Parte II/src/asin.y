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
%type <expr> expresionOpcional expresion expresionIgualdad expresionRelacional 
%type <expr> expresionAditiva expresionMultiplicativa expresionUnitaria expresionSufija

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
	{ 
		if (!insertarTDS($2, $1, dvar, -1))
            yyerror(E_REPEATED_DECLARATION);
        else
            dvar += TALLA_TIPO_SIMPLE; 
	}
	| declaracionFuncion
        ;
declaracionVariable    
	: tipoSimple ID_ PCOMA_ 
    | tipoSimple ID_ ACLAU_ CTE_ CCLAU_ PCOMA_
    ;
tipoSimple
	: INT_
	| BOOL_
	;
declaracionFuncion
	: cabeceraFuncion bloque
	;
cabeceraFuncion
	: tipoSimple ID_ APAR_ parametrosFormales CPAR_
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
		
		 if (sim.tipo == T_ERROR) yyerror("Objeto no declarado");
		 else if (! ((sim.tipo == $3.tipo == T_ENTERO) || (sim.tipo == $3.tipo == T_LOGICO)))
		 	yyerror("Error de tipos en la instrucción de asignación");
		}

	| ID_ ACLAU_ expresion CCLAU_ IGUAL_ expresion PCOMA_
	;
instruccionEntradaSalida
	: READ_ APAR_ ID_ CPAR_ PCOMA_
	| PRINT_ APAR_ expresion CPAR_ PCOMA_
	;
instruccionSeleccion
	: IF_ APAR_ expresion CPAR_ instruccion ELSE_ instruccion
	;
instruccionIteracion
	: FOR_ APAR_ expresionOpcional PCOMA_ expresion PCOMA_ expresionOpcional CPAR_ instruccion
	;
expresionOpcional 
	: expresion
	| ID_ IGUAL_ expresion
	|
	;
expresion 
	: expresionIgualdad
	| expresion operadorLogico expresionIgualdad
	;
expresionIgualdad 
	: expresionRelacional
	| expresionIgualdad operadorIgualdad expresionRelacional
	;
expresionRelacional 
	: expresionAditiva
	| expresionRelacional operadorRelacional expresionAditiva
	;
expresionAditiva 
	: expresionMultiplicativa
	| expresionAditiva operadorAditivo expresionMultiplicativa
	;
expresionMultiplicativa 
	: expresionUnitaria
	| expresionMultiplicativa operadorMultiplicativo expresionUnitaria
	;
expresionUnitaria 
	: expresionSufija
	| operadorUnitario expresionUnitaria
	| operadorIncremento ID_
	;
expresionSufija
	: APAR_ expresion CPAR_ 
	| ID_ operadorIncremento
	| ID_ ACLAU_ expresion CCLAU_
	| ID_ APAR_ parametrosActuales CPAR_
	| ID_
	| constante
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
	: CTE_
	| TRUE_
	| FALSE_
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