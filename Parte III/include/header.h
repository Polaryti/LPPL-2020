/*****************************************************************************/
/**   Ejemplo de un posible fichero de cabeceras donde situar las           **/
/** definiciones de constantes, variables y estructuras para MenosC.        **/
/** Los alumos deberan adaptarlo al desarrollo de su propio compilador.     **/
/*****************************************************************************/
#ifndef _HEADER_H
#define _HEADER_H
#define TALLA_TIPO_SIMPLE 1
#define TALLA_SEGENLACES 2

/****************************************************** Constantes generales */
#define TRUE  1
#define FALSE 0
#define CTE 2
#define INT 3
#define BOOL 4
/******************************************************** Oeradores unarios  */

#define ERROR_NO_HAY_MAIN "Se debe declarar un main"
#define ERROR_VAR_NO_DEFINIDA "La variable no esta definida"
#define ERROR_VARIABLE_DECLARADA "Esta variable ya ha sido declarada"
#define ERROR_ARRAY_INVALIDO "El tamaño del array debe de ser un entero positivo"
#define ERROR_DE_TIPO "El tipo no es correcto"
#define ERROR_OBJ_NO_DECLARADO "El objeto no ha sido declarado"



/******************************************************* Estructuras de datos */
typedef struct expr{
   int tipo;        // tipo
   //int n;           NUEVO nombre
   //int valor;       NUEVO valor
   int pos;
}EXPR;
typedef struct argu{
   int talla;       // talla
   int ref;         // valor
}ARGU;
typedef struct lista{
   int cent;
   int a;
   int b;
   int c;
   int d;
}LISTA;
/************************************* Variables externas definidas en el AL */
extern int yylex();
extern int yyparse();

extern FILE *yyin;                           /* Fichero de entrada           */
extern int   yylineno;                       /* Contador del numero de linea */
extern char *yytext;                         /* Patron detectado             */
/********* Funciones y variables externas definidas en el Programa Principal */
extern void yyerror(const char * msg) ;   /* Tratamiento de errores          */

extern int verTdS;
extern int dvar;
extern int niv;
extern int si;

extern int verbosidad;                   /* Flag si se desea una traza       */
extern int numErrores;              /* Contador del numero de errores        */


#endif  /* _HEADER_H */
/*****************************************************************************/
