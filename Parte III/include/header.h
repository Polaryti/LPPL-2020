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


/******************************************************* Estructuras de datos */
typedef struct texp{
   int tipo;        
   int pos;
} Expresion;

typedef struct lista{
   int talla;       
   int ref;  
} Lista;

typedef struct aux{
   int valor;
   int ref1;
   int ref2; 
   int ref3;
   int ref4;
} AUX;
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

extern int verbosidad;                   /* Flag si se desea una traza       */
extern int numErrores;              /* Contador del numero de errores        */

extern int si;

#endif  /* _HEADER_H */
/*****************************************************************************/
