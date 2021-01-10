/*****************************************************************************/
/**   Ejemplo de un posible fichero de cabeceras donde situar las           **/
/** definiciones de constantes, variables y estructuras para MenosC.        **/
/** Los alumos deberan adaptarlo al desarrollo de su propio compilador.     **/
/*****************************************************************************/
#ifndef _HEADER_H
#define _HEADER_H

/****************************************************** Constantes generales */
#define TRUE  1
#define FALSE 0
#define TALLA_TIPO_SIMPLE 1     /* Talla asociada a los tipos simples */
#define TALLA_SEGENLACES 2      /* Talla del segmento de Enlaces de Control */


#define OP_NOT 29
#define OP_INCR 30
#define OP_DECR 31
#define OP_OR 32
#define OP_AND 33

typedef struct lista{
    int ref;
    int talla;
} Lista;

typedef struct texp{
   int t;           
   int pos;       
} Expresion;

/************************************* Variables externas definidas en el AL */
extern int yylex();
extern int yyparse();

extern FILE *yyin;                           /* Fichero de entrada           */
extern int   yylineno;                       /* Contador del numero de linea */
extern char *yytext;                         /* Patron detectado             */

/********* Funciones y variables externas definidas en el Programa Principal */
extern void yyerror(const char * msg) ;     /* Tratamiento de errores          */

extern int verbosidad;                      /* Flag si se desea una traza       */
extern int numErrores;                      /* Contador del numero de errores        */
extern int verTdS;                          /* Flag para saber si mostrar la TdS */

extern int dvar;                            /* Desplazamiento en el Segmento de Variables */
extern int niv;                             /* Nivel de anidamiento "global" o "local" */

extern int si;

#endif  /* _HEADER_H */
/*****************************************************************************/
