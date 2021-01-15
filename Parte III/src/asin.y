/*****************************************************************************/
/**    Pere Marco Garcia | Antoni Mestre Gascón | Mario Campos Mocholí      **/
/*****************************************************************************/

%{
    #include <stdio.h>
    #include <string.h>
    #include "header.h"
    #include "libtds.h" 
    #include "libgci.h"
%}

%union {
    char *ident;            
    int cent;               
    Expresion texp;        
    Lista lista;            
    AUX aux;             
}
%token  
FALSE_ TRUE_ BOOL_ INT_ IF_ ELSE_ FOR_ READ_ PRINT_ RETURN_
IGUAL_ MAS_ DMAS_ MENOS_ DMENOS_ POR_ DIV_ DIF_ AND_ OR_ NEG_
MEN_ MAY_ DIGUAL_ MENIG_ MAYIG_ 
ALLAVE_ CLLAVE_ ACLAU_ CCLAU_ APAR_ CPAR_ PCOMA_ COMA_

%token<ident> ID_
%token<cent> CTE_

%type<lista> listaParametrosFormales parametrosFormales

%type<cent> listaDeclaraciones declaracion tipoSimple
           declaracionFuncion cabeceraFuncion operadorUnario
           operadorAditivo operadorIgualdad operadorIncremento 
           operadorLogico operadorMultiplicativo operadorRelacional
           listaParametrosActuales parametrosActuales
           bloque
            
%type<aux> instruccionSeleccion instruccionIteracion

%type<texp> expresionOpcional expresion expresionIgualdad
             expresionRelacional expresionAditiva expresionMultiplicativa
             expresionUnaria expresionSufija constante  
            
%%

programa: 
    {
        niv = 0; dvar = 0; si = 0; cargaContexto(niv); 
        $<aux>$.ref1 = creaLans(si);
        emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));
        $<aux>$.ref2 = creaLans(si);
        emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
    }
        listaDeclaraciones  
        { 
        if($2 != -1) { yyerror("No se ha encontrado la funcion main."); }
            
        completaLans($<aux>1.ref1, crArgEnt(dvar));

        SIMB sim = obtTdS("main");
        $<aux>$.ref3 = sim.d;

        completaLans($<aux>1.ref2, crArgEtq($<aux>$.ref3));
    };
/*****************************************************************************/
listaDeclaraciones:
      declaracion { $$ = $1; }
    | listaDeclaraciones declaracion {$$ = $1 + $2;}      
    ;
/*****************************************************************************/
declaracion:
      declaracionVariable {  $$ = 0; }                               
    | declaracionFuncion {  $$ = $1; }                               
    ;
/*****************************************************************************/
declaracionVariable:
      tipoSimple ID_ PCOMA_ {
        if (!insTdS($2,VARIABLE,$1,niv,dvar,-1)) { 
                yyerror("Error al declarar la variable.");
        } else {
                dvar += TALLA_TIPO_SIMPLE;                         
        } 
      }
    | tipoSimple ID_ ACLAU_ CTE_ CCLAU_ PCOMA_ {
        if ($4 <= 0) { yyerror("El array debe ser de tamaño mayor que cero.");}                
        else {    
              int ref; 
              ref = insTdA($1,$4);
              if (!insTdS($2, VARIABLE, T_ARRAY, niv, dvar, ref)) { yyerror("Error al declarar la variable.");
              } else { dvar += $4 * TALLA_TIPO_SIMPLE; }    
        }                             
    }
    ;
/*****************************************************************************/
tipoSimple:                  
      INT_ { $$ = T_ENTERO; }   
    | BOOL_ { $$ = T_LOGICO; }   
    ;
/*****************************************************************************/
declaracionFuncion:               
      cabeceraFuncion {$<cent>$ = dvar; dvar = 0;} bloque 
      {  
         if(verTdS) mostrarTdS();
         descargaContexto(niv); 
         niv = 0; 
         dvar = $<cent>2;
         $$=$1;
         }
                    
        ;
/*****************************************************************************/
cabeceraFuncion:
      tipoSimple ID_ {niv=1; cargaContexto(niv); } APAR_ parametrosFormales CPAR_ { 
         if(!insTdS($2,FUNCION,$1,0,si,$5.ref)) { yyerror("Error al declarar la variable."); } 
         if (strcmp($2,"main\0") == 0) { $$ = -1; } else { $$ = 0; } 
      }
    ;
/*****************************************************************************/
parametrosFormales:
    { $$.ref = insTdD(-1,T_VACIO);
      $$.talla = 0;}     
    | listaParametrosFormales
        { $$.ref = $1.ref;
          $$.talla = $1.talla; } 
    ;
/*****************************************************************************/
listaParametrosFormales:
      tipoSimple ID_ 
     {
        $$.ref = insTdD(-1,$1);            
        $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
        if(!insTdS($2,PARAMETRO,$1,niv,-$$.talla,-1)) { yyerror("Error al declarar la variable."); } 
     } 
    | tipoSimple ID_ COMA_ listaParametrosFormales    
    {
        $$.ref = insTdD($4.ref,$1);          
        $$.talla = $4.talla + TALLA_TIPO_SIMPLE;
        if(!insTdS($2,PARAMETRO,$1,niv,-$$.talla,-1)) { yyerror("Error al declarar la variable."); } 
     }
    ;
/*****************************************************************************/
bloque:            
      ALLAVE_ {
        emite( PUSHFP, crArgNul(), crArgNul(), crArgNul() );
        emite( FPTOP, crArgNul(), crArgNul(), crArgNul() );
        $<cent>$ = creaLans(si);
        emite( INCTOP, crArgNul(), crArgNul(), crArgEnt(-1) );                   
      } 
      declaracionVariableLocal listaInstrucciones RETURN_ expresion PCOMA_ CLLAVE_ { 
        INF inf = obtTdD(-1);
        if(inf.tipo != T_ERROR){
          if(inf.tipo != $6.tipo){ yyerror("Se ha producido una incompatibilidad de tipos."); }
        } 
        completaLans($<cent>2, crArgEnt(dvar));

        int dret = TALLA_SEGENLACES + TALLA_TIPO_SIMPLE + inf.tsp;
        emite(EASIG, crArgPos(niv, $6.pos), crArgNul(), crArgPos(niv, -dret));
        emite(TOPFP, crArgNul(), crArgNul(), crArgNul() );
        emite(FPPOP, crArgNul(), crArgNul(), crArgNul() );

        if (strcmp(inf.nom,"main") == 0) { emite(FIN, crArgNul(), crArgNul(), crArgNul()); }
        else { emite(RET, crArgNul(), crArgNul(), crArgNul()); }
        
      }
    ;  
/*****************************************************************************/
declaracionVariableLocal:              
    | declaracionVariableLocal declaracionVariable        
    ;
/*****************************************************************************/
listaInstrucciones:                    
    | listaInstrucciones instruccion
    ;
/*****************************************************************************/
instruccion:                            
      ALLAVE_ listaInstrucciones CLLAVE_
    | instruccionAsignacion 
    | instruccionSeleccion
    | instruccionEntradaSalida
    | instruccionIteracion
    ;
/*****************************************************************************/
instruccionAsignacion:                    
      ID_ IGUAL_ expresion PCOMA_            

        {   SIMB sim = obtTdS($1);
            if($3.tipo != T_ERROR){                         

                if (sim.t == T_ERROR) yyerror("No se ha declarado el objeto.");
                else if (! ((sim.t == $3.tipo && sim.t == T_ENTERO) || (sim.t == $3.tipo && sim.t == T_LOGICO)))               
                    yyerror("Se ha producido una incompatibilidad de tipos.");
            }

            emite(EASIG, crArgPos(niv, $3.pos), crArgNul(), crArgPos(sim.n, sim.d));

        }
    | ID_ ACLAU_ expresion CCLAU_ IGUAL_ expresion PCOMA_      

        {
            SIMB sim = obtTdS($1);
            if(sim.t != T_ARRAY){ yyerror("No es de tipo array"); } 
            else{ DIM dim = obtTdA(sim.ref);
            
            if($3.tipo != T_ERROR && $6.tipo != T_ERROR){                    
                if (sim.t == T_ERROR) yyerror("Error al declarar la variable.");                                          
                else if (! (sim.t == T_ARRAY))  yyerror("Se ha producido una incompatibilidad de tipos.");                                           
                else if (! ($3.tipo == T_ENTERO))  yyerror("El numero de elemento del array no es un entero");    
                else if (! ($6.tipo == dim.telem)) { yyerror("Se ha producido una incompatibilidad de tipos."); }                                     
                
            }
            }

            emite(EVA, crArgPos(sim.n, sim.d) , crArgPos(niv, $3.pos), crArgPos(niv, $6.pos));
        }
    ;
/*****************************************************************************/
instruccionEntradaSalida:
      READ_ APAR_ ID_ CPAR_ PCOMA_       
        {
            SIMB sim = obtTdS($3);
            
            if (sim.t != T_ENTERO && sim.t != T_LOGICO) yyerror("Se ha producido una incompatibilidad de tipos.");
            emite(EREAD, crArgNul(), crArgNul(),crArgPos(sim.n , sim.d));  
        }
      
    | PRINT_ APAR_ expresion CPAR_ PCOMA_

        {
            if ($3.tipo != T_ERROR && $3.tipo != T_ENTERO) yyerror("Solo se pueden imprimir variables enteras");
            emite(EWRITE, crArgNul(), crArgNul(), crArgPos(niv, $3.pos));  
        }
    ;
/*****************************************************************************/
instruccionSeleccion:
      IF_ APAR_ expresion CPAR_ 
      
      {
          if ($3.tipo != T_ERROR) {
              if ($3.tipo != T_LOGICO) { yyerror("Se ha producido una incompatibilidad de tipos."); }    
          }
          $<aux>$.valor = creaLans(si);  
          emite(EIGUAL, crArgPos(niv, $3.pos), crArgEnt(0), crArgEtq(-1));  
      }

      instruccion {
          $<aux>$.valor = creaLans(si); 
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));  
          completaLans($<aux>5.valor, crArgEtq(si));
        } 

      ELSE_ instruccion { completaLans($<aux>7.valor, crArgEtq(si)); }    
    ;
/*****************************************************************************/
instruccionIteracion:
      FOR_ APAR_ expresionOpcional PCOMA_ 
      {
       $<aux>$.valor = si; 
      } 
      expresion PCOMA_      
      
      {
          if ($6.tipo != T_ERROR) {
              if ($3.tipo != T_LOGICO && $3.tipo != T_VACIO && $3.tipo != T_ENTERO){ yyerror("Se ha producido una incompatibilidad de tipos."); }
          }
          $<aux>$.ref1 = creaLans(si);                              
          emite(EIGUAL, crArgPos(niv, $6.pos), crArgEnt(1), crArgEtq(-1) );
          $<aux>$.ref2 = creaLans(si);                              
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
          $<aux>$.ref3 = si;    
      }
      
      expresionOpcional CPAR_ {
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<aux>5.valor)); 
          completaLans($<aux>8.ref1, crArgEtq(si));
      } 
      instruccion {
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<aux>8.ref3)); 
          completaLans($<aux>8.ref2, crArgEtq(si));
      }              

    ;

/*****************************************************************************/
expresionOpcional:
        { $$.tipo = T_VACIO; }
    | expresion          
        {
            $$.tipo = $1.tipo;
            $$.pos = $1.pos; 
        }
    | ID_ IGUAL_ expresion   

        {
            $$.tipo = T_ERROR;
            SIMB sim = obtTdS($1);

            if (sim.t != T_ERROR && $3.tipo != T_ERROR) {
                if (sim.t == $3.tipo) { $$.tipo = sim.t; }
                else { yyerror("Se ha producido una incompatibilidad de tipos."); }
            }

            emite(EASIG, crArgPos(niv, $3.pos), crArgNul(), crArgPos(sim.n, sim.d));
        }
    ;
/*****************************************************************************/
expresion:
      expresionIgualdad { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresion operadorLogico expresionIgualdad    
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror("Se ha producido una incompatibilidad de tipos.");
            }                           
            else if ($1.tipo != T_LOGICO) { yyerror("Operacion logica invalida para no booleanos"); } 
            else { $$.tipo = T_LOGICO; }
        }

        $$.pos = creaVarTemp();
        if ($2 == EMULT) { 
            emite(EMULT, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
        }else {
            emite(ESUM, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
            emite(EMENEQ, crArgPos(niv, $$.pos), crArgEnt(1), crArgEtq(si+2));
            emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.pos));
        }
    }
    ;
/*****************************************************************************/
expresionIgualdad:
      expresionRelacional { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresionIgualdad operadorIgualdad expresionRelacional  
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) {yyerror("Se ha producido una incompatibilidad de tipos."); }                                
            else if ($3.tipo != T_LOGICO && $3.tipo != T_ENTERO) { yyerror("No se puede aplicar el operador de igualdad."); }  
            else { $$.tipo = T_LOGICO; }
        }

        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.pos));
        emite($2, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgEtq(si + 2));
        emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.pos));
    }
    ;
/*****************************************************************************/
expresionRelacional:    
      expresionAditiva { $$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expresionRelacional operadorRelacional expresionAditiva  
    {  $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror("Se ha producido una incompatibilidad de tipos."); }                                                  
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion relacional solo acepta argumentos enteros."); }   
            else { $$.tipo = T_LOGICO; }
        }

        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv, $$.pos));
        emite($2, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgEtq(si + 2));
        emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv, $$.pos));
    }
    ;
/*****************************************************************************/
expresionAditiva:
      expresionMultiplicativa { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresionAditiva operadorAditivo expresionMultiplicativa  
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror("Se ha producido una incompatibilidad de tipos."); }                
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion aditiva solo acepta argumentos enteros."); } 
            else { $$.tipo = T_ENTERO; }
        }

        $$.pos = creaVarTemp();
        emite($2, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
    }
    ;
/*****************************************************************************/
expresionMultiplicativa:
      expresionUnaria { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresionMultiplicativa operadorMultiplicativo expresionUnaria 
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror("Se ha producido una incompatibilidad de tipos."); }                 
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion multiplicativa solo acepta argumentos enteros."); }  
             else { $$.tipo = T_ENTERO; }
        }

        $$.pos = creaVarTemp();
        emite($2, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
    }
    ;
/*****************************************************************************/
expresionUnaria:
      expresionSufija{ $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | operadorUnario expresionUnaria     
    {   $$.tipo = T_ERROR;
        if ($2.tipo != T_ERROR) {
            if ($2.tipo == T_ENTERO) {                                                                         
                if ($1 == ESIG) { yyerror("Operacion NOT invalida en expresion entera."); }                    
                else { $$.tipo = T_ENTERO; }
            } else if ($2.tipo == T_LOGICO) {                                                                  
                if ($1==ESUM || $1 == EDIF) { yyerror("Operacion entera no vaida para un booleano."); }     
                else { $$.tipo = T_LOGICO;}
            }
            else { yyerror("Se ha producido una incompatibilidad de tipos."); }

            $$.pos = creaVarTemp();
            if ($1 == ESIG) {
                emite(EDIF, crArgEnt(1), crArgPos(niv, $2.pos), crArgPos(niv, $$.pos));    
            } else {
                emite($1, crArgEnt(0), crArgPos(niv, $2.pos), crArgPos(niv, $$.pos));
            }
        } 
    }
    | operadorIncremento ID_   
    {   SIMB sim = obtTdS($2);
        $$.tipo = T_ERROR;
        if (sim.t == T_ERROR) { yyerror("Error al declarar la variable."); }                                         
        else if (sim.t != T_ENTERO) { yyerror("Error no se puede incrementar una variable no entera"); }          
        else { $$.tipo = sim.t; }

        $$.pos = creaVarTemp();
        emite($1, crArgPos(sim.n, sim.d), crArgEnt(1), crArgPos(sim.n, sim.d));
        emite(EASIG, crArgPos(sim.n, sim.d), crArgNul(), crArgPos(niv, $$.pos));
    } 
    ;
/*****************************************************************************/
expresionSufija:
      APAR_ expresion CPAR_ { $$.tipo = $2.tipo; $$.pos = $2.pos; }
    | ID_ operadorIncremento   
    {   SIMB sim = obtTdS($1);
        $$.tipo = T_ERROR;
        if (sim.t == T_ERROR) { yyerror("Error al declarar la variable."); }                                  
        else if (sim.t != T_ENTERO){ yyerror("Error no se puede incrementar una variable no entera"); }   
        else { $$.tipo = sim.t; }

        $$.pos = creaVarTemp();
        emite(EASIG, crArgPos(sim.n, sim.d), crArgNul(), crArgPos(niv, $$.pos)); 
        emite($2, crArgPos(sim.n, sim.d), crArgEnt(1), crArgPos(sim.n, sim.d));
    }
    | ID_ ACLAU_ expresion CCLAU_  
    {   SIMB sim = obtTdS($1);
        $$.tipo = T_ERROR;
        if (sim.t == T_ERROR){  yyerror("Error al declarar la variable."); }                                  
        else if (sim.t != T_ARRAY) { yyerror("La variable no es un array no puede ser accedida"); }        
        else if ($3.tipo != T_ENTERO) { yyerror("Error , indice no entero"); }                                
        else{
            DIM dim = obtTdA(sim.ref);
            $$.tipo = dim.telem;
        }

        $$.pos = creaVarTemp();
        emite(EAV, crArgPos(sim.n, sim.d), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos)); 
    } 
    
    | ID_ APAR_               
    {   
        emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE)); 
    }
    parametrosActuales CPAR_
    {
        SIMB sim = obtTdS($1);
        $$.tipo = T_ERROR;
        if (sim.t == T_ERROR){ yyerror("Error al declarar la variable."); }                                  
        INF inf = obtTdD(sim.ref);
        if (inf.tipo == T_ERROR) { yyerror("Funcion no definida"); }                                   
        else { $$.tipo = inf.tipo; }
        
        emite(CALL, crArgNul(), crArgNul(), crArgEtq(sim.d)); 
        emite(DECTOP, crArgNul(), crArgNul(), crArgEnt(inf.tsp)); 
        $$.pos = creaVarTemp();
        emite(EPOP, crArgNul(), crArgNul(), crArgPos(niv, $$.pos));
    }
    | ID_
    {   SIMB sim = obtTdS($1);
        $$.tipo = T_ERROR;
        if (sim.t == T_ERROR) { yyerror("Error al declarar la variable."); }   
        else { $$.tipo = sim.t; }
        $$.pos = creaVarTemp();
        emite(EASIG, crArgPos(niv, sim.d), crArgNul(), crArgPos(niv, $$.pos));   
    }
    | constante {            
        $$.tipo = $1.tipo;
        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt($1.pos), crArgNul(), crArgPos(niv, $$.pos)); 
        }
    ;
/*****************************************************************************/
parametrosActuales:    { $$ = insTdD(-1,T_VACIO); }      
    | listaParametrosActuales { $$ = $1; }
    ;
/*****************************************************************************/
listaParametrosActuales:
    expresion
    { $$ = insTdD(-1,$1.tipo);
    emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.pos)); }
    | expresion COMA_ { emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, $1.pos)); }
    listaParametrosActuales
    { $$ = insTdD($4,$1.tipo); }
    ;
/*****************************************************************************/
constante:
      CTE_          { $$.tipo = T_ENTERO; $$.pos = $1; } 
    | TRUE_         { $$.tipo = T_LOGICO; $$.pos = 1; } 
    | FALSE_        { $$.tipo = T_LOGICO; $$.pos = 0; } 
    ;
/*****************************************************************************/
operadorLogico:
      AND_          { $$ = EMULT; }
    | OR_           { $$ = ESUM; }
    ;
/*****************************************************************************/
operadorIgualdad:
      DIGUAL_       { $$ = EIGUAL; }
    | DIF_          { $$ = EDIST; }
    ;
/*****************************************************************************/
operadorRelacional:
      MAY_        { $$ = EMAY; }
    | MEN_        { $$ = EMEN; }
    | MAYIG_      { $$ = EMAYEQ; }
    | MENIG_      { $$ = EMENEQ; }
    ;
/*****************************************************************************/
operadorAditivo:
      MAS_          { $$ = ESUM; }
    | MENOS_        { $$ = EDIF; }
    ;
/*****************************************************************************/
operadorMultiplicativo:
      POR_          { $$ = EMULT; }
    | DIV_          { $$ = EDIVI; }
    ;
/*****************************************************************************/
operadorUnario:
      MAS_          { $$ = ESUM; }     
    | MENOS_        { $$ = EDIF; }
    | NEG_          { $$ = ESIG; }
    ;
/*****************************************************************************/
operadorIncremento:
      DMAS_         { $$ = ESUM; }
    | DMENOS_       { $$ = EDIF; }
    ;

%%
/*****************************************************************************/
