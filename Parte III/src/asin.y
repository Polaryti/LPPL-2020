/*****************************************************************************/
/**    Marcos Rodriguez, Joaquin Martinez, Edu Aparicio Toni Blasco        **/
/*****************************************************************************/

%{
    #include <stdio.h>
    #include <string.h>
    #include "header.h"
    #include "libtds.h" 
    #include "libgci.h"
%}
%union {
    char *ident;      /* Para el terminal "identificador" */
    int aux;          /* Para los no-terminales con atributo simple */
    EXPR expr;        /* Para los no terminales con expresion */
    ARGU arg;         /* Para los argumentos" */
    LISTA list;       /* Para GCI */
}
%token  
/*****************************************************************************/
FALSE_ TRUE_ BOOL_ INT_ IF_ ELSE_ FOR_ READ_ PRINT_ RETURN_
/*****************************************************************************/
ASIG_ SUMA_ SUMA2_ RESTA_ RESTA2_ MULT_ DIV_ NOTIGUAL_ AND_ OR_ NOT_
/*****************************************************************************/
MENOR_ MAYOR_ IGUAL2_ MENORIG_ MAYORIG_ 
/*****************************************************************************/
LLAVE1_ LLAVE2_ COR1_ COR2_ PAR1_ PAR2_ PCOMA_ COMA_
/*****************************************************************************/
/*****************************************************************************/ 
%token<ident> ID_
%token<aux> CTE_
%type<aux> listaDeclaraciones declaracion tipoSimple
           declaracionFuncion cabeceraFuncion operadorUnario
           operadorAditivo operadorIgualdad operadorIncremento 
           operadorLogico operadorMultiplicativo operadorRelacional
           listaParametrosActuales parametrosActuales
           bloque
            
%type<arg> listaParametrosFormales parametrosFormales
%type<expr> expresionOpcional expresion expresionIgualdad
             expresionRelacional expresionAditiva expresionMultiplicativa
             expresionUnaria expresionSufija constante  
%type<list> instruccionIteracion instruccionSeleccion
            //bloque
%%

programa: {
niv = 0; dvar = 0; si = 0; cargaContexto(niv); 
 /*************** Reserva de espacio para variables globales */
       $<list>$.a = creaLans(si);
       emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(-1));
       /*************** Salto al comienzo de la funcion "main" */
       $<list>$.b = creaLans(si);
       emite(GOTOS, crArgNul(), crArgNul(), crArgEtq(-1));
}
      listaDeclaraciones  { 
      if($2 != -1) {     //$2 es listaDeclaraciones ya que {niv=0;...} ocupa el puesto $1   NOTA: $2 valdra -1 si hay una funcion main
        yyerror(ERROR_NO_HAY_MAIN);
        
        }
        /***** Completa espacio para las variables globales */
       completaLans($<list>1.a, crArgEnt(dvar));
       /***** Completa salto al comienzo del "main" */
       //TABLA DE SIMBOLOS --> acceder a la tabla
       SIMB sim = obtTdS("main");
       $<list>$.c = sim.d;
       completaLans($<list>1.b, crArgEtq($<list>$.c));

           // XX es la dirección de la primera instrucción del "main"
           // que está en la TdS y que la podéis obtener de diversas
           // formas dependiendo como controléis el "main"
        
        //if(verTdS) mostrarTdS();
      };
/*****************************************************************************/
listaDeclaraciones:
      declaracion {$$ = $1;}
    | listaDeclaraciones declaracion {$$ = $1 + $2;}      //La suma valdra -1 si hay una declaracion que represente a la funcion main
    ;
/*****************************************************************************/
declaracion:
      declaracionVariable {  $$ = 0; }                               //Propagamos un 0 porque no es una funcion y por tanto no puede ser un main
    | declaracionFuncion {  $$ = $1; }                               //La funcion se trata en declaracionFuncion y se propagara un 0 o un -1 dependiendo si es un main                           
    ;
/*****************************************************************************/
declaracionVariable:
      tipoSimple ID_ PCOMA_ {
        if (!insTdS($2,VARIABLE,$1,niv,dvar,-1)) { 
                yyerror(ERROR_VARIABLE_DECLARADA);
        } else {
                dvar += TALLA_TIPO_SIMPLE;                         
        } 
      }
    | tipoSimple ID_ COR1_ CTE_ COR2_ PCOMA_ {
        if ($4 <= 0) { yyerror(ERROR_ARRAY_INVALIDO);}                // 1 - el numero de elementos de una array no puede ser un valor negativo
        else {    
              int ref; 
              ref = insTdA($1,$4);
              if (!insTdS($2, VARIABLE, T_ARRAY, niv, dvar, ref)) {  //NUEVO array       2 - el nombre de dicho array (ID o $1) ya existe y por tanto no se puede declarar
                yyerror(ERROR_VARIABLE_DECLARADA);
              } else {
                dvar += $4 * TALLA_TIPO_SIMPLE;                          
              }    
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
      cabeceraFuncion {$<aux>$ = dvar; dvar = 0;} bloque 
      {  
         if(verTdS) mostrarTdS();
         descargaContexto(niv); niv=0; dvar=$<aux>2;
         $$=$1;
         }
                    //continuamos propagando el valor del atributo de la cabecera de la funcion que sera -1 si la funcion es un main        
        ;
/*****************************************************************************/
cabeceraFuncion:
      tipoSimple ID_ {niv=1; cargaContexto(niv); } PAR1_ parametrosFormales PAR2_ { 
         if(!insTdS($2,FUNCION,$1,0,si,$5.ref)){ 
             yyerror(ERROR_VARIABLE_DECLARADA);
         } 
         if (strcmp($2,"main\0")==0) { $$ = -1; } else{ $$ = 0; } /* para Funcion Main*/
      }
    ;
/*****************************************************************************/
parametrosFormales:
    { $$.ref = insTdD(-1,T_VACIO);
      $$.talla = 0;}     //NUEVO               no se utiliza luego $$.tipo = T_vacio; $$.talla = 0;}                 
    | listaParametrosFormales
        { $$.ref = $1.ref;
          $$.talla = $1.talla; } //NUEVO     $$.tipo = $1.tipo; $$.talla = $1.talla - TALLA_SEG_ENLACES; }                
    ;
/*****************************************************************************/
listaParametrosFormales:
      tipoSimple ID_ 
     {
        $$.ref = insTdD(-1,$1);            //$$.n representa la referencia a la tabla de dominios donde se guardaran los parametros
        $$.talla = TALLA_TIPO_SIMPLE + TALLA_SEGENLACES;
        if(!insTdS($2,PARAMETRO,$1,niv,-$$.talla,-1)) {
            yyerror(ERROR_VARIABLE_DECLARADA);
        } 
     } 
    | tipoSimple ID_ COMA_ listaParametrosFormales    
    {
        $$.ref = insTdD($4.ref,$1);          //insertamos el tipo simple $1 en la tabla de dominio con la referencia dada por la expresi�n listaParametrosFormales($4) y definida anteriormente
        $$.talla = $4.talla + TALLA_TIPO_SIMPLE;
        if(!insTdS($2,PARAMETRO,$1,niv,-$$.talla,-1)) {
            yyerror(ERROR_VARIABLE_DECLARADA);
        } 

     }
    ;
/*****************************************************************************/
bloque:            // GENERACION DE CODIGO
      LLAVE1_ {
        emite( PUSHFP, crArgNul(), crArgNul(), crArgNul() );
        emite( FPTOP, crArgNul(), crArgNul(), crArgNul() );
        $<aux>$ = creaLans(si);
        emite( INCTOP, crArgNul(), crArgNul(), crArgEnt(-1) );                   
      } 
      declaracionVariableLocal listaInstrucciones RETURN_ expresion PCOMA_ LLAVE2_ { 
        INF inf = obtTdD(-1);
        if(inf.tipo != T_ERROR){
          if(inf.tipo != $6.tipo){
            yyerror(ERROR_DE_TIPO);        
          }
        } 
        completaLans( $<aux>2, crArgEnt(dvar) );
        int dret = TALLA_SEGENLACES + TALLA_TIPO_SIMPLE + inf.tsp;
        emite( EASIG, crArgPos(niv,$6.pos) , crArgNul(), crArgPos(niv, -dret));
        emite( TOPFP, crArgNul(), crArgNul(), crArgNul() );
        emite( FPPOP, crArgNul(), crArgNul(), crArgNul() );
        if (strcmp(inf.nom,"main")==0) { emite( FIN, crArgNul(), crArgNul(), crArgNul() ); }
        else { emite( RET, crArgNul(), crArgNul(), crArgNul() ); }
        //MAIN --> fin SINO --> return
      }
    ;  
/*****************************************************************************/
declaracionVariableLocal:              // NO SE EMITE NADA 
    | declaracionVariableLocal declaracionVariable        
    ;
/*****************************************************************************/
listaInstrucciones:                    // NO SE EMITE NADA
    | listaInstrucciones instruccion
    ;
/*****************************************************************************/
instruccion:                            // NO SE EMITE NADA
      LLAVE1_ listaInstrucciones LLAVE2_
    | instruccionAsignacion 
    | instruccionSeleccion
    | instruccionEntradaSalida
    | instruccionIteracion
    ;
/*****************************************************************************/
instruccionAsignacion:                    
      ID_ ASIG_ expresion PCOMA_            //SI SE EMITE

        {   SIMB sim = obtTdS($1);
            if($3.tipo != T_ERROR){                         

                if (sim.t == T_ERROR) yyerror(ERROR_OBJ_NO_DECLARADO);
                else if (! ((sim.t == $3.tipo && sim.t == T_ENTERO) || (sim.t == $3.tipo && sim.t == T_LOGICO)))               //si el tipo de la expresion es diferente del tipo de la variable devolvemos error
                    yyerror(ERROR_DE_TIPO);
            }
            emite(EASIG, crArgPos(niv, $3.pos) , crArgNul(), crArgPos(sim.n, sim.d));

        }
    | ID_ COR1_ expresion COR2_ ASIG_ expresion PCOMA_      //TAMBIEN SE EMITE                                                  

        {
            SIMB sim = obtTdS($1);
            if(sim.t != T_ARRAY){ yyerror("No es de tipo array"); } 
            else{ DIM di = obtTdA(sim.ref);
            
            if($3.tipo != T_ERROR && $6.tipo != T_ERROR){                    
                if (sim.t == T_ERROR) yyerror(ERROR_VARIABLE_DECLARADA);                                          // 1 - el array ID ha sido declarado 
                else if (! (sim.t == T_ARRAY))  yyerror(ERROR_DE_TIPO);                                           // 2 - el objeto ID no es un array
                else if (! ($3.tipo == T_ENTERO))  yyerror("El numero de elemento del array no es un entero");    // 3 - el indice para acceder al array no es de tipo entero
                else if (! ($6.tipo == di.telem)) { yyerror(ERROR_DE_TIPO); }                                     // 4 - el tipo de los objetos del array no coincide con la expresi�n a asignar
                //else if (! ($3.valor < di.nelem)) yyerror("Error en el indice que accede al array (fuera de rango)");  // 5 - el indice del array accede a una posicion incorrecta
            }
            }
                        emite(EVA, crArgPos(sim.n, sim.d) , crArgPos(niv, $3.pos), crArgPos(niv, $6.pos));
        }
    ;
/*****************************************************************************/
instruccionEntradaSalida:
      READ_ PAR1_ ID_ PAR2_ PCOMA_       
        {
            SIMB sim = obtTdS($3);
            
            if (sim.t != T_ENTERO) yyerror(ERROR_DE_TIPO);
            emite(EREAD,crArgNul(),crArgNul(),crArgPos( sim.n , sim.d ));  
        }
      
    | PRINT_ PAR1_ expresion PAR2_ PCOMA_

        {

            if ($3.tipo != T_ERROR && $3.tipo != T_ENTERO) yyerror("Solo se pueden imprimir variables enteras");
            emite(EWRITE,crArgNul(),crArgNul(),crArgPos( niv , $3.pos) );  // escribimos la expresion

        }
    ;
/*****************************************************************************/
instruccionSeleccion:
      IF_ PAR1_ expresion PAR2_ 
      
      {
          if ($3.tipo != T_ERROR) {
              if ($3.tipo != T_LOGICO){ yyerror(ERROR_DE_TIPO);}    // expresion debe de ser de tipo logico
          }
          $<list>$.cent = creaLans(si);  
          emite(EIGUAL,crArgPos( niv , $3.pos ),crArgEnt(0),crArgEtq(-1));  //if expresion false (=0) va a else
      }

      instruccion {
          $<list>$.cent = creaLans(si); 
          emite(GOTOS,crArgNul(),crArgNul(),crArgEtq(-1));  
          completaLans($<list>5.cent,crArgEtq(si));
          } 

      ELSE_ instruccion {completaLans($<list>7.cent,crArgEtq(si));}    
    ;
/*****************************************************************************/
instruccionIteracion:
      FOR_ PAR1_ expresionOpcional PCOMA_ // for ( i = 1; i < t;i++)
      {
       $<list>$.cent = si; 
      } 
      expresion PCOMA_      
      
      {
          if ($6.tipo != T_ERROR) {
              if ($3.tipo != T_LOGICO && $3.tipo != T_VACIO && $3.tipo != T_ENTERO){ yyerror(ERROR_DE_TIPO);
              }
          }
          $<list>$.a = creaLans(si);                              // TRUE
          emite(EIGUAL,crArgPos( niv , $6.pos ),crArgEnt(1),crArgEtq(-1) );
          $<list>$.b = creaLans(si);                              //FALSE
          emite(GOTOS,crArgNul(),crArgNul(),crArgEtq(-1));
          $<list>$.c = si;    
      }
      
      expresionOpcional PAR2_ {
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<list>5.cent) ); 
          completaLans($<list>8.a,crArgEtq(si));
      } 
      instruccion {
          emite(GOTOS, crArgNul(), crArgNul(), crArgEtq($<list>8.c) ); 
          completaLans($<list>8.b,crArgEtq(si));
      }              

    ;
// -----------------------------------------------------------------------------------------------------------------XIMO Y EDU
/*****************************************************************************/
expresionOpcional:
        {
            $$.tipo = T_VACIO;
        }

    | expresion          // a 

        {
            $$.tipo = $1.tipo;
            $$.pos = $1.pos;
            //DIAPO 9 (Teoria)
        }

    | ID_ ASIG_ expresion   // a = 3

        {
            $$.tipo = T_ERROR;
            SIMB sim = obtTdS($1);

            if (sim.t != T_ERROR && $3.tipo != T_ERROR){
                if (sim.t == $3.tipo) {$$.tipo = sim.t;}
                else {yyerror(ERROR_DE_TIPO);
                }
            }

            //$$.pos = creaVarTemp();
            //emite(EASIG, crArgPos( niv , $3.pos), crArgNul(), crArgPos(niv , $$.pos));  // OJO : emite (id.d = E.d) ??  --> en todo caso es sim.d = S3.pos no ?  
            emite(EASIG, crArgPos(niv, $3.pos), crArgNul(), crArgPos(sim.n , sim.d));
        }

    ;
/*****************************************************************************/
expresion:
      expresionIgualdad { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresion operadorLogico expresionIgualdad    // a AND b |  a OR b
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror(ERROR_DE_TIPO);
            }                           // 1 - Los operadores logicos se aplican sobre variables del mismo tipo
            else if ($1.tipo != T_LOGICO) {yyerror("Operacion logica invalida para no booleanos");} // 2 - Los operadores logicos se aplican sobre variables booleanas
            else {$$.tipo = T_LOGICO;}
        }
        $$.pos = creaVarTemp();
        if ($2 == EMULT) { // DUDA
            emite(EMULT, crArgPos(niv, $1.pos), crArgPos(niv, $3.pos), crArgPos(niv, $$.pos));
        }else {
            emite(ESUM, crArgPos(niv,$1.pos), crArgPos(niv,$3.pos), crArgPos(niv,$$.pos));
            emite(EMENEQ, crArgPos(niv,$$.pos), crArgEnt(1), crArgEtq(si+2));
            emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv,$$.pos));
        }
    }
    ;
/*****************************************************************************/
expresionIgualdad:
      expresionRelacional { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresionIgualdad operadorIgualdad expresionRelacional  // a == b | a != b
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) {yyerror(ERROR_DE_TIPO);}                                // 1 - La operacion de igualdad opera con variables del mismo tipo
            else if ($3.tipo != T_LOGICO && $3.tipo != T_ENTERO) { yyerror("No se puede aplicar el operador de igualdad.");}  // 2 - La expresion de Igualdad se puede aplicar a enteros y a logicos aunque siempre se va a aplicar a valores logicos debido a la expresionRelacional
            else {$$.tipo = T_LOGICO;}
        }
        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv,$$.pos));
        emite($2, crArgPos(niv,$1.pos), crArgPos(niv,$3.pos), crArgEtq(si+2));
        emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv,$$.pos));
    }
    ;
/*****************************************************************************/
expresionRelacional:    //EDU
      expresionAditiva { $$.tipo = $1.tipo; $$.pos = $1.pos;}
    | expresionRelacional operadorRelacional expresionAditiva  // a > b | a < b | a <= b | a >= b     
    {  $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror(ERROR_DE_TIPO);
            }                                                  
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion relacional solo acepta argumentos enteros.");}   
            else {
                $$.tipo = T_LOGICO;                      
            }
        }
        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt(1), crArgNul(), crArgPos(niv,$$.pos));
        emite($2, crArgPos(niv,$1.pos), crArgPos(niv,$3.pos), crArgEtq(si+2));
        emite(EASIG, crArgEnt(0), crArgNul(), crArgPos(niv,$$.pos));
    }
    ;
/*****************************************************************************/
expresionAditiva:
      expresionMultiplicativa { $$.tipo = $1.tipo;$$.pos = $1.pos; }
    | expresionAditiva operadorAditivo expresionMultiplicativa  // a + b | a - b 
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror(ERROR_DE_TIPO);}                // 1 - El operador aditivo necesita dos variables del mismo tipo
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion aditiva solo acepta argumentos enteros.");} // 2 - El operador aditivo solo opera con variables enteras
            else { $$.tipo = T_ENTERO; }
        }

        $$.pos = creaVarTemp();
        emite($2, crArgPos(niv,$1.pos), crArgPos(niv,$3.pos), crArgPos(niv,$$.pos));
    }
    ;
/*****************************************************************************/
expresionMultiplicativa:
      expresionUnaria { $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | expresionMultiplicativa operadorMultiplicativo expresionUnaria // a/b | a*b 
    {   $$.tipo = T_ERROR;
        if ($1.tipo != T_ERROR && $3.tipo != T_ERROR) {
            if ($1.tipo != $3.tipo) { yyerror(ERROR_DE_TIPO);}                 // 1 - El operador de multiplicacion se debe aplicar a dos tipos iguales
            else if ($1.tipo != T_ENTERO) { yyerror("Operacion multiplicativa solo acepta argumentos enteros.");}  // 2 - El operador multiplicativo es solo para variables enteras
             else {$$.tipo = T_ENTERO;}
        }
        $$.pos = creaVarTemp();
        emite($2, crArgPos(niv,$1.pos), crArgPos(niv,$3.pos), crArgPos(niv,$$.pos));
    }
    ;
/*****************************************************************************/
expresionUnaria:
      expresionSufija{ $$.tipo = $1.tipo; $$.pos = $1.pos; }
    | operadorUnario expresionUnaria     // ?? ->  + a| -a| not a  
    {   $$.tipo = T_ERROR;
        if ($2.tipo != T_ERROR) {
            if ($2.tipo == T_ENTERO) {                                                                         //SI $2 ES UN ENTERO:
                if ($1 == ESIG) {yyerror("Operacion NOT invalida en expresion entera.");}                    //1 - el operador NOT no se puede aplicar a enteros
                else{ $$.tipo = T_ENTERO; }
            } else if ($2.tipo == T_LOGICO) {                                                                  //SI $2 ES UN BOOLEAN
                if($1==ESUM || $1 == EDIF){yyerror("Operacion entera no vaida para un booleano.");}     //2 - no se puede sumar ni restar booleanos
                else{ $$.tipo = T_LOGICO;}
            }
            else{yyerror(ERROR_DE_TIPO);
            }                                                               //3 - $2 debe ser entero o logico
            $$.pos = creaVarTemp();
            if ($1 == ESIG) {
                emite(EDIF, crArgEnt(1), crArgPos(niv,$2.pos), crArgPos(niv,$$.pos));    
            } else {
                emite($1, crArgEnt(0), crArgPos(niv,$2.pos), crArgPos(niv,$$.pos));
            }
        } 
    }
    | operadorIncremento ID_   // ++i | --i
    {   SIMB simb = obtTdS($2);
        $$.tipo = T_ERROR;
        if (simb.t == T_ERROR) {yyerror(ERROR_VARIABLE_DECLARADA);}                                         // 1 - La variable(ID) no esta definida
        else if (simb.t != T_ENTERO){yyerror("Error no se puede incrementar una variable no entera");}          // 2 - La variable(ID) no es entera y por tanto no se puede icrementar
        else {$$.tipo = simb.t;}
        $$.pos = creaVarTemp();
        emite($1, crArgPos(simb.n,simb.d), crArgEnt(1), crArgPos(simb.n,simb.d));
        emite(EASIG, crArgPos(simb.n,simb.d), crArgNul(), crArgPos(niv,$$.pos));
    }
    ;
/*****************************************************************************/
expresionSufija:
      PAR1_ expresion PAR2_ { $$.tipo = $2.tipo; $$.pos = $2.pos;}
    | ID_ operadorIncremento   // i++ | i--
    {   SIMB simb = obtTdS($1);
        $$.tipo = T_ERROR;
        if (simb.t == T_ERROR){ yyerror(ERROR_VARIABLE_DECLARADA); }                                  // 1 - La variable (ID) no esta definida
        else if (simb.t != T_ENTERO){ yyerror("Error no se puede incrementar una variable no entera");}   // 2 - La variable no es entera y no se puede incrementar
        else{ $$.tipo = simb.t; }
        $$.pos = creaVarTemp();
        emite(EASIG, crArgPos(simb.n,simb.d), crArgNul(), crArgPos(niv,$$.pos)); 
        emite($2, crArgPos(simb.n,simb.d), crArgEnt(1), crArgPos(simb.n,simb.d));
    }
    | ID_ COR1_ expresion COR2_  // a[1] --> array
    {   SIMB simb = obtTdS($1);
        $$.tipo = T_ERROR;
        if (simb.t == T_ERROR){  yyerror(ERROR_VARIABLE_DECLARADA);}                                  // 1 - La variable (ID) no esta definida
        else if (simb.t != T_ARRAY) {yyerror("La variable no es un array no puede ser accedida");}        // 2 - La variable no es un array y no se puede acceder a sus indices
        else if($3.tipo != T_ENTERO){yyerror("Error , indice no entero");}                                // 3 - El indice de acceso al array no es un entero
        else{
            DIM dim = obtTdA(simb.ref);
            $$.tipo = dim.telem;
        }
        $$.pos = creaVarTemp();
        emite(EAV, crArgPos(simb.n,simb.d), crArgPos(niv,$3.pos), crArgPos(niv,$$.pos)); 
    } 
    
    | ID_ PAR1_               // f (params) id 
    {   
        emite(INCTOP, crArgNul(), crArgNul(), crArgEnt(TALLA_TIPO_SIMPLE)); 
    }
    parametrosActuales PAR2_
    {
        SIMB simb = obtTdS($1);
        $$.tipo = T_ERROR;
        if (simb.t == T_ERROR){ yyerror(ERROR_VARIABLE_DECLARADA); }                                  // 1 - La variable (ID) no esta definida
        INF inf = obtTdD(simb.ref);
        if (inf.tipo == T_ERROR){ yyerror("Funcion no definida"); }                                   // 2 - La variable (ID) no es una funcion 
        else {$$.tipo = inf.tipo;}
        //emite(EPUSH, crArgNul(), crArgNul(), crArgPos(niv, si+2)); //Cuidado
        emite(CALL, crArgNul(), crArgNul(), crArgEtq(simb.d)); //Cuidado
        emite(DECTOP, crArgNul(), crArgNul(), crArgEnt(inf.tsp)); //cuidado
        $$.pos = creaVarTemp();
        emite(EPOP, crArgNul(), crArgNul(), crArgPos(niv, $$.pos));
    }
    | ID_
    {   SIMB simb = obtTdS($1);
        $$.tipo = T_ERROR;
        if (simb.t == T_ERROR) {yyerror(ERROR_VARIABLE_DECLARADA);}                                // 1 - La variable (ID) no esta definida
        else {$$.tipo = simb.t; }
        $$.pos = creaVarTemp();
        emite(EASIG, crArgPos(niv,simb.d), crArgNul(), crArgPos(niv,$$.pos));   
    }
    | constante {            // 4
        $$.tipo = $1.tipo;
        $$.pos = creaVarTemp();
        emite(EASIG, crArgEnt($1.pos), crArgNul(), crArgPos(niv,$$.pos)); 
        }
    ;
/*****************************************************************************/
parametrosActuales:    {$$ = insTdD(-1,T_VACIO);}      //OJO    
    | listaParametrosActuales {$$ = $1;}
    ;
/*****************************************************************************/
listaParametrosActuales:
    expresion
    { $$ = insTdD(-1,$1.tipo);
    emite(EPUSH,crArgNul(),crArgNul(),crArgPos(niv,$1.pos));}
    | expresion COMA_ { emite(EPUSH,crArgNul(),crArgNul(),crArgPos(niv,$1.pos));}
    listaParametrosActuales
    { $$ = insTdD($4,$1.tipo);}
    ;
/*****************************************************************************/
constante:
      CTE_          { $$.tipo = T_ENTERO; $$.pos = $1;} 
    | TRUE_         { $$.tipo = T_LOGICO; $$.pos = 1;} 
    | FALSE_        { $$.tipo = T_LOGICO; $$.pos = 0;} 
    ;
/*****************************************************************************/
/* En el caso de los operadores tienen un atributo de tipo int que nos servira 
   para saber de que tipo de operador se trata , en el siguiente caso:
    1 - El atributo del operadorLogico tomara dicho valor si se trata de una AND
    2 - El atributo del operadorLogico tomara dicho valor si se trata de una OR
  (Lo mismo se aplica para el resto de operadores que le siguen)
*/
operadorLogico:
      AND_          {$$ = EMULT;}
    | OR_           {$$ = ESUM;}
    ;
/*****************************************************************************/
operadorIgualdad:
      IGUAL2_       {$$ = EIGUAL;}
    | NOTIGUAL_     {$$ = EDIST;}
    ;
/*****************************************************************************/
operadorRelacional:
      MAYOR_        {$$ = EMAY;}
    | MENOR_        {$$ = EMEN;}
    | MAYORIG_      {$$ = EMAYEQ;}
    | MENORIG_      {$$ = EMENEQ;}
    ;
/*****************************************************************************/
operadorAditivo:
      SUMA_         {$$ = ESUM;}
    | RESTA_        {$$ = EDIF;}
    ;
/*****************************************************************************/
operadorMultiplicativo:
      MULT_         {$$ = EMULT;}
    | DIV_          {$$ = EDIVI;}
    ;
/*****************************************************************************/
operadorUnario:
      SUMA_         {$$ = ESUM;}     
    | RESTA_        {$$ = EDIF;}
    | NOT_          {$$ = ESIG;}
    ;
/*****************************************************************************/
operadorIncremento:
      SUMA2_        {$$ = ESUM;}
    | RESTA2_       {$$ = EDIF;}
    ;

%%
/*****************************************************************************/
