;EQUIVALENCIAS USADAS PARA REPRESENTAR LAS POSICIONES DE LA IMPRESION DE CADENAS EN PANTALLA
;ESTAS 4 ADEMAS SE USAN PARA CONTROLAR SI UN CLIC SE ENCUENTRA DENTRO DEL TABLERO 
XTABLEROINI EQU 10
YTABLEROINI EQU 2  
XTABLEROFIN EQU 26 ;XTABLEROINI+8*2
YTABLEROFIN EQU 18 ;YTABLEROINI+8*2  

XBLOQ EQU 35  
YBLOQ EQU 4
XMINAS EQU 35
YMINAS EQU 8
XMENSAJES EQU 30
YMENSAJES1 EQU 2
YMENSAJES2 EQU 14
YMENSAJES3 EQU 16
YMENSAJES4 EQU 18          
YMENSAJES5 EQU 20          
XMENSAJECLIC EQU 10
YMENSAJECLIC EQU 23

;EQUIVALENCIAS USADAS PARA REPRESENTAR LOS COLORES DE TEXTO
COLORBLOQUEADO EQU 47h
COLORTAPADO EQU 07h
COLORDESTAPADO EQU 97h

data segment

  ;Mensaje impreso con cada nueva partida
  msjBienvenida1 db 10, 13, 10, 13, 10, 13
              db " ±±±±±± ±±   ±±  ±±±±±± ±±±±±±± ±±±±±± ±±±   ±±± ±±±±±  ±±±   ± ±±±±±± ±±±±±±",10,13
              db " ±±  ±± ±±   ±±  ±      ±±      ±    ± ± ±± ±± ±   ±    ± ±±  ± ±    ± ±     ", 10, 13
              db " ±±±±   ±±   ±±  ±±±±±± ±±      ±±±±±± ±  ±±±  ±   ±    ±  ±± ± ±±±±±± ±±±±±±", 10, 13
              db " ±±  ±± ±±   ±±       ± ±±      ±    ± ±       ±   ±    ±   ±±± ±    ±      ±", 10, 13
              db " ±±±±±± ±±±±±±±  ±±±±±± ±±±±±±± ±    ± ±       ± ±±±±±  ±    ±± ±    ± ±±±±±±", 10, 13, '$'
  
  ;Mensaje que aparece unicamente al comenzar el juego o tras terminar y elegir jugar con un nuevo tablero
  msjBienvenida2 db 13,10,13,10, "Modo debug (con tablero precargado)? (s/n)$"  
  
  ;Mensjae de espera que aparece unicamente al elegir no jugar con un tablero precargado
  msjInicializando db 10,13,"Inicializando tablero pseudo-aleatorio ;-)"
                   db 10,13,"Me lleva un momentito...$"
  
  ;Dibujo del tablero de juego          
  dibTablero db 10,13,10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,                      
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,                      
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,                          
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,            
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+',10,13,
          db '          | | | | | | | | |',10,13,
          db '          +-+-+-+-+-+-+-+-+','$'     
  
  ;Numero de casillas del juego (8x8). Es DW por facilitar codigo (e.g., mov cx, TotalCasillas) 
  TotalCasillas dw 64  
  
  ;Numero de minas que se crean con cada nueva partida                                
  NumMinas db 10 
  ;Almacenara la posicion (0...63) de las 10 minas cuya posicion calculamos aleatoreamente
  vectorMinas db 10 dup(0)
  
  
  ;Mensaje para indicar al usuario que puede jugar haciendo clic con el raton
  EsperaClic db "Esperando clic de raton$"    
  
  ;Mensaje para borrar/sobreescribir el mensaje de "Esperando clic de raton" y el numero de casillas bloqueadas
  CadBorrar db "                       $" 
    
  ;Mensaje que precede al numero de casillas que el usuario a bloqueado
  mensajeMinas db "Bloqueadas:$" 
  
  ;Mensaje que precede al numero de minas bloqueadas
  mensajeMinasJ db "Minas Bloqueadas: $" 
  
  ;Mensaje que aparece cuando el usuario pulsa la 'X' de salir y selecciona "Salir del juego (s)"
  mensajeSalida db "Has abandonado la partida$"
  ;Mensaje que aparece al perder una partida
  mensajePerdida db "Has perdido la partida$"
  ;Mensaje que aparece al ganar una partida  
  mensajeGanada db "Enhorabuena! Has ganado la partida$" 
  
  ;Mensajes que aparece al terminar una partida para indicar al usuario que puede seleccionar: salir del juego o reiniciar con un nuevo tablero o el actual 
  mensajeSalir db "Salir del juego (s)$"
  mensajeNuevoJ1 db "Reiniciar con un nuevo tablero (n)$" 
  mensajeNuevoJ2 db "Reiniciar con el tablero actual (a)$"
  
  ;Mensaje que aparece durante la partida para indicar al usuario la forma de salir de la partida en cualquier momento
  mensajeS db "X - Clic sobre la 'X' para salir de la partida$"


  ;Tablero de debug, para poder testear mas rapidamente condiciones
  ;-1 indica una mina en esa casilla
  ;0 una posicion vacia 
  ;cualquier otro numero se refiere al contador de minas alrededor de esa casilla
  MtableroDBG db 0, 0, 0, 0, 0, 0, 0, 0
              db 0, 0, 0, 0, 0, 0, 0, 0
              db 1, 1, 1, 0, 0, 0, 0, 0
              db 1, -1, 1, 0, 0, 0, 0, 0
              db 2, 2, 2, 0, 0, 0, 0, 0
              db 2, -1, 2, 0, 1, 1, 1, 0
              db 3, -1, 5, 3, 4, -1, 3, 1
              db 2, -1, -1, -1, -1, -1, -1, 1   
  ;Tablero de juego
  Mtablero db 64 dup(0)
  
  ;Almacena las posiciones ya destapadas por el usuario 
  ;0 indica no destapada
  ;1 indica destapada 
  Destapado db 64 DUP(0)
  
  ;Almacena las posiciones bloqueadas por el usuario 
  ;0 indica no bloqueada
  ;1 indica bloqueada 
  Bloqueado db 64 DUP(0)

  ;Varibles usadas para colocar el cursor para escribir cadenas
  fila  db 0
  colum db 0
  
  ;Almacenan la fila y columna donde se ha hecho clic 
  fTablero db ?
  cTablero db ?
  
  ;Almacenan la fila, columna y boton pulsado -> 1: izquierdo; 2:derecho 
  fRaton db ?
  cRaton db ?
  botones db 0
  
  ;Caracteres para imprimir Mina (*), Bloque y borrar Bloque (espacio)
  CarMina db '*'
  CarBloq db 'B'
  CarEspa db ' '
  
  ;Almacena si se ha encontrado una mina (1) o no (0) en usa casilla
  hayMina db ?
  
  ;Numero de casillas bloqueadas por el jugador
  casillasBloq db 0
  
  ;Numero de minas bloqueadas
  minasBloq db 0    
  
  ;Numero de casillas destapadas
  destapadas db 0
  
  ;Indica si la condicion de finalizacion del juego:
  ;FIN=0 -> la partida no ha terminado
  ;FIN=1 -> la partida ha terminado por abandono/salida (clic en X)
  ;FIN=2 -> la partida ha terminado perdiendo el jugador (mina destapada)
  ;FIN=3 -> la partida ha terminado ganando el jugador  
  fin db 0

  ;cadena para la salida por pantalla
  cadenaEsc db 3 DUP('$')
  
  ;Almacena la respuesta del usuario sobre comenzar una nueva partida
  NPartida db ?
 
data ends
   
   
   
stack segment
  DW 128 DUP(0)
stack ends

   
   
code segment                                     
    
;**********************************   PROCEDIMIENTOS DE GENERACION DE NUMEROS ALEATORIOS *******************************    
       
  
  ;F: Calcula un valor aleatorio entre 0 y TotalCasillas-1 
  ;S: AH valor aleatorio
  NumAleatorio PROC
    push cx
    push dx

    mov ah,2Ch ;interrupcion que recupera la hora actual del sistema operativo
    int 21h
    ;ch=horas
    ;cl=minutos
    ;dh=segundos
    ;dl=centesimas de segundo, 1/100 secs

    xor ah,ah
    mov al,dl    
    div BYTE PTR TotalCasillas  ;BYTE PTR valor16 -> hace que del valor de 16 bits solo se usen los 8 bits de menor peso 
    ;AH contiene el aleatorio        
    
    pop dx
    pop cx
    ret
  NumAleatorio ENDP 
               
               
  ;F: Calcula un vector de numeros aleatorios todos ellos distintos entre si
  ;E: SI direccion de la variable vector donde almacenar los numeros
  ;   CX numero de aleatorios a generar y almacenar
  VectorAleatDist PROC
    push ax  
    push bx
    push si
    push di    
    
    mov bx, si
    
   bucleVectorAleat:            
    
    call NumAleatorio   ;Cambia AX

    mov di,si
   comprobarRepetidoVAleat: 
    dec di
    cmp di,bx
    jl insertarPosicionAleat
    cmp [di],ah
    je bucleVectorAleat
    jmp comprobarRepetidoVAleat

   insertarPosicionAleat: 
    mov [si],ah  
    inc si
    loop bucleVectorAleat  
    
    pop di
    pop si 
    pop bx
    pop ax
    ret
  VectorAleatDist ENDP

                                                                                                                        

;**********************************   PROCEDIMIENTOS YA UTILIZADOS EN OTRAS PRACTICAS O RELACIONADOS CON INT. SOFTWARE  *******************************                                                                                                                        
                                                                                                                        
  ;Convierte un numero entero a una cadena de caracteres terminada en $
  ;E: AX contiene el numero a convertir
  ;   DX contiene la direccion de la cadena donde almacena la cadena resultado
  NumeroACadena PROC 
    push ax
    push bx
    push cx
    push dx
    push di
    
    mov bx, 10
    mov di, dx
    
    xor cx, cx

    cmp ax, 0  
    jge BNumCad

    mov [di], '-'
    inc di 
    neg ax
    
   BNumCad:        ;Bucle que transforma cada digito a caracter, de menor a mayor peso     
    xor dx, dx
    div bx
    add dl, '0'
    push dx 
    inc cx
    cmp ax, 0
    jne BNumCad

   BInvertir:      ;Bucle para invertir los restos    
    pop [di]
    inc di
    loop BInvertir

    mov [di], '$'

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
  NumeroACadena ENDP
               
               
  ;F: Lee una tecla por teclado sin eco por pantalla
  ;S: AL tecla leida
  LeerTecla PROC    
    mov ah,8
    int 21h 
    ret  
  LeerTecla ENDP     
  

  ;F: Oculta el cursor del teclado
  OcultarCursor PROC
    push ax
    push cx

    mov ah,1
    mov ch,20h
    xor cl,cl
    int 10h

    pop cx
    pop ax
    ret
  OcultarCursor ENDP
               
               
  ;F: Muestra el cusor del teclado
  MostrarCursor PROC
    push ax
    push cx

    mov ah,1
    mov ch,0Bh
    mov cl,0Ch
    int 10h

    pop cx
    pop ax
    ret
  MostrarCursor ENDP
      
       
  ;F: Imprime una cadena terminada en $ en la posicion donde se encuentre el cursor 
  ;E: DX direccion de comienzo de la cadena a imprimir    
  Imprimir PROC
    push ax

    mov ah,9h
    int 21h

    pop ax
    ret
  Imprimir ENDP  
  
  
  ;F: Muestra el cursor del raton
  MostrarRaton PROC
    push ax

    mov ax,1
    int 33h

    pop ax
    ret
  MostrarRaton ENDP

  
  ;F: Oculta el cursor del raton
  OcultarRaton PROC
    push ax

    mov ax,2
    int 33h

    pop ax
    ret
  OcultarRaton ENDP

  
  ;F: Lee el estado del raton y espera hasta que se pulse un boton, momento en que devuelve la posicion y boton pulsado
  ;S: DL fila del raton
  ;   DH columna del raton
  ;   BL botones pulsados 1: izquierdo; 2:derecho
  EsperarClicRaton PROC
    push ax
    push cx
    
   bucleEPulsar:
    mov ax, 3
    int 33h
    and bx,3
    jz bucleEPulsar ;Si no se pulsa ningun boton se queda en bucle
    
    shr dx, 3  ;Ajuste modo texto
    shr cx, 3
    mov dh, cl
    
    pop cx
    pop ax
    ret
  EsperarClicRaton ENDP
 
  
  ;F: Borra la pantalla (la deja en negro)
  BorrarPantalla PROC
    push ax
    push bx
    push cx
    push dx

    mov ah,6h
    xor al,al
    mov bh,7
    xor cx,cx
    mov dh,24
    mov dl,79
    int 10h 

    pop dx
    pop cx
    pop bx
    pop ax
    ret
  BorrarPantalla ENDP


  ;F: Imprime un caracter a color en la posicion actual del cursor
  ;E: AL contiene el caracter y BL el codigo de color a imprimir
  ImprimeCarColor PROC
    push ax
    push bx
    push cx

    mov ah, 9
    add bl, 7
    xor bh, bh
    mov cx, 1
    int 10h

    pop cx
    pop bx
    pop ax
    ret
  ImprimeCarColor ENDP

  
  ;F: Coloca el cursor en una determinada fila y columna de pantalla
  ;E: las variables FILA y COLUM deben contener los valores de posicion deseados
  ColocarCursor PROC
    push ax
    push bx
    push dx

    mov ah, 2
    mov dh, fila
    mov dl, colum
    xor bh, bh
    int 10h

    pop dx
    pop bx
    pop ax
    ret         
  ColocarCursor ENDP                                                                                                   

  
  ;F: Limpia el buffer de entrada del teclado por si tuviera algo
  LimpiarBufferTeclado PROC
    push ax

    mov ax,0C00h
    int 21h

    pop ax
    ret
  LimpiarBufferTeclado ENDP  
  

;**********************************   PROCEDIMIENTOS DE INICIALIZACION Y GESTION DE LA INTERFAZ  *******************************  
      
  ;F: Dibuja el tablero en pantalla
  DibujarTablero PROC
    push dx  
     
    lea dx, dibTablero
    call imprimir 
 
    mov fila, YBLOQ
    mov colum, XBLOQ
    call ColocarCursor
    lea dx, mensajeMinas
    call imprimir
    
    mov fila, 0       ;Mensaje para pulsar y salir de la partida actual 
    mov colum, 0
    call ColocarCursor
    lea dx, mensajeS
    call imprimir
      
    pop dx
    ret
  DibujarTablero ENDP

  
  ;F: Copia todas las casillas de MtableroDBG a Mtablero
  InicializaTableroDBG PROC
    push cx
    push si
    push di
    
    mov cx, TotalCasillas   ;Esto es simplemente una forma mas eficiente de hacer 
    lea si, MtableroDBG     ;un loop que contendria mov [DI], [SI] y que aumenta
    lea di, Mtablero        ;automaticamente en cada repeticion SI y DI
    rep movsb               ;Igual que en el loop, CX funciona como contador
    
    pop di
    pop si
    pop cx
    ret
  InicializaTablerodbG ENDP

  
  ;F: Borra la pantalla e imprime las cadenas de la pantalla de inicio de cada partida 
  ;   pidiendo al jugador (si corresponde) por si quiere jugar con un tablero precargado 
  InicializarEntorno PROC
    push ax
    push dx              
    
    call BorrarPantalla
    mov fila, 0
    mov colum, 0
    call ColocarCursor
    lea dx, msjBienvenida1
    call Imprimir

    call LimpiarBufferTeclado
    
    cmp NPartida,2
    je fin_inic_Tab

    lea dx, msjBienvenida2
    call Imprimir


   leer_tecla_inic:
    call LeerTecla
    cmp al, 's'
    je inic_TabdbG
    cmp al, 'n'
    jne leer_tecla_inic
    
    
    lea dx,msjInicializando
    call Imprimir
   
    call InicializarTablero
    jmp fin_inic_tab
    
   inic_TabdbG:   
   
    call InicializaTablerodbG 
   
   fin_inic_tab: 
    call BorrarPantalla
    mov fila, 0
    mov colum, 0
    call ColocarCursor
    
    call DibujarTablero
    
    pop dx
    pop ax
    ret    
  InicializarEntorno ENDP  


  ;F: borra el numero de casillas bloqueadas anterior e imprime el numero de celdas bloqueadas actual
  ImprimirBloqueadas PROC
    push ax
    push dx

    mov fila, YBLOQ
    mov colum, XBLOQ + 13
    call ColocarCursor

    lea dx, CadBorrar
    call Imprimir

    mov fila, YBLOQ
    mov colum, XBLOQ + 13
    call ColocarCursor

    mov al, casillasBloq
    xor ah, ah    
    lea dx, cadenaEsc
    call NumeroACadena
    call imprimir

    pop dx
    pop ax
    ret
  ImprimirBloqueadas ENDP
  
  
  ;F: Imprime los mensajes relativos al fin de partida para dar la posibilidad al jugador
  ;   de salir, volver a jugar con el mismo tablero o con uno nuevo, recogiendo en NPartida 
  ;   la eleccion del jugador. Incluye la impresion del numero de minas bloqueadas
  ;E: FIN=1 -> se imprime el mensaje de abandono/salida
  ;   FIN=2 -> se imprime el mensaje de partida perdida
  ;   sino (FIN=3) -> se imprime el mensaje de partida ganada  
  ;S: NPartida=0 -> para salir del juego
  ;   NPartida=1 -> nueva partida con nuevo tablero
  ;   NPartida=2 -> nueva partida con el tablero actual
  ContinuarOnoJuego PROC
    push ax
    push dx
    
    ;imprime el numero de minas bloqueadas
    mov fila, YMINAS      
    mov colum, XMINAS 
    call ColocarCursor

    lea dx, mensajeMinasJ
    call imprimir
    
    mov al, minasBloq    
    xor ah, ah   
    lea dx, cadenaEsc
    call NumeroACadena
    call imprimir


    ;Imprime el mensaje correspondiente segun el valor de la variable FIN
    mov NPartida, 0
    mov fila, YMENSAJES2
    mov colum, XMENSAJES
    call ColocarCursor

    cmp fin,1
    je selec_msj_salida
    
    cmp fin,2
    je selec_msj_perdida
    
    ;sino
    lea dx, mensajeGanada 
    jmp imprime

   selec_msj_salida: 
    lea dx, mensajeSalida
    jmp imprime

   selec_msj_perdida: 
    lea dx, mensajePerdida

   imprime:
    call imprimir


    
    ;imprime los mensajes de opciones al terminar partida
    mov fila, YMENSAJES3  
    mov colum, XMENSAJES
    call ColocarCursor

    lea dx, mensajeSalir
    call imprimir
   
      
    mov fila, YMENSAJES4  
    mov colum, XMENSAJES
    call ColocarCursor

    lea dx, mensajeNuevoJ1
    call imprimir


    mov fila, YMENSAJES5  
    mov colum, XMENSAJES
    call ColocarCursor

    lea dx, mensajeNuevoJ2
    call imprimir
   
   rep_pide_tecla:
    call LeerTecla 
    cmp al, 's'
    je finContinuarJuego
        
    cmp al, 'n'
    jne sgte_tecla
    mov NPartida, 1
    jmp finContinuarJuego
    
   sgte_tecla: 
    cmp al, 'a'
    jne rep_pide_tecla
    mov NPartida, 2
   
     
   finContinuarJuego:       
    pop dx
    pop ax
    ret
    
  ContinuarOnoJuego ENDP  
  
  
  ;F: Mensaje y espera de clic de raton (mosatrandolo antes y ocultandolo al final)
  ;   almacenando en 3 variables el estado del raton en el momento de hacer clic en un boton
  ;S: cRaton = columna del raton
  ;   fRaton = fila del raton
  ;   botones = identificador de los botones pulsados (se explica en EsperarClicRaton)
  EsperarClic PROC
    push dx

    mov fila, YMENSAJECLIC
    mov colum, XMENSAJECLIC
    call ColocarCursor 

    lea dx, EsperaClic
    call Imprimir 

    call MostrarRaton

    call EsperarClicRaton
    mov cRaton, dh
    mov fRaton, dl
    mov botones, bl 
    
    ; ***** nuevo, se limpia zona mensaje de espera 
    mov fila, YMENSAJECLIC
    mov colum, XMENSAJECLIC
    call ColocarCursor 

    lea dx, CadBorrar
    call Imprimir

    call OcultarRaton
   
   finEsperarClic:
    pop dx
    ret
  EsperarClic ENDP  
  


;**********************************   PROCEDIMIENTOS DE CONTROL Y TRANSFORMACION DE POSICIONES *******************************  

  ;F: comprueba si la posicion del clic de raton se encuentra situada dentro del tablero, en una casilla valida
  ;E: posicion actual del puntero del raton (fRaton, cRaton)  
  ;S: AL=0 si es una posicion invalida; AL=1 si es una posicion dentro del tablero y valida  
  PosicionRatonValida PROC    
    xor al, al
        
    cmp cRaton, XTABLEROINI
    jl FinDentroTablero
    cmp cRaton, XTABLEROFIN
    jg FinDentroTablero
    cmp fRaton, YTABLEROINI
    jl FinDentroTablero
    cmp fRaton, YTABLEROFIN
    jg FinDentroTablero    
        
    
    ;si esta en posicion par el raton, la posicion no es una celda valida con toda seguridad
    test cRaton, 1       ;and
    je FinDentroTablero  ;jz
 
    test fRaton, 1       ;and
    je FinDentroTablero  ;jz
   
   RatonPosPar:
    mov al,1   
   
   FinDentroTablero:    
    ret    
  PosicionRatonValida ENDP  

  ;F: transforma la posicion del raton en una posicion de tablero
  ;E: posicion del raton (fRaton, cRaton)  
  ;S: posicion del tablero (fTablero, cTablero)
  PantallaATablero PROC
    push bx

    
    mov bl, cRaton
    sub bl, XTABLEROINI
    shr bl, 1
    mov cTablero, bl    

    mov bl, fRaton
    sub bl, YTABLEROINI
    shr bl, 1     
    mov fTablero, bl 

    pop bx
    ret
  PantallaATablero ENDP

  
  ;F: calcula la posicion de pantalla que le corresponde a una determinada posicion del tablero                                                                                                                   
  ;E: casilla del tablero (fTablero, cTablero)
  ;S: posicion de pantalla (fila y colum)                                                                                                                     
  TableroAPantalla PROC
    push bx

    mov bl, cTablero
    shl bl, 1
    add bl, XTABLEROINI+1
    mov colum, bl

    mov bl, fTablero
    shl bl, 1         
    add bl, YTABLEROINI+1   
    mov fila, bl

    pop bx
    ret
  TableroAPantalla ENDP
                          
  ;F: calcula el indice lineal que le corresponde a la posicion indicada del tablero                        
  ;E: posicion del tablero (fTablero, cTablero)
  ;S: SI
  CalculaIndiceLineal PROC
    push ax
    
    xor ah, ah
    mov al, fTablero
    shl al, 3 ;x8
    add al, cTablero
    mov si, ax
    
    pop ax
    ret
    
  CalculaIndiceLineal ENDP  

  
  
    
;**********************************   PROCEDIMIENTOS RELACIONADOS CON LA LOGICA DEL JUEGO  *******************************    

  
  ;F: Reinicia las variables, no es necesario llamarlo al inicio del juego, pues las variables ya estan inicializadas en su definicion
  ;   pero si es necesario ejecutarlo para jugar una nueva partida
  ResetVariables PROC
    push si
    push cx
    
    mov cx,TotalCasillas 
    xor si,si

  sigue:    
    cmp NPartida,2
    je salta_inic_Mtablero
    mov MTablero[si], 0

   salta_inic_Mtablero:  
    mov Destapado[si],0
    mov Bloqueado[si],0
    inc si
    loop sigue   
    
    mov minasBloq, 0
    mov casillasBloq, 0 
    mov destapadas, 0
    mov fin, 0
         
    pop cx
    pop si
    ret
  ResetVariables ENDP  
  

  ;F: Inicializa la variable MTablero generando posiciones aleatorias para la localizacion de minas
  ;   En base a las minas colocadas se calculan los contadores alrededor de cada mina
  InicializarTablero PROC
    
    ret
  InicializarTablero ENDP    


  ;F: Comprueba si una celda (SI) esta bloqueada o no y la bloquea o desbloquea incluida la salida por pantalla 
  ;E: SI posicion lineal para bloquear/desbloquear
  PosibleBloqueo PROC                                                                         

    ret
  PosibleBloqueo ENDP
  
  
  ;F: Destapa la casilla indicada por el indice SI (si no esta bloqueada). 
  ;   Pinta el caracter de mina si la hay, o lanza DestaparRecursivo si es casilla vacia
  ;E: SI es el indice de la casilla a destapar
  ;S: hayMina = 1 si hay mina; hayMina = 0 si no la hay
  DestaparCasilla PROC
    push ax                             ;Para el caracter a imprimir
    push bx                             ;Para el codigo de color
    push dx                             ;Para cargar la direccion de la cadena de escritura en pantalla 'cadenaEsc' y para manipular la posicion del tablero 
    
    ;Comprueba que la casilla no este bloqueada
    cmp Bloqueado[si], 1
    je finNoMina                        ;Esta bloqueada. No se hace nada
    
    ;No esta bloqueda la casilla. Comprueba si hay mina
    cmp MTablero[si], -1
    je finHayMina                       ;Hay mina
     
    ;No hay mina. Se destapa y si no hay ninguna alrededor se destapan tambien las adyacentes y viceversa 
    call DestaparRecursivo                         
    jmp finNoMina  
                
    finHayMina:                         
        ;Asigna los parametros para llamar a 'ImprimeCarColor'
        mov al, carMina
        xor bl, bl
        ;Coloca el cursor para imprimir la mina en la casilla destapada (indicada por la posicion del raton)
        call TableroAPantalla
        call ColocarCursor
        call ImprimeCarColor
        
        mov hayMina, 1                  ;Actualiza la bandera de condicion de final de partida
        jmp final           
                 
    finNoMina:
        mov hayMina, 0                  ;Actualiza la bandera de condicion de final de partida
        
    final:                              ;Libera la memoria reservada en pila
        pop dx
        pop bx
        pop ax
        ret
  DestaparCasilla ENDP

  
  ;F: Destapa de manera recursiva todas las casillas contiguas a una dada inicialmente por SI
  ;E: SI indica el indice lineal de la casilla a destapar 
  ;E: destapadas
  ;S: destapadas
  DestaparRecursivo PROC
    cmp Destapado[si], 1                 ;Comprueba que la casilla no este destapada
    je finRec                            ;Lo esta. Finaliza la recursion
    
    cmp Bloqueado[si], 1                 ;Comprueba que la casilla no este bloqueada
    je finRec                            ;Lo esta. Finaliza la recursion
    
    ;Se puede destapar la casilla. No esta ni destapada ni bloqueada
    ;Codigo comun a si hay mina alrededor o no
    mov bl, COLORDESTAPADO               ;Asigna el codigo de color
    ;Coloca el cursor para imprimir el numero de minas adyacentes en la casilla destapda (indicada por la posicion del tablero)  
    call TableroAPantalla
    call ColocarCursor

    ;Actualiza el contador y el vector de casillas destapadas
    inc Destapado[si]               
    inc destapadas                   
    
    cmp MTablero[si], 0                  ;Comprueba si hay mina alrededor
    jg imprimeNumero
    
    ;No hay mina alrededor. Se imprime un caracter en blanco y se destapan las adyacentes
    mov al, ' '
    call ImprimeCarColor
    
    ;Se llama recursivamente al procedimiento para DESTAPAR LAS CASILLAS ADYACENTES
    ;Pero primero hay que comprobar que casillas son potencialmente destapables comparando con los limites del tablero
    
    ;Destapa la casilla izquierda
    mov dl, cTablero
    dec dl
    js sgteSup                                              
    
    dec cTablero
    dec si
    call DestaparRecursivo                  
    inc si     
    inc cTablero
               
               
    ;Destapa la casilla inferior izquierda 
    mov dl, fTablero
    inc dl
    cmp dl, 7
    jg sgteSup
    
    dec cTablero
    inc fTablero
    add si, 7
    call DestaparRecursivo
    sub si, 7 
    dec fTablero
    inc cTablero
                  
                  
    ;Destapa la casilla superior izquierda
    sgteIzqSup:
    mov dl, cTablero
    dec dl
    js sgteSup
    mov dl, fTablero
    dec dl
    js sgteSup
    
    dec cTablero
    dec fTablero
    sub si, 9
    call DestaparRecursivo
    add si, 9
    inc fTablero
    inc cTablero               
               
               
    ;Destapa la casilla superior
    sgteSup:
    mov dl, fTablero
    dec dl
    js sgteDer
    
    dec fTablero
    sub si, 8
    call DestaparRecursivo
    add si, 8               
    inc fTablero
      
      
    ;Destapa la casilla superior derecha
    sgteSupDer:
    mov dl, cTablero
    inc dl
    cmp dl, 7
    jg sgteInf
    
    inc cTablero
    dec fTablero
    sub si, 7
    call DestaparRecursivo
    add si, 7
    inc fTablero
    dec cTablero               
         
         
    ;Destapa la casilla derecha
    sgteDer:
    mov dl, cTablero
    inc dl
    cmp dl, 7
    jg sgteInf
    
    inc cTablero
    inc si                           
    call DestaparRecursivo
    dec si
    dec cTablero
      
      
    ;Destapa la casilla inferior derecha
    sgteInfDer:
    mov dl, fTablero
    inc dl
    cmp dl, 7
    jg finRec
    
    inc cTablero
    inc fTablero
    add si, 9                           
    call DestaparRecursivo
    sub si, 9 
    dec fTablero
    dec cTablero  
       
       
    ;Destapa la casilla inferior
    sgteInf:
    mov dl, fTablero
    inc dl
    cmp dl, 7
    jg finRec
    
    inc fTablero
    add si, 8
    call DestaparRecursivo
    sub si, 8               
    dec fTablero
    
    jmp finRec
     
     
    imprimeNumero:                       ;Hay mina alrededor. Se imprime el numero de minas y finaliza la recursion
        ;Convierte el numero almacenado en el vector MTablero a una cadena para su impresion en el tablero
        mov al, MTablero[si]
        xor ah, ah                          
        lea dx, cadenaEsc
        call NumeroACadena
        ;Asigna los parametros para llamar a 'ImprimeCarColor'
        mov al, cadenaEsc             
        call ImprimeCarColor
    
    finRec:
        ret
  DestaparRecursivo ENDP
 
 
  ;F: Comprueba si existen las condiciones de fin de partida:
  ;S: fin=3     
  CompruebaFinPartidaGanada PROC
    push ax
    
    mov ax, TotalCasillas   ;Si NumMinas + destapadas = TotalCasillas ha ganado
    sub al, destapadas
    cmp al, NumMinas 
    jne SalirCompruebaFin
   
   ;ganado 
    mov fin, 3
    
   SalirCompruebaFin:
    pop ax
    ret
  CompruebaFinPartidaGanada ENDP
  
   
   
;********************************* PRINCIPAL ***********************************   

start:
    mov ax, data
    mov ds, ax
    mov es, ax 


    mov ah, 4ch
    int 21h

code ends

END start
