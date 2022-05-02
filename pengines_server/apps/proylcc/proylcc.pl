:- module(proylcc, 
	[  
		flick/4
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Proponemos como dinamicos los siguientes predicados, con objeto de poder implementar
% que el jugador elija una vez (al principio de la partida) un origen
:- dynamic fila_origen/1.
:- dynamic columna_origen/1.
:- dynamic adyacentes_actuales/1.
:- dynamic cant_filas/1.
:- dynamic cant_columnas/1.
:- dynamic inicializado/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Estado por defecto:
fila_origen(0).
columna_origen(0).
cant_filas(14).
cant_columnas(14).
adyacentes_actuales([]).
% Notese que el programa *no* esta inicializado originalmente, esto es "?- inicializado." retorna false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inicializar(+G, +F, +C, -AdyacenciasIniciales)
% Inicializa dinamicamente el origen a la posicion de matriz (F,C).
% Si el programa ya fue inicializado, falla.
% G: Grilla sobre la cual se operara
% F: Fila que sera el origen.
% C: Columna que sera el origen.
% AdyacenciasIniciales: Cantidad de celdas que son adyacenteC* con el origen que fue pasado por parametro.
inicializar(G, CF, CC, F, C, AdyacenciasIniciales) :-
    %"settear" el tamaño
    retract(cant_filas(_CantFilasPorDefecto)),
    retract(cant_columnas(_CantColumnasPorDefecto)),
    assert(cant_filas(CF)),
    assert(cant_columnas(CC)),

    %"settear" el origen
    retract(fila_origen(_FilaPorDefecto)),
    retract(columna_origen(_ColumnaPorDefecto)),
    assert(fila_origen(F)),
    assert(columna_origen(C)),

    % Calculamos las primeras adyacencias
    elemento_en(G, F, C, ColorOrigen),
    CasillaOrigen = casilla(ColorOrigen, F, C),
    adyacentes_a_origen(G, CasillaOrigen, Adyacentes),

    % las asertamos
    retract(adyacentes_actuales(_L)),
    assert(adyacentes_actuales(Adyacentes)),

    % extra, para mostrar la cantidad de adyacencias iniciales:
    length(Adyacentes, AdyacenciasIniciales),

    % terminar inicializacion
    assert(inicializado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buscar(+Lista, +PosBuscada, +PosActual, -Respuesta)
% Asocia el elemento en la posicion buscada de Lista a Respuesta.
% Falla si PosBuscada > length(Lista).
% Lista: Lista en la que se realizara la busqueda.
% Posicion Buscada: Posicion del elemento Respuesta.
% Posicion actual: Posicion de la lista que estamos investigando.
% Respuesta elemento buscado. 
buscar_shell(Lista, PosBuscada, Respuesta) :-
    buscar(Lista, PosBuscada, 0, Respuesta).

buscar([E | _Es], PosBuscada, PosBuscada, E).
buscar([_E | Es], PosBuscada, PosActual, Respuesta) :-
    SiguientePos is PosActual + 1,
    SiguientePos =< PosBuscada,
    buscar(Es, PosBuscada, SiguientePos, Respuesta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% elemento_en(+Grilla, +FilaBuscada, +ColumnaBuscada, -E).
% Asocia E con elemento en Grilla[FilaBuscada][ColumnaBuscada].
% Grilla: Lista de listas en la cual se realizara la busqueda.
% FilaBuscada: entero en el rango [0..TotalFilas-1] que denota la fila del elemento E
% Columna: entero en el rango [0..TotalColumnas-1] que denota la Columna del elemento E
% E: elemento buscado.
elemento_en(Grilla, FilaBuscada, ColumnaBuscada, E) :-
	buscar_shell(Grilla, FilaBuscada, Fila),
	buscar_shell(Fila, ColumnaBuscada, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Estructura: casilla(Color, Fila, Columna)
% Color: Color de la cual esta pintada la casilla
% Fila: Fila de una grilla a la cual se corresponde la casilla
% Columna: Columna de una grilla a la cual se corresponde una casilla.
% Numeramos filas y columnas de la misma manera que se numeran en una matriz:
% [[00, 01, 02, 03, ..., 0n],
%  [10, 11, 12, 13, ..., 1n],
%              ...
%  [m0, m1, m2, m3, ..., mn]]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes_a_origen(+Grilla, +CasillaOrigen, -Adyacentes).
% Asocia Adyacentes con una lista de todas las casillas que son adyacenteC* con CasillaOrigen en Grilla.
% Una casilla se considera AdyacenteC* de si misma, por lo que siempre se devolvera, como minimo [CasillaOrigen | []].
% Nota, si la CasillaOrigen NO es una casilla de la Grilla, aun asi se devolvera como minimo [CasillaOrigen | []].
% Grilla: Grilla en la que se operara.
% CasillaOrigen: Casilla a partir de la cual se calcularan las celdas adyacenteC*.
% Adyacentes: Lista de las celdas que son adyacenteC* con respecto a CasillaOrigen.

%Esto es solo una "shell":
adyacentes_a_origen(Grilla, CasillaOrigen, Adyacentes) :-
    CasillaOrigen = casilla(ColorOriginal, _FilaOriginal, _ColumnaOriginal),
    adyacentes_a(Grilla, ColorOriginal, CasillaOrigen, [], Adyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes_a(+Grilla, +CasillaOrigen, +CasillaActual, +Visitados, -Adyacentes).
% Asocia Adyacentes con una lista de todas las casillas que son adyacenteC* con CasillaOrigen en Grilla.
% Una casilla se considera AdyacenteC* de si misma, por lo que siempre se devolvera, como minimo [CasillaOrigen | []].
% Nota, si la CasillaOrigen NO es una casilla de la Grilla, aun asi se devolvera como minimo [CasillaOrigen | []].
% Grilla: Grilla en la que se operara.
% ColorOriginal: Color de la casilla a partir de la cual se calcularan las celdas adyacenteC*.
% CasillaActual: Casilla que se esta analizando en un determinado momento.
% Visitados: Lista que mantiene todas las casillas adyacenteC* respecto a CasillaOrigen que ya se visitaron, para evitar ciclos.
% Adyacentes: Lista de las celdas que son adyacenteC* con respecto a CasillaOrigen.

% Caso base: llegamos a una casilla en la cual NO compartimos color
adyacentes_a(_Grilla, ColorOriginal, casilla(ColorActual, _FA, _CA), _Visitados, []) :-
    ColorOriginal \== ColorActual.

% Caso base: Llegamos a una casilla visitada
adyacentes_a(_Grilla, _ColorOriginal, Casilla, Visitados, []) :-
    member(Casilla, Visitados).

% Caso recursivo: Llegamos a una casilla de color correcto y no visitada
% La agregamos a las adyacentes y analizamos todas sus lindantes, de manera transitiva.
adyacentes_a(Grilla, ColorOriginal, Casilla, Visitados, [Casilla | Adyacentes]) :-
    %Explicitamos esta condicion para evitar tener multiples resultados:
    not(member(Casilla, Visitados)),
    %El resto son condiciones necesarias.
    Casilla = casilla(ColorOriginal, _FA, _CA),
    append([Casilla], Visitados, VisitadosConCasilla),
    adyacentes_arriba(Grilla, ColorOriginal, Casilla, VisitadosConCasilla, AdArr),
    append(VisitadosConCasilla, AdArr, VisitadosArriba),
    adyacentes_abajo(Grilla, ColorOriginal, Casilla, VisitadosArriba, AdAbj),
    append(VisitadosArriba, AdAbj, VisitadosAbajo),
    adyacentes_izquierda(Grilla, ColorOriginal, Casilla, VisitadosAbajo, AdIzq),
    append(VisitadosAbajo, AdIzq, VisitadosIzquierda),
    adyacentes_derecha(Grilla, ColorOriginal, Casilla, VisitadosIzquierda, AdDer),
    append(AdArr, AdAbj, AdyacentesVertical),
    append(AdIzq, AdDer, AdyacentesHorizontal),
    append(AdyacentesVertical, AdyacentesHorizontal, Adyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Nota: Documentamos las siguientes 4 funciones de manera "generica" puesto que su comportamiento es analogo

% adyacentes_direccion(+Grilla, +ColorOriginal, +CasillaActual, +Visitados, -Adyacentes).
% Asocia Adyacentes con una lista de todas las casillas que son adyacenteC* con la casilla lindante a Casilla en direccion en Grilla.
% Grilla: Grilla en la que se operara.
% ColorOriginal: Color de la casilla a partir de la cual se calcularan las celdas adyacenteC*.
% CasillaActual: Casilla que se esta analizando en un determinado momento.
% Visitados: Lista que mantiene todas las casillas adyacenteC* respecto a CasillaOrigen que ya se visitaron, para evitar ciclos.
% Adyacentes: Lista de las celdas que son adyacenteC* con respecto a CasillaOrigen.


% Arriba:
% Caso Recursivo: Estamos en una casilla del color correcto.
% La agregamos a Adyacentes y Visitados y miramos todas sus lindantes.
adyacentes_arriba(Grilla, ColorOriginal, Casilla, Visitados, Adyacentes) :-
    Casilla = casilla(ColorOriginal, FA, CA),
    FN is FA - 1,
    FN >= 0,
    elemento_en(Grilla, FN, CA, ColorNuevo),
    CasillaNueva = casilla(ColorNuevo, FN, CA),
    adyacentes_a(Grilla, ColorOriginal, CasillaNueva, Visitados, Adyacentes).

% Caso Base: Estamos en una casilla de un color incorrecto.
% Asociamos el vacio a Adyacentes.
adyacentes_arriba(_G, ColorOriginal, casilla(ColorActual, _FA, _CA), _V, []) :-
	 ColorOriginal \== ColorActual.

% Caso Base: Estamos en un borde de la grilla, por lo que no podemos considerar la celda lindante en esa direccion.
% Asociamos el vacio a Adyacentes.
adyacentes_arriba(_G, _ColorOriginal, casilla(_ColorActual, 0, _CA), _V, []).

% Abajo:
% Caso Recursivo: Estamos en una casilla del color correcto.
% La agregamos a Adyacentes y Visitados y miramos todas sus lindantes.
adyacentes_abajo(Grilla, ColorOriginal, Casilla, Visitados, Adyacentes) :-
    cant_filas(CantFilas),
    Casilla = casilla(ColorOriginal, FA, CA),
    FN is FA + 1,
    FN < CantFilas,
    elemento_en(Grilla, FN, CA, ColorNuevo),
    CasillaNueva = casilla(ColorNuevo, FN, CA),
    adyacentes_a(Grilla, ColorOriginal, CasillaNueva, Visitados, Adyacentes).

% Caso Base: Estamos en una casilla de un color incorrecto.
% Asociamos el vacio a Adyacentes.
adyacentes_abajo(_G, ColorOriginal, casilla(ColorActual, _FA, _CA), _V, []) :-
	 ColorOriginal \== ColorActual.

% Caso Base: Estamos en un borde de la grilla, por lo que no podemos considerar la celda lindante en esa direccion.
% Asociamos el vacio a Adyacentes.
adyacentes_abajo(_G, _ColorOriginal, casilla(_ColorActual, UltimaFila, _CA), _V, []) :-
    cant_filas(CantFilas),
    UltimaFila is CantFilas-1.

% Izquierda:
% Caso Recursivo: Estamos en una casilla del color correcto.
% La agregamos a Adyacentes y Visitados y miramos todas sus lindantes.
adyacentes_izquierda(Grilla, ColorOriginal, Casilla, Visitados, Adyacentes) :-
    Casilla = casilla(ColorOriginal, FA, CA),
    CN is CA - 1,
    CN >= 0,
    elemento_en(Grilla, FA, CN, ColorNuevo),
    CasillaNueva = casilla(ColorNuevo, FA, CN),
    adyacentes_a(Grilla, ColorOriginal, CasillaNueva, Visitados, Adyacentes).

% Caso Base: Estamos en una casilla de un color incorrecto.
% Asociamos el vacio a Adyacentes.
adyacentes_izquierda(_G, ColorOriginal, casilla(ColorActual, _FA, _CA), _V, []) :-
	 ColorOriginal \== ColorActual.

% Caso Base: Estamos en un borde de la grilla, por lo que no podemos considerar la celda lindante en esa direccion.
% Asociamos el vacio a Adyacentes.
adyacentes_izquierda(_G, _ColorOriginal, casilla(_ColorActual, _FA, 0), _V, []).

% Derecha:
% Caso Recursivo: Estamos en una casilla del color correcto.
% La agregamos a Adyacentes y Visitados y miramos todas sus lindantes.
adyacentes_derecha(Grilla, ColorOriginal, Casilla, Visitados, Adyacentes) :-
    cant_columnas(CantCol),
    Casilla = casilla(ColorOriginal, FA, CA),
    CN is CA + 1,
    CN < CantCol,
    elemento_en(Grilla, FA, CN, ColorNuevo),
    CasillaNueva = casilla(ColorNuevo, FA, CN),
    adyacentes_a(Grilla, ColorOriginal, CasillaNueva, Visitados, Adyacentes).

% Caso Base: Estamos en una casilla de un color incorrecto.
% Asociamos el vacio a Adyacentes.
adyacentes_derecha(_G, ColorOriginal, casilla(ColorActual, _FA, _CA), _V, []) :-
	 ColorOriginal \== ColorActual.

% Caso Base: Estamos en un borde de la grilla, por lo que no podemos considerar la celda lindante en esa direccion.
% Asociamos el vacio a Adyacentes.
adyacentes_derecha(_G, _ColorOriginal, casilla(_ColorActual, _FA, UltimaCol), _V, []) :-
    cant_columnas(CantCol),
    UltimaCol is CantCol-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reemplazar(+Lista, E, +Pos, +PosActual -NuevaLista)
% Reemplaza el elemento en la posicion Pos de Lista por E, si es que Pos <= length(lista), y asocia la lista con el reemplazo hecho a NuevaLista 
% Lista: Lista sobre la cual se operara
% E: elemento que reemplazara al pos-eavo elemento de Lista
% Pos: Posicion en la cual se reemplazara por E
% PosActual: Posicion en le cual se esta analizando la posibilidad de hacer el reemplazo
% NuevaLista: Lista que imita a Lista, pero con el reemplazo hecho

% Shell
reemplazar_shell(Lista, E, Pos, NuevaLista) :-
    reemplazar(Lista, E, Pos, 0, NuevaLista).

% Caso base, llegamos a la posicion buscada
reemplazar([_X | Xs], E, Pos, Pos, [E | Xs]).

% Caso Recursivo: Aun no llegamos a la posicion buscada, seguimos buscando.
reemplazar([X | Xs], E, Pos, PosActual, [X | Resto]) :-
    NuevaPos is PosActual + 1,
    reemplazar(Xs, E, Pos, NuevaPos, Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% poner_en(+Grilla, +Fil, +Col, +E, -NuevaGrilla)
% Pone el elemento E en la posicion Fila, Columna (reemplazando el existente) de la Grilla, y asocia esta Grilla modificada a NuevaGrilla
% Grilla: Grilla (Lista de listas) original
% Fila: Entero no-negativo que denota la fila en la cual se pondra el elemento. Idealmente menor a length(Grilla)
% Columna: Entero no-negativo que denota la columna en la cual se pondra el elemento. Idealmente menor a length(Fila), Grilla = [Fila | Filas]
% NuevaGrilla: Grilla (lista de listas) que imita a Grilla a excepcion de Fil, Col donde tiene E.

poner_en(Grilla, Fil, Col, E, NuevaGrilla) :-
    buscar_shell(Grilla, Fil, FilaOriginal),
    reemplazar_shell(FilaOriginal, E, Col, FilaConCambio),
    reemplazar_shell(Grilla, FilaConCambio, Fil, NuevaGrilla).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% poner_todas(+Grilla, +Casillas, -NuevaGrilla)
% Realiza la operacion "poner_en" con todas las casillas de Casillas, y asocia la grilla modificada a NuevaGrilla.
% Grilla: grilla base sobre la cual se opera
% Casillas: Lista de casillas.
% NuevaGrilla: Grilla modificada

% Caso base, no hay mas casillas sobre las cuales operar
poner_todas(Grilla, [], Grilla).

% Caso recursivo: Hay al menos una casilla sobre la cual operar en la lista.
poner_todas(Grilla, [casilla(E, F, C) | Casillas], NuevaGrilla) :-
    poner_en(Grilla, F, C, E, GrillaConCasilla),
    poner_todas(GrillaConCasilla, Casillas, NuevaGrilla).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cambiar_color_todas(+Casillas, +Color, -NuevasCasillas)
% Cambia el color de todas las casillas en Casillas a Color y asocia esta lista modificada con NuevasCasillas
% Casillas: Lista de casillas a modificar
% Color: Color que se le pondra a las nuevas casillas
% NuevasCasillas: Lista de casillas ya modificadas

% Caso base, no hay casillas sobre las cuales operar
cambiar_color_todas([], _Color, []).

% Caso recursivo, quedan aun casillas
cambiar_color_todas([casilla(_X, F, C) | CasillasRestantes], Color, [casilla(Color, F, C) | NuevasCasillasRestantes]) :-
    cambiar_color_todas(CasillasRestantes, Color, NuevasCasillasRestantes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flick(+Grilla, +Color, -CantidadAdyacentes, -NuevaGrilla)
% Hace un "flick" al color Color y asocia la grilla modificada a NuevaGrilla. Falla si el programa no fue inicializado.
% Grilla: Grilla sobre la cual se opera
% Color: Color al cual se pasara la casilla origen y todas las adyacenteC* a ella
% CantidadAdyacentes: Cantidad de casillas que son adyacenteC* al origen despues de haber realizado el flick
% NuevaGrilla: Grilla post-flick.

flick(Grilla, Color, CantidadAdyacentes, NuevaGrilla) :-
    inicializado,
    fila_origen(FilaOrigen),
    columna_origen(ColumnaOrigen),

    %elemento_en(Grilla, FilaOrigen, ColumnaOrigen, ColorOrigen),
    %CasillaOrigen = casilla(ColorOrigen, FilaOrigen, ColumnaOrigen),
    %adyacentes_a_origen(Grilla, CasillaOrigen, Adyacentes),
    
    adyacentes_actuales(Adyacentes),

    cambiar_color_todas(Adyacentes, Color, AdyacentesFlicked),
    poner_todas(Grilla, AdyacentesFlicked, NuevaGrilla),
    
    NuevoOrigen = casilla(Color, FilaOrigen, ColumnaOrigen),
    adyacentes_a_origen(NuevaGrilla, NuevoOrigen, NuevasAdyacentes),

    retract(adyacentes_actuales(_L)),
    assert(adyacentes_actuales(NuevasAdyacentes)),

    length(NuevasAdyacentes, CantidadAdyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% push(+S, +E, -NS)
% Pone a E en el tope de la pila S y asocia esto a NS.
% S: Pila (Stack) sobre la cual se operara
% E: Elemento a pushear
% NS: Nueva pila (New Stack), con el elemento ya pusheado
push(S, E, [E | S]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% peek(+S, -E)
% Asocia a E el elemento tope de la pila S. Falla si S esta vacia.
% S: Pila sobre la cual se operara
% E: Tope de S.
peek([E | _Resto], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pop(+S, -E, -NS)
% Quita a E del tope de la pila S y asocia la nueva pila sin E a NS.
% Falla si S esta vacia.
% S: Pila (Stack) sobre la cual se operara
% E: Elemento a poppear
% NS: Nueva pila (New Stack), con el elemento ya pusheado
pop([E | NS], E, NS).

