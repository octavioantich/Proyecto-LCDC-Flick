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
colores([r, v, p, g, b, y]).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% casilla_adapter(+Color, +ListaListas, -ListaCasillas).
% Para cada elemento [f, c] en ListaListas crea una estructura casilla(Color, f, c)
% Implementamos esto para adaptar la solucion de adyacenteC* propuesta por la catedra a
% nuestra implementacion.
% Color: Color con el cual se asociaran las casillas creadas
% ListaListas: Lista de pares enteros [f, c] donde f denota una fila y c una columna.
% 			   Idealmente fue obtenida como respuesta del predicado adyCStar/3.
% ListaCasillas: Lista de casillas adaptada a partir de ListaListas.
casilla_adapter(Color, ListaListas, ListaCasillas) :-
    findall(casilla(Color, F, C), (member([F, C], ListaListas)), ListaCasillas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyacentes_a_origen(+Grilla, +CasillaOrigen, -Adyacentes).
% Asocia Adyacentes con una lista de todas las casillas que son adyacenteC* con CasillaOrigen en Grilla.
% Una casilla se considera AdyacenteC* de si misma, por lo que siempre se devolvera, como minimo [CasillaOrigen | []].
% Nota, si la CasillaOrigen NO es una casilla de la Grilla, aun asi se devolvera como minimo [CasillaOrigen | []].
% Grilla: Grilla en la que se operara.
% CasillaOrigen: Casilla a partir de la cual se calcularan las celdas adyacenteC*.
% Adyacentes: Lista de las celdas que son adyacenteC* con respecto a CasillaOrigen.

%Esto es solo una "shell":
adyacentes_a_origen(Grilla, casilla(ColorOriginal, FilaOriginal, ColumnaOriginal), Adyacentes) :-
    adyCStar([FilaOriginal, ColumnaOriginal], Grilla, AdyacentesPre),
	casilla_adapter(ColorOriginal, AdyacentesPre, Adyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyCStar(+Origin, +Grid, -Res)
% Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
% siguiendo una estrategia de propagación o expansión.
% Origin: Posicion (par de enteros [F, C]) a partir del cual se hara la expansion.
% Grid: Grilla sobre la cual se operara
% Res: Lista de posiciones que son adyacenteC* a Origin.
adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adyCStarSpread(+Pend, +Vis, +Grid, -Res)
% Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
% el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
% Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
% que ya fueron consideradas.
% Grid: idem adyCStar
% Res: idem adyCStar
% En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
% se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
% ya ni en pendientes ni visitados.

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verdadero ssi las casillas en las posiciones P y A son adyacenteC
% P: Posicion (par de enteros [F, C]) de la primera casilla.
% Grid: Grilla sobre la cual se opera
% A: Posicion de la primera casilla.
adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ady(+P, +Grid, +A)
% Verdadero ssi las casillas en las posiciones P y A son adyacentes.
% P: Posicion (par de enteros [F, C]) de la primera casilla.
% Grid: Grilla sobre la cual se opera
% A: Posicion de la primera casilla.

% Abajo
ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

% Arriba
ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

% Derecha
ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

% Izquierda
ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


/* 
 * color(P, Grid, C)
 */

color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remover(+E, +L, -LR)
% Remueve la primera ocurrencia del elemento E en L, y asocia el resultado de la op. a LR.
% E: Elemento a remover
% L: Lista sobre la cual se operara
% LR: L sin el primer E, si es que habia un E en L.
remover( _, [], []).
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% colores_sin_actual(+ColorActual, - ColoresSinActual):
% Asocia a ColoresSinActual una lista que contiene todos los colores posibles a excepcion del actual
% Postcondicion: ColoresSinActual siempre tiene length 5.
% ColorActual: Color a excluir.
% ColoresSinActual: Lista de todos los colores excepto ColorActual
colores_sin_actual(ColorActual, ColoresSinActual) :-
    colores(ColoresTotales),
    remover(ColorActual, ColoresTotales, ColoresSinActual).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
 * ARBOLES:
 * Representaremos un arbol como sigue: t(R, H),
 * Donde R es el rotulo del arbol (sera un color)
 * Y H es una lista de forma [t1, t2, ..., tn] que representa a los hijos del
 * nodo en el cual esta; o bien [] cuando el nodo es hoja.
 * 
 * Para el proyecto, length(H, 5) siempre que no sea hoja.
 * Queda hardcodeado de esa manera, por el momento. 
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% formar_arbol(+Rotulo, +Depth, -Arbol):
% Forma un "arbol de colores" (ver informe) de profundidad Depth.
% +Rotulo: Rotulo que tendra la raiz del arbol
% +Depth: Profundidad del arbol
% Arbol: Arbol de colores con raiz Rotulo y de profundidad Depth.

%CB: Profundidad 0 -> creamos una hoja.
formar_arbol(Rotulo, 0, t(Rotulo, [])).

%CR: Profundidad > 0 -> creamos un nodo con el dado rotulo, y creamos recursivamente sus hijos
formar_arbol(Rotulo, Depth, Arbol) :-
    Depth > 0,
    NuevaDepth is Depth - 1,
    
    colores_sin_actual(Rotulo, [RH0, RH1, RH2, RH3, RH4]),
    
    %Temporalmente queda hard-codeado que cada nodo tiene 5 hijos :/
    formar_arbol(RH0, NuevaDepth, H0),
    formar_arbol(RH1, NuevaDepth, H1),
    formar_arbol(RH2, NuevaDepth, H2),
    formar_arbol(RH3, NuevaDepth, H3),
    formar_arbol(RH4, NuevaDepth, H4),

	Arbol = t(Rotulo, [H0, H1, H2, H3, H4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% camino_hoja(+Arbol, -Camino)
% Devuelve un camino desde la raiz de Arbol a una hoja arbitraria.
% Arbol: Arbol de Colores sobre el cual se operara.
% Camino: Lista de rotulos desde la raiz hasta una hoja arbitraria.

%CB: LLegamos a una hoja -> El "camino" es el rotulo de la hoja.
camino_hoja(t(R, []), [R]).

%CR: Estamos en un nodo interno -> El camino es el rotulo de este nodo
%    seguido del camino de un hijo (arbitrario) hasta una hoja.
camino_hoja(t(R, Hijos), [R | Camino]) :-
    Hijos = [ _H | _Hs ], %Nos aseguramos de que no sea vacio -> este nodo no es hoja.
    member(Hijo, Hijos),  %Tomamos un hijo arbitrario de los hijos de este nodo
    camino_hoja(Hijo, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caminos_posibles(+ColorOrigen, +Depth, -Caminos)
% Crea un ADC de profundidad Depth con raiz ColorOrigen
% Y genera y devuelve los caminos desde la raiz a cada una de las hojas
% ColorOrigen: Rotulo de la raiz del ADC
% Depth: Profundidad del ADC
% Caminos: Lista de listas. Contiene los caminos desde la raiz del ADC hasta todas sus hojas.
caminos_posibles(ColorOrigen, Depth, Caminos) :-
    formar_arbol(ColorOrigen, Depth, Arbol),
    findall(Camino, camino_hoja(Arbol, Camino), Caminos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mejor_camino(+Grid, +Depth, -Solucion, -Adyacencias)
% Predice el "mejor camino" (ver informe para criterios) de profundidad Depth
% Grid: Grilla sobre la cual se operara
% Depth: Profundidad del camino
% Solucion: Mejor camino encontrado
% Adyacencias: Cantidad de adyacencias que habra al final de recorrer solucion
mejor_camino(Grid, Depth, Secuencia, CantidadAdyacentes) :-
    % Calculamos del primer color y la primera lista de casos
    fila_origen(F),
    columna_origen(C),
    elemento_en(Grid, F, C, ColorOrigen),

    % Calculamos las adyacencias originales
    adyacentes_a_origen(Grid, casilla(ColorOrigen, F, C), AdyacentesOriginales),

    % Encontramos los caminos de profundidad Depth con todos los colores que nos son de interesa
    caminos_posibles(ColorOrigen, Depth, Caminos),

    %Simulamos todos los caminos posibles
    simular_todos_caminos(Grid, AdyacentesOriginales, Caminos, Soluciones),
    
    %Ordenamos las soluciones y tomamos la mejor entre ellas
    insert_sort(Soluciones, SolucionesOrdenadas),
    SolucionesOrdenadas = [MejorSolucion | _Otras],
    MejorSolucion = [Secuencia, _Longitud, CantidadAdyacentes].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simular_flick(+Grid, +Color, +Adyacentes, -NuevasAdyacentes, -CantidadAdyacentes).
% Simula una jugada, sin cambiar el estado del juego.
% Grid: Grilla sobre la cual se operara
% Color: Color al cual se simulara hacer un flick
% Adyacentes: Lista de casillas adyacentes al origen en el momento de realizar la simulacion del flick.
%             Se deben pasar como parametro porque no podemos depender del estado del juego en la simulacion.
% NuevasAdyacentes: Lista de casillas adyacentes al origen al terminar la simulacion del flick
% CantidadAdyacentes: cantidad de elementos en NuevasAdyacentes
% NuevaGrilla: Grilla resultante tras la simulacion del flick.
simular_flick(Grilla, Color, Adyacentes, NuevasAdyacentes, CantidadAdyacentes, NuevaGrilla) :-
    inicializado,
    fila_origen(FilaOrigen),
    columna_origen(ColumnaOrigen),

    cambiar_color_todas(Adyacentes, Color, AdyacentesFlicked),
    poner_todas(Grilla, AdyacentesFlicked, NuevaGrilla),
    
    NuevoOrigen = casilla(Color, FilaOrigen, ColumnaOrigen),
    adyacentes_a_origen(NuevaGrilla, NuevoOrigen, NuevasAdyacentes),

    length(NuevasAdyacentes, CantidadAdyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simular_camino(+Grid, +Adyacencias, +Camino, -CantidadAdyacencias)
% Simula hacer los flicks del camino recibido y "retorna" y la cantidad final de adyacencias.
% Grid: Grilla sobre la cual se trabaja
% Adyacencias: Adyacencias originales, previa a la simulacion
% Camino: Lista de colores a los cuales realizar flick
% CaminoRecorrido: Lista de colores del camino a los cuales ya se realizo flick
% CantidadAdyacencias: Cantidad de celdas que son adyacenteC* al origen al finalizar el ultimo flick

%CB: Llegamos al final del camino
simular_camino(_Grid, Adyacencias, [], [], CantidadAdyacencias) :-
    length(Adyacencias, CantidadAdyacencias).

%CB: Ganamos antes de llegar al final del camino
simular_camino(Grid, Adyacencias, [Paso | _PasosRestantes], [Paso], CantidadAdyacencias) :-
    simular_flick(Grid, Paso, Adyacencias, _NuevasAdyacentes, CantidadAdyacencias, _NuevaGrilla),
    cant_columnas(CantCol),
    cant_filas(CantFil),
    CantidadAdyacencias is CantCol*CantFil.

%CR: Quedan cosas por simular
simular_camino(Grid, Adyacencias, [Paso | PasosRestantes], [Paso | CaminoRecorrido], CantidadAdyacencias) :-
    simular_flick(Grid, Paso, Adyacencias, NuevasAdyacentes, _CA, NuevaGrilla),
    simular_camino(NuevaGrilla, NuevasAdyacentes, PasosRestantes, CaminoRecorrido, CantidadAdyacencias).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simular_todos_caminos(+GrillaInicial, +AdyacenciasIniciales, +CaminosTotales, -Soluciones).
% Realiza la operacion "simular_camino" sobre todos los caminos en CaminosTotales, y asocia a Soluciones una lista del desempeño de dichos caminos.
% GrilaInicial: Grilla sobre la cual se opera.
% AdyacenciasIniciales: Adyacencias originales, previa a la simulacion.
% CaminosTotales: Lista de caminos a simular
% Soluciones: Lista de los caminos recorridos y su desempeño

simular_todos_caminos(_GrillaInicial, _AdyacenciasIniciales, [], []).

simular_todos_caminos(GrillaInicial, AdyacenciasIniciales, [Camino | CaminosRestantes], [Solucion | SolucionesRestantes]) :-
    Camino = [_R | CaminoUtil], %No nos sirve considerar la raiz, pues es el color "en el que ya estamos".
    simular_camino(GrillaInicial, AdyacenciasIniciales, CaminoUtil, CaminoRecorrido, CantidadAdyacencias),
    length(CaminoRecorrido, L),
    Solucion = [CaminoRecorrido | [L  | [CantidadAdyacencias]]],
    simular_todos_caminos(GrillaInicial, AdyacenciasIniciales, CaminosRestantes, SolucionesRestantes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparar_soluciones(+A, +B):
% Vale ssi A es estrictamente mejor solucion que B.
% Una solucion es mejor que otra si tiene mayor cantidad de adyacencias
% O, si tienen la misma cantidad de adyacencias, si tiene menos movimientos
% A: Solucion A.
% B: Solucion B
comparar_soluciones(SolucionA, SolucionB) :-
    SolucionA = [_CaminoA, _LongitudA, AdyacenciasA],
	SolucionB = [_CaminoB, _LongitudB, AdyacenciasB],
    AdyacenciasA > AdyacenciasB.

comparar_soluciones(SolucionA, SolucionB) :-
    SolucionA = [_CaminoA, LongitudA, AdyacenciasA],
	SolucionB = [_CaminoB, LongitudB, AdyacenciasB],
    AdyacenciasA = AdyacenciasB,
    LongitudA < LongitudB.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mayor_de_lista(-Mayor, +L)
% Asocia a Mayor el Mayor elemento de L.
% Definimos el orden segun el comparador comparar_soluciones
% Mayor: Mayor elemento de L
% L: Lista sobre la cual se operara

%Shell.
%Se supone a la hora de llamar que el primer elemento es el mayor
mayor_de_lista_shell(X, [Z | Zs]) :- mayor_de_lista(Z, [Z | Zs], X).

%Caso base. El mayor elemento de una lista vacia es el menor elemento hasta ahora
mayor_de_lista(MenorActual, [], MenorActual).

%Caso recursivo 1: El menor actual es menor que la cabeza
mayor_de_lista(MenorActual, [Z | Zs], Respuesta) :-
    comparar_soluciones(MenorActual, Z),
    mayor_de_lista(MenorActual, Zs, Respuesta).

%Caso recursivo 1: El menor actual NO es menor que la cabeza,
%la cabeza pasa a ser el menor actual
mayor_de_lista(MenorActual, [Z | Zs], Respuesta) :-
    not(comparar_soluciones(MenorActual, Z)),
    mayor_de_lista(Z, Zs, Respuesta).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_sort(+L, -LO).
% Asocia a LO una version ordenada de L. Aplica la estrategia insert sort para ordenar.
% L: Lista sobre la cual se operara
% LO: Lista ordenada.

%Caso base: Una lista vacia ya esta ordenada.
insert_sort([], []).

% Caso Recursivo: Tenemos aun elementos:
% Eliminamos el mayor, 
% ordenamos el resto, 
% ponemos el mayor a la cabeza del resto ordenado.
insert_sort(L, Ordenada) :-
    mayor_de_lista_shell(Menor, L),
	remover(Menor, L, ListaSinMenor),
    insert_sort(ListaSinMenor, OrdenadaRec),
    Ordenada = [Menor | OrdenadaRec].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mejor_origen(+Grid, -MejorFila, -MejorColumna)
% Asocia a [MejorFila, MejorColumna] la posicion con mas adyacencias iniciales.
% Grid: Grilla sobre la cual se operara
% MejorFila: Fila de la mejor posicion de origen
% MejorColumna: Columna de la mejor posicion de origen
mejor_origen(Grid, MejorFila, MejorColumna) :-
    % Obtenemos manualmente el tamano de Grid.
    % No podemos contar con nuestros predicados dinamicos porque
    % Este predicado se ejecuta cuando el programa aun no ha sido inicializado
    Grid = [F | _Fs],
    length(Grid, CantFil),
    length(F, CantCol),
    
    % Obtenemos una lista con la cantidad de adyacencias iniciales de cada origen
    simular_origenes(Grid, 0, 0, CantFil, CantCol, Origenes),
    
    % Obtenemos el mejor origen
	Origenes = [O | _Os],
    mejor_origen_lista(Origenes, O, MejorOrigen),
    
    % Extraemos la informacion que nos interesa
	MejorOrigen = [MejorFila, MejorColumna, _Ad].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simular_origenes(+Grid, +FA, +CA, +CF, +CC, -Origenes)
% Simula la eleccion de origen en todas las posibles casillas de grid y
% devuelve la lista con ternas [Fila, Columna, Cantidad de Adyacentes].
% Grid: Grilla sobre la cual se opera
% FA: Fila sobre la cual se esta considerando el origen
% CA: Columna sobre la cual se esta considerando el origen
% CF: Cantidad de filas en Grid
% CC: Cantidad de columnas en grid
% Origenes: Lista de ternas descriptas anteriormente

% CB: Recorrimos toda la grilla
simular_origenes(_Grid, CF, _CA, CF, _CC, []) :- !.

% CR: Llegamos al final de una fila
simular_origenes(Grid, FA, CC, CF, CC, Restantes) :-
    FN is FA+1,
    simular_origenes(Grid, FN, 0, CF, CC, Restantes).

% CR: Estamos "en el medio" de una dada fila
simular_origenes(Grid, FA, CA, CF, CC, [Origen | Restantes]) :-
    elemento_en(Grid, FA, CA, Color),
    adyacentes_a_origen(Grid, casilla(Color, FA, CA), Adyacencias),
    length(Adyacencias, CantAd),
    Origen = [FA, CA, CantAd],
    CN is CA+1,
    simular_origenes(Grid, FA, CN, CF, CC, Restantes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mejor_origen_lista(+Lista, +Mejor, -MejorAbsoluto)
% Asocia a MejorAbsoluto el origen que mayor cantidad de adyacencias tiene de Lista.
% Nota, si dos Origenes tienen la misma cantidad de adyacencias, prioriza el que este
% mas "arriba a la izquierda".
% Lista: Lista de Ternas [F, C, Ad], descripta en simular_origenes.
% Mejor: La mejor Terna de Lista hasta ahora.
% MejorAbsoluto: La mejor terna de Lista.

% CB: Ya no queda lista para recorrer.
mejor_origen_lista([], M, M).

%CR: Queda lista para recorrer y el elemento actual es mejor que M.
mejor_origen_lista([T | Ts], M, MejorAbs) :-
    T = [_FT, _CT, AT],
    M = [_FM, _CM, AM],
    AT > AM, !,
    mejor_origen_lista(Ts, T, MejorAbs).

%CR: Queda lista para recorrer y el elemento actual NO es mejor que M.
mejor_origen_lista([_T | Ts], M, MejorAbs) :-
    mejor_origen_lista(Ts, M, MejorAbs).
	% No necesitamos comparar nada por el cut en el caso recursivo anterior