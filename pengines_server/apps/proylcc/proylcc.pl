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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% puntos_para_ganar(?P)
% Asocia a P con la cantidad de puntos necesarios para ganar dada una cierta inicializacion.
% P: Cantidad de puntos necesarios para ganar.
puntos_para_ganar(P) :-
    inicializado,
    cant_filas(F),
    cant_columnas(C),
    P is F*C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% elemento_en(+Grilla, +FilaBuscada, +ColumnaBuscada, -E).
% Asocia E con elemento en Grilla[FilaBuscada][ColumnaBuscada].
% Grilla: Lista de listas en la cual se realizara la busqueda.
% FilaBuscada: entero en el rango [0..TotalFilas-1] que denota la fila del elemento E
% Columna: entero en el rango [0..TotalColumnas-1] que denota la Columna del elemento E
% E: elemento buscado.
elemento_en(Grilla, FilaBuscada, ColumnaBuscada, E) :-
	nth0(FilaBuscada, Grilla, Fila),
	nth0(ColumnaBuscada, Fila, E).

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
adyC([F, C], Grid, [FP, CP]):-
    ady([F, C], Grid, [FP, CP]),
    elemento_en(Grid, F, C, Color),
    elemento_en(Grid, FP, CP, Color).

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
    nth0(Fil, Grilla, FilaOriginal),
    reemplazar_shell(FilaOriginal, E, Col, FilaConCambio),
    reemplazar_shell(Grilla, FilaConCambio, Fil, NuevaGrilla).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% poner_todas_color(+Grilla, +Color, +Casillas, -NuevaGrilla)
% Realiza la operacion "poner_en" con todas las casillas de Casillas
% modificadas para que sean del color Color, y asocia la grilla modificada a NuevaGrilla.
% Grilla: grilla base sobre la cual se opera
% Color: Color al cual cambiaran las casillas
% Casillas: Lista de casillas.
% NuevaGrilla: Grilla modificada

% Caso base, no hay mas casillas sobre las cuales operar
poner_todas_color(Grilla, _C, [], Grilla).

% Caso recursivo: Hay al menos una casilla sobre la cual operar en la lista.
poner_todas_color(Grilla, Color, [casilla(_ColorAntiguo, F, C) | Casillas], NuevaGrilla) :-
    poner_en(Grilla, F, C, Color, GrillaConCasilla),
    poner_todas_color(GrillaConCasilla, Color, Casillas, NuevaGrilla).

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
    poner_todas_color(Grilla, Color, Adyacentes, NuevaGrilla),
    
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

/*
 * JUGADAS:
 * Una jugada es una lista de 6 elementos: J = [G, C, Ady, CA, Sec, Depth], donde:
 * G es la grid correspondiente a realizar los flick a los colores de Sec (en orden)
 * C es el color actual (Se podria obtener a partir de Sec, pero manteniendolo por separado se vuelve mas legible el codigo)
 * Ady son las celdas que son adyC* al origen
 * CA es la cantidad de adyacencias, o lo que es igual, la longitud de Ady
 * Sec es la secuencia (lista de colores)que conforman a la jugada
 * Depth es la profundidad de la jugada (Se podria obtener a partir de Sec, pero manteniendolo por separado se vuelve mas legible el codigo).
 * */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% siguiente_nivel(+Jugada, -SiguienteNivel)
% Obtiene el siguiente nivel de l busqueda por frontera para una dada jugada.
% El siguiente nivel se compone de todas las posibles jugadas que se pueden obtener a partir de la inicial y tienen mayor cantidad de adyacencias.
% Jugada: Jugada inicial.
% SiguienteNivel: Lista de hasta 5 jugadas que componen el siguiente nivel de la busqueda por frontera.

siguiente_nivel([G, C, Ady, CA, Sec, Depth], SiguienteNivel) :-
    colores_sin_actual(C, Colores),
    NuevaDepth is Depth+1,
    findall(Jugada, (
                    member(Col, Colores),
                    simular_flick(G, Col, Ady, NuevasAdy, NuevasCA, NuevaGrid),
                    NuevasCA > CA,
                    append(Sec, [Col], NuevaSec),
                    Jugada = [NuevaGrid, Col, NuevasAdy, NuevasCA, NuevaSec, NuevaDepth]
                    ), SiguienteNivel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% frontera(+Jugadas, -FronteraResultante, -ProfundidadBuscada)
% Realiza una busqueda exhaustiva pero optimizada del mejor camino de hasta profundidad ProfundidadBuscada, a partir de una cierta jugada.
% Jugadas: Lista de Jugadas. Cuando se llama al predicado deberia contener un solo elemento.
% FronteraResultante: Lista de jugadas que componen la frontera de la busqueda (tienen suficiente profundidad, o bien suficientes puntos para ganar)
% ProfundidadBuscada: Profundidad maxima de las soluciones buscadas.
%
% Nota: Los cut son criticos para mantener la semantica del predicado.
% Se aplica la optimizacion de condiciones completas y exhaustivas.

%CB: No queda mas frontera por recorrer
frontera([], [], _ProfundidadBuscada).

%CR: La jugada actual ya gano -> no calculamos su siguiente nivel, pero SI la guardamos.
frontera([Jugada | Jugadas], [Jugada | FronteraResultante], ProfundidadBuscada) :-
    Jugada = [_G, _Color, _Ady, PuntosGanar, _Sec, _Depth],
    puntos_para_ganar(PuntosGanar),
    !, % No es necesario comprobar si unifica con otra cosa :)
	frontera(Jugadas, FronteraResultante, ProfundidadBuscada).

%CR: La jugada actual es de la prof. buscada -> no calculamos su siguiente nivel, pero SI la guardamos.
frontera([Jugada | Jugadas], [Jugada | FronteraResultante], ProfundidadBuscada) :-
    Jugada = [_G, _Color, _Ady, _P, _Sec, ProfundidadBuscada],
    !, % No es necesario comprobar si unifica con otra cosa :)
	frontera(Jugadas, FronteraResultante, ProfundidadBuscada).

%CR: La jugada actual NO es de la prof buscada aun, y no gano -> calculamos su siguiente nivel, y descartamos
frontera([Jugada | Jugadas], FronteraResultante, ProfundidadBuscada) :-
	siguiente_nivel(Jugada, SiguienteNivel),
    append(Jugadas, SiguienteNivel, NuevaFrontera),
    frontera(NuevaFrontera, FronteraResultante, ProfundidadBuscada).

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
    
    poner_todas_color(Grilla, Color, Adyacentes, NuevaGrilla),
    
    NuevoOrigen = casilla(Color, FilaOrigen, ColumnaOrigen),
    adyacentes_a_origen(NuevaGrilla, NuevoOrigen, NuevasAdyacentes),

    length(NuevasAdyacentes, CantidadAdyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparar_soluciones(+JA, +JB):
% Vale ssi JA es estrictamente mejor solucion que JB.
% Una solucion es mejor que otra si tiene mayor cantidad de adyacencias
% O, si tienen la misma cantidad de adyacencias, si tiene menos movimientos (menor profundidad)
% JA: Jugada A.
% JB: Jugada B
comparar_jugadas(JA, JB) :-
    JA = [_GA, _CA, _AA, PA, _SA, _DA],
    JB = [_GB, _CB, _AB, PB, _SB, _DB],
    PA > PB, !. %Si PA > PB, PA y PB NO unificaran nunca, por lo que no necesitamos tener en cuenta el otro caso del predicado.

comparar_jugadas(JA, JB) :-
    JA = [_GA, _CA, _AA, P, _SA, DA],
    JB = [_GB, _CB, _AB, P, _SB, DB],
    DA < DB.

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
mayor_de_lista(MayorActual, [], MayorActual).

%Caso recursivo 1: El menor actual es menor que la cabeza
mayor_de_lista(MayorActual, [Z | Zs], Respuesta) :-
    comparar_jugadas(MayorActual, Z), !,
    mayor_de_lista(MayorActual, Zs, Respuesta).

%Caso recursivo 2: El menor actual NO es menor que la cabeza,
%la cabeza pasa a ser el menor actual
mayor_de_lista(_MayorActual, [Z | Zs], Respuesta) :-
    mayor_de_lista(Z, Zs, Respuesta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mejor_camino(Grid, Depth, Secuencia, CantidadAdyacentes) :-
    % Checkeamos que se haya inicializado el programa previo a tratar de encontrar un mejor camino
    inicializado,
    
    % Obtenemos la informacion necesaria para obtener la casilla de origen
    fila_origen(FilaOrigen),
    columna_origen(ColumnaOrigen),
    elemento_en(Grid, FilaOrigen, ColumnaOrigen, ColorOrigen),

    % Obtenemos las adyacencias iniciales, y la cantidad de las mismas
    adyacentes_a_origen(Grid, casilla(ColorOrigen, FilaOrigen, ColumnaOrigen), AdyacenciasIniciales),
    length(AdyacenciasIniciales, CA),

    % Obtenemos la jugada inicial
    J = [Grid, ColorOrigen, AdyacenciasIniciales, CA, [], 0],

    % Computamos los caminos de interes (frontera)
    frontera([J], FronteraResultante, Depth),

    % Obtenemos el mejor camino
    mayor_de_lista_shell([_G, _Col, _Ady, CantidadAdyacentes, Secuencia, _D], FronteraResultante).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% camino_greedy(+Jugada, +ProfundidadBuscada, -Secuencia, -CA)
% Obtiene, de manera greedy no optimal, un camino bueno.
% Jugada: Jugada a partir de la cual se busca el camino
% ProfundidadBuscada: Entero positivo que denota la longitud maxima de los caminos a considerar
% Secuencia: Lista de colores que es el mejor camino greedy.
% CA (Cantidad Adyacentes): Cantidad de casillas que son adyacenteC* con el origen al finalizar la secuencia propuesta como mejor.

% CB: Ganamos.
camino_greedy(Jugada, _ProfundidadBuscada, [Color], P) :-
    siguiente_nivel(Jugada, SiguienteNivel),
    mayor_de_lista_shell(Mejor, SiguienteNivel),
    Mejor = [_G, Color, _Ady, P, _Sec, _Profundidad],
    puntos_para_ganar(P), !. % No hace falta ningun camino alterno despues de esto.

% CB: Llegamos a profundidad buscada.
camino_greedy(Jugada, ProfundidadBuscada, [Color], P) :-
    siguiente_nivel(Jugada, SiguienteNivel),
    mayor_de_lista_shell(Mejor, SiguienteNivel),
    Mejor = [_G, Color, _Ady, P, _Sec, ProfundidadBuscada], !. % No hace falta ningun camino alterno despues de esto.

% CR: Seguimos recorriendo
camino_greedy(Jugada, ProfundidadBuscada, [Color | Resto], CA) :-
    siguiente_nivel(Jugada, SiguienteNivel),
    mayor_de_lista_shell(Mejor, SiguienteNivel),
    Mejor = [_G, Color, _Ady, _P, _Sec, _Profundidad],
    camino_greedy(Mejor, ProfundidadBuscada, Resto, CA).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mejor_camino_greedy(Grid, Depth, Secuencia, CantidadAdyacentes) :-
    % Checkeamos que se haya inicializado el programa previo a tratar de encontrar un mejor camino
    inicializado,
    
    % Obtenemos la informacion necesaria para obtener la casilla de origen
    fila_origen(FilaOrigen),
    columna_origen(ColumnaOrigen),
    elemento_en(Grid, FilaOrigen, ColumnaOrigen, ColorOrigen),

    % Obtenemos las adyacencias iniciales, y la cantidad de las mismas
    adyacentes_a_origen(Grid, casilla(ColorOrigen, FilaOrigen, ColumnaOrigen), AdyacenciasIniciales),
    length(AdyacenciasIniciales, CA),

    % Obtenemos la jugada inicial
    J = [Grid, ColorOrigen, AdyacenciasIniciales, CA, [], 0],

    %Obtenemos el mejor camino segun el criterio greedy
    camino_greedy(J, Depth, Secuencia, CantidadAdyacentes).

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