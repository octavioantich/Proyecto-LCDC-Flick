:- module(proylcc, 
	[  
		push/3,
        pop/3,
        peek/3
	]).

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
peek([E | Resto], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pop(+S, -E, -NS)
% Quita a E del tope de la pila S y asocia la nueva pila sin E a NS.
% Falla si S esta vacia.
% S: Pila (Stack) sobre la cual se operara
% E: Elemento a poppear
% NS: Nueva pila (New Stack), con el elemento ya pusheado
pop([E | NS], E, NS).