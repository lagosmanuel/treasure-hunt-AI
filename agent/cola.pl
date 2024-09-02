%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Este módulo modela una cola con prioridad sin elementos repetidos,
% donde cada elemento es una lista [IdElemento, ValorElemento].
% 

:- module(cola,
	  [
	    cola_insertar/3,
	    cola_eliminar/3,
        cola_obtener/3
	  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cola_insertar(+Cola, +Elemento, -NuevaCola)
%
% Inserta un elemento Elemento en una cola con prioridad, 
% donde Elemento es una lista [IdElemento, ValorElemento].
% 

cola_insertar([], Elemento, [Elemento]).
cola_insertar([Head|Tail], Elemento, [Elemento, Head|Tail]):-
    [_, ValorElemento] = Elemento,
    [_, ValorHead] = Head,
    ValorElemento =< ValorHead, !.
cola_insertar([Head|Tail], Elemento, [Head|Resto]):-
    cola_insertar(Tail, Elemento, Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cola_eliminar(+Cola, +IdElemento, -NuevaCola)
%
% Elimina el elemento con id IdElemento de una cola con prioridad,
% si es que existe. Si no existe, la cola permanece inalterada.
% 

cola_eliminar([], _, []).
cola_eliminar([[IdElemento, _]|Resto], IdElemento, Resto):- !.
cola_eliminar([Head|Tail], IdElemento, [Head|Resto]):-
    cola_eliminar(Tail, IdElemento, Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cola_obtener(+Cola, +IdElemento, -NuevaCola)
%
% Obtiene el valor del elemento con id IdElemento de una cola con 
% prioridad, si es que existe. Si no existe, el predicado falla.
% 

cola_obtener([[IdElemento, ValorElemento]|_], IdElemento, ValorElemento):- !.
cola_obtener([_|Tail], IdElemento, Resultado):-
    cola_obtener(Tail, IdElemento, Resultado).
