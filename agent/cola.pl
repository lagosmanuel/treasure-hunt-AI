%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Este módulo modela una cola con prioridad sin elementos repetidos,
% donde cada elemento es una lista [IdElemento, ValorElemento].

:- module(cola, [
    cola_insertar/3,
    cola_eliminar/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% cola_insertar(+Cola, +Elemento, -NuevaCola)
%
% Inserta un elemento en una cola con prioridad,
% donde el elemento es una lista [IdElemento, ValorElemento].

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
% Elimina un elemento de una cola con prioridad según el id.

cola_eliminar([], _, []).
cola_eliminar([[IdElemento, _]|Resto], IdElemento, Resto):- !.
cola_eliminar([Head|Tail], IdElemento, [Head|Resto]):-
    cola_eliminar(Tail, IdElemento, Resto).
