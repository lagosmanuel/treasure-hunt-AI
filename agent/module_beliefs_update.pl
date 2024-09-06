:- module(beliefs_update, [
    update_beliefs/1,
    time/1,
    node/5,
    at/3,
    direction/1,
    new_rare_item/0
]).

:- dynamic time/1, node/5, at/3, direction/1, new_rare_item/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.

update_beliefs(Perc):-

    % El agente olvida
    retractall(time(_)),
    retractall(direction(_)),
    retractall(at(_, agente, me)),
    retractall(new_rare_item),

    % El agente actualiza los relojes
    forall(at(IdNodo, reloj(X), IdReloj),
        update_clock(at(IdNodo, reloj(X), IdReloj))
    ),

    % El agente confirma sus hechos
    forall(member(node(IdNodo, _, _, _, _), Perc),
        check_fact(IdNodo, Perc)
    ),

    % El agente recuerda los nuevos hechos,
    % y si es un tesoro raro lo registra
    forall((member(Rel, Perc), \+call(Rel)), (
        assert(Rel), check_rare_item(Rel)
    )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_clock(+clock)
%
% El predicado update_clock/1 actualiza el tiempo de un
% reloj recordado por el agente.

update_clock(at(IdNodo, reloj(X), IdReloj)):-
    retract(at(IdNodo, reloj(X), IdReloj)),
    NX is X-1,
    NX > 0,
    assert(at(IdNodo, reloj(NX), IdReloj)).

update_clock(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_fact(+IdNodo, +Perc)
%
% El predicado check_fact/2 comprueba si un hecho at/3
% recordado por el agente sigue siendo verdadero según
% la percepción actual. Asume que en un mismo nodo
% hay a lo sumo una entidad.

check_fact(IdNodo, Perc):-
    at(IdNodo, TipoEntidad, IdEntidad),
    member(at(IdNodo, TipoEntidad, IdEntidad), Perc).

check_fact(IdNodo, _):-
    retractall(at(IdNodo, _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_rare_item(+Rel)
%
% El predicado check_new_item/1 comprueba si el nuevo
% hecho es un tesoro raro.

check_rare_item(at(_, diamante, _)):-
    assert(new_rare_item).

check_rare_item(at(_, reloj(_), _)):-
    assert(new_rare_item).

check_rare_item(_).
