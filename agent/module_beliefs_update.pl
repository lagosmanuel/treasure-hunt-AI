:- module(beliefs_update, [
    update_beliefs/1,
    time/1,
    node/5,
    at/3,
    direction/1
]).

:- dynamic time/1, node/5, at/3, direction/1.

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
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

update_beliefs(Perc):-

    % El agente olvida
    retractall(time(_)),
    retractall(direction(_)),
    retractall(at(_, agente, me)),

    % El agente actualiza los relojes
    forall(at(IdNodo, reloj(X), IdReloj),
        update_clock(at(IdNodo, reloj(X), IdReloj))
    ),

    % El agente confirma sus hechos
    forall(member(node(IdNodo, _, _, _, _), Perc),
        check_fact(IdNodo, Perc)
    ),

    % El agentente recuerda los nuevos hechos
    forall((member(Rel, Perc), \+call(Rel)),
        assert(Rel)
    ).

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
% la percepción actual.

check_fact(IdNodo, Perc):-
    at(IdNodo, TipoEntidad, IdEntidad),
    member(at(IdNodo, TipoEntidad, IdEntidad), Perc).

check_fact(IdNodo, _):-
    retractall(at(IdNodo, _, _)).
