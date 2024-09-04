:- use_module(module_beliefs_update, [
 	update_beliefs/1,
 	time/1,
 	node/5,
	at/3,
    direction/1,
    new_rare_item/0
]).

:- use_module(module_path_finding, [
 	buscar_plan_desplazamiento/4,
 	raiz/1,
 	padre/2,
 	esMeta/1
]).

:- use_module(extras, [
	append3/4
]).

:- dynamic plandesplazamiento/1, goalNode/1, tengo_pocion/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(+Perc, -Action, -Text, -Beliefs)
%
% El predicado run/4 implementa el comportamiento del agente.
%
% Es ejecutado automáticamente por el juego una vez por ciclo.
% Implementa la interface con el mundo virtual, 
% recibiendo la información que el agente puede percibir del entorno
% y permite hacer que el agente actue en el mundo virtual
% No pueden cambiarse los parámetros, pero si el cuerpo.
% 
% El primer parámetro (Perc) recibe una lista con la percepción del agente.
% Los otros parámetros envían información al juego.
% La idea es que el agente actualice su estado interno usando Perc, 
% y luego decida que acción hacer instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
% Beliefs es una lista con un subconjunto de creencias del agente, particularmente las que hacer referencia a objetos.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
% IMPORTANTE: La lista de la percepción no viene ordenada. El formato anterior es solo a modo de referencia.
%
% Action debe ser un functor totalmente instanciado (sin variables) que corresponda a una acción valida.
% Si Action tiene algún error el agente pierde el ciclo de ejecución.

run(Perc, Action, Text, Beliefs):-
	update_beliefs(Perc), % implementado en module_beliefs_update
	decide_action(Action, Text),
	findall(at(X, Y, Z), at(X, Y, Z), Beliefs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% decide_action(-Action, -Text)
%
% Decide la acción a realizar por el agente, instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.

% Si estoy en la misma posición que un tesoro, intento levantarlo.
decide_action(Action, Mensaje):-
    at(MyNode, agente, me),
    check_tesoro(MyNode, IdTesoro, Mensaje),
    node(MyNode, PosX, PosY, _, _),
    Action = levantar_tesoro(IdTesoro, PosX, PosY),
    retractall(at(MyNode, _, IdTesoro)),
	retractall(plandesplazamiento(_)).

% Si tengo un plan de movimientos y no hay un nuevo tesoro raro,
% ejecuto la siguiente acción.
decide_action(Action, 'Avanzar...'):-
	plandesplazamiento(Plan),
    \+new_rare_item,
    evaluar_plan(Plan),
	length(Plan, LargoPlan),
    LargoPlan > 0, !,
    obtener_movimiento(Plan, Destino, Resto),
	retractall(plandesplazamiento(_)),
	assert(plandesplazamiento(Resto)),
	Action = Destino.
	
% Si no tengo un plan guardado, busco uno nuevo.
decide_action(Action, 'Avanzar con nuevo plan...'):-
 	busqueda_plan(Plan, _Destino, _Costo),
	Plan \= [],
    obtener_movimiento(Plan, Action, Resto),
	assert(plandesplazamiento(Resto)).

% Me muevo a una posición vecina seleccionada de manera aleatoria.
decide_action(Action, 'Me muevo random para explorar el territorio...'):-
    random(Chance),
    Chance =< 0.6,
    at(MyNode, agente, me),
    node(MyNode, _, _, _, AdyList),
    length(AdyList, LenAdyList), LenAdyList > 0,
    random_member([IdAdyNode, _CostAdyNode], AdyList), !,
    Action = avanzar(IdAdyNode).

% Giro en sentido horario, para conocer mas terreno.
decide_action(Action, 'Girar para conocer el territorio...'):-
	(
		direction(w)
		-> Action = girar(d)
		; ( direction(d)
			-> Action = girar(s)
			; ( direction(s)
				-> Action = girar(a)
				; Action = girar(w)
				)			
			)	
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% evaluar_plan(+Plan)
%
% Tiene éxito si el plan termina en un tesoro.

evaluar_plan(Plan):-
    reverse(Plan, [avanzar(Meta)|_]),
    at(Meta, Tipo, _),
    write('estoy persiguiendo un/a '), write(Tipo), write(' en '), write(Meta), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtener_movimiento(?Lista, ?Movimiento, ?Resto)
%
% Obtiene el primer movimiento de una lista de movimientos.

obtener_movimiento([], [], []).
obtener_movimiento([X|Xs], X, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% busqueda_plan(-Plan, -Destino, -Costo)
%
% Busca un plan de desplazamiento hacia el tesoro que se encuentre mas cerca.
% Primero busca planes para los tesoros que son más raros de conseguir.

busqueda_plan(Plan, Destino, Costo):-
 	retractall(plandesplazamiento(_)),
 	retractall(esMeta(_)),

    tipo_tesoro(TipoTesoro),
    findall(Nodo, at(Nodo, TipoTesoro, _), Metas),

    buscar_plan_desplazamiento(Metas, Plan, Destino, Costo). % implementado en module_path_finding

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% check_tesoro(+Nodo, -IdTesoro, -Mensaje)
%
% Comprueba si en el nodo hay un tesoro. En caso de haberlo,
% devuelve el identificador y un mensaje 'Quiero levantar...'.

check_tesoro(Nodo, IdTesoro, 'Quiero levantar una copa...'):-
    at(Nodo, copa, IdTesoro),
    retractall(tengo_pocion).

check_tesoro(Nodo, IdTesoro, 'Quiero levantar un cofre...'):-
    at(Nodo, cofre, IdTesoro),
    retractall(tengo_pocion).

check_tesoro(Nodo, IdTesoro, 'Quiero levantar un diamante...'):-
    at(Nodo, diamante, IdTesoro),
    retractall(tengo_pocion).

check_tesoro(Nodo, IdTesoro, 'Quiero levantar una poción...'):-
    at(Nodo, pocion, IdTesoro),
    assert(tengo_pocion).

check_tesoro(Nodo, IdTesoro, 'Quiero levantar un reloj...'):-
    at(Nodo, reloj(_), IdTesoro),
    retractall(tengo_pocion).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% tipo_tesoro(-TipoTesoro)
%
% Instancia TipoTesoro con un tipo de tesoro válido.
% Si es una poción, falla si el agente ya tiene una poción.

tipo_tesoro(reloj(_)).
tipo_tesoro(diamante).
tipo_tesoro(cofre).
tipo_tesoro(pocion):- \+tengo_pocion.
tipo_tesoro(copa).
