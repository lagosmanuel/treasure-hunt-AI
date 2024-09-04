:- module(path_finding, [
    buscar_plan_desplazamiento/4,
    raiz/1,
    padre/2,
    esMeta/1
]).

:- use_module(module_beliefs_update, [
	at/3
]).

:- use_module(module_beliefs_update, [node/5, at/3]).
:- use_module(cola, [cola_insertar/3, cola_eliminar/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.

eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.

seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.

encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.

crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
    CantMetas > 0, !,
	retractall(raiz(_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan).
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.

buscarEstrella(Frontera, Metas, C3, Costo, Destino):-
	retractall(padre(_, _)),
	buscar(Frontera, [], Metas, Destino),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
    esMeta(Nodo), !.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% generarVecinos(+Nodo, -Vecinos)
%
% Genera los vecinos del nodo.

generarVecinos([IdNodo, _], Vecinos):-
    node(IdNodo, _, _, _, Conexiones),
    findall(Vecino, member(Vecino, Conexiones), Vecinos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar(+Frontera, +Vecinos, -NuevaFrontera, +Visitados, +Nodo, +Metas)
%
% Agrega los vecinos a la frontera.

agregar(Frontera, [], Frontera, _, _, _).
agregar(Frontera, [Vecino|Vecinos], NuevaFrontera, Visitados, Nodo, Metas):-
    [_, CostoNodo] = Nodo,
    [IdVecino, CostoVecino] = Vecino,
    \+member([IdVecino, _], Visitados), !,
    calcularMejorH(IdVecino, H, Metas),
    CostoTotal is CostoNodo + CostoVecino + H,
    agregarVecino([IdVecino, CostoTotal], Frontera, Nodo, FronteraConVecino),
    agregar(FronteraConVecino, Vecinos, NuevaFrontera, Visitados, Nodo, Metas).

agregar(Frontera, [_|Vecinos], NuevaFrontera, Visitado, Nodo, Metas):-
    agregar(Frontera, Vecinos, NuevaFrontera, Visitado, Nodo, Metas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregarVecino(+Vecino, +Frontera, +Nodo, -NuevaFrontera)
%
% Agrega un vecino a la Frontera.

agregarVecino([IdVecino, CostoVecino], Frontera, [IdNodo, _], NuevaFrontera):-
    \+member([IdVecino, _], Frontera), !,
    retractall(padre(IdVecino, _)),
    assert(padre(IdVecino, IdNodo)),
    cola_insertar(Frontera, [IdVecino, CostoVecino], NuevaFrontera).

agregarVecino([IdVecino, CostoVecino], Frontera, _, Frontera):-
    member([IdVecino, CostoAnterior], Frontera),
    CostoAnterior =< CostoVecino, !.

agregarVecino([IdVecino, CostoVecino], Frontera, [IdNodo, _], NuevaFrontera):-
    retractall(padre(IdVecino, _)),
    assert(padre(IdVecino, IdNodo)),
    cola_eliminar(Frontera, IdVecino, FronteraAux),
    cola_insertar(FronteraAux, [IdVecino, CostoVecino], NuevaFrontera).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.

agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.

costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularMejorH(+Nodo, ?Resultado, +Metas)
%
% Calcula el mínimo valor de la heurística para el nodo a una meta
% de una lista de metas.

calcularMejorH(Nodo, Resultado, Metas):-
    findall(H, (member(Meta, Metas), calcularH(Nodo, Meta, H)), Hs),
    min_list(Hs, Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo a una meta.
% La heurística es la distancia euclidea, multiplicado por un bonus
% por haber un tesoro en la meta.

calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
    distance([X1, Y1], [X2, Y2], Distancia),
    bonus_tesoro(Meta, Bonus),
    Resultado is Distancia * Bonus.

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
    Distance is sqrt(DX^2 + DY^2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% bonus_tesoro(+Nodo, -Bonus)
%
bonus_tesoro(Nodo, 0.9):-
    at(Nodo, copa, _).

bonus_tesoro(Nodo, 0.8):-
    at(Nodo, cofre, _).

bonus_tesoro(Nodo, 0.2):-
    at(Nodo, anillo, _).

bonus_tesoro(Nodo, 0.7):-
    at(Nodo, pocion, _).

bonus_tesoro(Nodo, 0.4):-
    at(Nodo, reloj(_), _).

bonus_tesoro(_, 1).
