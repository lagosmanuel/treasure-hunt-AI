:- use_module(module_path_finding, [
	buscar_plan_desplazamiento/4,
    agregar/6,
	raiz/1,
	padre/2,
	esMeta/1
]).

:- dynamic plandesplazamiento/1, goalNode/1.

/* test_case/8(Id, Nodos, Frontera, NuevosVecinos, Visitados, FronteraEsperada, VisitadosEsperados, Metas)
Nodos: lista de nodos del mapa.
Frontera: frontera sobre la que se agregarán los nodos vecinos.
Vecinos: nuevos vecinos que se deben agregar a la frontera.
Visitados: lista de nodos visitados durante la búsqueda.
FronteraEsperada: frontera que debe formarse al agregar los vecinos.
VisitadosEsperados: lista de nodos visitados luego de agregar los vecinos.
Metas: lista de nodos meta.
*/

/*
Caso 1: No hay nuevos vecinos que agregar a la frontera.
*/
test_case(
     1, 
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n2, 2]], % frontera
    [], % vecinos
    [[n1, 1]], % visitados
    [[n2, 2]], % frontera esperada
    [[n1, 1]], % visitados esperados
    [n9] % metas
).


/*
Caso 2: Nuevo vecino que ya estaba en la frontera. La nueva versión tiene peor valor de f (o costo).
*/
test_case(
     2, 
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n2, 2], [n5, 2]], % frontera
    [[n2, 10]], % vecinos
    [[n1, 1]], % visitados
    [[n2, 2], [n5, 2]], % frontera esperada
    [[n1, 1]], % visitados esperados
    [n9] % metas
).

/*
Caso 3: Nuevo vecino que ya estaba en la frontera. La nueva versión tiene mejor valor de f (o costo).
*/
test_case(
     3,
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n2, 10], [n5, 7]], % frontera
    [[n2, 5]], % vecinos
    [[n1, 1]], % visitados
    [[n2, 5], [n5, 7]], % frontera esperada
    [[n1, 1]], % visitados esperados
    [n9] % metas
).

/*
Caso 4: Nuevo vecino que estaba entre los visitados. La nueva versión tiene mejor valor de f (o costo).
*/
test_case(
     4, 
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n4, 2], [n5, 7]], % frontera
    [[n1, 2]], % vecinos
    [[n1, 10]], % visitados
    [[n4, 2], [n5, 7], [n1, 2]], % frontera esperada
    [], % visitados esperados
    [n9] % metas
).

/*
Caso 5: Nuevo vecino que estaba entre los visitados. La nueva versión tiene peor valor de f (o costo).
*/
test_case(
     5,
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n4, 2], [n5, 7]], % frontera
    [[n1, 5]], % vecinos
    [[n1, 1]], % visitados
    [[n4, 2], [n5, 7]], % frontera esperada
    [[n1, 1]], % visitados esperados
    [n9] % metas
).

/*
Caso 6: Vecino nuevo, no estaba en frontera ni en visitados.
*/
test_case(
     6, 
     [  node(n1, 1, 1, 1, []), 
        node(n2, 1, 2, 1, []), 
        node(n3, 1, 3, 1, []),
        node(n4, 2, 1, 1, []),
        node(n5, 2, 2, 1, []),
        node(n6, 2, 3, 1, []),
        node(n7, 3, 1, 1, []),
        node(n8, 3, 2, 1, []),
        node(n9, 3, 3, 1, []) 
    ],
    [[n3, 4]], % frontera
    [[n8, 5]], % vecinos
    [[n1, 1], [n2, 2]], % visitados
    [[n3, 4], [n8, 5]], % frontera esperada
    [[n1, 1], [n2, 2]], % visitados esperados
    [n9] % metas
).



/*
Predicados para correr batería de tests.
    run_tests/0 permite correr los tests. Consultar el predicado desde swi-prolog o swish.
    Importante: Los tests no verifican una búsqueda completa, sino que verifican el funcionamiento del predicado agregar/6.
*/

comparar(L1, L2) :-
    msort(L1, SortedL1),
    msort(L2, SortedL1).

run_particular_test(test_case(_TestID, _Nodos, Frontera, Vecinos, Visitados, FronteraEsperada, VisitadosEsperados, Metas)):-
    agregar(Frontera, Vecinos, FronteraSalida, Visitados, [a, 1], Metas),
    write('Frontera: '), write(Frontera), nl, 
    write('FronteraSalida: '), write(FronteraSalida), nl, 
    write('FronteraEsperada: '), write(FronteraEsperada), nl,
    comparar(FronteraEsperada, FronteraSalida),
    comparar(VisitadosEsperados, VisitadosEsperados).
run_particular_test(_):-
    write("Fallo en agregar..."), nl, fail.

verify_tests([]).
verify_tests([TestCase | RestOfTests]):-
    TestCase = test_case(TestID, Nodos, _, _, _, _, _, _),
    retractall(node(_, _, _, _, _)),
    forall(member(N, Nodos), assert(N)),
    run_particular_test(TestCase), !,
    write("Test superado: "), write(TestID), nl, nl,
    verify_tests(RestOfTests).
verify_tests([test_case(TestID, _, _, _, _, _, _, _) | RestOfTests]):-
    write("Falla en test: "), write(TestID), nl, nl,
    verify_tests(RestOfTests).


run_tests():-
    findall(test_case(TestID, Nodos, Frontera, Vecinos, Visitados, FronteraEsperada, VisitadosEsperada, Metas), 
            test_case(TestID, Nodos, Frontera, Vecinos, Visitados, FronteraEsperada, VisitadosEsperada, Metas),
            Test_cases),
    verify_tests(Test_cases).