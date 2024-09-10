:- use_module(module_beliefs_update, [
	update_beliefs/1,
	time/1,
	node/5,
    at/3,
    direction/1
]).

/* test_case/4(Id, Percepciones, PostCondicionesPos, PostCondicionesNeg)
Percepciones es una lista con dos percepciones, cada una siendo una lista con los mínimos elementos necesarios.
PostCondicionesPos son creencias que deben verificarse luego de actualizar creencias con las dos percepciones (en forma de secuencia).
PostCondicionesNeg son creencias que NO debe verificarse luego de actualizar creencias con las dos percepciones.
*/

/*
Caso 1: Agregar nodo nuevo (n1, n2, n3) y no agregar nodo repetido (n1 en segunda percepción).
*/
test_case(
    1, 
    [
        [node(n1, 1, 1, 1, []), node(n2, 2, 2, 1, []), time(1), direction(w), at(n1, agente, me)],
        [node(n1, 1, 1, 1, []), node(n3, 3, 3, 1, []), time(2), direction(w), at(n1, agente, me)]
    ],
    [node(n1, 1, 1, 1, []), node(n2, 2, 2, 1, []), node(n3, 3, 3, 1, []), (findall(node(n1, 1, 1, 1, []), node(n1, 1, 1, 1, []), Occurrences), length(Occurrences, 1))],
    []
).

/*
Caso 2: Modificar de manera correcta time/1 y direction/1.
*/
test_case(
    2, 
    [
        [node(n1, 1, 1, 1, []), time(1), direction(w), at(n1, agente, me)],
        [node(n1, 1, 1, 1, []), time(2), direction(s), at(n1, agente, me)]
    ],
    [node(n1, 1, 1, 1, []), time(2), direction(s), at(n1, agente, me)],
    [time(1), direction(w)]
).

/*
Caso 3: Modificar de manera correcta la posición del agente.
*/
test_case(
    3, 
    [
        [node(n1, 1, 1, 1, []), time(1), direction(w), at(n1, agente, me)],
        [node(n2, 2, 2, 2, []), time(1), direction(w), at(n2, agente, me)]
    ],
    [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), at(n2, agente, me)], 
    [at(n1, agente, me)]
).

/*
Caso 4: Agregar nuevo nodo (n2) y nueva entidad (cofre c1).
*/
test_case(
    4, 
    [
        [node(n1, 1, 1, 1, []), time(1), direction(w), at(n1, agente, me)],
        [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), time(2), direction(w), at(n2, cofre, c1), at(n1, agente, me)]
    ],
    [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), at(n1, agente, me), at(n2, cofre, c1)], 
    []
).

/*
Caso 5: No agregar una entidad que ya existe en el mismo lugar.
*/
test_case(
    5, 
    [
        [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), time(1), direction(w), at(n2, cofre, c1), at(n1, agente, me)],
        [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), time(2), direction(w), at(n2, cofre, c1), at(n1, agente, me)]
    ],
    [node(n1, 1, 1, 1, []), node(n2, 2, 2, 2, []), at(n1, agente, me), at(n2, cofre, c1), (findall(at(n2, cofre, c1),at(n2, cofre, c1), Occurrences), length(Occurrences, 1))], 
    []
).


/*
Predicados para correr batería de tests.
    run_tests/0 permite correr los tests. Consultar el predicado desde swi-prolog o swish.
*/

check(Fact) :-
    call(Fact), !.
check(_Fact):- fail.

is_satisfied([]).
is_satisfied([Belief | RestOfBeliefs]):-
    check(Belief), !,
    is_satisfied(RestOfBeliefs).
is_satisfied([Belief | _RestOfBeliefs]):-
    write("Falta Verificar: "), write(Belief), nl, fail.

is_not_satisfied([]).
is_not_satisfied([Belief | RestOfBeliefs]):-
    not(check(Belief)), !, 
    is_not_satisfied(RestOfBeliefs).
is_not_satisfied([Belief | _RestOfBeliefs]):-
    write("No debería verificarse: "), write(Belief), nl, fail.

run_particular_test(test_case(_TestId, Perceptions, PostCondicionesPos, PostCondicionesNeg)):-
    forall(member(PercList, Perceptions), update_beliefs(PercList)),
    is_satisfied(PostCondicionesPos),
    is_not_satisfied(PostCondicionesNeg).
run_particular_test(_):-
    write("Fallo en update_beliefs..."), nl, fail.
    
    
verify_tests([]):-
    write("Tests superados."), nl.
verify_tests([TestCase | RestOfTests]):-
    retractall(node(_, _, _, _, _)),
    retractall(at(_, _, _)),
    retractall(time(_)),
    run_particular_test(TestCase), !,
    TestCase = test_case(TestID, _, _, _),
    write("Test superado: "), write(TestID), nl,
    verify_tests(RestOfTests).
verify_tests([test_case(TestID, _, _, _) | RestOfTests]):-
    write("Falla en test: "), write(TestID), nl,
    verify_tests(RestOfTests).

run_tests():-
    findall(test_case(TestID, X, Y, Z), test_case(TestID, X, Y, Z), Test_cases),
    verify_tests(Test_cases).

