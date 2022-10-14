-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
% bst(Key, Value) ->
%     ?LET(KVS, eqc_gen:list({Key, Value}),
%          lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
%                      empty(),
%                      KVS)).

bst(Key, Value) ->
    ?LET(KVS, eqc_gen:list({Key, Value}),
         lists:foldl(fun({K,V}, T) -> {call, bst, insert, [K, V, T]} end,
                     {call, bst, empty, []},
                     KVS)).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(T)).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, T))).

prop_empty_valid() ->
            valid (empty()).

prop_delete_valid() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            valid (delete(K, T))).

prop_union_valid() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            valid (union(T1, T2))).




%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, T)),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, T)
                       end)).

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, insert(K, V, T)),
                       {found, V})).


prop_find_post_absent() ->
     % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K, delete(K, T)),
                        nothing)).

prop_union_post() ->
    % ∀ k v t1 t2. find k (union t1 (insert k v t2)) === {found, v}
    ?FORALL({T1, T2, K, V},
            {bst(atom_key(), int_value()), bst(atom_key(), int_value()), atom_key(), int_value()},
            eqc:equals(find(K, union(T1, insert(K, V, T2))),
                        {found, V})).



%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, T)) >= bst:size(T)).

prop_size_delete() ->
    % ∀ k t. size (delet k t) <= size t
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            bst:size(delete(K, T)) =< bst:size(T)).

prop_size_union() ->
    % ∀ t1 t2. size (union t1 t2) == size t1 + size t2
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            bst:size(union(T1, T2)) == bst:size(T1) + bst:size(T2)).



obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, insert(K2, V2, T)),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, T);
                           false -> insert(K2, V2, insert(K1, V1, T))
                       end)).

prop_insert_delete() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            obs_equals(delete(K1, insert(K2, V, T)),
                        case K1 =:= K2 of
                            true -> delete(K1, T);
                            false -> insert(K2, V, delete(K1, T))
                        end)).

prop_insert_union() ->
    ?FORALL({K, V, T1, T2},
            {atom_key(), int_value(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            obs_equals(union(T1, insert(K, V, T2)), insert(K, V, union(T1, T2)))).


%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(insert(K, V, T)),
                   sorted_insert(K, V, delete_key(K, model(T))))).

prop_find_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(find(K, insert(K, V, T)),
                {found, find_key(K, model(T))})).

prop_empty_model() ->
    equals(model(empty()), []).

prop_delete_model() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            equals(model(delete(K, T)),
                delete_key(K, model(T)))).

prop_union_model() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            equals(model(union(T1, T2)),
                (union_model(model(T1), model(T2))))).


-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

-spec find_key(Key, [{Key, Value}]) -> Value.
find_key(Key, KVS) -> [ V || {K, V} <- KVS, K =:= Key ].

-spec union_model([{Key, Value}], [{Key, Value}]) -> [{Key, Value}].
union_model([{K, V}], KVS2) -> sorted_insert(K, V, KVS2);
union_model([{K, V}|Rest], KVS2) -> union_model(Rest, sorted_insert(K, V, KVS2)).



%% -- Test all properties in the module: eqc:module(test_bst)
