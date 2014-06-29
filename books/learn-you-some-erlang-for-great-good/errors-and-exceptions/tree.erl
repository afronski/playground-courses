-module(tree).
-export([ empty/0, insert/3, lookup/2, has_value/2 ]).

empty() -> { node, 'nil' }.

insert(Key, Val, { node, 'nil' }) ->
	{ node, { Key, Val, { node, 'nil' }, { node, 'nil' } } };
insert(NewKey, NewVal, { node, { Key, Val, Smaller, Larger } }) when NewKey < Key ->
	{ node, { Key, Val, insert(NewKey, NewVal, Smaller), Larger } };
insert(NewKey, NewVal, { node, { Key, Val, Smaller, Larger } }) when NewKey > Key ->
	{ node, { Key, Val, Smaller, insert(NewKey, NewVal, Larger) } };
insert(Key, Val, { node, { Key, _, Smaller, Larger } }) ->
	{ node, { Key, Val, Smaller, Larger } }.

lookup(_, { node, 'nil' }) ->
	undefined;
lookup(Key, { node, { Key, Val, _, _ } }) ->
	{ ok, Val };
lookup(Key, { node, { NodeKey, _, Smaller, _ } }) when Key < NodeKey ->
	lookup(Key, Smaller);
lookup(Key, { node, { _, _, _, Larger } }) ->
	lookup(Key, Larger).

has_value(Val, Tree) ->
	try has_value1(Val, Tree) of
		_ -> false
	catch
		true -> true
	end.

has_value1(_, { node, 'nil' }) ->
	false;
has_value1(Val, { node, { _, Val, _, _ } }) ->
	throw(true);
has_value1(Val, { node, { _, _, Left, Right } }) ->
	has_value1(Val, Left),
	has_value1(Val, Right).