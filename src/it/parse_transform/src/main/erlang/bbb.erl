-module(bbb).
-export([parse_transform/2]).
parse_transform(Forms, Options) ->
    io:format("forms: ~p~n option: ~p~n", [Forms, Options]),
    [Attr1, Attr2, Attr3, _Fun1, Eof] = Forms,
    [Attr1, Attr2, Attr3, {function,4,foobar,0,[{clause,4,[],[],[{atom,5,foobar}]}]}, Eof].
