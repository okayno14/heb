-module(heb_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

some_test_() ->
    [
        {"simple tag", fun simple_tag/0},
        {"simple numeric list", fun simple_numeric_list/0}
    ].

simple_tag() ->
    HTMLDocumentFun = heb:tag(<<"div">>, [], [], #{type => oneline}),
    ?assertEqual(<<"<div> </div>">>, HTMLDocumentFun(<<"">>)).

simple_numeric_list() ->
    HTMLDocumentFun =
        heb:tag(<<"ul">>, [], [
            heb:tag(<<"li">>, [], [<<"a">>]),
            heb:tag(<<"li">>, [], [<<"b">>]),
            heb:tag(<<"li">>, [], [<<"c">>])
        ], #{type => oneline}),

    ?assertEqual(<<"<ul> <li> a </li> <li> b </li> <li> c </li> </ul>">>, HTMLDocumentFun(<<"">>)).

-endif.

