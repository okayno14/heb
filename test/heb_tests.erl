-module(heb_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

oneline_test_() ->
    [
        {"simple tag", fun simple_tag/0},
        {"simple numeric list", fun simple_numeric_list/0},
        {"simple ref", fun simple_ref/0}
    ].

human_test_() ->
    [
        {"simple numeric list human format", fun simple_numeric_list_human/0},
        {"deep numeric list human format", fun deep_numeric_list_human/0}
    ].

simple_tag() ->
    HTMLDocumentFun = heb:tag(<<"div">>, [], [], #{type => oneline}),
    ?assertEqual(<<"<div> </div>">>, heb:build(HTMLDocumentFun)).

simple_numeric_list() ->
    HTMLDocumentFun =
        heb:tag(<<"ul">>, [], [
            heb:tag(<<"li">>, [], [<<"a">>]),
            heb:tag(<<"li">>, [], [<<"b">>]),
            heb:tag(<<"li">>, [], [<<"c">>])
        ], #{type => oneline}),

    ?assertEqual(<<"<ul> <li> a </li> <li> b </li> <li> c </li> </ul>">>, heb:build(HTMLDocumentFun)).

simple_ref() ->
    HTMLDocumentFun =
        heb:tag(<<"a">>, [heb:attr(<<"href">>, <<"images/xxx.jpg">>)], [<<"My Photo !!!">>], #{
            type => oneline
        }),

    ?assertEqual(<<"<a href=\"images/xxx.jpg\"> My Photo !!! </a>">>, heb:build(HTMLDocumentFun)).

simple_numeric_list_human() ->
    HTMLDocumentFun =
        heb:tag(<<"ul">>, [], [
            heb:tag(<<"li">>, [], [<<"a">>]),
            heb:tag(<<"li">>, [], [<<"b">>]),
            heb:tag(<<"li">>, [], [<<"c">>])
        ], #{type => human, format_opts => #{space_tab => 4}}),

    ?debugFmt("~n~ts~n", [heb:build(HTMLDocumentFun)]),
    ok.

deep_numeric_list_human() ->
    HTMLDocumentFun =
        heb:tag(<<"ul">>, [], [
            heb:tag(<<"ul">>, [], [
                heb:tag(<<"li">>, [], [<<"1">>]),
                heb:tag(<<"li">>, [], [<<"2">>])
            ]),
            heb:tag(<<"li">>, [], [<<"b">>]),
            heb:tag(<<"li">>, [], [<<"c">>])
        ], #{type => human, format_opts => #{space_tab => 4}}),

    ?debugFmt("~n~ts~n", [heb:build(HTMLDocumentFun)]),
    ok.

%      ?assertEqual(<<"<ul> <li> a </li> <li> b </li> <li> c </li> </ul>">>, heb:build(HTMLDocumentFun)).



-endif.

