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
        {"deep numeric list human format", fun deep_numeric_list_human/0},
        {"table test (oneline tags in human-tag)", fun oneline_tags_in_human_tag/0},
        {"table test (human tags in oneline-tag)", fun human_tags_in_oneline_tag/0}
    ].

simple_tag() ->
    HTMLDocumentFun = heb:tag(<<"div">>, [], [<<"some text">>], #{type => oneline}),
    ?assertEqual(<<"<div> some text </div>">>, heb:build(HTMLDocumentFun)).

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

    HTMLDocument =
        "<ul>\n"
        "    <li>\n"
        "        a\n"
        "    </li>\n"
        "    <li>\n"
        "        b\n"
        "    </li>\n"
        "    <li>\n"
        "        c\n"
        "    </li>\n"
        "</ul>",

    ?assertEqual(erlang:list_to_binary(HTMLDocument), heb:build(HTMLDocumentFun)).

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

    HTMLDocument =
        "<ul>\n"
        "    <ul>\n"
        "        <li>\n"
        "            1\n"
        "        </li>\n"
        "        <li>\n"
        "            2\n"
        "        </li>\n"
        "    </ul>\n"
        "    <li>\n"
        "        b\n"
        "    </li>\n"
        "    <li>\n"
        "        c\n"
        "    </li>\n"
        "</ul>",

    ?assertEqual(erlang:list_to_binary(HTMLDocument), heb:build(HTMLDocumentFun)).

oneline_tags_in_human_tag() ->
    HTMLDocumentFun =
        heb:tag(
            <<"table">>,
            [],
            [
                heb:tag(
                    <<"tr">>,
                    [],
                    [
                        heb:tag(<<"td">>, [], [<<"1">>]),
                        heb:tag(<<"td">>, [], [<<"vasya">>]),
                        heb:tag(<<"td">>, [], [<<"qwerty">>])
                    ],
                    #{type => oneline}
                ),
                heb:tag(
                    <<"tr">>,
                    [],
                    [
                        heb:tag(<<"td">>, [], [<<"2">>]),
                        heb:tag(<<"td">>, [], [<<"petya">>]),
                        heb:tag(<<"td">>, [], [<<"qwerty">>])
                    ],
                    #{type => oneline}
                )
            ],
            #{type => human, format_opts => #{space_tab => 4}}
        ),

    HTMLDocument =
        "<table>\n"
        "    <tr> <td> 1 </td> <td> vasya </td> <td> qwerty </td> </tr>\n"
        "    <tr> <td> 2 </td> <td> petya </td> <td> qwerty </td> </tr>\n"
        "</table>",

    ?assertEqual(erlang:list_to_binary(HTMLDocument), heb:build(HTMLDocumentFun)).

human_tags_in_oneline_tag() ->
    HTMLDocumentFun =
        heb:tag(
            <<"table">>,
            [],
            [
                heb:tag(
                    <<"tr">>,
                    [],
                    [
                        heb:tag(<<"td">>, [], [<<"2">>]),
                        heb:tag(<<"td">>, [], [<<"petya">>]),
                        heb:tag(<<"td">>, [], [<<"qwerty">>])
                    ],
                    #{type => human, format_opts => #{space_tab => 4}}
                )
            ],
            #{type => oneline}
        ),

    HTMLDocument =
        "<table> <tr>\n"
        "    <td>\n"
        "        2\n"
        "    </td>\n"
        "    <td>\n"
        "        petya\n"
        "    </td>\n"
        "    <td>\n"
        "        qwerty\n"
        "    </td>\n"
        "</tr> </table>",

    ?assertEqual(erlang:list_to_binary(HTMLDocument), heb:build(HTMLDocumentFun)).

-endif.

