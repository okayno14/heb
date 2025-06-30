-module(heb_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

some_test_() ->
    [
        {"simple tag", fun simple_tag/0}
    ].

simple_tag() ->
    HTMLDocumentFun = heb:tag(<<"div">>, [], [], #{type => oneline}),
    ?assertEqual(<<"<div> </div>">>, HTMLDocumentFun(<<"">>)).


-endif.

