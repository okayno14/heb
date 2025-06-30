-module(heb).

-export([
    %      attr/2,

    tag/3,
    tag/4

    %      build/2
]).

-export_type([
    tag_fun/0,
    tag_fun_inherit_config/0,

    attr_fun/0,

    config/0
]).

-type tag_fun() :: fun((HTMLDocument :: binary()) -> HTMLDocument2 :: binary()).
-type tag_fun_inherit_config() :: fun(
    (HTMLDocument :: binary(), Config :: config()) -> HTMLDocument2 :: binary()
).

-type config() ::
    #{type := oneline}
    | #{
        type := human,
        format_opts := #{space_tab := pos_integer()}
    }.

-type attr_fun() :: fun(() -> AttrString :: binary()).


%%--------------------------------------------------------------------
%% @doc
-spec tag(
    Name :: binary(),
    AttrList :: [Attr :: attr_fun()],
    ChildrenList :: [Child :: binary() | tag_fun() | tag_fun_inherit_config()]
) ->
    tag_fun_inherit_config().
%%--------------------------------------------------------------------
tag(Name, AttrList, ChildrenList) ->
    fun(HTMLDocument, Config) ->
        tag_1(HTMLDocument, Name, AttrList, ChildrenList, Config)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec tag(
    Name :: binary(),
    AttrList :: [Attr :: attr_fun()],
    ChildrenList :: [Child :: binary() | tag_fun() | tag_fun_inherit_config()],
    Config :: config()
) ->
    tag_fun().
%%--------------------------------------------------------------------
tag(Name, AttrList, ChildrenList, Config) ->
    fun(HTMLDocument) ->
        tag_1(HTMLDocument, Name, AttrList, ChildrenList, Config)
    end.

%% TODO сделать human-версию
tag_1(HTMLDocument, Name, AttrList, ChildrenList, Config = #{type := oneline}) ->
    TagBody =
        lists:foldl(
            fun
                (Child, Acc) when is_binary(Child) andalso is_binary(Acc) ->
                    <<Acc/binary, " ", Child/binary>>;
                (ChildFun, Acc) when is_function(ChildFun, 1) andalso is_binary(Acc) ->
                    Child = ChildFun(<<"">>),
                    <<Acc/binary, " ", Child/binary>>;
                (ChildFun, Acc) when is_function(ChildFun, 2) andalso is_binary(Acc) ->
                    Child = ChildFun(<<"">>, Config),
                    <<Acc/binary, " ", Child/binary>>
            end,
            <<"">>, ChildrenList
        ),

    TagBegining = <<"<", Name/binary, ">">>,
    TagEnding = <<"</", Name/binary, ">">>,

    Tag =
        case TagBody of
            <<"">> ->
                <<TagBegining/binary, " ", TagEnding/binary>>;
            _ ->
                <<TagBegining/binary, TagBody/binary, " ", TagEnding/binary>>
        end,

    case HTMLDocument of
        <<"">> ->
            Tag;
        _ ->
        <<HTMLDocument/binary, " ", Tag/binary>>
    end.
%%--------------------------------------------------------------------

