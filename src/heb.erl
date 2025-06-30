-module(heb).

-export([
    build/1,
    build/2,

    tag/3,
    tag/4,

    attr/2
]).

-record(tag_state, {
    doc :: binary(),
    deep_lvl = 0 :: non_neg_integer()
}).

-export_type([
    tag_fun/0,
    tag_fun_inherit_config/0,

    attr_fun/0,

    config/0
]).

-type tag_fun() :: fun((#tag_state{}) -> HTMLDocument2 :: binary()).
-type tag_fun_inherit_config() :: fun(
    (#tag_state{}, Config :: config()) -> HTMLDocument2 :: binary()
).

-type config() ::
    #{type := oneline}
    | #{
        type := human,
        format_opts := #{
            %% TODO rename (посмотреть, как этот параметр называется в виме)
            space_tab := pos_integer()
        }
    }.

-type attr_fun() :: fun(() -> AttrString :: binary()).

%%--------------------------------------------------------------------
%% @doc
-spec build(TagFun :: tag_fun()) ->
    HTMLDocument2 :: binary().
%%--------------------------------------------------------------------
build(TagFun) ->
    TagFun(#tag_state{doc = <<"">>}).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec build(HTMLDocument :: binary(), TagFun :: tag_fun()) ->
    HTMLDocument2 :: binary().
%%--------------------------------------------------------------------
build(HTMLDocument, TagFun) ->
    TagFun(#tag_state{doc = HTMLDocument}).
%%--------------------------------------------------------------------

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
    fun(TagState = #tag_state{}, Config) ->
        tag_1(TagState, Name, AttrList, ChildrenList, Config)
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
    fun(TagState = #tag_state{}) ->
        tag_1(TagState, Name, AttrList, ChildrenList, Config)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% TODO сделать human-версию
tag_1(TagState = #tag_state{}, Name, AttrList, ChildrenList, Config = #{type := oneline}) ->
    TagBody =
        lists:foldl(
            fun
                (Child, Acc) when is_binary(Child) andalso is_binary(Acc) ->
                    <<Acc/binary, " ", Child/binary>>;
                (ChildFun, Acc) when is_function(ChildFun, 1) andalso is_binary(Acc) ->
                    Child = ChildFun(TagState),
                    <<Acc/binary, " ", Child/binary>>;
                (ChildFun, Acc) when is_function(ChildFun, 2) andalso is_binary(Acc) ->
                    Child = ChildFun(TagState, Config),
                    <<Acc/binary, " ", Child/binary>>
            end,
            <<"">>, ChildrenList
        ),

    TagAttrs =
        lists:foldl(
            fun(Attr, Acc) when is_function(Attr, 0) andalso is_binary(Acc) ->
                TagAttr = Attr(),
                <<Acc/binary, " ", TagAttr/binary>>
            end,
            <<"">>, AttrList
        ),

    TagBegining =
        case TagAttrs of
            <<"">> ->
                <<"<", Name/binary, ">">>;
            _ ->
                <<"<", Name/binary, TagAttrs/binary, ">">>
        end,

    TagEnding = <<"</", Name/binary, ">">>,

    Tag =
        case TagBody of
            <<"">> ->
                <<TagBegining/binary, " ", TagEnding/binary>>;
            _ ->
                <<TagBegining/binary, TagBody/binary, " ", TagEnding/binary>>
        end,

    HTMLDocument = TagState#tag_state.doc,
    case HTMLDocument of
        <<"">> ->
            Tag;
        _ ->
            <<HTMLDocument/binary, " ", Tag/binary>>
    end;
tag_1(HTMLDocument, Name, AttrList, ChildrenList, Config = #{type := human}) ->
    #{format_opts := #{space_tab := SpaceTab}} = Config,
    HTMLDocument.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec attr(Key :: binary(), Value :: binary()) ->
    attr_fun().
%%--------------------------------------------------------------------
attr(Key, Value) ->
    fun() ->
        <<Key/binary, "=", "\"", Value/binary, "\"">>
    end.
%%--------------------------------------------------------------------

