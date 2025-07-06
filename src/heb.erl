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

-define(is_child(Child), (is_binary(Child) orelse is_function(Child))).

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

-type tag_child() :: binary() | tag_fun() | tag_fun_inherit_config().

-type config() ::
    #{type := oneline}
    | #{
        type := human,
        format_opts := #{
            space_tab := pos_integer()
        }
    }.

-type attr_fun() :: fun(() -> AttrString :: binary()).

%%%===================================================================
%%% API
%%%===================================================================

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
    ChildrenList :: nonempty_list(tag_child())
) ->
    tag_fun_inherit_config().
%%--------------------------------------------------------------------
tag(Name, AttrList, ChildrenList) ->
    fun(TagState = #tag_state{}, Config) ->
        tag_1(TagState, Name, AttrList, ChildrenList, Config)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Build a html-tag
-spec tag(
    Name :: binary(),
    AttrList :: [Attr :: attr_fun()],
    ChildrenList :: nonempty_list(tag_child()),
    Config :: config()
) ->
    tag_fun().
%%--------------------------------------------------------------------
tag(Name, AttrList, ChildrenList, Config) ->
    fun(TagState = #tag_state{}) ->
        tag_1(TagState, Name, AttrList, ChildrenList, Config)
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% Tag.builder.recursion
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec tag_1(
    TagState :: #tag_state{},
    Name :: binary(),
    AttrList :: [Attr :: attr_fun()],
    ChildrenList :: nonempty_list(tag_child()),
    Config :: config()
) ->
    HTMLDocument2 :: binary().
%%--------------------------------------------------------------------
tag_1(TagState = #tag_state{}, Name, AttrList, ChildrenList, Config = #{type := oneline}) ->
    TagBody =
        lists:foldl(
            fun(Child, Acc) when is_binary(Acc) and ?is_child(Child) ->
                Child2 = tag_child(Child, Config, TagState),
                <<Acc/binary, " ", Child2/binary>>
            end,
            <<"">>,
            ChildrenList
        ),
    true = is_binary(TagBody),

    TagAttrs = tag_attrs(AttrList),
    TagBegining = tag_begining(Name, TagAttrs),
    TagEnding = tag_ending(Name),

    <<TagBegining/binary, TagBody/binary, " ", TagEnding/binary>>;
tag_1(TagState = #tag_state{}, Name, AttrList, ChildrenList, Config = #{type := human}) ->
    #{format_opts := #{space_tab := SpaceTab}} = Config,
    #tag_state{deep_lvl = DeepLvl} = TagState,

    Tab = tab(DeepLvl,SpaceTab),
    TabBody = shift_1_tab(SpaceTab,Tab),

    TagBody =
        lists:foldl(
            fun(Child, Acc) when is_binary(Acc) and ?is_child(Child) ->
                Child2 = tag_child_inc_deep_lvl(Child, Config, TagState),
                case is_binary(Child) of
                    true ->
                        %% Add Tabs to simle-text-body
                        <<Acc/binary, TabBody/binary, Child2/binary, "\n">>;
                    false ->
                        case string:find(Child2, <<"\n">>) of
                            nomatch ->
                                %% Add Tabs to oneline html-tag
                                <<Acc/binary, TabBody/binary, Child2/binary, "\n">>;
                            _ ->
                                %% Got human-html-tag, do nothuig
                                <<Acc/binary, Child2/binary, "\n">>
                        end
                end
            end,
            <<"">>,
            ChildrenList
        ),
    true = is_binary(TagBody),

    TagAttrs = tag_attrs(AttrList),
    TagBegining = tag_begining(Name, TagAttrs),
    TagEnding = tag_ending(Name),

    <<
        Tab/binary, TagBegining/binary, "\n",
        TagBody/binary,
        Tab/binary, TagEnding/binary
    >>.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec tag_child_inc_deep_lvl(tag_child(), config(), #tag_state{}) ->
    HTMLDocument2 :: binary().
%%--------------------------------------------------------------------
tag_child_inc_deep_lvl(Child, Config, TagState) ->
    DeepLvl = TagState#tag_state.deep_lvl + 1,
    TagState2 = TagState#tag_state{deep_lvl = DeepLvl},
    tag_child(Child, Config, TagState2).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec tag_child(tag_child(), config(), #tag_state{}) ->
    HTMLDocument2 :: binary().
%%--------------------------------------------------------------------
tag_child(Child, _Config, _TagState) when is_binary(Child) ->
    Child;
tag_child(ChildFun, _Config, TagState) when is_function(ChildFun, 1) ->
    ChildFun(TagState);
tag_child(ChildFun, Config, TagState) when is_function(ChildFun, 2) ->
    ChildFun(TagState, Config).
%%--------------------------------------------------------------------

%%%===================================================================
%%% Tag.builder
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec tag_begining(Name :: binary(), TagAttrs :: binary()) ->
    TagBegining :: binary().
%%--------------------------------------------------------------------
tag_begining(Name, TagAttrs) ->
    case TagAttrs of
        <<"">> ->
            <<"<", Name/binary, ">">>;
        _ ->
            <<"<", Name/binary, TagAttrs/binary, ">">>
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec tag_ending(Name :: binary()) ->
    TagEnding :: binary().
%%--------------------------------------------------------------------
tag_ending(Name) ->
    TagEnding = <<"</", Name/binary, ">">>,
    TagEnding.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec tag_attrs(AttrList :: [Attr :: attr_fun()]) ->
    TagAttrs :: binary().
%%--------------------------------------------------------------------
tag_attrs(AttrList) ->
    TagAttrs =
        lists:foldl(
            fun(Attr, Acc) when is_function(Attr, 0) andalso is_binary(Acc) ->
                TagAttr = Attr(),
                true = is_binary(TagAttr),
                <<Acc/binary, " ", TagAttr/binary>>
            end,
            <<"">>,
            AttrList
        ),
    true = is_binary(TagAttrs),
    TagAttrs.
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

%%%===================================================================
%%% utils
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec tab(DeepLvl :: non_neg_integer(), SpaceTab :: pos_integer()) ->
    Tab :: binary().
%%--------------------------------------------------------------------
tab(DeepLvl, SpaceTab) ->
    Tab =
        lists:foldl(fun(_, Acc) -> shift_1_tab(SpaceTab, Acc) end, <<"">>, lists:seq(1, DeepLvl)),
    true = is_binary(Tab),
    Tab.

shift_1_tab(SpaceTab, Tab) ->
    lists:foldl(
        fun(_, Acc2) when is_binary(Acc2) -> <<Acc2/binary, " ">> end, Tab, lists:seq(1, SpaceTab)
    ).
%%--------------------------------------------------------------------

