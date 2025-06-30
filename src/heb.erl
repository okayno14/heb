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
    ChildrenList :: nonempty_list(Child :: binary() | tag_fun() | tag_fun_inherit_config())
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
    ChildrenList :: nonempty_list(Child :: binary() | tag_fun() | tag_fun_inherit_config()),
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
tag_1(TagState = #tag_state{}, Name, AttrList, ChildrenList, Config = #{type := oneline}) ->
    TagBody =
        lists:foldl(
            fun(Child, Acc) ->
                Child2 = tag_body2(Child, Config, TagState),
                <<Acc/binary, " ", Child2/binary>>
            end,
            <<"">>,
            ChildrenList
        ),

    TagAttrs = tag_attrs(AttrList),
    TagBegining = tag_begining(Name, TagAttrs),
    TagEnding = tag_ending(Name),

    case TagBody of
        <<"">> ->
            <<TagBegining/binary, " ", TagEnding/binary>>;
        _ ->
            <<TagBegining/binary, TagBody/binary, " ", TagEnding/binary>>
    end;
tag_1(TagState = #tag_state{}, Name, AttrList, ChildrenList, Config = #{type := human}) ->
    #{format_opts := #{space_tab := SpaceTab}} = Config,
    #tag_state{deep_lvl = DeepLvl} = TagState,

    Tab = shift_deep_lvl(DeepLvl,SpaceTab),
    TabBody = shift_1_tab(SpaceTab,Tab),

    TagBody =
        lists:foldl(
            fun(Child, Acc) ->
                Child2 = tag_body2_inc_deep(Child, Config, TagState),
                case is_binary(Child) of
                    true ->
                        %% TODO translate
                        %% Подали в теле простой текст, надо добавить отступ,
                        %% потому что иначе мы просто не попадём в место функции,
                        %% где добавляются отступы для тегов
                        <<Acc/binary, TabBody/binary, Child2/binary, "\n">>;
                    false ->
                        case string:find(Child2, <<"\n">>) of
                            nomatch ->
                                %% TODO translate
                                %% Спарсили однострочный html-тэг, надо добавить отступ
                                <<Acc/binary, TabBody/binary, Child2/binary, "\n">>;
                            _ ->
                                %% TODO translate
                                %% Подали в теле другой тег, отступы уже добавлены на выходе из рекурсии
                                <<Acc/binary, Child2/binary, "\n">>
                        end
                end
            end,
            <<"">>,
            ChildrenList
        ),

    TagAttrs = tag_attrs(AttrList),
    TagBegining = tag_begining(Name, TagAttrs),
    TagEnding = tag_ending(Name),

    case TagBody of
        <<"">> ->
            <<Tab/binary, TagBegining/binary, " ", TagEnding/binary>>;
        _ ->
            <<
                Tab/binary, TagBegining/binary, "\n",
                TagBody/binary,
                Tab/binary, TagEnding/binary
            >>
    end.
%%--------------------------------------------------------------------

shift_deep_lvl(DeepLvl,SpaceTab) ->
    lists:foldl(fun(_, Acc) -> shift_1_tab(SpaceTab, Acc) end, <<"">>, lists:seq(1, DeepLvl)).

shift_1_tab(SpaceTab, Tab) ->
    lists:foldl(fun(_, Acc2) -> <<Acc2/binary, " ">> end, Tab, lists:seq(1, SpaceTab)).

tag_begining(Name, TagAttrs) ->
    case TagAttrs of
        <<"">> ->
            <<"<", Name/binary, ">">>;
        _ ->
            <<"<", Name/binary, TagAttrs/binary, ">">>
    end.

tag_ending(Name) ->
    TagEnding = <<"</", Name/binary, ">">>,
    TagEnding.

tag_body2_inc_deep(Child, Config, TagState) ->
    DeepLvl = TagState#tag_state.deep_lvl + 1,
    TagState2 = TagState#tag_state{deep_lvl = DeepLvl},
    tag_body2(Child, Config, TagState2).

tag_body2(Child, Config, TagState) when is_binary(Child) ->
    Child;
tag_body2(ChildFun, Config, TagState) when is_function(ChildFun, 1) ->
    Child = ChildFun(TagState);
tag_body2(ChildFun, Config, TagState) when is_function(ChildFun, 2) ->
    Child = ChildFun(TagState, Config).

tag_attrs(AttrList) ->
    lists:foldl(
        fun(Attr, Acc) when is_function(Attr, 0) andalso is_binary(Acc) ->
            TagAttr = Attr(),
            <<Acc/binary, " ", TagAttr/binary>>
        end,
        <<"">>,
        AttrList
    ).

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

