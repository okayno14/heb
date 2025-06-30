-module(heb).

-export([
    %      attr/2,

    %      tag/3,
    tag/4

    %      build/2
]).

-export_type([
    tag_fun/0,
    tag_fun_custom_config/0,

    attr_fun/0,

    config/0
]).

-type tag_fun() :: fun((HTMLDocument :: binary()) -> HTMLDocument2 :: binary()).
-type tag_fun_custom_config() :: fun(
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
    ChildrenList :: [Child :: binary() | tag_fun()],
    Config :: config()
) ->
    tag_fun().
%%--------------------------------------------------------------------
tag(Name, AttrList, ChildrenList, Config) ->
    fun(HTMLDocument) -> HTMLDocument end.
%%--------------------------------------------------------------------

