# heb - Html Erlang Builder

This is a library for recursive binary-html building.

# Examples

```erlang
    HTMLDocumentFun = heb:tag(<<"div">>, [], [<<"some text">>], #{type => oneline}),
    %% "<div> some text </div>"
    HTMLDocument = heb:build(HTMLDocumentFun).
```

```erlang
    HTMLDocumentFun =
        heb:tag(<<"ul">>, [], [
            heb:tag(<<"li">>, [], [<<"a">>]),
            heb:tag(<<"li">>, [], [<<"b">>]),
            heb:tag(<<"li">>, [], [<<"c">>])
        ], #{type => human, format_opts => #{space_tab => 4}}),
	% <ul>
	%     <li>
	%         a
	%     </li>
	%     <li>
	%         b
	%     </li>
	%     <li>
	%         c
	%     </li>
	% </ul>
    HTMLDocument = heb:build(HTMLDocumentFun).
```
# How use

To build HTML-document you must:

* get anonymous function, that returned by `heb:tag/4`;
* call `heb:build/1`.

`heb:tag/4` accepts:

* Name - tag name;
* AttrList - list of values, returned by `heb:attr/2`. This functions can add any attribute to tag;
* Body - can be a binary or another `heb:tag/3` (inherits Config), `heb:tag/4` (accepts new Config);
* Config - map from examples: `oneline` for oneline tag without `\n`, `human` - for human-readable formatting;


