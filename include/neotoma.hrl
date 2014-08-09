-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.

%%----------------------------
%% Parser records and types
%%----------------------------

%% A nonterminal, which must have a corresponding declaration
-record(nonterminal, {
          name :: atom(),
          index :: index()
         }).

%% A regexp
-record(regexp, {
          regexp :: unicode:chardata(),
          index :: index()
         }).

%% A literal string
-record(string, {
          string :: unicode:chardata(),
          index :: index()
         }).

%% A character class, e.g. [A-Z]
-record(charclass, {
          charclass :: unicode:chardata(),
          index :: index()
         }).

%% The '.' parsing expression
-record(anything, {
          index :: index()
         }).

%% Either the '~' sigil or a block of Erlang code
-record(code, {
          code :: unicode:chardata() | undefined,
          identity = false :: boolean(),
          index :: index(),
          parsed :: syntax_tools:syntaxTree(),
          comments :: [ erl_comment_scan:comment() ],
          used_args = []
         }).

%% A primary parsing expression, which may be labeled and have a
%% modifier, such as lookahead or repetition
-record(primary, {
          expr :: terminal() | #nonterminal{} | expression(),
          label :: atom(),
          modifier :: modifier() | undefined,
          index :: index()
         }).

%% A sequence of consecutive expressions
-record(sequence, {
          exprs = [ #primary{} ],
          index :: index()
         }).

%% A series of alternative expressions that use ordered choice
-record(choice, {
          alts :: [ #sequence{} | #primary{} ],
          index :: index()
         }).

%% A declaration maps a nonterminal to a parsing expression and
%% optional associated transformation code.
-record(declaration, {
          name :: #nonterminal{},
          expr :: expression(),
          code :: #code{} | undefined,
          index :: index()
         }).

%% An entire grammar, with its optional code block
-record(grammar, {
          declarations :: [ #declaration{} ],
          code :: #code{} | undefined,
          analysis % :: #symbols{} | undefined
         }).

-type terminal() :: #regexp{} | #string{} | #charclass{} | #anything{}.
-type modifier() :: one_or_more | zero_or_more | optional | assert | deny.

%% An abstract representation of a parsing expression.
-type expression() :: #choice{} | #sequence{} | #primary{}.
-type syntax_error() :: {syntax_error, tuple()}.

%%----------------------------
%% Analyzer records and types
%%----------------------------
-type rule() :: {atom(), index(), #code{}}.
-record(symbols, {
          nts = orddict:new() :: orddict:orddict(),
          rules = [] :: [ rule() ],
          combinators = ordsets:new() :: ordsets:ordset()
         }).

-type unused_rule() :: {unused_rule, {atom(), index()}}.
-type no_reduction() :: {no_reduction, {atom(), [ index() ]}}.
-type duplicate_rule() :: {duplicate_rule, atom(), OrigIndex::index(), Duplicates::[index()]}.
-type code_scan_error() :: {erl_scan:error_info(), erl_scan:location()}.
-type code_parse_error() :: erl_parse:error_info().
-type semantic_error() :: no_reduction() | code_scan_error() | code_parse_error() | duplicate_rule().
-type semantic_warning() :: unused_rule().
