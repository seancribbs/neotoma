-define(FMT(F,A), lists:flatten(io_lib:format(F,A))).

-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type cost() :: non_neg_integer() | infinite | undefined.
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

%% The '.' parsing expression, matches any character.
-record(anything, {
          index :: index()
         }).

%% The epsilon expression, trivially matches nothing. This is not
%% directly available in grammars but is used in transformation and
%% optimization phases.
-record(epsilon, {
          index :: index()
         }).

%% Either the '~' sigil or a block of Erlang code
-record(code, {
          code :: unicode:chardata() | undefined,
          identity = false :: boolean(),
          index :: index(),
          parsed :: erl_syntax:syntaxTree(),
          comments :: [ erl_comment_scan:comment() ],
          used_args = []
         }).

%% A labeling of an expression for simpler extraction
-record(label, {
          expr :: expression(),
          label :: atom(),
          index :: index()
         }).

%% A Kleene '+' operator representing greedy repetition of at least
%% one.
-record(plus, {
          expr :: expression(),
          index :: index()
         }).

%% A Kleene '*' operator representing greedy repetition of zero or
%% more.
-record(star, {
          expr :: expression(),
          index :: index()
         }).

%% Optional expression, which consumes nothing if not present
-record(optional, {
          expr :: expression(),
          index :: index()
         }).

%% Positive lookahead
-record(assert, {
          expr :: expression(),
          index :: index()
         }).

%% Negative lookahead
-record(deny, {
          expr :: expression(),
          index :: index()
         }).


%% A sequence of consecutive expressions
-record(sequence, {
          exprs :: [ choice() | primary() ],
          index :: index()
         }).

%% A series of alternative expressions that use ordered choice
-record(choice, {
          alts :: [ sequence() | primary() ],
          index :: index()
         }).

%% A declaration maps a nonterminal to a parsing expression and
%% optional associated transformation code.
-record(declaration, {
          name :: atom(),
          expr :: expression(),
          code :: #code{} | undefined,
          cost :: cost(),
          calls = orddict:new() :: orddict:orddict(),
          index :: index()
         }).

%% An entire grammar, with its optional code block
-record(grammar, {
          declarations :: [ #declaration{} ],
          code :: #code{} | undefined,
          analysis, % :: #symbols{} | undefined
          filename :: file:filename()
         }).

-type terminal() :: #regexp{} | #string{} | #charclass{} | #anything{} |
                    #epsilon{}.

-type primary() :: terminal() | #nonterminal{} | #star{} | #plus{} |
                   #optional{} | #assert{} | #deny{} | #label{}.

-type choice() :: #choice{}.
-type sequence() :: #sequence{}.

%% An abstract representation of a parsing expression.
-type expression() :: choice() | sequence() | primary().
-type syntax_error() :: {syntax_error, tuple()}.

%%----------------------------
%% Analyzer records and types
%%----------------------------
-type rule() :: {atom(), index(), #code{}}.
-record(symbols, {
          nts = orddict:new() :: orddict:orddict(),
          rules = [] :: [ rule() ]
         }).

-type unused_rule() :: {unused_rule, {atom(), index()}}.
-type no_reduction() :: {no_reduction, {atom(), [ index() ]}}.
-type duplicate_rule() :: {duplicate_rule, atom(), OrigIndex::index(), Duplicates::[index()]}.
-type code_scan_error() :: {erl_scan:error_info(), erl_scan:location()}.
-type code_parse_error() :: erl_parse:error_info().
-type semantic_error() :: no_reduction() | code_scan_error() | code_parse_error() | duplicate_rule().
-type semantic_warning() :: unused_rule().


%%----------------------------
%% Some useful guard macros
%%----------------------------
-define(IS_REPETITION(Record), is_record(Record, plus) orelse is_record(Record, star)).

-define(IS_MODIFIER(Record),
        ?IS_REPETITION(Record) orelse
        is_record(Record, optional) orelse
        is_record(Record, assert) orelse
        is_record(Record, deny)).

-define(IS_PRIMARY(Record),
        ?IS_MODIFIER(Record) orelse
        is_record(Record, label)).

-define(PRIMARY_EXPR(Record), element(2, Record)).
-define(SET_EXPR(Record, Value), setelement(2, Record, Value)).
