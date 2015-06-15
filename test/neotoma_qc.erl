-module(neotoma_qc).
-include("neotoma.hrl").
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-compile(export_all).

prop_peephole_equiv() ->
    %% The peephole optimizer should not add or change existing errors
    %% in the analysis
    ?FORALL(G, grammar(),
            begin
                Normal = neotoma_analyze:analyze(G),
                PeepOpt = neotoma_simplify:simplify(G),
                case Normal of
                    {ok, _G0} ->
                        case neotoma_analyze:analyze(PeepOpt) of
                            {ok, _G1} -> true;
                            _ -> false
                        end;
                    Errs ->
                        equals(Errs, neotoma_analyze:analyze(PeepOpt))
                end
            end).

prop_pp() ->
    ?FORALL(G, grammar(),
            begin
                Printed = iolist_to_binary(neotoma_pp:print(G)),
                G1 = neotoma_parse2:parse(Printed),
                ?WHENFAIL(begin
                              io:format("PP:~n~s~n"
                                        "Simplified:~n~p~n"
                                        "Parsed:~n~p~n",

                                        [Printed,
                                         neotoma_simplify:simplify(G),
                                         G1])
                          end,
                          is_record(G1, grammar) andalso tree_equal(G, G1))
            end).

%% Do the same as the peephole optimizer and remove choice and
%% sequence with single sub-expressions.
tree_equal(#choice{alts=[A]}, B) ->
    tree_equal(A,B);
tree_equal(A, #choice{alts=[B]}) ->
    tree_equal(A,B);

tree_equal(#sequence{exprs=[A]}, B) ->
    tree_equal(A,B);
tree_equal(A, #sequence{exprs=[B]}) ->
    tree_equal(A,B);

%% If we're dealing with lists of expressions, as from within a choice
%% or sequence or the top-level declaration list, compare them
%% pair-wise.
tree_equal(L1, L2) when is_list(L1), is_list(L2),
                        length(L1) == length(L2) ->
    lists:all(fun({A,B}) ->
                      tree_equal(A,B)
              end, lists:zip(L1, L2));

tree_equal(#grammar{declarations=D1, code=C1},
           #grammar{declarations=D2, code=C2}) ->
    tree_equal(C1, C2) andalso tree_equal(D1, D2);

tree_equal(#declaration{name=N, expr=E1, code=C1},
           #declaration{name=N, expr=E2, code=C2}) ->
    tree_equal(E1, E2) andalso tree_equal(C1, C2);

tree_equal(#sequence{exprs=Es1},
           #sequence{exprs=Es2}) ->
    tree_equal(Es1, Es2);

tree_equal(#choice{alts=As1},
           #choice{alts=As2}) ->
    tree_equal(As1, As2);

tree_equal(#primary{label=L, modifier=M, expr=E1},
           #primary{label=L, modifier=M, expr=E2}) ->
    tree_equal(E1, E2);

tree_equal(#nonterminal{name=N},
           #nonterminal{name=N}) ->
    true;

tree_equal(#anything{} = _, #anything{} = _B) ->
    true;

tree_equal(#epsilon{} = _, #epsilon{} = _B) ->
    true;

tree_equal(#charclass{charclass=C}, #charclass{charclass=C}) ->
    true;

tree_equal(#string{string=S}, #string{string=S}) ->
    true;

tree_equal(#regexp{regexp=R}, #regexp{regexp=R}) ->
    true;

tree_equal(#code{code=C, identity=I},
           #code{code=C, identity=I}) ->
    true;

tree_equal(A,A) ->
    %% mostly for code = undefined
    true;

tree_equal(_A,_B) ->
    false.



%% Grammar generator
grammar() ->
    #grammar{declarations=non_empty(list(declaration())),
             code = undefined}.

declaration() ->
    #declaration{name = name(),
                 expr = expr(),
                 code = rule_code(),
                 index = index()}.

name() ->
    elements([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
              t, u, v, w, x, y, z]).

index() ->
    {{line, line()}, {column, column()}}.

line() ->
    ?SUCHTHAT(I, noshrink(nat()), I > 0).

column() ->
    line().

rule_code() ->
    oneof([ undefined,
            #code{identity=true,
                  index=index()},
            #code{code = code_content(),
                  index = index()}
          ]).

code_content() ->
    oneof([" ok ",
           " Node ",
           " Idx ",
           " {Node, Idx} "
          ]).

expr() ->
    ?SIZED(Size, expr(Size)).

expr(Size) ->
    oneof([primary(Size),
           choice(Size),
           sequence(Size)]).

sequence(Size) ->
    #sequence{exprs = non_empty(list(primary(Size div 2))),
              index = index()}.

choice(Size) ->
    #choice{alts=non_empty(list(oneof([sequence(Size div 2), primary(Size div 2)]))),
            index = index()}.

primary(Size) ->
    ?LET({M,Expr,I}, {modifier(), primary_expr(Size), index()},
         if M == undefined ->
                 Expr;
            true ->
                 #primary{expr = Expr,
                          label = undefined,
                          modifier = M,
                          index = I}
         end).

primary_expr(Size) ->
    oneof([terminal(), nonterminal()] ++
              [expr(Size div 2) || Size > 1]).

terminal() ->
    oneof([string(), regexp(), charclass(), anything()]).

nonterminal() ->
    #nonterminal{name = name(), index = index()}.

string() ->
    #string{string = text(), index = index()}.

regexp() ->
    #regexp{regexp = text(), index = index()}.

charclass() ->
    #charclass{charclass = text(), index = index()}.

anything() ->
    #anything{index = index()}.

epsilon() ->
    #epsilon{index = index()}.

modifier() ->
    elements([undefined,
              one_or_more,
              zero_or_more,
              optional,
              assert,
              deny]).

text() ->
    ?LET(B, non_empty(list(char())), unicode:characters_to_binary(B,latin1,utf8)).

-endif.
