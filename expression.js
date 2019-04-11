var vm = _ =>
         { with ({})
		   { syntax_error = message => Error(`syntax error at line ${line}, column ${column}${message ? '; ' + message : ''}`)
             rest = (fun, pure) =>
                    { return local = fun.reduce((n, f) => !pure  ? n.concat(f)
                                                        : f.pure ? n.concat(f)
                                                                 : n, []);
                    }
             push = (fun, pure) =>
                    { stack[stack.length++] = local;
                      return rest(local.concat(fun), pure);
                    }
             pull = _ => func = stack.pop()
             atom = `('(?:\\'|\\\\|[^'])*"|\\S+)`
             not = tail => `((?:\\s*(?!${tail})))`
             expr_str = tail => `((${atom}${not(tail)})*${atom}?)(\\s*${tail}\\s*)`
             impure_expr = tail_str =>
                             tail_ctx => ({ apply: _ => _
								          , pattn: RegExp(`^${expr_str(tail_str)}`)
                                          , match: m => console.log(`This is considered 'impure' because it shares scope with up-stream parts of the universe and thus can affect changes; keeping locals...`)
                                                     || rest(tail_ctx, false)
                                          , pure: true
                                          })
             pure_expr = tail_str =>
                           tail_ctx => ({ apply: _ => _
							            , pattn: RegExp(`^\\{\\s+${expr_str(tail_str)}`)
                                        , match: m => console.log(`This is considered 'pure' because it exists in a scope isolated from the up-stream parts of the universe that aren't pure; pushing locals into stack and filtering for purity...`)
                                                   || push(tail_ctx.concat({ apply: _ => _
											                               , pattn: /^\s+\}/
                                                                           , match: m => pull()
                                                                           , pure: true
                                                                           }), true)
                                        , pure: true
                                        })
             fun_cat = (fun, fun_) => m => fun(fun_(m))
             expr_map = (apply_fun, match_fun) => expr => 
                           (expr.apply = fun_cat(apply_fun, expr.apply))
                        && (expr.match = fun_cat(match_fun, expr.match))
                        && expr
             expr = tail_str =>
                      tail_ctx =>
                        [ pure_expr, impure_expr].map(fun => fun(tail_str)(tail_ctx))
             funct = expr_map(_ => _, match => console.log(`This is a declaration; inserting this expression as a function signature into the function locals`) || local.push(match) && match)
             funct_expr = tail_str =>
                            tail_ctx => pattn_expr(tail_str)(tail_ctx).map(funct)
             guard = expr_map(_ => _, match => console.log('This is a guard') || match)
             guard_expr = tail_str =>
                            tail_ctx => pattn_expr(tail_str)(tail_ctx).map(guard)
             pattn = expr_map(match =>
                                match.reduce((token_list, c) =>
                                                (prev =>
                                                   { switch (c)
                                                     { case '\\': if (!prev || prev.type != 'literal' || prev.done)
														            return token_list.concat({ type: 'abstract', value: '' });
											                      if (prev.escape)
                                                                  { delete prev.escape;
													                break;
													              }
                                                                  prev.escape = true;
                                                                  return token_list;
                                                       case ' ':
                                                       case '\n':
                                                       case '\t': if (!prev || ![ 'literal'
                                                                                , 'whitespace'
                                                                                ].find(t => t.type == prev.type) || prev.done)
										      		                return token_list.concat({ type: 'whitespace', value: c });
                                                                  break;
                                                       case '\'': if (!prev || prev.type != 'literal' || prev.done)
													                return token_list.concat({ type: 'literal', value: '' });
                                                                  if (prev.escape)
													              { delete prev.escape;
                                                                    break;
													              }
												                  prev.done = true;
														      	  return token_list;
													   default:   if (!prev || prev.type == 'whitespace' || prev.done)
													                return token_list.concat({ type: 'abstract', value: c });
                                                     }
                                                     if (prev)
                                                     { if (prev.escape)
													     return syntax_error('malformed escape sequence');
													   prev.value += c;
												     }
												     return token_list;
												   })(token_list.slice(-1)[0]), []), match => console.log('This type of expression is common throughout the language') || match)
             pattn_expr = tail_str =>
                            tail_ctx => expr(tail_str)(tail_ctx).map(pattn)
             value = expr_map(match => match, match => console.log('This is the pattern we replace future applications of this function with, up until recursion, then use B-reduction or something...') || match)
             value_expr = tail_str =>
                            tail_ctx => pattn_expr(tail_str)(tail_ctx).map(value)
             terminal = expr_map(match => console.log(`This is what translation ends on: ${JSON.stringify(match)}`) || match, match => console.log('This is where the translation ends') || match)
             terminal_guard_expr = tail_str => guard_expr(tail_str)([]).map(terminal)
             terminal_value_expr = tail_str => value_expr(tail_str)([]).map(terminal)
             grammar = [ funct_expr('\\<\\-')(value_expr('\\:')(terminal_guard_expr('\\;')))
                       , funct_expr('\\-\\>')(guard_expr('\\:')(terminal_value_expr('\\;')))
                       ].reduce((list, val) => list.concat(val), [])
             local = grammar.slice(0)
             column = 0
             line = 0
             stack = []
             evaluate_ = (list, expr) =>
                           code =>
                             code &&
                             expr.length
                             ? !console.log(`Attempting to match ${code} with ${expr[0].pattn.toString()}`) &&
                               (m => (m ? evaluate_(list.concat(expr[0].apply(Array.from(m[1]), list)), rest(expr[0].match(), expr[0].pure))
                                        : evaluate_(list, expr.slice(1))
                                     )(code.slice(m && m[0].length)))(expr[0].pattn.exec(code))
                             : !console.log('No further replacements remaining...')
             evaluate = evaluate_([], local.slice(0))
             public = [ 'expr'      , 'funct_expr'         , 'guard_expr'         , 'value_expr'
                      , 'pattn_expr', 'terminal_guard_expr', 'terminal_value_expr', 'grammar'
                      , 'local'     , 'column'             , 'line'               , 'stack'
                      , 'evaluate'
                      ]
             get = prop =>
                     public.find(prop_ => prop == prop_) && eval(prop)
             return this;
           }
         }

var vm_test = vm(_ => console.log(_));
_ => vm_test.evaluate(`'hello' world -> kek : world;`);
_ => vm_test.evaluate(`{ 'goodbye' galaxy -> galaxy : haha; }`);
