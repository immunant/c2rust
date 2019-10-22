refactor:transform(
   function(transform)
      return transform:match(
         function(mcx)
            local pat = mcx:parse_stmts([[
$i:Ident = $start:Expr;
$'label:?Ident: while $cond:Expr {
    $body:MultiStmt;
    $incr:Stmt;
}]])
            local lt_cond = mcx:parse_expr("$i < $end:Expr")
            local le_cond = mcx:parse_expr("$i <= $end:Expr")

            local i_plus_eq = mcx:parse_expr("$i += $step:Expr")
            local i_eq_plus = mcx:parse_expr("$i = $i + $step:Expr")

            local range_one_excl = mcx:parse_stmts("$'label: for $i in $start .. $end { $body; }")
            local range_one_incl = mcx:parse_stmts("$'label: for $i in $start ..= $end { $body; }")
            local range_step_excl = mcx:parse_stmts("$'label: for $i in ($start .. $end).step_by($step as usize) { $body; }")
            local range_step_incl = mcx:parse_stmts("$'label: for $i in ($start ..= $end).step_by($step as usize) { $body; }")


            mcx:fold_with(
               pat,
               function(orig, mcx)
                  cond = mcx:get_expr("$cond")
                  if mcx:try_match(lt_cond, cond) then
                     range_excl = true
                  elseif mcx:try_match(le_cond, cond) then
                     range_excl = false
                  else
                     return orig
                  end

                  incr = mcx:get_stmt("$incr")
                  incr_kind = incr:get_kind()
                  if (incr_kind == "Semi" or
                      incr_kind == "Expr") then
                     incr = incr:get_node()
                  else
                     return orig
                  end

                  if (not mcx:try_match(i_plus_eq, incr) and
                      not mcx:try_match(i_eq_plus, incr)) then
                     return orig
                  end


                  step = mcx:get_expr("$step")
                  if (step:get_kind() == "Lit" and
                      step:get_node():get_value() == 1) then
                     if range_excl then
                        repl_step = range_one_excl
                     else
                        repl_step = range_one_incl
                     end
                  else
                     if range_excl then
                        repl_step = range_step_excl
                     else
                        repl_step = range_step_incl
                     end
                  end

                  return mcx:subst(repl_step)
               end
            )
         end
      )
   end
)
refactor:save_crate()
