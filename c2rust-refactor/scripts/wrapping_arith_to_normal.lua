refactor:transform(
    function(transform)
        transform:replace_expr("$x:Expr.wrapping_add($y:Expr)", "$x + $y")
        transform:replace_expr("$x:Expr.wrapping_sub($y:Expr)", "$x - $y")
        transform:replace_expr("$x:Expr.wrapping_mul($y:Expr)", "$x * $y")
        transform:replace_expr("$x:Expr.wrapping_div($y:Expr)", "$x / $y")
        transform:replace_expr("$x:Expr.wrapping_rem($y:Expr)", "$x % $y")
        transform:replace_expr("$x:Expr.wrapping_neg()", "-$x")
        transform:replace_expr("$x:Expr.wrapping_shl($y:Expr)", "$x << $y")
        transform:replace_expr("$x:Expr.wrapping_shr($y:Expr)", "$x >> $y")
        transform:replace_expr("$x:Expr.wrapping_abs()", "$x.abs()")
    end
)
refactor:save_crate()
