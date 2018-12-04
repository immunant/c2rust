```refactor-options hidden
revert
diff-style = full
no-show-filename
no-collapse-diff
```

TODO: `select` / marks tutorial goes here

For now, here's a demo of mark rendering in diffs:

```rust refactor-target hidden

fn main() {
    let x = 1 + 1;
    let y = 2 + 2;
}
```

```refactor
select target 'item(main);'
```

```refactor
select target 'crate; desc(match_expr(1 + 1));'
```

```refactor
select target 'item(main);' ;
select target 'crate; desc(match_expr(1 + 1));' ;
```

