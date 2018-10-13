mod mac_table;
mod macros;
mod nt_match;


use syntax::ast::Crate;

pub fn test(unexpanded: &Crate,
            expanded: &Crate,
            rewritten: &Crate,
            sess: &::rustc::session::Session) {
    info!(" ** unexpanded spans:");
    ::print_spans::print_spans(unexpanded, sess.codemap());
    info!(" ** expanded spans:");
    ::print_spans::print_spans(expanded, sess.codemap());

    let mac_table = mac_table::collect_macro_invocations(unexpanded, expanded);
    {
        info!("mac_table: collected {} invocations", mac_table.map.len());
        let mut mac_table = mac_table.map.iter().map(|(k, v)| (*k, v.clone())).collect::<Vec<_>>();
        mac_table.sort_by_key(|e| e.0);

        for (expanded_id, mac_info) in mac_table {
            info!("mac result {:?}: {:?}, {}", expanded_id, mac_info.id,
                  ::syntax::print::pprust::mac_to_string(mac_info.mac));
        }
    }

    let collapsed = macros::collapse_macros(rewritten.clone(), &mac_table);
    info!("collapsed crate =====\n{}\n ===== end collapsed crate",
          ::syntax::print::pprust::to_string(|s| s.print_mod(&collapsed.module, &collapsed.attrs)));
}
