mod mac_table;
mod macros;
mod node_map;
mod nt_match;


pub use self::mac_table::{MacTable, MacInfo, collect_macro_invocations};
pub use self::node_map::match_nonterminal_ids;
pub use self::macros::collapse_macros;


use syntax::ast::Crate;

pub fn test(unexpanded: &Crate,
            expanded: &Crate,
            rewritten: &Crate,
            sess: &::rustc::session::Session) {
    info!(" ** unexpanded spans:");
    ::print_spans::print_spans(unexpanded, sess.codemap());
    info!(" ** expanded spans:");
    ::print_spans::print_spans(expanded, sess.codemap());

    let (mac_table, matched_ids) = mac_table::collect_macro_invocations(unexpanded, expanded);
    {
        info!("mac_table: collected {} invocations", mac_table.map.len());
        let mut mac_table = mac_table.map.iter().map(|(k, v)| (*k, v.clone())).collect::<Vec<_>>();
        mac_table.sort_by_key(|e| e.0);

        for (expanded_id, mac_info) in mac_table {
            info!("mac result {:?}: {:?}, {}", expanded_id, mac_info.id,
                  ::syntax::print::pprust::mac_to_string(mac_info.mac));
        }
    }

    let (collapsed, matched_ids) = macros::collapse_macros(rewritten.clone(), &mac_table);
    info!("collapsed crate =====\n{}\n ===== end collapsed crate",
          ::syntax::print::pprust::to_string(|s| s.print_mod(&collapsed.module, &collapsed.attrs)));
}
