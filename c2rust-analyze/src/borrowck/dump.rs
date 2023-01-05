use crate::borrowck::atoms::{AllFacts, AtomMaps, Loan, Origin, Output, Path, Point, Variable};
use rustc_hash::{FxHashMap, FxHashSet};
/// Copied partly from rustc `compiler/rustc_borrowck/src/facts.rs`, which is dual-licensed MIT and
/// Apache 2.0.
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt::Write as _;
use std::fs::{self, File};
use std::hash::Hash;
use std::io::{BufWriter, Write};
use std::path;

pub fn dump_facts_to_dir(
    facts: &AllFacts,
    maps: &AtomMaps,
    dir: impl AsRef<path::Path>,
) -> Result<(), Box<dyn Error>> {
    let dir: &path::Path = dir.as_ref();
    fs::create_dir_all(dir)?;
    let wr = FactWriter { maps, dir };
    macro_rules! write_facts_to_path {
        ($wr:ident . write_facts_to_path($this:ident . [
            $($field:ident,)*
        ])) => {
            $(
                $wr.write_facts_to_path(
                    &$this.$field,
                    &format!("{}.facts", stringify!($field))
                )?;
            )*
        }
    }
    write_facts_to_path! {
        wr.write_facts_to_path(facts.[
            loan_issued_at,
            universal_region,
            cfg_edge,
            loan_killed_at,
            subset_base,
            loan_invalidated_at,
            var_used_at,
            var_defined_at,
            var_dropped_at,
            use_of_var_derefs_origin,
            drop_of_var_derefs_origin,
            child_path,
            path_is_var,
            path_assigned_at_base,
            path_moved_at_base,
            path_accessed_at_base,
            known_placeholder_subset,
            placeholder,
        ])
    }
    Ok(())
}

pub fn dump_output_to_dir(
    output: &Output,
    maps: &AtomMaps,
    dir: impl AsRef<path::Path>,
) -> Result<(), Box<dyn Error>> {
    let dir: &path::Path = dir.as_ref();
    fs::create_dir_all(dir)?;
    let wr = FactWriter { maps, dir };
    macro_rules! write_output_to_path {
        ($wr:ident . write_output_to_path($this:ident . [
            $($field:ident,)*
        ])) => {
            let Output { $(ref $field,)* } = $this;
            $(
                $wr.write_output_to_path(
                    $field,
                    &format!("{}.output", stringify!($field))
                )?;
            )*
        }
    }
    write_output_to_path! {
        wr.write_output_to_path(output.[
            errors,
            subset_errors,
            move_errors,
            dump_enabled,
            loan_live_at,
            origin_contains_loan_at,
            origin_contains_loan_anywhere,
            origin_live_on_entry,
            loan_invalidated_at,
            subset,
            subset_anywhere,
            var_live_on_entry,
            var_drop_live_on_entry,
            path_maybe_initialized_on_exit,
            path_maybe_uninitialized_on_exit,
            known_contains,
            var_maybe_partly_initialized_on_exit,
        ])
    }
    Ok(())
}

struct FactWriter<'tcx, 'w> {
    maps: &'w AtomMaps<'tcx>,
    dir: &'w path::Path,
}

impl FactWriter<'_, '_> {
    fn write_facts_to_path<T>(&self, rows: &[T], file_name: &str) -> Result<(), Box<dyn Error>>
    where
        T: FactRow,
    {
        let file = &self.dir.join(file_name);
        let mut file = BufWriter::new(File::create(file)?);
        for row in rows {
            row.write(&mut file, self.maps)?;
        }
        Ok(())
    }

    fn write_output_to_path<T>(&self, rows: &T, file_name: &str) -> Result<(), Box<dyn Error>>
    where
        T: OutputTable,
    {
        let file = &self.dir.join(file_name);
        let mut file = BufWriter::new(File::create(file)?);
        rows.write(&mut file, self.maps)?;
        Ok(())
    }
}

trait FactRow {
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>>;
}

impl FactRow for Origin {
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[self])
    }
}

impl<A, B> FactRow for (A, B)
where
    A: Render,
    B: Render,
{
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1])
    }
}

impl<A, B, C> FactRow for (A, B, C)
where
    A: Render,
    B: Render,
    C: Render,
{
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1, &self.2])
    }
}

impl<A, B, C, D> FactRow for (A, B, C, D)
where
    A: Render,
    B: Render,
    C: Render,
    D: Render,
{
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1, &self.2, &self.3])
    }
}

fn write_row(
    out: &mut dyn Write,
    maps: &AtomMaps,
    columns: &[&dyn Render],
) -> Result<(), Box<dyn Error>> {
    for (index, c) in columns.iter().enumerate() {
        let tail = if index == columns.len() - 1 {
            "\n"
        } else {
            "\t"
        };
        write!(out, "{:?}{}", c.to_string(maps), tail)?;
    }
    Ok(())
}

trait OutputTable {
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>>;
}

impl<K: Render + Ord, V: Render> OutputTable for FxHashMap<K, V> {
    fn write(&self, out: &mut dyn Write, maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        let mut entries = self.iter().collect::<Vec<_>>();
        entries.sort_by_key(|&(k, _)| k);
        for (k, v) in entries {
            writeln!(out, "{}: {}", k.to_string(maps), v.to_string(maps))?;
        }
        Ok(())
    }
}

impl OutputTable for bool {
    fn write(&self, out: &mut dyn Write, _maps: &AtomMaps) -> Result<(), Box<dyn Error>> {
        writeln!(out, "{self}")?;
        Ok(())
    }
}

trait Render {
    fn to_string(&self, maps: &AtomMaps) -> String;
}

impl<K: Render + Hash + Eq, V: Render> Render for FxHashMap<K, V> {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let mut s = String::new();
        write!(s, "{{").unwrap();
        let mut first = true;
        for (k, v) in self {
            if !first {
                write!(s, ",").unwrap();
            }
            first = false;
            write!(s, " {}: {}", k.to_string(maps), v.to_string(maps)).unwrap();
        }
        if !first {
            write!(s, " ").unwrap();
        }
        write!(s, "}}").unwrap();
        s
    }
}

impl<T: Render + Hash + Eq> Render for FxHashSet<T> {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let mut s = String::new();
        write!(s, "{{").unwrap();
        let mut first = true;
        for x in self {
            if !first {
                write!(s, ",").unwrap();
            }
            first = false;
            write!(s, " {}", x.to_string(maps)).unwrap();
        }
        if !first {
            write!(s, " ").unwrap();
        }
        write!(s, "}}").unwrap();
        s
    }
}

impl<K: Render + Ord, V: Render> Render for BTreeMap<K, V> {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let mut s = String::new();
        write!(s, "{{").unwrap();
        let mut first = true;
        for (k, v) in self {
            if !first {
                write!(s, ",").unwrap();
            }
            first = false;
            write!(s, " {}: {}", k.to_string(maps), v.to_string(maps)).unwrap();
        }
        if !first {
            write!(s, " ").unwrap();
        }
        write!(s, "}}").unwrap();
        s
    }
}

impl<T: Render + Ord> Render for BTreeSet<T> {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let mut s = String::new();
        write!(s, "{{").unwrap();
        let mut first = true;
        for x in self {
            if !first {
                write!(s, ",").unwrap();
            }
            first = false;
            write!(s, " {}", x.to_string(maps)).unwrap();
        }
        if !first {
            write!(s, " ").unwrap();
        }
        write!(s, "}}").unwrap();
        s
    }
}

impl<T: Render> Render for Vec<T> {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let mut s = String::new();
        write!(s, "[").unwrap();
        let mut first = true;
        for x in self {
            if !first {
                write!(s, ", ").unwrap();
            }
            first = false;
            write!(s, "{}", x.to_string(maps)).unwrap();
        }
        write!(s, "]").unwrap();
        s
    }
}

impl<A: Render, B: Render> Render for (A, B) {
    fn to_string(&self, maps: &AtomMaps) -> String {
        format!("({}, {})", self.0.to_string(maps), self.1.to_string(maps))
    }
}

impl Render for Origin {
    fn to_string(&self, _maps: &AtomMaps) -> String {
        format!("'_#{}r", usize::from(*self))
    }
}

impl Render for Loan {
    fn to_string(&self, _maps: &AtomMaps) -> String {
        format!("bw{}", usize::from(*self))
    }
}

impl Render for Point {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let (bb, idx, sub) = maps.get_point(*self);
        format!("{sub:?}({bb:?}[{idx}])")
    }
}

impl Render for Variable {
    fn to_string(&self, _maps: &AtomMaps) -> String {
        format!("_{}", usize::from(*self))
    }
}

impl Render for Path {
    fn to_string(&self, _maps: &AtomMaps) -> String {
        format!("mp{}", usize::from(*self))
    }
}
