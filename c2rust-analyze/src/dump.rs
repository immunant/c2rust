/// Copied partly from rustc `compiler/rustc_borrowck/src/facts.rs`, which is dual-licensed MIT and
/// Apache 2.0.
use std::error::Error;
use std::fmt::Debug;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path;
use crate::atoms::{AllFacts, AtomMaps, Origin, Loan, Point, Variable, Path};

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
            //loan_issued_at,
            //universal_region,
            cfg_edge,
            //loan_killed_at,
            //subset_base,
            //loan_invalidated_at,
            //var_used_at,
            //var_defined_at,
            //var_dropped_at,
            //use_of_var_derefs_origin,
            //drop_of_var_derefs_origin,
            //child_path,
            //path_is_var,
            //path_assigned_at_base,
            //path_moved_at_base,
            //path_accessed_at_base,
            //known_placeholder_subset,
            //placeholder,
        ])
    }
    Ok(())
}

struct FactWriter<'w> {
    maps: &'w AtomMaps,
    dir: &'w path::Path,
}

impl<'w> FactWriter<'w> {
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
}

trait FactRow {
    fn write(
        &self,
        out: &mut dyn Write,
        maps: &AtomMaps,
    ) -> Result<(), Box<dyn Error>>;
}

impl<A, B> FactRow for (A, B)
where
    A: FactCell,
    B: FactCell,
{
    fn write(
        &self,
        out: &mut dyn Write,
        maps: &AtomMaps,
    ) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1])
    }
}

impl<A, B, C> FactRow for (A, B, C)
where
    A: FactCell,
    B: FactCell,
    C: FactCell,
{
    fn write(
        &self,
        out: &mut dyn Write,
        maps: &AtomMaps,
    ) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1, &self.2])
    }
}

impl<A, B, C, D> FactRow for (A, B, C, D)
where
    A: FactCell,
    B: FactCell,
    C: FactCell,
    D: FactCell,
{
    fn write(
        &self,
        out: &mut dyn Write,
        maps: &AtomMaps,
    ) -> Result<(), Box<dyn Error>> {
        write_row(out, maps, &[&self.0, &self.1, &self.2, &self.3])
    }
}

fn write_row(
    out: &mut dyn Write,
    maps: &AtomMaps,
    columns: &[&dyn FactCell],
) -> Result<(), Box<dyn Error>> {
    for (index, c) in columns.iter().enumerate() {
        let tail = if index == columns.len() - 1 { "\n" } else { "\t" };
        write!(out, "{:?}{}", c.to_string(maps), tail)?;
    }
    Ok(())
}

trait FactCell {
    fn to_string(&self, maps: &AtomMaps) -> String;
}

impl FactCell for Point {
    fn to_string(&self, maps: &AtomMaps) -> String {
        let (bb, idx, sub) = maps.get_point(*self);
        format!("{:?}({:?}[{}])", sub, bb, idx)
    }
}

/*
impl<A: Debug> FactCell for A {
    default fn to_string(&self, _maps: &AtomMaps) -> String {
        format!("{:?}", self)
    }
}

impl FactCell for LocationIndex {
    fn to_string(&self, maps: &AtomMaps) -> String {
        format!("{:?}", maps.to_location(*self))
    }
}
*/
