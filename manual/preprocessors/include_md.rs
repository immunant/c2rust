/* Based on links.rs from mdbook, licensed under the Mozilla Public License,
 * version 2.0
 *
 * This file is subject to the terms of the Mozilla Public License, v. 2.0. If a
 * copy of the MPL was not distributed with this file, you can obtain one at
 * http://mozilla.org/MPL/2.0/.
 *
 * Original authors of mdbook: Mathieu David <mathieudavid@mathieudavid.org>",
 * "Michael-F-Bryan <michaelfbryan@gmail.com>", "Matt Ickstadt
 * <mattico8@gmail.com>
 */

extern crate clap;
extern crate env_logger;
#[macro_use(lazy_static)]
extern crate lazy_static;
#[macro_use(error, warn)]
extern crate log;
extern crate mdbook;
extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;
extern crate regex;
extern crate serde_json;

use std::io;
use std::path::{Path, PathBuf};
use std::process;

use clap::{App, Arg, ArgMatches, SubCommand};
use mdbook::book::{Book, BookItem};
use mdbook::errors::*;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use mdbook::utils::fs::file_to_string;
use mdbook::utils::normalize_id;
use pulldown_cmark::{Event, Parser, Tag};
use pulldown_cmark_to_cmark::fmt::cmark;
use regex::{CaptureMatches, Captures, Regex};

pub fn make_app() -> App<'static, 'static> {
    App::new("mdbook-links_ext")
        .about("An mdbook preprocessor to include markdown sections")
        .subcommand(
            SubCommand::with_name("supports")
                .arg(Arg::with_name("renderer").required(true))
                .about("Check whether a renderer is supported by this preprocessor"),
        )
}

fn main() {
    env_logger::init();

    let matches = make_app().get_matches();

    // Users will want to construct their own preprocessor here
    let preprocessor = LinkPreprocessor;

    if let Some(sub_args) = matches.subcommand_matches("supports") {
        handle_supports(&preprocessor, sub_args);
    } else {
        if let Err(e) = handle_preprocessing(&preprocessor) {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<()> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    if ctx.mdbook_version != mdbook::MDBOOK_VERSION {
        // We should probably use the `semver` crate to check compatibility
        // here...
        eprintln!(
            "Warning: The {} plugin was built against version {} of mdbook, \
             but we're being called from version {}",
            pre.name(),
            mdbook::MDBOOK_VERSION,
            ctx.mdbook_version
        );
    }

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

fn handle_supports(_pre: &dyn Preprocessor, _sub_args: &ArgMatches) -> ! {
    // All renderers are supported
    process::exit(0);
}

const ESCAPE_CHAR: char = '\\';
const MAX_LINK_NESTED_DEPTH: usize = 10;

/// A preprocessor for expanding the `{{# include .md}}` helpers in a chapter.
pub struct LinkPreprocessor;

impl LinkPreprocessor {
    pub(crate) const NAME: &'static str = "links_ext";

    /// Create a new `LinkPreprocessor`.
    pub fn new() -> Self {
        LinkPreprocessor
    }
}

impl Preprocessor for LinkPreprocessor {
    fn name(&self) -> &str {
        Self::NAME
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book> {
        let src_dir = ctx.root.join(&ctx.config.book.src);

        book.for_each_mut(|section: &mut BookItem| {
            if let BookItem::Chapter(ref mut ch) = *section {
                let base = ch
                    .path
                    .parent()
                    .map(|dir| src_dir.join(dir))
                    .expect("All book items have a parent");

                let content = replace_all(&ch.content, base, &ch.path, 0);
                ch.content = content;
            }
        });

        Ok(book)
    }
}

fn replace_all<P1, P2>(s: &str, path: P1, source: P2, depth: usize) -> String
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    // When replacing one thing in a string by something with a different length,
    // the indices after that will not correspond,
    // we therefore have to store the difference to correct this
    let path = path.as_ref();
    let source = source.as_ref();
    let mut previous_end_index = 0;
    let mut replaced = String::new();

    for link in find_links(s) {
        replaced.push_str(&s[previous_end_index..link.start_index]);

        match link.render_with_path(&path) {
            Ok(new_content) => {
                if depth < MAX_LINK_NESTED_DEPTH {
                    if let Some(rel_path) = link.link.relative_path(path) {
                        replaced.push_str(&replace_all(&new_content, rel_path, source, depth + 1));
                    } else {
                        replaced.push_str(&new_content);
                    }
                } else {
                    error!(
                        "Stack depth exceeded in {}. Check for cyclic includes",
                        source.display()
                    );
                }
                previous_end_index = link.end_index;
            }
            Err(e) => {
                error!("Error replacing \"{}\", {}", link.link_text, e);
                for cause in e.iter().skip(1) {
                    warn!("Caused By: {}", cause);
                }

                // This should make sure we include the raw `{{# ... }}` snippet
                // in the page content if there are any errors.
                previous_end_index = link.start_index;
            }
        }
    }

    replaced.push_str(&s[previous_end_index..]);
    replaced
}

#[derive(PartialEq, Debug, Clone)]
enum LinkType {
    Escaped,
    IncludeMDFile(PathBuf),
    IncludeMDSection(PathBuf, String),
}

impl LinkType {
    fn relative_path<P: AsRef<Path>>(self, base: P) -> Option<PathBuf> {
        let base = base.as_ref();
        match self {
            LinkType::Escaped => None,
            LinkType::IncludeMDFile(p) | LinkType::IncludeMDSection(p, _) => {
                Some(return_relative_path(base, &p))
            }
        }
    }
}
fn return_relative_path<P: AsRef<Path>>(base: P, relative: P) -> PathBuf {
    base.as_ref()
        .join(relative)
        .parent()
        .expect("Included file should not be /")
        .to_path_buf()
}

fn parse_include_path(path: &str) -> LinkType {
    let mut parts = path.split('#');
    let path = parts.next().unwrap().into();
    let section = parts.next();
    match section {
        None => LinkType::IncludeMDFile(path),
        Some(section) => LinkType::IncludeMDSection(path, section.to_string()),
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Link<'a> {
    start_index: usize,
    end_index: usize,
    link: LinkType,
    link_text: &'a str,
}

impl<'a> Link<'a> {
    fn from_capture(cap: Captures<'a>) -> Option<Link<'a>> {
        let link_type = match (cap.get(0), cap.get(1), cap.get(2)) {
            (_, Some(typ), Some(rest)) => {
                let mut path_props = rest.as_str().split_whitespace();
                let file_arg = path_props.next();

                match (typ.as_str(), file_arg) {
                    ("include_md", Some(pth)) => Some(parse_include_path(pth)),
                    _ => None,
                }
            }
            (Some(mat), None, None) if mat.as_str().starts_with(ESCAPE_CHAR) => {
                Some(LinkType::Escaped)
            }
            _ => None,
        };

        link_type.and_then(|lnk| {
            cap.get(0).map(|mat| Link {
                start_index: mat.start(),
                end_index: mat.end(),
                link: lnk,
                link_text: mat.as_str(),
            })
        })
    }

    fn render_with_path<P: AsRef<Path>>(&self, base: P) -> Result<String> {
        let base = base.as_ref();
        let read_file = |path: &PathBuf| {
            let target = base.join(path);

            file_to_string(&target).chain_err(|| {
                format!(
                    "Could not read file for link {} ({})",
                    self.link_text,
                    target.display()
                )
            })
        };

        match self.link {
            // omit the escape char
            LinkType::Escaped => Ok((&self.link_text[1..]).to_owned()),
            LinkType::IncludeMDFile(ref path) => read_file(path),
            LinkType::IncludeMDSection(ref path, ref section) => read_file(path).and_then(|s| {
                let mut found_heading = false;
                let mut section_level = None;
                let mut section_events = Parser::new(&s).filter(|event| {
                    match (event, section_level) {
                        (Event::Text(t), _) => {
                            if normalize_id(&t) == *section {
                                found_heading = true;
                            }
                        }
                        (Event::End(Tag::Header(level)), None) => {
                            if found_heading {
                                section_level = Some(*level);
                            }
                        }
                        (Event::Start(Tag::Header(level)), Some(old_level)) => {
                            if *level == old_level {
                                found_heading = false;
                            }
                        }
                        _ => (),
                    };
                    found_heading
                });

                let header_event = section_events.next().ok_or({
                    Error::from(format!(
                        "Could not find markdown heading {} in file {}",
                        section,
                        path.to_string_lossy()
                    ))
                })?;

                let header_level = match section_events.next() {
                    Some(Event::End(Tag::Header(level))) => level,
                    _ => panic!("Expected closing header tag"),
                };

                let header_events = vec![
                    Event::Start(Tag::Header(header_level)),
                    header_event,
                    Event::End(Tag::Header(header_level)),
                ];
                let section_events = header_events.into_iter().chain(section_events);

                let mut buf = String::new();
                cmark(section_events, &mut buf, None)
                    .map(|_| buf)
                    .map_err(|err| Error::from(format!("Markdown serialization failed: {}", err)))
            }),
        }
    }
}

struct LinkIter<'a>(CaptureMatches<'a, 'a>);

impl<'a> Iterator for LinkIter<'a> {
    type Item = Link<'a>;
    fn next(&mut self) -> Option<Link<'a>> {
        for cap in &mut self.0 {
            if let Some(inc) = Link::from_capture(cap) {
                return Some(inc);
            }
        }
        None
    }
}

fn find_links(contents: &str) -> LinkIter {
    // lazily compute following regex
    // r"\\\{\{#.*\}\}|\{\{#([a-zA-Z0-9]+)\s*([a-zA-Z0-9_.\-:/\\\s]+)\}\}")?;
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?x)                     # insignificant whitespace mode
            \\\{\{\#.*\}\}             # match escaped link
            |                          # or
            \{\{\s*                    # link opening parens and whitespace
            \#([a-zA-Z0-9_]+)          # link type
            \s+                        # separating whitespace
            ([a-zA-Z0-9\s_.\-:/\\\#]+) # link target path
            \s*\}\}                    # whitespace and link closing parens"
        )
        .unwrap();
    }
    LinkIter(RE.captures_iter(contents))
}
