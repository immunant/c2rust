//! Simple program for testing multi-statement glob matching and rewriting.  Best used with
//! `test_replace_stmts`, which takes a pattern and replacement as string arguments.  Examples:
//!
//!     __m_stmts;  ->  __m_stmts;
//!     __m_stmts; __s;  ->  __m_stmts;
//!     __m_stmts; __s;  ->  __s; __m_stmts;
//!     __s; __m_stmts;  ->  __m_stmts; __s;
//!     __s1; __m_stmts; __s2;  ->  __s1; __m_stmts; __s2;
//!     __m_stmts1; y = __e; __m_stmts2;  ->  __m_stmts1; __m_stmts2;
//!     __m_stmts1; __s; __m_stmts2;  ->  __s


fn main() {
    let mut x = 0;
    let mut y = 0;
    let mut z = 0;
    let mut other = 0;

    {
        x = 1;
        y = 1;
        z = 1;
    }
}
