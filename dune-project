(lang dune 3.10)

(name cbe_cash_plot)

(generate_opam_files true)

(source
 (github frectonz/cbe_cash_plot))

(authors "frectonz")

(maintainers "frectonz")

(license MIT)

(package
 (name cbe_cash_plot)
 (synopsis "Generate a plot of your CBE balance overtime.")
 (depends ocaml dune xml-light gnuplot))

