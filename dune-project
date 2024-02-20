(lang dune 3.8)

(using melange 0.1)

(using directory-targets 0.1)

(name subd)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name subd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(package
 (name twitch_hackery)
 (synopsis "Oh baby, we parsin' the html")
 (description "You do not wanna know what is happening here")
 (depends ocaml dune)
 (tags
  (topics "twitch")))

(package
 (name frontend)
 (synopsis "yayaya, making some frontend")
 (description "JS? Oh no, we don't do that here")
 (depends ocaml dune)
 (tags
  (topics "twitch")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
