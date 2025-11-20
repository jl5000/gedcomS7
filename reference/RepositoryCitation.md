# Create a repository citation object

Create a repository citation object

## Usage

``` r
RepositoryCitation(
  repo_xref = "@VOID@",
  notes = list(),
  note_xrefs = character(0),
  call_numbers = list()
)
```

## Arguments

- repo_xref:

  The cross-reference identifier of a repository record. If the
  repository does not have a record, then this can be left blank and a
  void xref will be used. However, you should describe the repository in
  @notes.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- call_numbers:

  Call number(s) used to file and retrieve items from the repository.
  This can either be a `SourceCallNumber` object, a list of them, or a
  character vector of call numbers.

## Value

An S7 object representing a GEDCOM SOURCE_REPOSITORY_CITATION.
